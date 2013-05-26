open Unix
open Queue

type 'a process = int -> 'a result
and 'a result =
  | Proc of 'a process
  | Doco of unit process list * 'a process
  | Res of 'a * int
  | U

(* Client to server *)
type request =
  | Put of int * string
  | Get of int
  | New_channel
  | Return of unit result

type response =
  | Elt of string
  | Get_resp of string option
  | Chan of int

exception Unfinished of unit process * int * (response list * int)
exception Break

type past = response list * int
let no_past:past = [],0

let no_past = [],0

type 'a queue = { fifo : 'a t; mut : Mutex.t}

type io_channel = {
  origin : file_descr;
  in_chan : in_channel;
  out_chan : out_channel;
}

let elapsed =
  let start = time () in
  fun () -> time () -. start

(* We keep port as a reference because we will find the first available
 * port and need its value in the future *)
let server_name,port,time_out,timer =
  let port = ref 10000 in
  let server_name = ref "" in
  let time_out = ref 1000 in
  let timer = ref (-1.) in
  let spec =
    [ "-s", Arg.Set_string server_name, "Choose server";
      "-p", Arg.Set_int port, "Choose port";
      "-t", Arg.Set_int time_out,
        "Maximum number of bindings processed before returning to server";
      "-time", Arg.Set_float timer,
        "Ends client ASAP after the specified time in seconds.\
        A negative value deactivates the timer (default)."; ]
  in
  let usage = "UUUU" in
  Arg.parse spec (fun _ -> ()) usage;
  (!server_name,port,!time_out,!timer)

let string_of_sockaddr = function
  | ADDR_UNIX s -> s
  | ADDR_INET (i,p) -> Printf.sprintf "%s:%d" (string_of_inet_addr i) p

let channel_of_descr s = {
  origin=s;
  in_chan=in_channel_of_descr s;
  out_chan=out_channel_of_descr s;
}

let close_c {origin=s} = close s

let send {out_chan=out_chan} x =
  Marshal.to_channel out_chan x [Marshal.Closures];
  flush out_chan

let recv {in_chan=in_chan} =
  Marshal.from_channel in_chan

let create () = {fifo=create();mut=Mutex.create()}

let push x {fifo=fifo;mut=mut} =
  Mutex.lock mut;
  push x fifo;
  Mutex.unlock mut

let pop {fifo=fifo;mut=mut} =
  Mutex.lock mut;
  try
    let y = pop fifo in
    Mutex.unlock mut;
    Some y
  with Empty -> Mutex.unlock mut; None

let tasks : (unit process * int * past) queue = create ()

let ports : (int,string queue) Hashtbl.t = Hashtbl.create 100

(* Fresh secure counter *)
let fresh_create () : unit -> int =
  let fresh_mutex = Mutex.create () in
  let fresh = ref 0 in
  fun () ->
    Mutex.lock fresh_mutex;
    let x = !fresh in
    incr fresh;
    Mutex.unlock fresh_mutex;
    x

let fresh_chan = fresh_create ()
let fresh_task = fresh_create ()

(* Keep track of task tree *)
let tn_mutex = Mutex.create ()
let tasknumber : (int,int*unit process*int) Hashtbl.t= Hashtbl.create 100

(* Assignate a value to new task list
 * and add it to queue tasks *)
let new_task_list l p k =
  if l <> [] then
    begin
      let k2 = fresh_task () in
      Mutex.lock tn_mutex;
      Hashtbl.add tasknumber k2 (List.length l,p,k);
      Mutex.unlock tn_mutex;
      List.iter (fun q -> push (q,k2,no_past) tasks) l
    end
  else
    push (p,k,no_past) tasks

let task_out k =
  Mutex.lock tn_mutex;
  let m,p,k2 = Hashtbl.find tasknumber k in
  if m=1 then
    begin
      Hashtbl.remove tasknumber k;
      if k2 > -1 (* i.e. not a primitive task *) then
        push (p,k2,no_past) tasks
    end
    else
      Hashtbl.replace tasknumber k (m-1,p,k2);
    Mutex.unlock tn_mutex

let unpack k past = function
  | Proc p -> push (p,k,past) tasks
  | Doco (l,p) -> new_task_list l p k
  | U -> task_out k
  | Res _ -> assert false

let track chan k (to_send,to_recv) add_sent add_rcvd =
(*
  let wait_proc {origin=s} t =
    let timeout = gettimeofday () +. t in
    let rec wait_p t =
      try select [s] [] [] t
      with Unix_error (EINTR,"select","") ->
        let rem = timeout -. gettimeofday () in
        if rem > 0. then wait_p rem
        else [],[],[]
    in
    let r,_,_ = wait_p t in
    r = []
  in*)
  let rec deal () =
      Thread.yield ();
      match recv chan with
      | Put (i,x) ->
          add_rcvd (Elt x);
          push x (Hashtbl.find ports i);
          deal ()
      | Get i ->
          let y = pop (Hashtbl.find ports i) in
          add_sent (Get_resp y);
          send chan y;
          deal ()
      | New_channel ->
          let c = fresh_chan () in
          Hashtbl.add ports c (create ());
          add_sent (Chan c);
          send chan c;
          deal ()
      | Return p -> 
          unpack k no_past p
  in
  let rec redeal l y =
    if l = [] && y = 0 then
      deal ()
    else begin
      Thread.yield ();
      match recv chan,l with
      | Put (_,_),_ when y > 0 -> redeal l (y-1)
      | Get _,Get_resp s::t ->
          send chan s;
          redeal t y
      | New_channel,Chan c::t ->
          send chan c;
          redeal t y;
      | Return p,_ ->
          unpack k (l,y) p
      | _,_ ->
          Printf.eprintf "Unexpected replay. \
            Probably encountered unrecommended operations.";
          exit 2
    end
  in
  redeal to_send to_recv

let deal_with chan p k r =
  let sent = ref [] in
  let rcvd = ref 0 in
  let add_sent x = sent := x :: !sent in
  let add_rcvd _ = incr rcvd in
  try
    send chan (p:unit process);
    track chan k r add_sent add_rcvd
  with _ -> raise (Unfinished (p,k,(List.rev !sent,!rcvd)))

let client_handler s a =
  let self = Thread.id (Thread.self ()) in
  let addr = string_of_sockaddr a in
  let chan = channel_of_descr s in
  let disconnected () =
    Printf.printf "%3.0f:%d:%s disconnected\n%!" (elapsed ()) self addr
  in
  let rec handler () =
    if recv chan then begin
      begin
        match pop tasks with
          | Some (p,k,r) ->
              begin
                try
                  send chan true;
                  deal_with chan p k r
                with
                | Unfinished (p,k,r) ->
                    push (p,k,r) tasks;
                    close_c chan;
                    disconnected ();
                    Thread.exit ()
              end
          | None -> send chan false; Thread.delay 0.1
      end;
      handler () end
  in
  try
    Printf.printf "%3.0f:%d:Connected to %s\n%!" (elapsed ()) self addr;
    handler ()
  with _ -> disconnected ()

let rec accepter s =
  let s1,a = accept s in
  let _ = Thread.create (client_handler s1) a in
  accepter s

let server () =
  let hostname = gethostname () in
  let host = gethostbyname hostname in
  let hostiaddr = host.h_addr_list.(0) in
  let s = Unix.socket PF_INET SOCK_STREAM 0 in
  let cont = ref true in
  while !cont do
    let init_addr = ADDR_INET (hostiaddr,!port) in
    try
      bind s init_addr;
      Printf.printf "Port:%d\n%!" !port;
      cont := false
    with
    | Unix_error (EADDRINUSE,"bind","") -> incr port
  done;
  listen s 10;
  (* First connection *)
  let first_connect () =
    let s1,_ = accept s in
    let chan = channel_of_descr s1 in
    ignore (Thread.create accepter s);
    track chan (-1) no_past (fun _ -> ()) (fun _ -> ())
  in
  let _ = Thread.create first_connect () in
  ()

let () = if server_name = "" then server ()

(* Connect to server *)
let chan =
  try
    let serv_name = if server_name="" then gethostname() else server_name in
    let serv = gethostbyname serv_name in
    let serv_addr = serv.h_addr_list.(0) in
    let s = socket PF_INET SOCK_STREAM 0 in
    connect s (ADDR_INET (serv_addr,!port));
    channel_of_descr s
  with
  | Unix_error (ECONNREFUSED,"connect","") ->
      Printf.eprintf "Server not found";
      exit 1
