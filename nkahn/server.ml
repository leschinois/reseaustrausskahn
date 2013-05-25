open Unix
open Queue

exception Break

type 'a process =
  | Proc of (int -> 'a process)
  | Doco of (unit process list*'a process)
  | Res of ('a*int)

(* Client to server *)
type request =
  | Put of int*string
  | Get of int
  | New_channel
  | Return of unit process

type 'a queue = { fifo:'a t; mut:Mutex.t}
type 'a port = 'a queue

type io_channel = {
  origin:file_descr;
  in_chan:in_channel;
  out_chan:out_channel;
}

let time_out = 10

let string_of_sockaddr = function
  | ADDR_UNIX s -> s
  | ADDR_INET (i,p) -> Printf.sprintf "%s:%d" (string_of_inet_addr i) p

let sleep_float t =
  let timeout = gettimeofday () +. t in
  let rec s t =
    try ignore (select [] [] [] t)
    with
      | Unix_error (EINTR,"select","") ->
          let now = gettimeofday () in
          let rem = timeout -. now in
          if rem > 0. then s rem
  in s t
      

(* We keep port as a reference because we will find the first available
 * port and need its value in the future *)
let server_name,port =
  let port = ref 10000 in
  let server_name = ref "" in
  let spec =
    [ "-s", Arg.Set_string server_name, "Choose server";
      "-p", Arg.Set_int port, "Choose port"; ]
  in
  let usage = "UUUU" in
  Arg.parse spec (fun _ -> ()) usage;
  (!server_name,port)

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

let tasks = create ()

let ports = Hashtbl.create 100

let fresh_create () =
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

let tn_mutex = Mutex.create ()
let tasknumber = Hashtbl.create 100

(* Assignate a value to new task list
 * and add it to queue tasks *)
let new_task_list l p k =
  if l <> [] then
    begin
      let k2 = fresh_task () in
      Mutex.lock tn_mutex;
      Hashtbl.add tasknumber k2 (List.length l,p,k);
      Mutex.unlock tn_mutex;
      List.iter (fun q -> push (q,k2) tasks) l
    end
  else
    push (p,k) tasks

let task_out k =
  Mutex.lock tn_mutex;
  let m,p,k2 = Hashtbl.find tasknumber k in
  if m=1 then
    begin
      Hashtbl.remove tasknumber k;
      if k > -1 (* i.e. not a primitive task *) then
        push (p,k2) tasks
    end
    else
      Hashtbl.replace tasknumber k (m-1,p,k2);
    Mutex.unlock tn_mutex

let unpack k = function
  | Proc _ as p -> push (p,k) tasks
  | Doco (l,p) -> new_task_list l p k
  | Res ((),_) -> task_out k

let track chan k =
  let rec deal () =
      Thread.yield ();
      match recv chan with
      | Put (i,x) -> push x (Hashtbl.find ports i); deal ()
      | Get i -> send chan (pop (Hashtbl.find ports i)); deal ()
      | New_channel ->
          let c = fresh_chan () in
          Hashtbl.add ports c (create ());
          send chan c;
          deal ()
      | Return p -> unpack k p
  in
  deal ()

let deal_with chan p k =
  send chan p;
  track chan k

let client_handler s =
  let chan = channel_of_descr s in
  let rec handler () =
    begin
      match pop tasks with
        | Some (p,k) ->
            begin
              try deal_with chan p k
              with End_of_file ->
                push (p,k) tasks;
                close_c chan;
                Thread.exit ()
            end
        | None -> Thread.yield ()
    end;
    handler ()
  in handler ()

let rec accepter s =
  let s1,a = accept s in
  Printf.printf "Connected to %s\n%!" (string_of_sockaddr a);
  ignore (Thread.create client_handler s1);
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
      | Unix_error (EADDRINUSE,"bind","") ->
        incr port
  done;
  listen s 10;
  (* First connection *)
  let first_connect () =
    let s1,_ = accept s in
    let chan = channel_of_descr s1 in
    track chan (-1)
  in
  let _ = Thread.create first_connect () in
  let _ = Thread.create accepter s in
  ()

let () = if server_name = "" then server ()
