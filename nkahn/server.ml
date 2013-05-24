open Unix
open Queue

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

type 'a queue = { fifo:'a t ; mut:Mutex.t}
type 'a port = 'a queue

let time_out = 10

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

let send out_chan x =
  Marshal.to_channel out_chan x [Marshal.Closures];
  flush out_chan

let recv in_chan =
  Marshal.from_channel in_chan

let create () = {fifo=create();mut=Mutex.create()}

let push x queue =
  Mutex.lock queue.mut;
  push x queue.fifo;
  Mutex.unlock queue.mut

let pop queue =
  Mutex.lock queue.mut;
  try
    let y = pop queue.fifo in
    Mutex.unlock queue.mut;
    Some y
  with Empty -> Mutex.unlock queue.mut; None

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

let fresh = fresh_create ()
let fresh2 = fresh_create ()

let tn_mutex = Mutex.create ()
let tasknumber = Hashtbl.create 100

let unpack k = function
  | Proc _ as p -> push (p,k) tasks
  | Doco (l,p) ->
      let k2 = fresh2 () in
      Mutex.lock tn_mutex;
      Hashtbl.add tasknumber k2 (List.length l,p,k);
      Mutex.unlock tn_mutex;
      List.iter (fun q -> push (q,k2) tasks) l
  | Res ((),_) ->
      Mutex.lock tn_mutex;
      let m,p,k2 = Hashtbl.find tasknumber k in
      if m = 1 then
        begin
          Hashtbl.remove tasknumber k;
          if k2 > -1 then push (p,k2) tasks
        end
      else
        Hashtbl.replace tasknumber k (m-1,p,k2);
      Mutex.unlock tn_mutex

let rec deal_with k i_c o_c =
  let cont = ref true in
  begin
    match recv i_c with
      | Put (i,x) -> push x (Hashtbl.find ports i)
      | Get i -> send o_c (pop (Hashtbl.find ports i))
      | New_channel -> Hashtbl.add ports (fresh ()) (create ());
          send o_c (fresh ())
      | Return p -> unpack k p; cont := false
  end;
  Thread.yield ();
  if !cont then deal_with k i_c o_c

let client_handler s =
  let in_chan = in_channel_of_descr s in
  let out_chan = out_channel_of_descr s in
  let rec handler () =
    begin
      match pop tasks with
        | Some (p,k) -> send out_chan p;
            deal_with k in_chan out_chan
        | None -> Thread.yield ()
    end;
    handler ()
  in handler ()

let string_of_sockaddr = function
  | ADDR_UNIX s -> s
  | ADDR_INET (i,p) -> Printf.sprintf "%s:%d" (string_of_inet_addr i) p

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
  let first_call () =
    let s1,_ = accept s in
    let i_c = in_channel_of_descr s1 in
    let o_c = out_channel_of_descr s1 in
    deal_with (-1) i_c o_c
  in
  let _ = Thread.create first_call () in
  let _ = Thread.create accepter s in
  ()

let () = if server_name = "" then server ()
