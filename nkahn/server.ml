open Client
open Queue

type 'a port = { fifo:'a t ; mut:Mutex.t}
type 'a queue = 'a port

let create () = {fifo=create();mut=Mutex.create()}

let tasks = create ()

let push x port =
  Mutex.lock port.mut;
  push x port.fifo;
  Mutex.unlock port.mut

let pop port =
  Mutex.lock port.mut;
  try
    let y = pop port.fifo in
    Mutex.unlock port.mut;
    Some y
  with Empty -> Mutex.unlock port.mut; None

let client_handler s =
  let in_chan = in_channel_of_descr s in
  let out_chan = out_channel_of_descr s in
  let rec handler () =
    begin
      match pop tasks with
        | Some p -> send out_chan p
        | None -> Thread.yield ()
    end;
    handler ()
  in handler ()

let rec accepter s =
  let s1,_ = accept s in
  ignore (Thread.create client_handler s1);
  accepter s

let server () =
  let hostname = gethostname () in
  let host = gethostbyname hostname in
  let hostiaddr = host.h_addr_list.(0) in
  let s = socket PF_INET SOCK_STREAM 0 in
  let port = ref port in
  while !port > 0 do
    let init_addr = ADDR_INET (hostiaddr,!port) in
    try
      bind s init_addr;
      Printf.printf "Port:%d\n%!" !port;
      port := 0
    with
      | Unix_error (EADDRINUSE,"bind","") ->
        incr port
  done;
  listen s 10;
  let t = Thread.create accepter s in
  (**)
  Thread.kill t
