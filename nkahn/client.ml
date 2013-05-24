open Unix

let server,port =
  let port = ref 10000 in
  let server = ref "" in
  let spec =
    [ "-s", Arg.Set_string server, "Choose server";
      "-p", Arg.Set_int port, "Choose port"; ]
  in
  let usage = "UUUU" in
  Arg.parse spec (fun _ -> ()) usage;
  (!server,!port)

(* Client to server *)
type request =
  | Put of int*string
  | Get of int
  | New_channel
  | Return of unit process

(* Se connecter au serveur *)
let socket,in_chan,out_chan =
  let serv = gethostbyname server in
  let serv_addr = serv.h_addr_list.(0) in
  let s = socket PF_INET SOCK_STREAM 0 in
  connect s (ADDR_INET (serv_addr,port));
  let i_c = in_channel_of_descr s in
  let o_c = out_channel_of_descr s in
  s,i_c,o_c

let send out_chan x =
  Marshal.to_channel out_chan x [Marshal.Closures];
  flush out_chan

let recv in_chan =
  Marshal.from_channel in_chan

let time_out = 1000000

let client () =
  try
    while true do
      match (recv in_chan : unit process) with
        | Proc p -> send (p time_out)
        | Doco (_,_) as p -> send p
        | Res _ -> assert false
    done
  with Unix_error (_,_,_) -> exit 0
