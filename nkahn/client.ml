open Unix
open Server

(* Se connecter au serveur *)
let socket,in_chan,out_chan =
  let serv_name = if server_name="" then gethostname() else server_name in
  let serv = gethostbyname serv_name in
  let serv_addr = serv.h_addr_list.(0) in
  let s = socket PF_INET SOCK_STREAM 0 in
  connect s (ADDR_INET (serv_addr,!port));
  let i_c = in_channel_of_descr s in
  let o_c = out_channel_of_descr s in
  s,i_c,o_c

let client () =
  try
    while true do
      print_endline "Hi";
      match (recv in_chan : unit process) with
        | Proc p -> print_endline "Bye"; send out_chan (Return (p
time_out))
        | Doco (_,_) as p -> print_endline "Be"; send out_chan (Return
p)
        | Res _ -> assert false
    done
  with Unix_error (_,_,_) -> exit 0

let () =
  if server_name <> "" then client ()
