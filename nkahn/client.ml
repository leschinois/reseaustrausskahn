open Unix
open Server

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

let elapsed =
  let start = gettimeofday () in
  fun () -> gettimeofday () -. start

let client () =
  try
    let notime = timer < 0. in
    while (notime || elapsed ()<timer) do
      send chan true;
      match (recv chan : unit process) with
        | Proc p -> send chan (Return (p time_out))
        | Doco (_,_) as p -> send chan (Return p)
        | Res _ -> assert false
    done;
    send chan false;
    exit 0
  with
  | End_of_file
  | Sys_error _
  | Unix_error (ECONNRESET,"",_) -> exit 0

let () =
  if server_name <> "" then client ()
