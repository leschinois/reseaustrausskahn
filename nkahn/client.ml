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

let ret = function
  | Proc _
  | Doco _ as p -> Return p
  | Res ((),_) -> Return Unit
  | Unit -> assert false

let client () =
  try
    let notime = timer < 0. in
    while (notime || elapsed ()<timer) do
      send chan true;
      if recv chan then begin
        let p = (recv chan : unit process) in
        let res = p time_out in
        Printf.eprintf "a\n%!";
        send chan (ret res)
      end
    done;
    send chan false;
    exit 0
  with
  | End_of_file
  | Sys_error _
  | Unix_error (ECONNRESET,"",_) -> exit 0

let () =
  if server_name <> "" then client ()
