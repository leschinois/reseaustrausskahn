open Unix
open Server

let elapsed =
  let start = gettimeofday () in
  fun () -> gettimeofday () -. start

let ret = function
  | Proc _
  | Doco _ as p -> Return p
  | Res ((),_) -> Return U
  | U -> assert false

let client () =
  try
    let notime = timer < 0. in
    while (notime || elapsed ()<timer) do
      send chan true;
      if recv chan then begin
        let p = (recv chan : unit process) in
        let res = p time_out in
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
