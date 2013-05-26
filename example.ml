module Example (K : I.S) = struct
  module K = K
  module Lib = I.Lib(K)
  open Lib

  let bound = 100000

  let integers (qo : int K.out_port) : unit K.process =
    let rec loop n =
      (K.put n qo) >>= (fun () ->
        if n = bound then
          K.put 0 qo
        else loop (n + 1))
    in
    loop 2

  let output (qi : int K.in_port) : unit K.process =
    let rec loop () =
      (K.get qi) >>= (fun v -> Format.printf "\r%d@?" v;
        if v > 0 then loop () else K.return ())
    in
    loop ()

  let main : unit K.process =
    (delay K.new_channel ()) >>=
    (fun (q_in, q_out) -> K.doco [ integers q_out ; output q_in ; ])

end

module E = Example(Kahn)

let () =
  let start = Unix.time () in
  E.K.run E.main;
  Printf.printf "Time: %.0fs\n%!" (Unix.time ()-.start)
