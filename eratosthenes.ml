module Example (K : I.S) = struct
  module K = K
  module Lib = I.Lib(K)
  open Lib

  let integers (qo : int K.out_port) : unit K.process =
    let rec loop n =
      if n = 100 then K.put 0 qo
      else (K.put n qo) >>= (fun () -> loop (n + 1))
    in
    loop 2

  let filter n qi qo =
    let rec loop () =
      (K.get qi) >>=
      (fun x ->
        if x = 0 then K.put 0 qo
        else if x mod n = 0 then loop ()
        else (K.put x qo) >>= (fun () -> loop ()))
    in loop ()

  let eratosthenes qi qo =
    let rec loop q =
      (K.get q) >>= (fun x ->
        (K.put x qo) >>= (fun () ->
          if x = 0 then
            K.return ()
          else
            (delay K.new_channel ()) >>= (fun (q1,q2) ->
              (K.doco [
                filter x q q2;
                loop q1;
              ])
            )
        )
      )
    in loop qi

  let output (qi : int K.in_port) : unit K.process =
    let rec loop () =
      (K.get qi) >>= (fun v ->
        Format.printf "%d@." v;
        if v = 0 then K.return ()
        else loop ())
    in
    loop ()

  let main : unit K.process =
    (delay K.new_channel ()) >>= (fun (q_in, q_out) ->
      (delay K.new_channel ()) >>= (fun (q0_in,q0_out) ->
        K.doco [ integers q0_out; eratosthenes q0_in q_out; output q_in; ]
      )
    )

end

module E = Example(Kahn)

let () =
  let start = Unix.time () in
  E.K.run E.main;
  Printf.printf "Time: %.0fs\n%!" (Unix.time ()-.start)
