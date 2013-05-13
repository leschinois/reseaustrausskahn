module Switch (K : I.S) = struct
  module Lib = I.Lib(K)
  open Lib

  let switch n q1 q2 =
    let rec loop n (q1,q2) =
      (K.put (n land 1 = 0) q1) >>=
        (fun () -> 
          if n = 1 then K.return () else loop (n-1) (q2,q1))
    in
    loop n (q1,q2)
  
  let output n q =
    let rec loop n =
      (K.get q) >>=
        (fun x ->
          Printf.printf "%d" (if x then 1 else 0);
          if n = 1 then K.return () else loop (n-1))
    in
    loop n

  let main n =
    (delay (fun () -> (K.new_channel (),K.new_channel ())) ()) >>=
    (fun ((q11,q12),(q21,q22)) ->
      K.doco [switch n q12 q22;output ((n+1)/2) q11;output (n/2) q21])
end

module E = Switch(Kahn)

let () =
  Kahn.run (E.main 11);
  Printf.printf "\n"
