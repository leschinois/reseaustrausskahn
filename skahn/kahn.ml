(* Sequential implementation of Kahn networks *)

(* Running a process makes it progress by one time step *)
(* If the process has not yet terminated, return the remaining computation *)
(* If it has terminated, return value *)
type 'a process = unit -> 'a result
and 'a result =
  | Proc of 'a process
  | Res of 'a

type 'a in_port = 'a Queue.t
type 'a out_port = 'a Queue.t

let new_channel () =
  let q = Queue.create () in
  q,q

let put a i () = Res (Queue.push a i)

let rec get o () =
  try
    Res (Queue.pop o)
  with Queue.Empty -> Proc (get o)

(* All processes in the list progress at the same speed *)
let rec doco l =
  let rec exec = function
    | [] -> []
    | p::t ->
          match p () with
            | Proc q -> q::exec t
            | Res () -> exec t
  in fun () ->
    match exec l with
      | [] -> Res ()
      | l -> Proc (doco l)

let return a () = Res a

let rec bind p q () =
  match p () with
    | Res a -> Proc (q a)
    | Proc p_ -> Proc (bind p_ q)

let rec run p = match p () with
  | Res a -> a
  | Proc q -> run q
