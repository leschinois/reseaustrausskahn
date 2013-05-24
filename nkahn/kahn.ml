open Unix
open Client

type 'a process =
  | Proc of int -> 'a process
  | Doco of unit process list*'a process
  | Res of ('a,int)

type 'a in_port = int

type 'a out_port = int

let new_channel () =
  send out_chan New_channel;
  let c = recv in_chan in
  c,c

let put x i =
  Proc (fun n ->
    send out_chan (Put (i,Marshal.to_string x [Marshal.Closures]));
    Res ((),n))

let rec get o =
  let rec ask = function
    | 0 -> get o
    | n ->
    send out_chan (Get o);
    match recv in_chan with
      | Some x -> Res (x,n)
      | None -> ask (n-1)
  in
  Proc ask

let doco l = Doco (l,return ())

let return x = Proc (fun n -> Res (x,n))

let bind p q = match p with
  | Proc f ->
      Proc (
        fun n -> match f (n-1) with
          | Res (x,m) ->
              begin
                match q x with
                  | Proc g as r -> if m < 2 then r else g m
                  | Doco (_,_) as q732 -> q732
                  | Res _ -> assert false
              else
              end
          | Doco (l,p789) -> Doco (l, bind p789 q)
          | p2342 -> bind p2342 q
       )
  | Doco (l,p789) -> Doco (l,bind p789 q)
  | Res _ -> assert false

let rec run = function
  | Res (x,_) -> x
  | Doco (l,p) -> failwith "TODO"
  | Proc p -> run (p max_int)
