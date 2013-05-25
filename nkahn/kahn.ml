open Unix
open Client
open Server

type 'a process = 'a Server.process

type 'a in_port = int

type 'a out_port = int

let new_channel () =
  send chan New_channel;
  let c = (recv chan:int) in
  c,c

let put x i =
  Proc (fun n ->
    send chan (Put (i,Marshal.to_string x [Marshal.Closures]));
    Res ((),n))

let rec get o =
  let rec ask = function
    | 0 -> get o
    | n ->
    send chan (Get o);
    match (recv chan:string option) with
      | Some x -> Res ((Marshal.from_string x 0),n)
      | None -> ask (n-1)
  in
  Proc ask

let return x = Proc (fun n -> Res (x,n))

let doco l = Doco (l,return ())

let rec bind p q = match p with
  | Proc f ->
      Proc (
        fun n -> match f (n-1) with
          | Res (x,m) ->
              begin
                match q x with
                  | Proc g as r -> if m < 2 then r else g m
                  | Doco (_,_) as q732 -> q732
                  | Res _ -> assert false
              end
          | Doco (l,p789) -> Doco (l, bind p789 q)
          | p2342 -> bind p2342 q
       )
  | Doco (l,p789) -> Doco (l,bind p789 q)
  | Res _ -> assert false

let rec run = function
  | Res (x,_) -> Printf.printf "End\n%!"; x
  | Doco (l,p) ->
      new_task_list l (Res((),0)(*dummy*)) (-1);
      while not (Queue.is_empty tasks.fifo) do
        sleep 1
      done;
      run p
  | Proc p -> run (p max_int)
