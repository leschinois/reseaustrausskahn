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

let rec bind p q =
  match p with
  | Proc f ->
      Proc (
        fun n ->
          match f (n-1) with
          | Res (x,m) ->
              begin
                match q x with
                | Proc g as r -> if m > 0 then g m else r
                | Doco (_,_) as q732 -> q732
                | Res _ -> assert false
              end
          | Doco (l,p789) -> Doco (l, bind p789 q)
          | p2342 -> bind p2342 q
       )
  | Doco (l,p789) -> Doco (l,bind p789 q)
  | Res _ -> assert false

let run p =
  let rec run = function
    | Res (x,_) -> x
    | Doco (l,p) ->
        if l <> [] then begin
          new_task_list l (Res((),0)(*dummy*)) (-1);
          while (Mutex.lock tn_mutex; 0 <> Hashtbl.length tasknumber) do
            Mutex.unlock tn_mutex;
            sleep 1
          done;
          Mutex.unlock tn_mutex
        end;
        run p
    | Proc p -> run (p max_int)
  in
  Printf.printf "Run: start\n%!";
  let x = run p in
  Printf.printf "Run: end. Time %.1fs\n%!" (elapsed ());
  x
