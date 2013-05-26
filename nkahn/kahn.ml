open Unix
open Server

type 'a process = 'a Server.process

type 'a in_port = int

type 'a out_port = int

let new_channel () =
  send chan New_channel;
  let c = (recv chan:int) in
  c,c

let put x i n =
  send chan (Put (i,Marshal.to_string x [Marshal.Closures]));
  Res ((),n)

let rec get o n =
  send chan (Get o);
  match (recv chan:string option) with
  | Some x -> Res ((Marshal.from_string x 0),n)
  | None -> Proc (get o)

let return x n = Res (x,n)

let doco l n = Doco (l,return ())

let rec bind p q = function
  | 0 -> Proc (bind p q)
  | n ->
      match p (n-1) with
      | Proc p1 -> Proc (bind p1 q)
      | Doco (l,p789) -> Doco (l, bind p789 q)
      | Res (x,m) -> q x m
      | U -> assert false

let rec run p =
  match p max_int with
  | Doco (l,p) ->
      if l <> [] then begin
        new_task_list l (fun _ -> U(*dummy*)) (-1);
        while (Mutex.lock tn_mutex; 0 <> Hashtbl.length tasknumber) do
          Mutex.unlock tn_mutex;
          sleep 1
        done;
        Mutex.unlock tn_mutex
      end;
      run p
  | Proc q -> run q
  | Res (x,_) -> x
  | U -> assert false
