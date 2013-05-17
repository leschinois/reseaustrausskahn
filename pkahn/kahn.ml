open Unix

type 'a process = (unit -> 'a)

type 'a in_port = in_channel
type 'a out_port = out_channel

let new_channel () = 
  let (in_pipe,out_pipe) = pipe () in
  (in_channel_of_descr in_pipe, out_channel_of_descr out_pipe)

let put x out_chan () = 
  Marshal.to_channel out_chan x [Marshal.Closures];
  flush out_chan

let get in_chan () = 
  Marshal.from_channel in_chan

let fork f = 
begin match fork () with
  | 0 -> f (); exit 0
  | n -> n
end 

let doco liste () = 
  let pid = List.map fork liste in
  for i = 1 to List.length pid do
    ignore (wait ())
  done

let return x () = x

let bind p q () = q (p ()) ()

let run f = f ()  

