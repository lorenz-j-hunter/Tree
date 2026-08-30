class ['a] stack = object
  val mutable l = ([] : 'a list) 

  method pop () =
    match l with
    | hd :: tl -> l <- tl; hd
    | [] -> failwith "pop: can't pop an empty stack" 
  method peek () =
    match l with
    | hd :: tl -> hd 
    | [] -> failwith "peek: can't peek an empty stack"
  method push v = l <- v :: l
  method get_size () = List.length l
end