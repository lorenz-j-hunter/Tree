class ['a] stack = object (self)
  val mutable l = ([] : 'a list)

  method peek () = List.hd l
  method pop () =
    let result = List.hd l in
    l <- List.tl l; result
  method push item = l <- item :: l;
  method is_empty () = (List.length l = 0)
  method size () = List.length l
end
