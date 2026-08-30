class ['a] stack = object
  val mutable l : 'a list

  method pop : unit -> 'a
  method peek : unit -> 'a
  method push : 'a -> unit
  method get_size : unit -> int
end