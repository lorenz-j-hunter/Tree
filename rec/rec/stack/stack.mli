class ['a] stack : object 
  val mutable l: 'a list

  method peek: unit -> 'a 
  method pop: unit -> 'a 
  method push: 'a -> unit 
  method is_empty: unit -> bool
  method size: unit -> int
end