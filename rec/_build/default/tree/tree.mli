type 'a node = {value : 'a; index : int; subtrees : 'a node list}

type 'a tree = 
  | Leaf
  | Node of 'a node * 'a tree list * int

val fill : 'a -> 'a list -> int -> 'a list

val executeall : ('a -> 'b) -> 'a list -> 'b Stack.stack ref -> unit 

val root_ : ?bf:int -> 'a -> 'a tree

val preorder : 'a tree -> 'b tree Stack.stack ref -> unit 