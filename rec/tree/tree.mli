type 'a node = {value : 'a; index : int; subtrees : 'a node list}

type 'a tree = 
  | Leaf
  | Node of 'a node * 'a tree list * int

val root_ : ?bf:int -> 'a -> 'a tree

val preorder : 'a tree -> 'b tree Stack.stack ref -> unit 