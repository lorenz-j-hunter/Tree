type 'a node = {value : 'a; index : int; subtrees : 'a node list}

type 'a tree = 
  | Leaf
  | Node of 'a node * 'a tree list * int

val fill : 'a -> 'a list -> int -> 'a list

val root_ : ?bf:int -> 'a -> 'a tree

type 'a action =
  | Insert of 'a
  | Remove of int
  | Search of int
  | Test 

val preorder : 'a action -> 'a tree -> ('a tree * int) Stack.stack ref -> unit 