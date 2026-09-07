type 'a node = {value : 'a; index : int; bf : int}

type 'a tree_node = 
  | Leaf
  | Node of 'a node * 'a tree_node list 

type 'a tree = {bf: int; the_tree : 'a tree_node}

val fill : 'a -> 'a list -> int -> 'a list

val new_tree : ?bf:int -> ?index:int -> 'a -> 'a tree

val new_node : ?bf:int -> ?index:int -> 'a -> 'a tree_node 

type 'a action =
  | Insert of 'a
  | Remove of int
  | Test

val preorder : mode:('a action) -> t:('a tree) -> results:(('a tree_node * int) Stack.stack) -> unit