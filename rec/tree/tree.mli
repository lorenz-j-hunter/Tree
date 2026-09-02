type 'a node = {value : 'a; index : int; bf : int}

type 'a tree = 
  | Leaf
  | Node of 'a node * 'a tree list 

val fill : 'a -> 'a list -> int -> 'a list

val new_node : ?bf:int -> ?index:int -> 'a -> 'a tree

type 'a action =
  | Insert of 'a
  | Remove of int
  | Test

val preorder : 'a action -> 'a tree -> ('a tree * int) Stack.stack -> unit 