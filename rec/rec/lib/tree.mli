type 'a tree =
  | Leaf
  | Node of 'a * 'a tree list

val preorder : 'a tree -> unit 