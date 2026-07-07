class type node_class =
  object 
    val mutable pair: float * int
    val mutable subtrees: node_class list

    method get_pair: float * int
    method get_subtrees: unit -> node_class list
    method get_stsize: unit -> int
  end 

class node (init: float*int) : node_class = 
  object (self)
    val mutable pair = init 
    val mutable subtrees: node_class list = []

    method get_pair = pair
    method get_subtrees () = subtrees
    method get_stsize () =
      List.length subtrees
end;;

let new_node = new node (0.0, -1);
