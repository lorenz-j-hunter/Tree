class type node_class =
  object 
    val mutable data: float * int
    val mutable subtrees: node_class list
  end 

class node (init: float*int) : node_class = 
  object (self)
    val mutable data = init 
    val mutable subtrees = []
end;;

let new_node = new node (0.0, -1);
