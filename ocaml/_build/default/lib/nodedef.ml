class type node =
  object 
    val mutable pair: float * int
    val mutable subtrees: node list

    method pair: float * int
    method subtrees: node list
    method stsize: int
  end 

class _node_ (init: float*int) : node = 
  object (self)
    val mutable pair = init 
    val mutable subtrees: node list = []

    method pair = pair
    method subtrees = subtrees
    method stsize = List.length subtrees
end;;

let void_node = new _node_ (0.0, -1);
