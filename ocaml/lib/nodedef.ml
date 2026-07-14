class type node =
  object 
    val mutable pair: float * int
    val mutable subtrees: node list
    val mutable size: int 
    val mutable capacity: int

    method pair: float * int
    method subtrees: node list
    method stsize: int
    method stcap: int
    method setcap: int -> unit
    method incr_sz: unit
    method setst: node list -> unit
  end 

class _node_ (init: float*int) : node = 
  object (self)
    val mutable pair = init 
    val mutable subtrees: node list = []
    val mutable size = 0
    val mutable capacity = 0

    method pair = pair
    method subtrees = subtrees
    method stsize = size 
    method stcap = capacity
    method setcap cap = capacity <- cap
    method incr_sz = size <- size + 1;
    if size > capacity then raise(Failure "Node size>capacity");
    method setst nlist = subtrees <- nlist;
end;;

let void_node = new _node_ (0.0, -1)
let unalc = new _node_ (0.0, -2)