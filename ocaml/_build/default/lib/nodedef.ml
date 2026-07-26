class type node =
  object 
    val mutable pair: float * int
    val mutable subtrees_: node list ref
    val mutable size: int 
    val mutable capacity: int

    method pair: float * int
    method subtrees: node list ref
    method stsize: int
    method setsize: int -> unit
    method stcap: int
    method setcap: int -> unit
    method incr_cap: unit 
    method incr_sz: unit
    method decr_sz: unit
    method setst: node list ref -> unit 
  end 

class _node_ (init: float*int) : node = 
  object (self)
    val mutable pair = init 
    val mutable subtrees_: node list ref = ref []
    val mutable size = 0
    val mutable capacity = 0

    method pair = pair
    method subtrees = subtrees_
    method stsize = size 
    method stcap = capacity
    method setsize nsize = size <- nsize;
    method setcap cap = capacity <- cap
    method incr_cap = capacity <- capacity + 1
    method incr_sz = size <- size + 1;
    if size > capacity then raise(Failure "Node size>capacity");
    method decr_sz = size <- size - 1;
    if size < 0 then raise(Failure "Node size<0");
    method setst arg = subtrees_ <- arg;
end;;

let void_node = new _node_ (0.0, -1)
let unalc = new _node_ (0.0, -2)

(*options*)
type node_option = Null_node | Node of node
type pair_option = Null_pair | Pair of float*int 