class type node_type =
  object 
    val mutable data: float * int
    val mutable subtrees: node_type list
    method construct : int -> unit
  end 

class node : node_type = 
  object (self)
    val mutable data = (0.0, 0) 
    val mutable subtrees = []
    
    method construct (bf: int) : unit =
      subtrees <- List.init bf (fun _ -> new node)
end;;