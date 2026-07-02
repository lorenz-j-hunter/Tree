(*declare function parameters and return types here*)
class type tree_type = object 
  val mutable root_: unit
  val mutable branching_factor_: int
  val mutable size_: int
  val mutable unq_: int 
  val mutable alc_unq_: int
  val mutable fv_: int 
  method print_helper: unit -> unit
  method set_size: int -> unit
  method increment_size: unit -> unit
  method decrement_size: unit -> unit
  method increment_unq: unit -> unit
  method set_unq: int -> unit
  method set_alc_unq: int -> unit
  method increment_alc_unq: unit -> unit
  method decrement_alc_unq: unit -> unit
  method cap_less_one: int -> int
  method cap: int -> int
  method sort: unit -> unit
  method allocate: unit -> unit 
  method is_alloc_bal: unit -> bool
  method alc_ht: unit -> bool
  method count_alc: unit -> int
  method r: int -> int -> int list -> unit
  (*public functions*)
  method append: int -> unit 
  method print: unit -> unit
  method rmlast: unit -> unit 
  method convert: unit -> unit 
  method insert: int -> int -> int -> unit
  method remove: int -> int -> unit 
  method is_balanced_unit: unit -> bool 
  method is_balanced_h: int -> bool 
  method alloc_by_bal: unit -> unit
  method alloc_lvl: unit -> unit
  method height_unit: unit -> int 
  method height_i: int -> int 
  method unalc_ht: unit -> unit 
  method dfs: int -> unit
  method ndfs: int -> unit
  method dfst: unit -> unit
  method bfs: int -> unit
  method get_branching_factor: unit -> int 
  method get_size: unit -> int 
  method get_unq: unit -> int 
  method get_alc_unq: unit -> int 
  method set_branching_factor: int -> unit
  (*Rule of Three*)
  method constructor: int -> unit
end

class tree: tree_type =
  object (self)
    val mutable root_ = () 
    val mutable branching_factor_ = (0 : int)
    val mutable size_ = (0 : int) 
    val mutable unq_ = (0 : int) 
    val mutable alc_unq_ = (0 : int) 
    val mutable fv_ = (0 : int)
    (*private functions*)
    method print_helper () = Tree.print_helper () 
    method set_size data = Tree.set_size data
    method increment_size () = Tree.increment_size ()
    method decrement_size () = Tree.decrement_size ()
    method increment_unq () = Tree.increment_unq ()
    method set_unq data = Tree.set_unq data
    method set_alc_unq data = Tree.set_alc_unq data
    method increment_alc_unq () = Tree.increment_alc_unq ()
    method decrement_alc_unq () = Tree.decrement_alc_unq ()
    method cap_less_one h = Tree.cap_less_one h
    method cap h = Tree.cap h 
    method sort () = Tree.sort ()
    method allocate () = Tree.allocate ()
    method is_alloc_bal () = Tree.is_alloc_bal ()
    method alc_ht () = Tree.alc_ht ()
    method count_alc () = Tree.count_alc ()
    method r n bf path = Tree.r n bf path 
    (*public functions*)
    method append data = Tree.append data
    method print () = Tree.print ()
    method rmlast () = Tree.rmlast ()
    method convert () = Tree.convert ()
    method insert data h d = Tree.insert data h d 
    method remove h d = Tree.remove h d
    method is_balanced_unit () = Tree.is_balanced_unit ()
    method is_balanced_h h = Tree.is_balanced_h h
    method alloc_by_bal () = Tree.alloc_by_bal ()
    method alloc_lvl () = Tree.alloc_lvl ()
    method height_unit () = Tree.height_unit ()
    method height_i i = Tree.height_i i 
    method unalc_ht () = Tree.unalc_ht ()
    method dfs abs_index = Tree.dfs abs_index
    method ndfs abs_index = Tree.ndfs abs_index
    method dfst () = Tree.dfst ()
    method bfs abs_index = Tree.bfs abs_index
    method get_branching_factor () = Tree.get_branching_factor ()
    method get_size () = Tree.get_size ()
    method get_unq () = Tree.get_unq ()
    method get_alc_unq () = Tree.get_alc_unq ()
    method set_branching_factor data = Tree.set_branching_factor data
    (*rule of three*)
    method constructor data = Tree.constructor data
  end
