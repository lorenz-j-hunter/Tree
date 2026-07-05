open Nodedef
type node_type = Null of unit | Node of node_class
(*declare function parameters and return types here*)
class type tree_type = object 
  val mutable root_: node_type 
  val mutable bf_: int
  val mutable size_: int
  val mutable unq_: int 
  val mutable alc_unq_: int
  val mutable fv_: int 
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
  method ndfs: int -> float*int 
  method dfst: unit -> unit
  method bfs: int -> unit
  method get_branching_factor: unit -> int 
  method get_size: unit -> int 
  method get_unq: unit -> int 
  method get_alc_unq: unit -> int 
  method get_root: unit -> node_type 
  method set_branching_factor: int -> unit
  (*Rule of Three*)
  method constructor: int -> unit
end

class tree: tree_type =
  object (self)
    val mutable root_: node_type = Null () 
    val mutable bf_ = (0 : int)
    val mutable size_ = (0 : int) 
    val mutable unq_ = (0 : int) 
    val mutable alc_unq_ = (0 : int) 
    val mutable fv_ = (0 : int)
    (*private functions*)
    method set_size data = Tree.set_size data
    method increment_size () =
      size_ <- size_ + 1
    method decrement_size () =
      size_ <- size_ - 1 
    method increment_unq () =
      unq_ <- unq_ + 1 
    method set_unq data =
      unq_ <- data 
    method set_alc_unq data =
      alc_unq_ <- data 
    method increment_alc_unq () =
      alc_unq_ <- alc_unq_ + 1 
    method decrement_alc_unq () =
      alc_unq_ <- alc_unq_ - 1 
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
    method print () =
      let root = self#get_root () in
      if root <> Null () then begin
        for node = 1 to self#get_unq () do
          let cur = (0.0, -1) in (*This should be the result of ndfs.*)
          let value = fst cur in
          if value <> float_of_int 0 then
            print_endline (string_of_float value)
          done;
        end
      else print_endline "Tree is null"
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
    method ndfs abs_index =
      (0.0, -1);
    method dfst () = Tree.dfst ()
    method bfs abs_index = Tree.bfs abs_index
    method get_branching_factor () = bf_ 
    method get_size () = size_ 
    method get_unq () = unq_ 
    method get_alc_unq () = alc_unq_ 
    method get_root () = root_
    method set_branching_factor data =
      bf_ <- data
    (*rule of three*)
    method constructor data = Tree.constructor data
  end
