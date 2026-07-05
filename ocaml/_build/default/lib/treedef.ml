open Nodedef
type node_type = Null of unit | Node of node
type node_pair_type = Null_p of unit | Pair of float*int 
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
  method dfs: int -> node_pair_type 
  method ndfs: int -> node_pair_type 
  method dfst: unit -> node_type 
  method bfs: int -> node_pair_type 
  method get_bf: unit -> int 
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
    method set_size data = () 
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
    method cap_less_one h =
      if h = 1 then
        1
      else if h < 1 then
        -1
      else begin
        let capacity = 0 in
        let rec add = fun (c: int) (ch: int) ->
          match (ch < h) with
          | false -> c
          | true ->
            let c_ref = ref c in
              c_ref := c + int_of_float ( float_of_int (self#get_bf()) ** float_of_int (ch) );
            add c (ch+1)
        in add capacity 0 
      end 
    method cap h =  
      if h = 1 then
        1
      else if h < 1 then
        -1
      else begin
        let capacity = 0 in
        let rec add = fun (c: int) (ch: int) ->
          match (ch <= h) with
          | false -> c
          | true ->
            let c_ref = ref c in
              c_ref := c + int_of_float ( float_of_int (self#get_bf()) ** float_of_int (ch) );
            add c (ch+1)
        in add capacity 0 
      end 
    method sort () = () 
    method allocate () = () 
    method is_alloc_bal () = false 
    method alc_ht () = false 
    method count_alc () = 0 
    method r n bf path = () 
    (*public functions*)
    method append data = () 
    method print () =
      let root = self#get_root () in
      if root <> Null () then begin
        for node = 1 to self#get_unq () do
          let cur: node_pair_type = self#ndfs node in
          match cur with
          | Null_p () -> ()
          | Pair (fst, snd) ->
            if fst <> float_of_int 0 then
              print_endline (string_of_float fst)
          done;
        end
      else print_endline "Tree is null, won't print."
    method rmlast () = () 
    method convert () = () 
    method insert data h d = () 
    method remove h d = () 
    method is_balanced_unit () = false 
    method is_balanced_h h = false 
    method alloc_by_bal () = () 
    method alloc_lvl () = () 
    method height_unit () = 0 
    method height_i i = 0 
    method unalc_ht () = () 
    method dfs abs_index = Null_p () 
    method ndfs d_abs_ind =
      if d_abs_ind > self#get_unq () || d_abs_ind < 0 then
        Null_p ()
      else
        let dh = self#height_i d_abs_ind in
        let cap_less_one_ = self#cap_less_one (dh) in
          match ((d_abs_ind=0), (cap_less_one_= -1)) with
          | (false, false) -> (*normal operation*) 
              Pair (0.0, -1)
          | (true, false) -> failwith "Should never get here"
          | (false, true) -> failwith "should never get here" 
          | (true, true) ->  begin
            match self#get_root () with
            | Null () -> Null_p ()
            | Node n -> 
              let (a,b) = n#get_pair in Pair (a, b) 
            end
    method dfst () = Null () 
    method bfs abs_index = Null_p () 
    method get_bf () = bf_ 
    method get_size () = size_ 
    method get_unq () = unq_ 
    method get_alc_unq () = alc_unq_ 
    method get_root () = root_
    method set_branching_factor data =
      bf_ <- data
    (*rule of three*)
    method constructor data = () 
  end
