open Nodedef
(*options*)
type node_type = Null of unit | Node of node_class
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
          | (false, false) -> begin (*normal operation*) 
              let ret = Pair (0.0, -1) in
              match self#get_root () with
              | Node n -> begin
                let cur_node_ref = ref n in
                let do_and_return = fun x -> x; () in 
                (*For each loop iteration ...*)
                let rec func = fun ch future offset clo_ blw cap_ cur -> begin
                  if blw = -1 then begin
                    let allvals = [
                      ("denom", 0.0);
                      ("cur", 0.0);
                      ("r_cur", 0.0);
                      ("near_begin", 0.0); 
                      ("near_end", 0.0)
                    (*Mix strict and lazy evaluation.*)
                    ] in let rec define = fun l -> begin
                      (*Modify the ref while cdring the pointer until it is none.*)
                      match l with
                      | [] -> ()
                      | hd :: tl ->
                        let f = fun x -> begin List.map (fun elem ->
                          (*Since the map function works from left to right, 
                          the updated values are used in every elem mapping.*)
                          match elem with
                          | ("denom", denom) -> 
                            let ref_denom = ref denom in
                              ref_denom := 1.0 /. float_of_int (self#get_bf ()); ()  
                          | ("cur", cur) ->
                            let denom = snd (List.nth x 0) in
                            let ref_cur = ref cur in
                              ref_cur := 
                              Stdlib.floor (
                                  (
                                  ( ( float_of_int (d_abs_ind) -. float_of_int (cap_less_one_) )
                                  /. (float_of_int (self#get_bf ()) ** float_of_int (dh)) ) 
                                  /. denom
                                  ) +. 1.0
                              )
                          | ("r_cur", r_cur) -> 
                            let cur = snd (List.nth x 1) in
                            let ref_r_cur = ref r_cur in
                              ref_r_cur := (
                                cur -. float_of_int (clo_) +. 1.0
                              ) /. (
                                float_of_int (self#get_bf ())
                                ** float_of_int (ch)
                              )
                          | ("near_begin", near_begin) -> 
                            let r_cur = snd (List.nth x 2) in
                            let ref_near_begin = ref near_begin in
                              ref_near_begin := Stdlib.floor (
                                r_cur
                                /. ( 1.0 /. (float_of_int (self#get_bf ()) ** float_of_int (ch)))
                              ) *. float_of_int (self#get_bf ())
                          | ("near_end", near_end) -> 
                            let near_begin = snd (List.nth x 3) in
                            let ref_near_end = ref near_end in
                              ref_near_end := near_begin +. float_of_int (self#get_bf ()); ()
                          | (_, _) -> failwith "should never get here"
                        ) x; end in do_and_return (f l);
                        define tl;
                    end in define allvals;
                    (*Node traversal.*)
                    let cur = snd (List.nth allvals 1) in
                    let trav_index = (int_of_float (cur) - 1) mod (self#get_bf ()) in
                    if (!cur_node_ref)#get_stsize () <= trav_index then
                      ()
                    else
                      cur_node_ref := List.nth ((!cur_node_ref)#get_subtrees ()) trav_index;
                    if snd (n#get_pair) = d_abs_ind then (*Break.*)
                      let (fst, snd) = n#get_pair in
                        ref ret := Pair (fst, snd)
                    else (*Create offset, future.*)
                      ref blw := 0;
                      let d: float = float_of_int (d_abs_ind) -. float_of_int (cap_less_one_) in
                      let frame: float = float_of_int (self#get_bf()) ** float_of_int (dh-1) in
                        ref offset := int_of_float (Stdlib.floor (d /. frame));
                      ref future := offset;
                      ref clo_ := 0;
                      ref cap_ := 1;
                  (*Else*)
                  end else begin
                    ref cap_ := cap_ + int_of_float(
                      float_of_int (self#get_bf ()) ** float_of_int (ch - 1)
                    ); (*same level as cur, currently.*)
                    ref clo_ := clo_ +  int_of_float(
                      float_of_int (self#get_bf ()) ** float_of_int (ch - 2)
                    ); (*same level as cur, currently.*)

                    let allvals = [
                      ("denom", 0.0);
                      ("r_cur", 0.0);
                      ("near_begin", 0.0); 
                      ("near_end", 0.0);
                      ("frame", 0.);
                      ("end", 0.);
                      ("num", 0.);
                      ("r", 0.);
                      ("addition", 0.);
                    (*Mix strict and lazy evaluation.*)
                    ] in let rec define = fun l -> begin
                      (*Modify the ref while cdring the pointer until it is none.*)
                      match l with
                      | [] -> ()
                      | hd :: tl ->
                        let f = fun x -> begin List.map (fun elem ->
                          (*Since the map function works from left to right, 
                          the updated values are used in every elem mapping.*)
                          match elem with
                          | ("denom", denom) -> 
                            let ref_denom = ref denom in
                              ref_denom := 1.0 /. float_of_int (self#get_bf ()); ()  
                          | ("r_cur", r_cur) ->
                            let denom = snd (List.nth x 0) in
                            let ref_r_cur = ref r_cur in
                              ref_r_cur := 
                              (cur -. float_of_int clo_) /. denom
                          | ("near_begin", near_begin) -> 
                            let denom = snd (List.nth x 0) in
                            let r_cur = snd (List.nth x 1) in
                            let ref_near_begin = ref near_begin in
                              ref_near_begin := Stdlib.floor (
                                r_cur *. denom
                              ) *. float_of_int (self#get_bf ())
                          | ("near_end", near_end) -> 
                            let near_begin = snd (List.nth x 3) in
                            let ref_near_end = ref near_end in
                              ref_near_end := near_begin +. float_of_int (self#get_bf ()); ()
                          | ("frame", frame) -> 
                            ref frame := 
                              float_of_int (self#get_bf ()) ** 
                              float_of_int (dh - ch + 1) 
                          | ("end", end_) ->
                              let frame = snd (List.nth x 4) in
                              ref end_ := float_of_int offset +. frame
                          | ("num", num) ->
                            ref num := float_of_int d_abs_ind -. float_of_int cap_less_one_ -. float_of_int offset 
                          | ("r", r) ->
                            let end_ = snd (List.nth x 5) in
                            let num = snd (List.nth x 6) in
                              ref r := num /. (end_ /. float_of_int offset)
                          | ("addition", addition) ->
                            let r = snd (List.nth x 7) in
                            ref addition := Stdlib.floor (r *. float_of_int (self#get_bf ()))
                          | (_, _) -> failwith "should never get here"
                        ) x; end in do_and_return (f l);
                        define tl;
                    end in define allvals;
                    (*Node traversal.*)
                    if ch = dh then
                      let addition = snd (List.nth allvals 8) in
                      ref blw := cap_ + offset + int_of_float addition;
                    else
                      let addition = snd (List.nth allvals 8) in
                      let near_begin = snd (List.nth allvals 7) in
                      ref blw := cap_ + int_of_float near_begin + int_of_float addition;
                    let trav_index = (blw - 1) mod (self#get_bf ()) in
                    if (!cur_node_ref)#get_stsize () <= trav_index then
                      ()
                    else
                      cur_node_ref := List.nth ((!cur_node_ref)#get_subtrees ()) trav_index;
                    if snd (n#get_pair) = d_abs_ind then (*Break.*)
                      let (fst, snd) = n#get_pair in
                        ref ret := Pair (fst, snd)
                    else (*Create offset, future.*)
                      ref cur := float_of_int blw;
                      ref future := 
                        int_of_float (snd (List.nth allvals 8))
                        * int_of_float (
                          float_of_int (self#get_bf ()) ** float_of_int (dh-ch)
                        );
                      ref offset := offset + future
                  end;
                  func (ch+1) future offset clo_ blw cap_ cur;
                end in do_and_return (func 1, -1, 0, 0, -1, 0, 0 );
                ret; (*return the pair*)
              end
              | Null () -> Null_p () 
            end
          | (true, true) -> begin
            match self#get_root () with
            | Null () -> Null_p ()
            | Node n -> 
              let (a,b) = n#get_pair in Pair (a, b) 
            end
          | (_, _) -> failwith "Should never get here."
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
