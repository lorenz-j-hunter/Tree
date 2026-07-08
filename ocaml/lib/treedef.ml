open Nodedef
(*options*)
type node_option = Null_node | Node of node
type pair_option = Null_pair | Pair of float*int 
(*declare function parameters and return types here*)
class type tree_type = object 
  val mutable root_: node_option 
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
  method increment_fv: unit -> unit
  method cap_less_one: int -> int
  method cap: int -> int
  method sort: unit -> unit
  method allocate: unit -> unit 
  method is_alloc_bal: unit -> bool
  method alc_ht: unit -> bool
  method count_alc: unit -> int
  method r: int -> int -> int list -> unit
  (*public functions*)
  method append: float -> unit 
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
  method ndfs: int -> pair_option 
  method dfst: int -> node_option 
  method bfs: int -> pair_option 
  method get_bf: unit -> int 
  method get_size: unit -> int 
  method get_unq: unit -> int 
  method get_alc_unq: unit -> int 
  method get_root: unit -> node_option 
  method get_fv: unit -> int
  method set_branching_factor: int -> unit
  (*Rule of Three*)
  method constructor: int -> unit
end

class tree: tree_type =
  object (self)
    val mutable root_: node_option = Null_node
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
    method increment_fv () = fv_ <- fv_ + 1
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
    method append data =
      let root = self#get_root () in
        match root with
        | Node n ->(*normal operation*)
          let h = 0 in
          let d_abs_ind = 0 in
            (*1. begin at first void index. end at next void index*)
            if fv_ = unq_ then begin
              ref d_abs_ind := self#get_unq ();
              ref h := self#height_unit ();
            end else begin
              for abs_ind = fv_ to unq_ do
                let search = self#ndfs abs_ind in
                match search with
                | Pair (fst, snd) ->
                  if snd = -1 then
                    ref h := self#height_i (d_abs_ind);
                    fv_ <- abs_ind;
                    ();
                  if abs_ind = self#get_unq () then
                    ref d_abs_ind := abs_ind + 1;
                    fv_ <- unq_ + 1;
                    ref h := self#height_unit ();
                    ();
                  ref d_abs_ind := abs_ind + 1;
                | Null_pair -> ()
              done 
            end;
            (*2. calculate dh*)
            let dh = 0 in
              let truth = self#is_balanced_h h in
              if truth = true then begin
                ref dh := h + 1;
                ref h := h + 1;
              end else
                ref dh := h; 
              let cap_less_one_ = self#height_i dh in
              let future = -1 in
              let offset = 0 in
              let clo_ = 0 in
              let blw = -1 in
              let cap_ = 0 in
              let cur = 0 in
              let cur_node = ref n in
              let bf = self#get_bf () in
                for ch = 1 to dh - 1 do
                  if blw = -1 then begin
                    ref clo_ := 1;
                    ref cap_ := 1 + bf;
                    if ch = dh then (); (*exit point*)
                    let denom = 1.0 /. float_of_int bf in
                      ref cur := int_of_float ( Stdlib.floor ( 
                        float_of_int ( (d_abs_ind - cap_less_one_) / int_of_float (float_of_int bf ** float_of_int dh) )
                        /. denom
                      ) +. 1. );
                    let trav_index = (cur - 1) mod bf in
                    if (!cur_node)#stsize <= trav_index then (); (*exit point.*)
                    cur_node := List.nth ((!cur_node)#subtrees) trav_index;
                    ref blw := 0;
                    let d = d_abs_ind - cap_less_one_ in
                    let frame = int_of_float (float_of_int bf ** float_of_int (dh-1)) in
                      ref offset := int_of_float (Stdlib.floor (float_of_int d /. float_of_int frame));
                    ref future := offset;
                    ref clo_ := 0;
                    ref cap_ := 1;
                  end else
                    ref clo_ := clo_ + int_of_float ( float_of_int (bf) ** float_of_int (ch-2) );
                    ref cap_ := cap_ + int_of_float ( float_of_int (bf) ** float_of_int (ch-1) );
                    let denom = 1.0 /. float_of_int bf in
                    let r_cur = (float_of_int cur -. float_of_int clo_) /. float_of_int bf in
                    let near_begin = int_of_float ( Stdlib.floor ( r_cur *. denom ) *. float_of_int bf ) in
                    let frame = int_of_float ( float_of_int bf ** float_of_int (dh-ch+1) ) in
                    let end_ = offset + frame in
                    let num = d_abs_ind - cap_less_one_ - offset in
                    let r = float_of_int num /. (float_of_int end_ -. float_of_int offset ) in
                    let addition = int_of_float ( Stdlib.floor (r *. float_of_int bf) ) in
                    if ch = dh then
                      ref blw := cap_ + offset + addition
                    else
                      ref blw := cap_ + near_begin + addition; 
                    (*travese to blw*)
                    let trav_index = (blw - 1) mod bf in
                    if (!cur_node)#stsize <= trav_index then (); (*exit point.*)
                    cur_node := List.nth ((!cur_node)#subtrees) trav_index;
                done;
                (*insert*)
                let insertion_index =
                if d_abs_ind = unq_ - 1 then
                  d_abs_ind mod bf
                else
                  (d_abs_ind - 1) mod bf
                in let insertion = new _node_ (data, unq_) in
                if (!cur_node)#stsize = 0 then
                  for index = 1 to bf do
                    ref (!cur_node)#subtrees := void_node :: (!cur_node)#subtrees
                  done;
                ref (List.nth (!cur_node)#subtrees insertion_index) := insertion;
                (*calculate new alc_unq_ and unq_*)
                if d_abs_ind >= unq_ then
                  if fv_ = unq_ then
                    fv_ <- fv_ + 1;
                  unq_ <- d_abs_ind + 1;
                  self#increment_size ();
                if d_abs_ind >= alc_unq_ then
                  alc_unq_ <- d_abs_ind + bf - (d_abs_ind mod bf) + 1; 
        | Null_node ->
          let n = new _node_ (data, self#get_unq ()) in
            root_ <- Node n;
          self#increment_unq ();
          self#increment_size ();
          self#increment_alc_unq ();
          self#increment_fv (); 
    method print () =
      let root = self#get_root () in
      if root <> Null_node then begin
        for node = 1 to self#get_unq () do
          let cur: pair_option = self#ndfs node in
          match cur with
          | Null_pair -> ()
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
    method ndfs d_abs_ind =
      if d_abs_ind > self#get_unq () || d_abs_ind < 0 then
        Null_pair
      else
        let dh = self#height_i d_abs_ind in
        let cap_less_one_ = self#cap_less_one (dh) in
          match ((d_abs_ind=0), (cap_less_one_= -1)) with
          | (false, false) -> 
            let bf: int = self#get_bf () in
            let cur_node =
              match self#get_root () with
              | Node n -> ref n
              | Null_node -> failwith "Null_node root"
            in
            let future = -1 in
            let offset = 0 in
            let clo_ = 0 in
            let blw = -1 in
            let cap_ = 0 in
            let cur = 0 in
            for ch = 1 to dh do
              if blw = -1 then begin
                ref clo_ := 1;
                ref cap_ := 1 + bf;
                if ch = dh then (); (*exit point*)
                let denom = 1.0 /. float_of_int bf in
                  ref cur := int_of_float ( Stdlib.floor ( 
                    float_of_int ( (d_abs_ind - cap_less_one_) / int_of_float (float_of_int bf ** float_of_int dh) )
                    /. denom
                  ) +. 1. );
                let trav_index = (cur - 1) mod bf in
                if (!cur_node)#stsize <= trav_index then (); (*exit point.*)
                cur_node := List.nth ((!cur_node)#subtrees) trav_index;
                ref blw := 0;
                let d = d_abs_ind - cap_less_one_ in
                let frame = int_of_float (float_of_int bf ** float_of_int (dh-1)) in
                  ref offset := int_of_float (Stdlib.floor (float_of_int d /. float_of_int frame));
                ref future := offset;
                ref clo_ := 0;
                ref cap_ := 1;
              end else
                ref clo_ := clo_ + int_of_float ( float_of_int (bf) ** float_of_int (ch-2) );
                ref cap_ := cap_ + int_of_float ( float_of_int (bf) ** float_of_int (ch-1) );
                let denom = 1.0 /. float_of_int bf in
                let r_cur = (float_of_int cur -. float_of_int clo_) /. float_of_int bf in
                let near_begin = int_of_float ( Stdlib.floor ( r_cur *. denom ) *. float_of_int bf ) in
                let frame = int_of_float ( float_of_int bf ** float_of_int (dh-ch+1) ) in
                let end_ = offset + frame in
                let num = d_abs_ind - cap_less_one_ - offset in
                let r = float_of_int num /. (float_of_int end_ -. float_of_int offset ) in
                let addition = int_of_float ( Stdlib.floor (r *. float_of_int bf) ) in
                if ch = dh then
                  ref blw := cap_ + offset + addition
                else
                  ref blw := cap_ + near_begin + addition; 
                (*travese to blw*)
                let trav_index = (blw - 1) mod bf in
                if (!cur_node)#stsize <= trav_index then (); (*exit point.*)
                cur_node := List.nth ((!cur_node)#subtrees) trav_index;
            done; 
            if dh = 1 then
              let trav_index = (d_abs_ind - 1) mod bf in
                cur_node := List.nth ((!cur_node)#subtrees) trav_index;
                let (a, b) = (!cur_node)#pair in Pair (a, b)
            else
              let (a, b) = (!cur_node)#pair in Pair (a, b)
          | (true, true) -> begin
            match self#get_root () with
            | Null_node -> Null_pair
            | Node n -> 
              let (a,b) = n#pair in Pair (a, b) 
            end
          | (_, _) -> failwith "Should never get here.";
    method dfst d_abs_ind =
      if d_abs_ind > self#get_unq () || d_abs_ind < 0 then
        Null_node
      else
        let dh = self#height_i d_abs_ind in
        let cap_less_one_ = self#cap_less_one (dh) in
          match ((d_abs_ind=0), (cap_less_one_= -1)) with
          | (false, false) -> 
            let bf: int = self#get_bf () in
            let cur_node =
              match self#get_root () with
              | Node n -> ref n
              | Null_node -> failwith "Null_node root"
            in
            let future = -1 in
            let offset = 0 in
            let clo_ = 0 in
            let blw = -1 in
            let cap_ = 0 in
            let cur = 0 in
            for ch = 1 to dh do
              if blw = -1 then begin
                ref clo_ := 1;
                ref cap_ := 1 + bf;
                if ch = dh then (); (*exit point*)
                let denom = 1.0 /. float_of_int bf in
                  ref cur := int_of_float ( Stdlib.floor ( 
                    float_of_int ( (d_abs_ind - cap_less_one_) / int_of_float (float_of_int bf ** float_of_int dh) )
                    /. denom
                  ) +. 1. );
                let trav_index = (cur - 1) mod bf in
                if (!cur_node)#stsize <= trav_index then (); (*exit point.*)
                cur_node := List.nth ((!cur_node)#subtrees) trav_index;
                ref blw := 0;
                let d = d_abs_ind - cap_less_one_ in
                let frame = int_of_float (float_of_int bf ** float_of_int (dh-1)) in
                  ref offset := int_of_float (Stdlib.floor (float_of_int d /. float_of_int frame));
                ref future := offset;
                ref clo_ := 0;
                ref cap_ := 1;
              end else
                ref clo_ := clo_ + int_of_float ( float_of_int (bf) ** float_of_int (ch-2) );
                ref cap_ := cap_ + int_of_float ( float_of_int (bf) ** float_of_int (ch-1) );
                let denom = 1.0 /. float_of_int bf in
                let r_cur = (float_of_int cur -. float_of_int clo_) /. float_of_int bf in
                let near_begin = int_of_float ( Stdlib.floor ( r_cur *. denom ) *. float_of_int bf ) in
                let frame = int_of_float ( float_of_int bf ** float_of_int (dh-ch+1) ) in
                let end_ = offset + frame in
                let num = d_abs_ind - cap_less_one_ - offset in
                let r = float_of_int num /. (float_of_int end_ -. float_of_int offset ) in
                let addition = int_of_float ( Stdlib.floor (r *. float_of_int bf) ) in
                if ch = dh then
                  ref blw := cap_ + offset + addition
                else
                  ref blw := cap_ + near_begin + addition; 
                (*travese to blw*)
                let trav_index = (blw - 1) mod bf in
                if (!cur_node)#stsize <= trav_index then (); (*exit point.*)
                cur_node := List.nth ((!cur_node)#subtrees) trav_index;
            done; 
            if dh = 1 then
              let trav_index = (d_abs_ind - 1) mod bf in
                cur_node := List.nth ((!cur_node)#subtrees) trav_index;
                Node !cur_node 
            else
              Node !cur_node
          | (true, true) -> begin
            match self#get_root () with
            | Null_node -> Null_node
            | Node n -> Node n 
            end
          | (_, _) -> failwith "Should never get here.";
    method bfs abs_index = Null_pair
    method get_bf () = bf_ 
    method get_size () = size_ 
    method get_unq () = unq_ 
    method get_alc_unq () = alc_unq_ 
    method get_root () = root_
    method get_fv () = fv_
    method set_branching_factor data =
      bf_ <- data
    (*rule of three*)
    method constructor data = () 
  end
