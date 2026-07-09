open Nodedef
open Funcs
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
  (*private functions*)
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
  method allocate: unit -> unit 
  method is_alloc_bal: unit -> bool
  method alc_ht: unit -> int 
  method count_alc: unit -> int
  method sort: unit -> unit
  (*public functions*)
  method append: float -> unit 
  method print: unit -> unit
  method pop: unit -> unit 
  method convert: unit -> pair_option list 
  method insert: float -> int -> int -> unit
  method remove: int -> int -> unit 
  method is_balanced_unit: unit -> bool 
  method is_balanced_h: int -> bool 
  method alloc_by_bal: unit -> unit
  method alloc_lvl: unit -> unit
  method height_unit: unit -> int 
  method height_i: int -> int 
  method unalc_ht: unit -> int 
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
    method allocate () =
      let dh = 0 in
        let d_abs_ind = 0 in
        let n = self#get_root () in
        while n <> Null_node do
          ref d_abs_ind := d_abs_ind + 1;
          ref n := self#dfst d_abs_ind;
        done;
      ref dh := self#height_i d_abs_ind;
      let cap_less_one_ = self#cap_less_one dh in
      if cap_less_one_ = -1 then failwith "Error in allocate(). self#cap_less_one -> -1"
      else
        let future = -1 in
        let offset = 0 in
        let clo_ = 0 in
        let blw = -1 in
        let cap_ = 0 in
        let cur = 0 in
        let cur_node = match n with
        | Node a -> ref a
        | Null_node -> failwith "should not get here" 
        in
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
          let insertion_index = (d_abs_ind - 1) mod bf in
          let insertion = new _node_ (0., unq_) in
          if (!cur_node)#stsize <= bf then
            ref (!cur_node)#subtrees := void_node :: (!cur_node)#subtrees;
          ref (List.nth (!cur_node)#subtrees insertion_index) := insertion;
          (*calculate new alc_unq_ and unq_*)
          if d_abs_ind >= alc_unq_ then
            alc_unq_ <- d_abs_ind + bf - (d_abs_ind mod bf) + 1; 
    method is_alloc_bal () =
      let bf = self#get_bf () in
      let height = self#height_unit () in
      let capacity = 0 in
      while height >= 0 do
        ref capacity := capacity + int_of_float ( float_of_int bf ** float_of_int height );
        ref height := height - 1;
      done;
      unq_ = capacity
    method alc_ht () = 
      let ch = 0 in
      let ccap = 1 in
      let index = 0 in
      let pair = Pair (0., -1) in
      while (match pair with | Pair (fst, snd) -> snd | Null_pair -> 0) <> 0 do
        if ccap = self#cap ch then
          ref ch := ch + 1;
          ref ccap := self#cap ch;
        ref pair := self#ndfs index;
        incr (ref index)
      done; 
      ch + 1
    method count_alc () =
      let ch = 1 in
      let abs_indices: int list ref = ref [] in
      let line: node list ref = ref [] in
      let prev_line: node list ref = ref [] in
      let prev_abs_indices: int list ref = ref [] in
      let rec do_while = fun ch current indices condition ->
        indices := [];
        current := [];
        for treenode = self#cap_less_one ch to (self#cap ch) - 1 do
          match self#dfst treenode with
          | Node n -> current := n :: !current
          | Null_node -> current := unalc :: !current
        done;
        for abs_index = 0 to (self#cap ch - self#cap_less_one ch) - 1 do
          let n = List.nth !current abs_index in
          let _, snd = n#pair in
          indices := snd :: !indices
        done;
        if none_of !indices condition then ()
        else
          prev_line := !current;
          prev_abs_indices := !indices;
          incr (ref ch);
        if not (none_of !indices condition) then
          do_while ch current indices condition;
      in do_while ch line abs_indices g_neg_one;
      (*Get d.*)
      let d = ref 0 in
      let n = ref 0 in
      let prev_size = ref (int_of_float (float_of_int (self#get_bf ()) ** float_of_int (ch-1) )) in
      while n < prev_size do
        if List.nth !prev_abs_indices !n > -2 then d := !n;
        incr n
      done;
      self#cap_less_one (ch-1) + !d + 1
    method sort () = ()
    (*public functions*)
    method append data =
      let root = self#get_root () in
        match root with
        | Node n -> (*normal operation*)
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
    method pop () =
      let root = self#get_root () in
      match root with
      | Node n -> (*normal operation*)
        let d_abs_ind = unq_ - 1 in
        let dh = self#height_i d_abs_ind in
          (*create and traverse path*)
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
            let insertion_index = (d_abs_ind - 1) mod bf in
            ref (List.nth (!cur_node)#subtrees insertion_index) := void_node;
            (*calculate new alc_unq_ and unq_*)
            if d_abs_ind < fv_ then
              fv_ <- d_abs_ind;
            if ((d_abs_ind - 1) mod bf) = 0 then
              alc_unq_ <- d_abs_ind - (d_abs_ind mod bf) + 1;
            decr (ref unq_);
            decr (ref size_);
      | Null_node -> ()
    method convert () = 
      let v = ref [] in
        for i = 0 to unq_ - 1 do
          let p = self#ndfs i in v := p :: !v
        done;
        !v
    method insert data h d =
      let root = self#get_root () in
      let d_abs_ind = (self#cap_less_one h) + d in
      let dh = self#height_i d_abs_ind in
        match root with
        | Node n -> (*normal operation*)
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
            let insertion_index = (d_abs_ind - 1) mod bf in
            let insertion = new _node_ (data, d_abs_ind) in
            if (!cur_node)#stsize = 0 then
              for index = 1 to bf do
                ref (!cur_node)#subtrees := void_node :: (!cur_node)#subtrees
              done;
            if snd (!cur_node)#pair = -1 then failwith "Error: attempt to create a disconnected graph"
            else
              ref (List.nth (!cur_node)#subtrees insertion_index) := insertion;
            (*calculate new alc_unq_ and unq_*)
            self#sort ()
        | Null_node ->
          if d_abs_ind <> 0 then failwith "error in insert data h d: null root"
          else
            let n = new _node_ (data, self#get_unq ()) in
              root_ <- Node n;
            self#increment_unq ();
            self#increment_size ();
            self#increment_alc_unq ();
            self#increment_fv (); 

    method remove h d =  
      let root = self#get_root () in
      let d_abs_ind = (self#cap_less_one h) + d in
      let dh = self#height_i d_abs_ind in
        match root with
        | Node n -> (*normal operation*)
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
            (*sort*)
            if d_abs_ind < fv_ then
              fv_ <- d_abs_ind;
            if d_abs_ind < unq_ - 1 then
              let temp = ref unalc#pair in
              let abs_ind = ref (d_abs_ind - 1) in
              while snd !temp <> -2 || snd !temp <> -1 do
                temp := (match self#ndfs !abs_ind with | Pair (a, b) -> (a, b) | Null_pair -> (0., -2));
                if abs_ind = ref 0 then ();
                decr abs_ind;
              done;
              unq_ <- (snd !temp) + 1;
              alc_unq_ <- (unq_ - 1) - ( (unq_ - 1) mod bf) + 1;
            decr (ref size_);
            (*remove*)
            let insertion_index = (d_abs_ind - 1) mod bf in
            let index = ref (List.nth !cur_node#subtrees insertion_index) in
            index := void_node;
            (*if allof indices below are -1, make blw an empty list.*)
            let abs_indices = List.map (fun i -> snd (i#pair)) !cur_node#subtrees in
            if all_of abs_indices eq_neg_one then ref !cur_node#subtrees := [];
        | Null_node -> failwith "Error in remove h d: null root."
    method is_balanced_unit () =
      let height = ref (self#height_unit ()) in
      let capacity = ref 0 in
      while height >= ref 0 do
        capacity := !capacity + pow bf_ !height;
        decr height;
      done;
      size_ = !capacity
    method is_balanced_h h = 
      let ret = ref true in
      let cap = self#cap h in
      for node = 0 to cap-1 do
        let p = (match self#ndfs node with | Pair (snd, fst) -> (snd, fst) | Null_pair -> (0., -2)) in
        if snd p = -1 || snd p = -2 then ret := false; ()
      done;
      !ret
    method alloc_by_bal () =
      let dh = self#height_i unq_ in
      let cap = self#cap dh in
      let fill = cap - alc_unq_ in
      for node = 0 to fill - 1 do
        self#allocate ();
      done;
      alc_unq_ <- self#count_alc ()
    method alloc_lvl () =
      match root_ with
      | Node n ->
        if not (self#is_alloc_bal ()) then failwith "error: alloc_lvl(). cannot allocate level to unbalanced tree"
        else
          let h = self#alc_ht () in
          for index = 0 to (pow bf_ (h+1)) - 1 do
            self#allocate ()
          done;
          alc_unq_ <- self#count_alc ()
      | Null_node -> self#allocate (); 
    method height_unit () =
      if (unq_ - 1) = 0 then 0
      else
        let ch = 1 in
        while not (self#cap_less_one ch <= (unq_ - 1) && (unq_ - 1) < self#cap ch) do
          ref ch := ch + 1
        done;
      ch
    method height_i i =
      if i = 0 then 0
      else
        let ch = 1 in
        while not (self#cap_less_one ch <= i && i < self#cap ch) do
          ref ch := ch + 1
        done;
      ch
    method unalc_ht () =
      let ch = 0 in
      let ccap = 1 in
      for index = 0 to unq_ - 1 do
        if ccap = self#cap ch then
          ref ch := ch + 1;
          ref ccap := self#cap (ch + 1);
        let r = self#ndfs index in
        match r with
        | Pair (fst, snd) -> if snd = -1 then ();
        | Null_pair -> () 
      done; ch
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
    method bfs abs_index =
      let ret = ref (0., -1) in
      let line = Queue.create () in
      let copy = Queue.create () in
      let root = match root_ with | Node n -> n | Null_node -> failwith "Error BFS: Null Root" in
      let do_while = fun base_case ->
        (*base case*)
        if base_case = true then
          for i = 1 to bf_ do
            let elem = List.nth root#subtrees i in
              Queue.push elem line;
              if snd elem#pair = abs_index then ret := elem#pair;
          done;
        (*new line*)
        while not (Queue.is_empty line) do
          let popped = Queue.pop line in
          for i = 1 to bf_ do
            let elem = List.nth popped#subtrees i in
              Queue.push elem copy;
              if snd elem#pair = abs_index then ret := elem#pair;
          done;
        done;
        let line_swap = copy in
        let copy_swap = line in
          ref line := line_swap;
          ref copy := copy_swap;
        ref base_case := false
      in do_while true; 
      let (fst, snd) = !ret in Pair (fst, snd)
    method get_bf () = bf_ 
    method get_size () = size_ 
    method get_unq () = unq_ 
    method get_alc_unq () = alc_unq_ 
    method get_root () = root_
    method get_fv () = fv_
    method set_branching_factor data =
      bf_ <- data
  end
