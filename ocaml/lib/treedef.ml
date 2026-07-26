open Funcs
open Nodedef
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
  (*public functions*)
  method fill: float -> unit 
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
    val mutable bf_ = (3 : int)
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
        failwith "cap_less_one: h < 1"
      else begin
         let capacity = ref 0 in begin
          for height = 0 to h - 1 do
            capacity := !capacity + pow bf_ height;
          done;
          !capacity; end
      end 
    method cap h =  
      if h = 0 then
        1
      else if h < 0 then
        failwith "cap: h < 0"
      else begin
        let capacity = ref 0 in begin
          for height = 0 to h do
            capacity := !capacity + pow bf_ height;
          done;
          !capacity; end
      end 
    method allocate () =
      match root_ with
      | Node n -> (*normal operation*)
        let d_abs_ind = ref 0 in
          (*1. begin at first unalc index. end at next unalc index*)
          let rec get_d_abs_ind = fun i ->
            match self#ndfs i with
            | Pair (0., -2) ->  d_abs_ind := i; 
            | Null_pair -> d_abs_ind := i;
            | Pair (_, _) -> get_d_abs_ind (i + 1); 
          in get_d_abs_ind 0;
          let dh = ref (self#height_i !d_abs_ind) in
          (*2. Traverse path*)
          let cur_node = ref n in
          let cap_less_one_ = self#cap_less_one !dh in
          let rec loop_and_break = fun future offset clo_ blw cap_ cur ch -> 
            (*1st case*)
            if !blw = -1 then begin
              clo_ := 1;
              cap_ := 1 + bf_;
              if ch = !dh then begin (*Stop potential.*)
                (*insert*)
                if (!cur_node)#stcap = bf_ then failwith "allocate: stcap = bf_";
                (!cur_node)#subtrees := !((!cur_node)#subtrees) @ [void_node];
                (!cur_node)#incr_cap;
                (*calculate new alc_unq_ and unq_*)
                if !d_abs_ind >= alc_unq_ then alc_unq_ <- !d_abs_ind + 1;
              end else begin (*recursive case*)
                (*Calculate blw*)
                let denom = 1.0 /. float_of_int bf_ in
                  cur := ( float_of_int (!d_abs_ind - cap_less_one_) /. float_of_int (pow bf_ !dh) /. denom ) +. 1.
                  |> Stdlib.floor |> int_of_float;
                let trav_index = (!cur - 1) mod bf_ in begin
                  cur_node := List.nth !((!cur_node)#subtrees) trav_index; end;
                blw := 0;
                let d = !d_abs_ind - cap_less_one_ in
                let frame = pow bf_ (!dh-1) in
                  offset := (float_of_int d /. float_of_int frame) |> Stdlib.floor |> int_of_float;
                future := !offset;
                clo_ := 0;
                cap_ := 1;
                loop_and_break future offset clo_ blw cap_ cur ch end;
            (*Nth case*)
            end else begin
              clo_ := !clo_ + pow bf_ (ch-2);
              cap_ := !cap_ + pow bf_ (ch-1);
              let denom = 1.0 /. float_of_int bf_ in
              let r_cur = (float_of_int !cur -. float_of_int !clo_) /. float_of_int bf_ in
              let near_begin = int_of_float ( Stdlib.floor ( r_cur *. denom ) *. float_of_int bf_ ) in
              let frame: int = pow bf_ (!dh-ch+1) in
              let end_: int = !offset + frame in
              let num = !d_abs_ind - cap_less_one_ - !offset in
              let r = float_of_int num /. (float_of_int end_ -. float_of_int !offset ) in
              let addition = int_of_float ( Stdlib.floor (r *. float_of_int bf_) ) in
                blw := !cap_ + near_begin + addition; 
              (*travese to blw*)
              let trav_index = (!blw - 1) mod bf_ in (*Stop potential.*)
                if ch = !dh - 1 then begin (*stop case*)
                  (*insert*)
                  (!cur_node)#subtrees := !((!cur_node)#subtrees) @ [void_node];
                  (!cur_node)#incr_cap;
                  (*calculate new alc_unq_ and unq_*)
                  if !d_abs_ind >= alc_unq_ then alc_unq_ <- !d_abs_ind + 1;
                end else begin (*recursive case*)
                  cur_node := List.nth !((!cur_node)#subtrees) trav_index;
                  loop_and_break future offset clo_ blw cap_ cur (ch+1);
                end
            end
          in loop_and_break (ref (-1)) (ref 0) (ref 0) (ref (-1)) (ref 0) (ref 0) 1
      | Null_node ->
        root_ <- Node void_node;
        self#increment_alc_unq ();
    method is_alloc_bal () =
      let height = ref (self#height_unit ()) in
      let capacity = ref 0 in
      while !height >= 0 do
        capacity := !capacity + pow bf_ !height;
        height := !height - 1;
      done;
      (*semantic bug: this should compare to alc_unq_ since its alloc_bal.*)
      unq_ = !capacity
    (*Height of highest allocted node*)
    method alc_ht () =
      let ch = ref 0 in
      let ccap = ref 1 in
      let index = ref 0 in
      let pair = ref (Pair (0., -1)) in
      while not ((match !pair with | Pair (fst, snd) -> snd | Null_pair -> 0) = 0) do
        if !ccap = self#cap !ch then
          incr ch;
          ccap := self#cap (!ch+1);
        pair := self#ndfs !index;
        incr index
      done; 
      !ch + 1
    method count_alc () =
      let rec loop_while_true = fun indices (current: node list ref) (prev: node list ref) prev_indices h ->
        indices := [];
        current := [];
        for treenode = self#cap_less_one h to (self#cap h) - 1 do
          match self#dfst treenode with
          | Node n -> current := n :: !current
          | Null_node -> current := unalc :: !current
        done;
        for abs_index = 0 to (self#cap h - self#cap_less_one h) - 1 do
          let n = List.nth !current abs_index in
          let _, snd = n#pair in
          indices := snd :: !indices
        done;
        if none_of !indices g_neg_one then begin (*return.*)
          (*Get d.*)
          let d = ref 0 in
          let n = ref 0 in
          let prev_size = ref (pow bf_ (h-1)) in
          while !n < !prev_size do
            if List.nth !prev_indices !n > -2 then d := !n;
            incr n
          done;
          (self#cap_less_one h-1) + !d + 1;
        end else begin
          prev := !current;
          prev_indices := !indices;
          loop_while_true indices current prev prev_indices (h+1); end
      in loop_while_true (ref []) (ref []) (ref []) (ref []) 1 
    (*public functions*)
    method fill (data: float) =
      match root_ with
      | Node n -> (*normal operation*)
        let h = ref 0 in
        let d_abs_ind = ref 0 in
          (*1. begin at first void index. end at next void index*)
          if fv_ = unq_ then begin
            d_abs_ind := unq_;
            h := self#height_unit ();
          end else begin
            let init_fv = fv_ in
              for abs_ind = init_fv to unq_ do
                let search = self#ndfs abs_ind in
                match search with
                | Pair (fst, snd) ->
                  if snd = -1 then begin
                    h := self#height_i !d_abs_ind;
                    fv_ <- abs_ind;
                    raise Exit; end;
                  if abs_ind = unq_ then begin
                    d_abs_ind := abs_ind + 1;
                    fv_ <- unq_ + 1;
                    h := self#height_unit ();
                    raise Exit; end;
                  d_abs_ind := abs_ind + 1;
                | Null_pair ->
                  d_abs_ind := abs_ind + 1
              done 
          end;
          (*2. calculate dh*)
          let dh = ref 0 in
            let is_balanced = self#is_balanced_h !h in
            if is_balanced then begin
              dh := !h + 1;
              incr h;
            end else
              dh := !h; 
          (*3. Traverse path*)
          let cur_node = ref n in
          let cap_less_one_ = self#cap_less_one !dh in
          let rec loop_and_break = fun future offset clo_ blw cap_ cur ch -> 
            (*1st case*)
            if !blw = -1 then begin
              clo_ := 1;
              cap_ := 1 + bf_;
              if ch = !dh then begin (*Stop potential.*)
                (*insert*)
                let insertion_index = (!d_abs_ind - 1) mod bf_ in
                let insertion = new _node_ (data, unq_) in
                if (!cur_node)#stcap = 0 then begin
                  for index = 1 to bf_ do (!cur_node)#subtrees := !((!cur_node)#subtrees) @ [void_node] done;
                  (!cur_node)#setcap bf_;
                end;
                let r = ref (Funcs.replace !((!cur_node)#subtrees) insertion_index insertion) in
                  (!cur_node)#setst r;
                (!cur_node)#incr_sz;
                (*calculate new alc_unq_ and unq_*)
                if !d_abs_ind >= unq_ then begin
                  if fv_ = unq_ then fv_ <- fv_ + 1;
                  unq_ <- !d_abs_ind + 1;
                  self#increment_size (); end;
                if !d_abs_ind >= alc_unq_ then
                  alc_unq_ <- !d_abs_ind + bf_ - (!d_abs_ind mod bf_) + 1;
              end else begin (*recursive case*)
                (*Calculate blw*)
                let denom = 1.0 /. float_of_int bf_ in
                  cur := float_of_int (!d_abs_ind - cap_less_one_) /. float_of_int (pow bf_ !dh) /. denom +. 1.
                  |> Stdlib.floor |> int_of_float;
                let trav_index = (!cur - 1) mod bf_ in begin
                  cur_node := List.nth !((!cur_node)#subtrees) trav_index; end;
                blw := 0;
                let d = !d_abs_ind - cap_less_one_ in
                let frame = pow bf_ (!dh-1) in
                  offset := float_of_int d /. float_of_int frame
                  |> Stdlib.floor |> int_of_float;
                future := !offset;
                clo_ := 0;
                cap_ := 1;
                loop_and_break future offset clo_ blw cap_ cur ch end;
            (*Nth case*)
            end else begin
              clo_ := !clo_ + pow bf_ (ch-2);
              cap_ := !cap_ + pow bf_ (ch-1);
              let denom = 1.0 /. float_of_int bf_ in
              let r_cur = (float_of_int !cur -. float_of_int !clo_) /. float_of_int bf_ in
              let near_begin = ( r_cur *. denom ) *. float_of_int bf_ |> Stdlib.floor |> int_of_float in
              let frame: int = pow bf_ (!dh-ch+1) in
              let end_: int = !offset + frame in
              let num = !d_abs_ind - cap_less_one_ - !offset in
              let r = float_of_int num /. (float_of_int end_ -. float_of_int !offset ) in
              let addition = (r *. float_of_int bf_) |> Stdlib.floor |> int_of_float in
                blw := !cap_ + near_begin + addition; 
              (*travese to blw*)
              let trav_index = (!blw - 1) mod bf_ in (*Stop potential.*)
                if ch = !dh - 1 then begin (*stop case*)
                  (*insert*)
                  let insertion_index = (!d_abs_ind - 1) mod bf_ in
                  let insertion = new _node_ (data, unq_) in
                  if (!cur_node)#stcap = 0 then begin
                    for index = 1 to bf_ do (!cur_node)#subtrees := !((!cur_node)#subtrees) @ [void_node] done;
                    (!cur_node)#setcap bf_;
                  end;
                  let r = ref (Funcs.replace !((!cur_node)#subtrees) insertion_index insertion) in
                    (!cur_node)#setst r;
                  (!cur_node)#incr_sz;
                  (*calculate new alc_unq_ and unq_*)
                  if !d_abs_ind >= unq_ then begin
                    if fv_ = unq_ then fv_ <- fv_ + 1;
                    unq_ <- !d_abs_ind + 1;
                    self#increment_size ();
                    end;
                  if !d_abs_ind >= alc_unq_ then
                    alc_unq_ <- !d_abs_ind + bf_ - (!d_abs_ind mod bf_) + 1;
                end else begin (*recursive case*)
                  cur_node := List.nth !((!cur_node)#subtrees) trav_index;
                  loop_and_break future offset clo_ blw cap_ cur (ch+1);
                end
            end
          in loop_and_break (ref (-1)) (ref 0) (ref 0) (ref (-1)) (ref 0) (ref 0) 1
      | Null_node ->
        let n = new _node_ (data, unq_) in
          root_ <- Node n;
        self#increment_unq ();
        self#increment_size ();
        self#increment_alc_unq ();
        self#increment_fv (); 
    method print () =
      if root_ <> Null_node then begin
        for node = 0 to unq_ - 1 do
          let cur: pair_option = self#ndfs node in
          match cur with
          | Null_pair -> ()
          | Pair (fst, snd) ->
            if not (snd = -1 || snd = -2) then Format.printf "%f " fst
          done;
        end
      else print_endline "Tree is null, won't print."
    method pop () =
      let d_abs_ind = unq_ - 1 in
      match root_ with
      | Node n -> (*normal operation*)
        let dh = self#height_unit () in
          (*3. Traverse path*)
          let cur_node = ref n in
          let cap_less_one_ = self#cap_less_one dh in
          let rec loop_and_break = fun future offset clo_ blw cap_ cur ch -> 
            (*1st case*)
            if !blw = -1 then begin
              clo_ := 1;
              cap_ := 1 + bf_;
              if ch = dh then begin (*Stop potential.*)
                (*pop*)
                let r = ref (Funcs.replace !((!cur_node)#subtrees) ( (d_abs_ind - 1) mod bf_) void_node) in
                  (!cur_node)#setst r; 
                (!cur_node)#decr_sz;
                let abs_indices = List.map (fun x -> snd (x#pair)) !((!cur_node)#subtrees) in
                  if all_of abs_indices eq_neg_one then begin (!cur_node)#setst (ref []); (!cur_node)#setcap 0; (!cur_node)#setsize 0; end;
                (*sort*)
                if d_abs_ind < fv_ then fv_ <- unq_ - 1;
                if (d_abs_ind - 1) mod bf_ = 0 then alc_unq_ <- d_abs_ind - (d_abs_ind mod bf_) + 1;
                unq_ <- unq_ - 1;
                self#decrement_size () ;
              end else begin (*recursive case*)
                (*Calculate blw*)
                let denom = 1.0 /. float_of_int bf_ in
                  cur := int_of_float ( Stdlib.floor ( 
                    float_of_int (d_abs_ind - cap_less_one_) /. float_of_int (pow bf_ dh)
                    /. denom
                  ) +. 1. );
                let trav_index = (!cur - 1) mod bf_ in begin
                  cur_node := List.nth !((!cur_node)#subtrees) trav_index; end;
                blw := 0;
                let d = d_abs_ind - cap_less_one_ in
                let frame = pow bf_ (dh-1) in
                  offset := int_of_float (Stdlib.floor (float_of_int d /. float_of_int frame));
                future := !offset;
                clo_ := 0;
                cap_ := 1;
                loop_and_break future offset clo_ blw cap_ cur ch end;
            (*Nth case*)
            end else begin
              clo_ := !clo_ + pow bf_ (ch-2);
              cap_ := !cap_ + pow bf_ (ch-1);
              let denom = 1.0 /. float_of_int bf_ in
              let r_cur = (float_of_int !cur -. float_of_int !clo_) /. float_of_int bf_ in
              let near_begin = int_of_float ( Stdlib.floor ( r_cur *. denom ) *. float_of_int bf_ ) in
              let frame: int = pow bf_ (dh-ch+1) in
              let end_: int = !offset + frame in
              let num = d_abs_ind - cap_less_one_ - !offset in
              let r = float_of_int num /. (float_of_int end_ -. float_of_int !offset ) in
              let addition = int_of_float ( Stdlib.floor (r *. float_of_int bf_) ) in
                blw := !cap_ + near_begin + addition; 
              (*travese to blw*)
              let trav_index = (!blw - 1) mod bf_ in (*Stop potential.*)
                if ch = dh - 1 then begin (*stop case*)
                  (*pop*)
                  let r = ref (Funcs.replace !((!cur_node)#subtrees) ( (d_abs_ind - 1) mod bf_) void_node) in
                    (!cur_node)#setst r; 
                  (!cur_node)#decr_sz;
                  let abs_indices = List.map (fun x -> snd (x#pair)) !((!cur_node)#subtrees) in
                    if all_of abs_indices eq_neg_one then begin
                      (!cur_node)#setst (ref []); (!cur_node)#setcap 0; (!cur_node)#setsize 0;
                    end;
                  (*sort*)
                  if d_abs_ind < fv_ then fv_ <- unq_ - 1;
                  if (d_abs_ind - 1) mod bf_ = 0 then alc_unq_ <- d_abs_ind - (d_abs_ind mod bf_) + 1;
                  unq_ <- unq_ - 1;
                  size_ <- size_ - 1;
                end else begin (*recursive case*)
                  cur_node := List.nth !((!cur_node)#subtrees) trav_index;
                  loop_and_break future offset clo_ blw cap_ cur (ch+1);
                end
            end
          in loop_and_break (ref (-1)) (ref 0) (ref 0) (ref (-1)) (ref 0) (ref 0) 1;
      | Null_node -> failwith "pop: Null root"
    method convert () =
      (*semantic bug: this prints null and void nodes*)
      match root_ with
      | Node n -> 
        let v = ref [] in
          for i = unq_ - 1 downto 0 do v := (self#ndfs i) :: !v done;
          !v
      | Null_node -> [] 
    (*Note for insert h d: d is zero indexed.*)
    method insert data h d =
      match root_ with
      | Node n -> (*normal operation*)
        let d_abs_ind = ref ( (self#cap_less_one h) + d) in
        (*Traverse path*)
        let cur_node = ref n in
        let cap_less_one_ = self#cap_less_one h in
        let rec loop_and_break = fun future offset clo_ blw cap_ cur ch -> 
          (*1st case*)
          if !blw = -1 then begin
            clo_ := 1;
            cap_ := 1 + bf_;
            if ch = h then begin (*Stop potential.*)
              if (!cur_node)#stcap = 0 then begin
                for index = 1 to bf_ do
                  (!cur_node)#subtrees := !((!cur_node)#subtrees) @ [void_node]
                done; (!cur_node)#setcap bf_; end;
              let insertion = new _node_ (data, !d_abs_ind) in
              let insertion_index = (!d_abs_ind - 1) mod bf_ in
                let r = ref (Funcs.replace !((!cur_node)#subtrees) insertion_index insertion) in
                (!cur_node)#setst r;
              (!cur_node)#incr_sz;
              (*Case: we inserted a void node/unalc.*)
              let target_node = ref (List.nth !((!cur_node)#subtrees) insertion_index) in
              let pair = (!target_node)#pair in
                if (snd pair = -1 || snd pair = -2) then (!cur_node)#incr_sz;
              (*sort*)
              if !d_abs_ind >= unq_ then unq_ <- !d_abs_ind + 1;
              if !d_abs_ind < fv_ then begin (*search for next void index.*) 
                let p = ref (Pair (0., 0)) in
                let i = ref fv_ in
                  while not (!p = Pair (0., -1) || !p = Pair (0., -2)) do
                    p := self#ndfs !i;
                    incr i;
                  done;
                  fv_ <- !i;
              end;
              let d = !d_abs_ind - cap_less_one_ in
                alc_unq_ <- !d_abs_ind + bf_ - ( d mod bf_ );
              self#increment_size();
            end else begin (*recursive case*)
              (*Calculate blw*)
              let denom = 1.0 /. float_of_int bf_ in
                cur := int_of_float ( Stdlib.floor ( 
                  float_of_int (!d_abs_ind - cap_less_one_) /. float_of_int (pow bf_ h)
                  /. denom
                ) +. 1. );
              let trav_index = (!cur - 1) mod bf_ in begin
                (*check for errors.*)
                if (!cur_node)#stcap <= trav_index then failwith "insert: attempt to create a disconnected graph"
                else let k = (List.nth !((!cur_node)#subtrees) trav_index) in
                if snd (k#pair) < 0 then failwith "insert: attempt to create a disconnected graph"; 
                (*traverse.*)
                cur_node := List.nth !((!cur_node)#subtrees) trav_index; end;
              blw := 0;
              let d = !d_abs_ind - cap_less_one_ in
              let frame = pow bf_ (h-1) in
                offset := float_of_int d /. float_of_int frame |> Stdlib.floor |> int_of_float;
              future := !offset;
              clo_ := 0;
              cap_ := 1;
              loop_and_break future offset clo_ blw cap_ cur ch end;
          (*Nth case*)
          end else begin
            clo_ := !clo_ + pow bf_ (ch-2);
            cap_ := !cap_ + pow bf_ (ch-1);
            let denom = 1.0 /. float_of_int bf_ in
            let r_cur = (float_of_int !cur -. float_of_int !clo_) /. float_of_int bf_ in
            let near_begin = int_of_float ( Stdlib.floor ( r_cur *. denom ) *. float_of_int bf_ ) in
            let frame: int = pow bf_ (h-ch+1) in
            let end_: int = !offset + frame in
            let num = !d_abs_ind - cap_less_one_ - !offset in
            let r = float_of_int num /. (float_of_int end_ -. float_of_int !offset ) in
            let addition = int_of_float ( Stdlib.floor (r *. float_of_int bf_) ) in
              blw := !cap_ + near_begin + addition; 
            (*travese to blw*)
            let trav_index = (!blw - 1) mod bf_ in (*Stop potential.*)
              if ch = h - 1 then begin (*stop case*)
                (*insert*)
                if (!cur_node)#stcap = 0 then begin
                  for index = 1 to bf_ do (!cur_node)#subtrees := !((!cur_node)#subtrees) @ [void_node] done;
                  (!cur_node)#setcap bf_; end;
                let insertion = new _node_ (data, !d_abs_ind) in
                let insertion_index = (!d_abs_ind - 1) mod bf_ in
                  let r = ref (Funcs.replace !((!cur_node)#subtrees) insertion_index insertion) in
                  (!cur_node)#setst r;
                (!cur_node)#incr_sz;
                (*Case: we inserted a void/null node.*)
                let target_node = ref (List.nth !((!cur_node)#subtrees) insertion_index) in
                let pair = (!target_node)#pair in
                  if (snd pair = -1 || snd pair = -2) then (!cur_node)#incr_sz;
                (*sort*)
                if !d_abs_ind >= unq_ then unq_ <- !d_abs_ind + 1;
                if !d_abs_ind < fv_ then begin (*search for next void index.*) 
                  let p = ref (Pair (0., 0)) in
                  let i = ref fv_ in
                    while not (!p = Pair (0., -1) || !p = Pair (0., -2)) do
                      p := self#ndfs !i;
                      incr i;
                    done;
                    fv_ <- !i;
                end;
                let d = !d_abs_ind - cap_less_one_ in
                  alc_unq_ <- !d_abs_ind + bf_ - ( d mod bf_ );
                self#increment_size();
              end else begin (*recursive case*)
                (*check for errors.*)
                if (!cur_node)#stcap <= trav_index then failwith "insert: attempt to create a disconnected graph"
                else let k = (List.nth !((!cur_node)#subtrees) trav_index) in
                if snd (k#pair) < 0 then failwith "insert: attempt to create a disconnected graph"; 
                (*traverse.*)
                cur_node := List.nth !((!cur_node)#subtrees) trav_index; 
                loop_and_break future offset clo_ blw cap_ cur (ch+1);
              end
          end
        in loop_and_break (ref (-1)) (ref 0) (ref 0) (ref (-1)) (ref 0) (ref 0) 1
      | Null_node ->
        let n = new _node_ (data, unq_) in
          root_ <- Node n;
        self#increment_unq ();
        self#increment_size ();
        self#increment_alc_unq ();
        self#increment_fv (); 
    method remove h d =  
      match root_ with
      | Node n -> (*normal operation*)
        let d_abs_ind = ref ( (self#cap_less_one h) + d) in
        (*Traverse path*)
        let cur_node = ref n in
        let cap_less_one_ = self#cap_less_one h in
        let rec loop_and_break = fun future offset clo_ blw cap_ cur ch -> 
          (*1st case*)
          if !blw = -1 then begin
            clo_ := 1;
            cap_ := 1 + bf_;
            if ch = h then begin (*Stop potential.*)
              if (!cur_node)#stcap = 0 then failwith "remove: Attempt to remove a nonexistent node";
              let insertion_index = (!d_abs_ind - 1) mod bf_ in
                let r = ref (Funcs.replace !((!cur_node)#subtrees) insertion_index void_node) in
                  (!cur_node)#setst r;
              (!cur_node)#decr_sz;
              let abs_indices = List.map (fun x -> snd (x#pair)) !((!cur_node)#subtrees) in
                if all_of abs_indices eq_neg_one then begin
                  (!cur_node)#setst (ref []); (!cur_node)#setcap 0; (!cur_node)#setsize 0;
                  let d = !d_abs_ind - cap_less_one_ in alc_unq_ <- !d_abs_ind - ( d mod bf_ );
                end;
              (*sort*)
              if !d_abs_ind = unq_ - 1 then unq_ <- !d_abs_ind;
              if !d_abs_ind < fv_ then fv_ <- !d_abs_ind;
              self#decrement_size();
            end else begin (*recursive case*)
              (*Calculate blw*)
              let denom = 1.0 /. float_of_int bf_ in
                cur := (float_of_int (!d_abs_ind - cap_less_one_) /. float_of_int (pow bf_ h) /. denom) +. 1.
                |> Stdlib.floor |> int_of_float;
              let trav_index = (!cur - 1) mod bf_ in begin
                if (!cur_node)#stcap <= trav_index then failwith "remove: node does not exist" 
                else let k = (List.nth !((!cur_node)#subtrees) trav_index) in
                if snd (k#pair) < 0 then failwith "remove: node does not exist"; 

                cur_node := List.nth !((!cur_node)#subtrees) trav_index; end;
              blw := 0;
              let d = !d_abs_ind - cap_less_one_ in
              let frame = pow bf_ (h-1) in
                offset := (float_of_int d /. float_of_int frame) |> Stdlib.floor |> int_of_float;
              future := !offset;
              clo_ := 0;
              cap_ := 1;
              loop_and_break future offset clo_ blw cap_ cur ch end;
          (*Nth case*)
          end else begin
            clo_ := !clo_ + pow bf_ (ch-2);
            cap_ := !cap_ + pow bf_ (ch-1);
            let denom = 1.0 /. float_of_int bf_ in
            let r_cur = (float_of_int !cur -. float_of_int !clo_) /. float_of_int bf_ in
            let near_begin = int_of_float ( Stdlib.floor ( r_cur *. denom ) *. float_of_int bf_ ) in
            let frame: int = pow bf_ (h-ch+1) in
            let end_: int = !offset + frame in
            let num = !d_abs_ind - cap_less_one_ - !offset in
            let r = float_of_int num /. (float_of_int end_ -. float_of_int !offset ) in
            let addition = int_of_float ( Stdlib.floor (r *. float_of_int bf_) ) in
              blw := !cap_ + near_begin + addition; 
            (*travese to blw*)
            let trav_index = (!blw - 1) mod bf_ in (*Stop potential.*)
              if ch = h - 1 then begin (*stop case*)
                if (!cur_node)#stcap = 0 then failwith "remove: attempt to remove a nonexistent node";
                let insertion_index = (!d_abs_ind - 1) mod bf_ in
                  let r = ref (Funcs.replace !((!cur_node)#subtrees) insertion_index void_node) in
                    (!cur_node)#setst r;
                (!cur_node)#decr_sz;
                let abs_indices = List.map (fun x -> snd (x#pair)) !((!cur_node)#subtrees) in
                  if all_of abs_indices eq_neg_one then begin
                    (!cur_node)#setst (ref []); (!cur_node)#setcap 0; (!cur_node)#setsize 0;
                    let d = !d_abs_ind - cap_less_one_ in alc_unq_ <- !d_abs_ind - ( d mod bf_ );
                  end;
                (*sort*)
                if !d_abs_ind = unq_ - 1 then unq_ <- !d_abs_ind;
                if !d_abs_ind < fv_ then fv_ <- !d_abs_ind;
                self#decrement_size();
              end else begin (*traverse*)
                if (!cur_node)#stcap <= trav_index then failwith "remove: node does not exist" 
                else let k = (List.nth !((!cur_node)#subtrees) trav_index) in
                if snd (k#pair) < 0 then failwith "remove: node does not exist"; 
                cur_node := List.nth !((!cur_node)#subtrees) trav_index;
                loop_and_break future offset clo_ blw cap_ cur (ch+1);
              end
          end
        in loop_and_break (ref (-1)) (ref 0) (ref 0) (ref (-1)) (ref 0) (ref 0) 1
      | Null_node -> failwith "remove h d: Null root"
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
        let p = (match self#ndfs node with | Pair (snd, fst) -> (snd, fst) | Null_pair -> (0., -2)) in begin
        if snd p = -1 || snd p = -2 then ret := false; () end
      done;
      !ret
    method alloc_by_bal () =
      let dh = self#height_i alc_unq_ in
      let cap = self#cap dh in
      while not (alc_unq_  = cap) do self#allocate (); done;
    method alloc_lvl () =
      match root_ with
      | Node n ->
        if not (self#is_alloc_bal ()) then failwith "error: alloc_lvl(). cannot allocate level to unbalanced tree"
        else
          let h = self#height_i alc_unq_ in
          for index = 0 to (pow bf_ h) - 1 do self#allocate () done;
      | Null_node -> self#allocate ();
    method height_unit () =
      if (unq_ - 1) = 0 then 0
      else begin
        let ch = ref 1 in
        while not (self#cap_less_one !ch <= (unq_ - 1) && (unq_ - 1) < self#cap !ch) do
          incr ch
        done;
        !ch
        end;
    method height_i i =
      if i = 0 then 0
      else begin
        let ch = ref 1 in
        while not (self#cap_less_one !ch <= i && i < self#cap !ch) do
          incr ch;
        done;
        !ch
        end;
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
      match root_ with
      | Node n -> begin
        if d_abs_ind >= alc_unq_ || d_abs_ind < 0 then
          Null_pair
        else if d_abs_ind = 0 then let (a, b) = n#pair in Pair (a, b)
        else begin
          let dh = self#height_i d_abs_ind in
          (*Traverse path*)
          let cur_node = ref n in
          let cap_less_one_ = self#cap_less_one dh in
          let rec loop_and_break = fun future offset clo_ blw cap_ cur ch -> 
            (*1st case*)
            if !blw = -1 then begin
              clo_ := 1;
              cap_ := 1 + bf_;
              if ch = dh then begin (*Stop potential.*)
                if dh = 1 then begin
                  let trav_index = (d_abs_ind - 1) mod bf_ in
                    if (!cur_node)#stcap <= trav_index then Pair (0., -2)
                    else begin
                      cur_node := List.nth !((!cur_node)#subtrees) trav_index;
                      let (a, b) = (!cur_node)#pair in Pair (a, b) end
                end else
                  let (a, b) = (!cur_node)#pair in Pair (a, b)
              end else begin (*recursive case*)
                (*Calculate blw*)
                let denom = 1.0 /. float_of_int bf_ in
                  cur := (float_of_int (d_abs_ind - cap_less_one_) /. float_of_int (pow bf_ dh) /. denom) +. 1.
                  |> Stdlib.floor |> int_of_float;
                  let trav_index = (!cur - 1) mod bf_ in begin
                    cur_node := List.nth !((!cur_node)#subtrees) trav_index;
                  end;
                blw := 0;
                let d = d_abs_ind - cap_less_one_ in
                let frame = pow bf_ (dh-1) in
                  offset := (float_of_int d /. float_of_int frame) |> Stdlib.floor |> int_of_float;
                future := !offset;
                clo_ := 0;
                cap_ := 1;
                loop_and_break future offset clo_ blw cap_ cur ch
              end;
            (*Nth case*)
            end else begin
              clo_ := !clo_ + pow bf_ (ch-2);
              cap_ := !cap_ + pow bf_ (ch-1);
              let denom = 1.0 /. float_of_int bf_ in
              let r_cur = (float_of_int !cur -. float_of_int !clo_) /. float_of_int bf_ in
              let near_begin = int_of_float ( Stdlib.floor ( r_cur *. denom ) *. float_of_int bf_ ) in
              let frame: int = pow bf_ (dh-ch+1) in
              let end_: int = !offset + frame in
              let num = d_abs_ind - cap_less_one_ - !offset in
              let r = float_of_int num /. (float_of_int end_ -. float_of_int !offset ) in
              let addition = (r *. float_of_int bf_) |> Stdlib.floor |> int_of_float in
                blw := !cap_ + near_begin + addition; 
              (*travese to blw*)
              let trav_index = (!blw - 1) mod bf_ in (*Stop potential.*)
                if ch = dh - 1 then begin (*stop case*)
                  let trav_index = (d_abs_ind - 1) mod bf_ in
                    if (!cur_node)#stcap <= trav_index then Pair (0., -2)
                    else begin
                      cur_node := List.nth !((!cur_node)#subtrees) trav_index;
                      let (a, b) = (!cur_node)#pair in Pair (a, b) end
                end else begin (*recursive case*)
                    if (!cur_node)#stcap <= trav_index then Pair (0., -2)
                    else begin
                      cur_node := List.nth !((!cur_node)#subtrees) trav_index;
                      loop_and_break future offset clo_ blw cap_ cur (ch+1); end
                end
            end
          in loop_and_break (ref (-1)) (ref 0) (ref 0) (ref (-1)) (ref 0) (ref 0) 1;
        end
      end
      | Null_node -> Null_pair
    method dfst d_abs_ind =
      match root_ with
      | Node n -> begin
        if d_abs_ind >= alc_unq_ || d_abs_ind < 0 then
          Null_node
        else if d_abs_ind = 0 then match self#get_root () with
          | Node n -> Node n
          | Null_node -> Node unalc 
        else begin
          let dh = self#height_i d_abs_ind in
          (*Traverse path*)
          let cur_node = ref n in
          let cap_less_one_ = self#cap_less_one dh in
          let rec loop_and_break = fun future offset clo_ blw cap_ cur ch -> 
            (*1st case*)
            if !blw = -1 then begin
              clo_ := 1;
              cap_ := 1 + bf_;
              if ch = dh then begin (*Stop potential.*)
                if dh = 1 then begin
                  let trav_index = (d_abs_ind - 1) mod bf_ in
                    if (!cur_node)#stcap <= trav_index then let ret = new _node_ (0., -2) in Node ret;
                    else begin
                      cur_node := List.nth !((!cur_node)#subtrees) trav_index;
                      Node !cur_node end
                end else
                  Node !cur_node; 
              end else begin (*recursive case*)
                (*Calculate blw*)
                let denom = 1.0 /. float_of_int bf_ in
                  cur := int_of_float ( Stdlib.floor ( 
                    float_of_int (d_abs_ind - cap_less_one_) /. float_of_int (pow bf_ dh)
                    /. denom
                  ) +. 1. );
                  let trav_index = (!cur - 1) mod bf_ in begin
                    cur_node := List.nth !((!cur_node)#subtrees) trav_index;
                  end;
                blw := 0;
                let d = d_abs_ind - cap_less_one_ in
                let frame = pow bf_ (dh-1) in
                  offset := int_of_float (Stdlib.floor (float_of_int d /. float_of_int frame));
                future := !offset;
                clo_ := 0;
                cap_ := 1;
                loop_and_break future offset clo_ blw cap_ cur ch
              end;
            (*Nth case*)
            end else begin
              clo_ := !clo_ + pow bf_ (ch-2);
              cap_ := !cap_ + pow bf_ (ch-1);
              let denom = 1.0 /. float_of_int bf_ in
              let r_cur = (float_of_int !cur -. float_of_int !clo_) /. float_of_int bf_ in
              let near_begin = int_of_float ( Stdlib.floor ( r_cur *. denom ) *. float_of_int bf_ ) in
              let frame: int = pow bf_ (dh-ch+1) in
              let end_: int = !offset + frame in
              let num = d_abs_ind - cap_less_one_ - !offset in
              let r = float_of_int num /. (float_of_int end_ -. float_of_int !offset ) in
              let addition = int_of_float ( Stdlib.floor (r *. float_of_int bf_) ) in
                blw := !cap_ + near_begin + addition; 
              (*travese to blw*)
              let trav_index = (!blw - 1) mod bf_ in (*Stop potential.*)
                if ch = dh - 1 then begin (*stop case*)
                  let trav_index = (d_abs_ind - 1) mod bf_ in
                    if (!cur_node)#stcap <= trav_index then let ret = new _node_ (0., -2) in Node ret
                    else begin
                      cur_node := List.nth !((!cur_node)#subtrees) trav_index;
                      Node (!cur_node); end
                end else begin (*recursive case*)
                    if (!cur_node)#stcap <= trav_index then let ret = new _node_ (0., -2) in Node ret;
                    else begin
                      cur_node := List.nth !((!cur_node)#subtrees) trav_index;
                      loop_and_break future offset clo_ blw cap_ cur (ch+1); end
                end
            end
          in loop_and_break (ref (-1)) (ref 0) (ref 0) (ref (-1)) (ref 0) (ref 0) 1;
        end
      end
      | Null_node -> Null_node
    method bfs abs_index =
      let ret = ref (0., -1) in
      let line = Queue.create () in
      let copy = Queue.create () in
      let root = match root_ with | Node n -> n | Null_node -> failwith "Error BFS: Null Root" in
      let do_while = fun base_case ->
        (*base case*)
        if base_case = true then
          for i = 1 to bf_ do
            let elem = List.nth !(root#subtrees) i in
              Queue.push elem line;
              if snd elem#pair = abs_index then ret := elem#pair;
          done;
        (*new line*)
        while not (Queue.is_empty line) do
          let popped = Queue.pop line in
          for i = 1 to bf_ do
            let elem = List.nth !(popped#subtrees) i in
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
      if self#get_root () = Null_node then bf_ <- data
      else raise (Failure "Attempt to change branching factor of non-empty tree");
  end
