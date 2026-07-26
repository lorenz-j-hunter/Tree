open Nodedef
let g_neg_one = fun num -> if num > -1 then true else false
let g_zero = fun num -> if num > 0 then true else false
let eq_neg_one = fun num -> if num = -1 then true else false
let rec none_of = fun (l: int list) condition -> (*tail recursive.*)
  match l with
  | [] -> true
  | hd :: tl ->
    if condition hd = true then false
    else 
      none_of tl condition 

let rec all_of = fun (l: int list) condition -> (*tail recursive*)
  match l with
  | [] -> true
  | hd :: tl ->
    if condition hd = false then false
    else
      all_of tl condition
let pow = fun base exp -> int_of_float (float_of_int base ** float_of_int exp)
let rec replace = fun (l: 'a list) index value ->
  if index >= List.length l || index < 0 then failwith "Funcs.replace: index out of range";
  match index with
  | 0 -> value :: List.tl l
  | _ -> (List.hd l) :: replace (List.tl l) (index - 1) value
let time_us () = 
  Sys.time () *. 1_000_000.
let unpack_pair_op p = match p with | Pair (a, b) -> (a, b) | Null_pair -> (0., -2)
let unpack_node_op n = match n with | Node n -> n#pair | Null_node -> (0., -2)
let unpack_to_node n = match n with | Node n -> n | Null_node -> new _node_ (0., -2)
  