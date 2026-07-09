let g_neg_one = fun num -> if num > -1 then true else false
let rec none_of = fun (l: int list) condition -> (*tail recursive.*)
  match l with
  | [] -> true
  | hd :: tl ->
    if condition hd = false then false
    else 
      none_of tl condition 