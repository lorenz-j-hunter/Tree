open Stack

let s = new stack ;

type 'a tree =
  | Leaf
  | Node of 'a * 'a tree list

let rec fill = fun l bf f ->
  match l with
  | [] -> l @ [f] 
  | hd :: tl ->
    if List.length tl = bf - 1 then
      l @ [f]
    else
      fill l bf f 

let rec preorder = fun (t : 'a tree) ->
  match t with
  | Leaf -> () 
  | Node (v, sts) -> 
    let l = fill [] (List.length sts) preorder in
      ignore (l)