type 'a tree =
  | Leaf
  | Node of 'a * 'a tree list

let rec preorder = fun (t : 'a tree) ->
  match t with
  | Leaf -> () 
  | Node (v, sts) -> 
    let bf = List.length sts in
      for i = 0 to bf - 1 do
        preorder (List.nth sts i)
      done