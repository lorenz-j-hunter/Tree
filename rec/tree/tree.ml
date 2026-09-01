open Stack

type 'a node = {value:'a; index:int; subtrees:'a node list}

type 'a tree =
  | Leaf
  | Node of 'a node * 'a tree list * int

let root_ ?bf:(bf=3) init = Node ({value=init; index=0; subtrees=[]}, (fill Leaf [] bf), bf)

(*(Presumably) fills `results` with the results of preorder.*)
let rec preorder = fun t (results: 'a stack ref)  ->
  match t with
  | Leaf -> !results#push Leaf  
  | Node (n, sts, bf) ->
    for i = 0 to bf - 1 do preorder (List.nth sts i) results done;
