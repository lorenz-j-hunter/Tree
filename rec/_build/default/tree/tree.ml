open Stack

type 'a node = {value:'a; index:int; subtrees:'a node list}

type 'a tree =
  | Leaf
  | Node of 'a node * 'a tree list * int

let rec fill f l bf =
  let arg = f :: l in
    match l with
    | [] -> fill f arg bf 
    | hd :: tl ->
      if List.length l = bf then l
      else fill f arg bf

let root_ ?bf:(bf=3) init = Node ({value=init; index=0; subtrees=[]}, (fill Leaf [] bf), bf)

let rec executeall task args (stk: 'a stack ref) =
  match args with
    | [] -> () 
    | hd :: [] -> let ret = task hd in !stk#push ret; executeall task [] stk 
    | hd :: tl -> let ret = task hd in !stk#push ret; executeall task tl stk ;;

(*(Presumably) fills `results` with the results of preorder.*)
let rec preorder = fun t (results: 'a stack ref)  ->
  match t with
  | Leaf -> !results#push Leaf  
  | Node (n, sts, bf) ->
    for i = 0 to bf - 1 do preorder (List.nth sts i) results done;
