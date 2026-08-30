open Tree

let rec leaves_of_len = fun bf l v ->
  match l with
  | [] -> leaves_of_len bf [v] v 
  | hd :: tl ->
    if List.length tl = bf - 1 then
      (l @ [v])
    else
      leaves_of_len bf (l @ [v]) v

let root_ ?bf:(bf=3) init  = Node (init, leaves_of_len bf [] Leaf)