let rec fill = fun s ->
  match s#size () > 2 with
  | true -> s 
  | false -> s#push 1; fill s