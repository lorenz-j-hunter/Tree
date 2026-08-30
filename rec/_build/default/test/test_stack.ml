open Stack

let test_push () =
  Alcotest.(check bool) "add doesn't crash" true (try
    let s = new stack in s#push 1; true
    with _ -> false)

let test_pop () =
  Alcotest.(check bool) "pop doesn't crash" true (try
    let s = new stack in for i = 0 to 3 do s#push i done;
    ignore (s#pop ());
    true
    with _ -> false)

let test_peek () =
  Alcotest.(check bool) "peek doesn't crash" true (try
    let s = new stack in for i = 0 to 3 do s#push i done;
    ignore (s#peek ());
    true
    with _ -> false)

let () =
  let open Alcotest in
  run "Stack Methods" [
    "push", [test_case "doesn't crash" `Quick test_push];
    "pop", [test_case "doesn't crash" `Quick test_pop];
    "peek", [test_case "doesn't crash" `Quick test_peek]
  ]