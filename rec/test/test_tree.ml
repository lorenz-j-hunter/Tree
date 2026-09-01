open Tree
open Stack

let test_preorder () =
  Alcotest.(check bool) "preorder doesn't crash" true (try
    let s = ref (new stack) in
    let r = root_ 1 in ignore (preorder r s); true
    with _ -> false)

let () =
  let open Alcotest in
  run "Tree" [
    "preorder", [test_case "doesn't crash" `Quick test_preorder];
  ]