open Lib.Tree
open Lib.Funcs

let test_add () =
  Alcotest.(check bool) "doesn't crash" true (try
    let t = root_ "Test" in
      preorder t; true
  with _ -> false)

let () =
  let open Alcotest in
  run "Add" [
    "preorder add", [test_case "doesn't crash" `Quick test_add]
  ]