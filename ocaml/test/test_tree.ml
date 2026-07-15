open Treelib.Treedef
open Treelib.Funcs
open Format
let () =
  let tree_test_code () =
    let t0 = new Treelib.Treedef.tree in
      (*test append*)
      open_vbox 1;
      let start = int_of_float (time_ms ()) in begin
        for i = 5 to 12 do
          t0#append (float_of_int i);
        done;
        let duration = int_of_float (time_ms ()) - start in
          printf "This should read '5 6 7 8 9 10 11 12':\t";
          print_break 3 1;
          t0#print ();
          print_space (); printf "(duration, ms=%d)" duration;
      end;
      close_box (); print_newline (); print_newline ();
      (*test pop*)
      open_vbox 1;
      let start = int_of_float (time_ms ()) in begin
        t0#pop (); t0#pop (); t0#pop ();
        let duration = int_of_float (time_ms ()) - start in
          printf "This should read '5 6 7 8 9':\t";
          print_break 3 1;
          t0#print ();
          print_space (); printf "(duration, ms=%d)" duration;
      end;
      close_box (); print_newline (); print_newline ();
      (*test convert*)
      open_vbox 1;
      let start = int_of_float (time_ms ()) in begin
        let array = unpack (t0#convert ()) in
        let duration = int_of_float (time_ms ()) - start in
          printf "This should read '5 6 7 8 9':\t";
          print_break 3 1;
          for i = 1 to List.length array do
            Format.printf "%f " (fst (List.nth array i));
          done;
          print_space (); printf "(duration, ms=%d)" duration;
      end;
      close_box ();
    
  in tree_test_code ()
