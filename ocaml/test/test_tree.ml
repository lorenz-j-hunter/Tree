open Treelib.Treedef
open Treelib.Funcs
open Format
let () =
  let tree_test_code () =
    let t0 = new Treelib.Treedef.tree in
      (*Test append*)
      open_vbox 1; Format.printf "Test append\n";
      let start = int_of_float (time_ms ()) in begin
        for i = 5 to 12 do
          t0#append (float_of_int i);
        done;
        let duration = int_of_float (time_ms ()) - start in
          printf "This should read '5 6 7 8 9 10 11 12':\t";
          print_break 3 1;
          t0#print ();
          print_break 3 1; printf "(duration, us=%d)" duration;
      end;
      close_box (); print_newline (); print_newline ();
      (*Test pop*)
      open_vbox 1; Format.printf "Test pop\n";
      let start = int_of_float (time_ms ()) in begin
        t0#pop (); t0#pop (); t0#pop ();
        let duration = int_of_float (time_ms ()) - start in
          printf "This should read '5 6 7 8 9':\t";
          print_break 3 1;
          t0#print ();
          print_break 3 1; printf "(duration, us=%d)" duration;
      end;
      close_box (); print_newline (); print_newline ();
      (*Test convert*)
      open_vbox 1; Format.printf "Test convert\n";
      let start = int_of_float (time_ms ()) in begin
        let array = unpack (t0#convert ()) in
        let duration = int_of_float (time_ms ()) - start in
          printf "This should read '5 6 7 8 9':\t";
          print_break 3 1;
          for i = 0 to (List.length array) - 1 do
            Format.printf "%f " (fst (List.nth array i));
          done;
          print_break 3 1; printf "(duration, us=%d)" duration;
      end;
      close_box (); print_newline (); print_newline ();
    let t1 = ref (new Treelib.Treedef.tree) in
      (*Test operator=*)
      open_vbox 1; Format.printf "Test operator=\n";
      let start = int_of_float (time_ms ()) in
        t1 := t0;
        let duration = int_of_float (time_ms ()) - start in
          printf "This should read '5 6 7 8 9':\t";
          print_break 3 1;
          (!t1)#print();
          print_break 3 1; printf "(duration, us=%d)" duration;
      close_box (); print_newline (); print_newline ();   
      (*Test convert*)
      open_vbox 1; Format.printf "Test convert\n";
      let start = int_of_float (time_ms ()) in begin
        let array = unpack ((!t1)#convert ()) in
        let duration = int_of_float (time_ms ()) - start in
          printf "This should read '5 6 7 8 9':\t";
          print_break 3 1;
          for i = 0 to (List.length array) - 1 do
            Format.printf "%f " (fst (List.nth array i));
          done;
          print_break 3 1; printf "(duration, us=%d)" duration;
      end;
      close_box (); print_newline (); print_newline ();
      (*Test insert*)
      open_vbox 1; Format.printf "Test insert\n";
      let start = int_of_float (time_ms ()) in
        (!t1)#insert 15. 2 2;
        let duration = int_of_float (time_ms ()) - start in
          printf "unq_ should read '7': %d\t.alc_unq_ should read '7': %d\n" ((!t1)#get_unq()) ((!t1)#get_alc_unq());
          printf "This should read '5 6 7 8 9 15':\t"; print_break 3 1; (!t1)#print();
          print_break 3 1; printf "(duration, us=%d)" duration;
      close_box (); print_newline (); print_newline ();
      let start = int_of_float (time_ms ()) in
        (!t1)#insert 20. 2 7;
        let duration = int_of_float (time_ms ()) - start in
          printf "unq_ should read '12': %d\t.alc_unq_ should read '13': %d\n" ((!t1)#get_unq()) ((!t1)#get_alc_unq());
          printf "This should read '5 6 7 8 9 15 20':\t"; print_break 3 1; (!t1)#print();
          print_break 3 1; printf "(duration, us=%d)" duration;
    close_box (); print_newline (); print_newline ();
    let t2 = new Treelib.Treedef.tree in
      let start = int_of_float (time_ms ()) in
        t2#append 1.; t2#insert 1. 1 2;
        let duration = int_of_float (time_ms ()) - start in
          printf "This should read '1 1'::\t"; t2#print ();
          print_break 3 1; printf "(duration, us=%d)" duration;
    close_box (); print_newline (); print_newline ();
    (*test remove*)
      Format.printf "Test remove\n";
      let start = int_of_float (time_ms ()) in
      t2#remove 1 2;
      let duration = int_of_float (time_ms ()) - start in
        printf "This should read '1'::\t"; t2#print ();
        print_break 3 1; printf "(duration, us=%d)" duration;
  in tree_test_code ()
