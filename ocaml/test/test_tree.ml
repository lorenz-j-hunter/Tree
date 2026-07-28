open Treelib.Treedef
open Treelib.Funcs
open Format
let () =
  printf "@;<0 3>%s" "()";
  let tree_test_code () =
    printf "@;<0 6>tree_test_code ()"; print_newline ();
    let t0 = new Treelib.Treedef.tree in
      printf "@;<0 9>t0 = new Treelib.Treedef.tree@."; print_newline ();
      (*Test fill*)
      printf "@;<0 9>@[(%s" "Test fill"; print_newline ();
      let start = int_of_float (time_us ()) in
        printf "@;<0 12>("; print_newline ();
        for i = 5 to 12 do t0#fill (float_of_int i); printf "@;<0 12>%s" "t0#fill"; print_newline (); done;
        let duration = int_of_float (time_us ()) - start in
          printf "@;<0 15>("; print_newline ();
          printf "@;<0 15>This should read '5 6 7 8 9 10 11 12':"; print_newline ();
          printf "@;<0 15>"; t0#print (); print_newline ();
          printf "@;<0 15>(duration, us=%d)" duration; print_newline ();
          printf "@;<0 15>)"; print_newline ();
        printf "@;<0 12>)"; print_newline ();
      printf "@;<0 9>@])"; print_newline ();
      (*Test pop*)
      Format.printf "@;<0 9>@[(%s" "Test pop"; print_newline ();
      let start = int_of_float (time_us ()) in
        printf "@;<0 12>("; for i = 1 to 3 do t0#pop (); printf "@;<0 12>t0#pop ()"; print_newline (); done;
        let duration = int_of_float (time_us ()) - start in
          printf "@;<0 15>("; print_newline ();
          printf "@;<0 15>This should read '5 6 7 8 9':\t"; print_newline ();
          printf "@;<0 15>"; t0#print (); print_newline ();
          printf "@;<0 15>(duration, us=%d)" duration; print_newline ();
          printf "@;<0 15>)"; print_newline ();
        printf "@;<0 12>)"; print_newline ();
      printf "@;<0 9>@])"; print_newline ();
      (*Test convert*)
      Format.printf "@;<0 9>@[(%s" "Test convert"; print_newline ();
      let start = int_of_float (time_us ()) in
        printf "@;<0 12>(%s" "List.map unpack_pair_op (t0#convert ())"; print_newline ();
        let array = List.map unpack_pair_op (t0#convert ()) in
        let duration = int_of_float (time_us ()) - start in
          printf "@;<0 15>("; print_newline ();
          printf "@;<0 15>This should read '5 6 7 8 9':"; print_newline ();
          printf "@;<0 15>"; for i = 0 to (List.length array) - 1 do printf "%f " (fst (List.nth array i)); done; print_newline ();
          printf "@;<0 15>(duration, us=%d)" duration; print_newline ();
        printf "@;<0 12>)"; print_newline ();
      printf "@;<0 9>@])"; print_newline ();
    printf "@;<0 6>)"; printf "@;<0 6>(";
    let t1 = ref (new Treelib.Treedef.tree) in
      (*Test operator=*)
      Format.printf "@;<0 9>@[(%s" "Test operator="; print_newline ();
      printf "@;<0 12>(%s" "t0 := t0";
      let start = int_of_float (time_us ()) in
        t1 := t0; 
        let duration = int_of_float (time_us ()) - start in
          printf "@.@;<0 15>("; print_newline ();
          printf "@;<0 15>This should read '5 6 7 8 9':"; print_newline ();
          printf "@;<0 15>"; (!t1)#print(); print_newline ();
          printf "@;<0 15>(duration, us=%d)" duration; print_newline ();
        printf "@;<0 12>)"; print_newline ();
      printf "@;<0 9>@])"; print_newline ();
      (*Test convert*)
      Format.printf "@;<0 9>@[(%s" "Test convert"; print_newline ();
      let start = int_of_float (time_us ()) in 
        printf "@;<0 12>(%s" "List.map unpack_pair_op ((!tl)#convert ())"; print_newline ();
        let array = List.map unpack_pair_op ((!t1)#convert ()) in
        let duration = int_of_float (time_us ()) - start in
          printf "@;<0 15>("; print_newline ();
          printf "@;<0 15>This should read '5 6 7 8 9':"; print_newline ();
          printf "@;<0 15>"; for i = 0 to (List.length array) - 1 do Format.printf "%f " (fst (List.nth array i)); done; print_newline ();
          printf "@;<0 15>(duration, us=%d)" duration; print_newline ();
        printf "@;<0 12>)"; print_newline ();
      printf "@;<0 9>@])"; print_newline ();
      (*Test insert*)
      Format.printf "@;<0 9>@[(%s" "Test insert"; print_newline ();
      let start = int_of_float (time_us ()) in
        printf "@;<0 12>(%s" "(!t1)#insert 15. 2 2"; (!t1)#insert 15. 2 2; print_newline ();
        let duration = int_of_float (time_us ()) - start in
          printf "@;<0 15>("; print_newline (); 
          printf "@;<0 15>unq_ should read '7': %d\t.alc_unq_ should read '7': %d\n" ((!t1)#get_unq()) ((!t1)#get_alc_unq()); print_newline ();
          printf "@;<0 15>This should read '5 6 7 8 9 15':"; print_newline (); 
          printf "@;<0 15>"; (!t1)#print(); print_newline ();
          printf "@;<0 15>(duration, us=%d)" duration; print_newline ();
        printf "@;<0 12>)"; print_newline ();
      printf "@;<0 9>@])"; print_newline ();
      printf "@;<0 9>@["; print_newline ();
      printf "@;<0 12>(%s" "(!t1)#insert 20. 2 7";
      let start = int_of_float (time_us ()) in
        (!t1)#insert 20. 2 7; 
        let duration = int_of_float (time_us ()) - start in
          printf "@.@;<0 15>("; print_newline ();
          printf "@;<0 15>unq_ should read '12': %d\t.alc_unq_ should read '13': %d\n" ((!t1)#get_unq()) ((!t1)#get_alc_unq()); print_newline ();
          printf "@;<0 15>This should read '5 6 7 8 9 15 20':"; print_newline ();
          printf "@;<0 15>"; (!t1)#print(); print_newline ();
          printf "@;<0 15>(duration, us=%d)" duration; print_newline ();
        printf "@;<0 12>)"; print_newline ();
      printf "@;<0 9>@])"; print_newline ();
    printf "@;<0 6>)"; print_newline ();
    printf "@;<0 6>(%s" "t2 = new Treelib.Treedef.tree"; print_newline ();
    let t2 = new Treelib.Treedef.tree in
      printf "@;<0 9>@[("; print_newline ();
      printf "@;<0 12>(%s" "t2#fill 1.; t2#insert 1. 1 2";
      let start = int_of_float (time_us ()) in
         t2#fill 1.; t2#insert 1. 1 2; 
        let duration = int_of_float (time_us ()) - start in
          printf "@.@;<0 15>("; print_newline ();
          printf "@;<0 15>This should read '1 1':"; print_newline ();
          printf "@;<0 15>"; t2#print (); print_newline ();
          printf "@;<0 15>(duration, us=%d)" duration; print_newline ();
        printf "@;<0 12>)"; print_newline (); 
      printf "@;<0 15>@])"; print_newline ();
      (*test remove*)
      Format.printf "@;<0 9>(@[<v 0>%s" "Test remove"; print_newline (); 
      printf "@;<0 12>(%s" "t2#remove 1 2";
      let start = int_of_float (time_us ()) in
        t2#remove 1 2; 
        let duration = int_of_float (time_us ()) - start in
          printf "@.@;<0 15>("; print_newline (); 
          printf "@;<0 15>This should read '1.00':\t"; t2#print (); printf "(duration, us=%d)" duration; print_newline ();
          printf "@;<0 15>)"; print_newline (); 
        printf "@;<0 12>)"; print_newline ();
      t2#insert 1. 1 2; Format.printf "@;<0 9>@])"; print_newline ();
      (*test alloc_lvl*)
      Format.printf "@;<0 9>(@[<v 0>%s" "Test alloc_lvl"; print_newline (); 
      printf "@;<0 9>"; printf "alc_unq_ should read '4':\t%d\n" (t2#get_alc_unq ()); print_newline ();
      printf "@;<0 12>(%s" "t2#alloc_lvl ()";
      let start = int_of_float (time_us ()) in
        t2#alloc_lvl (); 
        let duration = int_of_float (time_us ()) - start in
          printf "@.@;<0 15>("; printf "unq_ is %d" (t2#get_unq ()); print_newline (); 
          printf "@;<0 15>"; printf "alc_unq_ should read '13'. alc_unq_=%d" (t2#get_alc_unq ()); print_newline ();
          printf "@;<0 15>(duration, us=%d)" duration; print_newline ();
          printf "@;<0 15>)"; print_newline ();
        printf "@;<0 12>)"; print_newline ();
      printf "@;<0 9>@])"; print_newline ();
      Format.printf "@;<0 9>(@[<v 0>%s" "alloc_by_bal"; print_newline (); 
      printf "@;<0 12>(%s" "t2#alloc_by_bal ()";
      let start = int_of_float (time_us ()) in
        t2#alloc_by_bal (); 
        let duration = int_of_float (time_us ()) - start in
          printf "@.@;<0 15>("; print_newline ();
          printf "@;<0 15>(duration, us=%d)" duration; print_newline ();
          printf "@;<0 15>)"; print_newline ();
        printf "@;<0 12>)"; print_newline ();
      printf "@;<0 9>@])"; print_newline ();
      Format.printf "@;<0 9>(@[<v 0>%s" "alloc_lvl ()"; print_newline ();
      printf "@;<0 12>(%s" "t2#alloc_lvl ()";
      let start = int_of_float (time_us ()) in
        t2#alloc_lvl (); 
        let duration = int_of_float (time_us ()) - start in
          printf "@.@;<0 15>(%s" "This should read '1 1':\t"; t2#print (); print_newline ();
          printf "@;<0 15>(duration, us=%d)" duration; print_newline ();
        printf "@;<0 12>)"; print_newline ();
      printf "@;<0 9>@])"; print_newline ();
      printf "@;<0 9>This should raise 'Attempt to create a disconnected graph'."; print_newline ();
      try
        t2#insert 1. 2 3; t2#print ();
      with Failure _ -> printf "Attempt to create a disconnected graph";
    printf "@;<0 6>)@.@;<0 6>(@.";
    (*Test branching factor*)
    let t3 = new tree in
      printf "@;<0 9>@[<v 0>(%s" "t3 = new tree"; print_newline ();
      Format.printf "@;<0 9>%s" "Test branching factor"; print_newline ();
      printf "@;<0 9>(%s" "t3#set_branching_factor 4"; t3#set_branching_factor 4; print_newline ();
      printf "@;<0 12>(@.";
      let start = time_us () |> int_of_float in
        for i = 44 to 58 do t3#fill (float_of_int i); printf "@;<0 12>t3#fill %d@." i; done;
        let duration = (time_us () |> int_of_float) - start in
          printf "@;<0 15>(@."; printf "@;<0 15>(duration, us = %d)" duration;
          printf "@.@;<0 15>%s@;<0 15>" "This should read 44-58:"; t3#print ();
          printf "@.@;<0 15>)@.";
        printf "@;<0 12>)@.";
      printf "@;<0 9>@])@.";
    printf "@;<0 6>)@.@;<0 6>(@.";
    (*test assignment*)
    let t4 = t3 in
      printf "@;<0 9>@[(%s@." "t4 = t3";
      printf "@;<0 9>%s@." "This should read 44-58:";
      printf "@;<0 9>"; t4#print ();
      (*test remove*)
      printf "@;<0 9>%s@." "This should read '44 45 46 47 48 49 50 51 52 53 55 56 57 58':";
      printf "@;<0 12>(%s@." "t4#remove 2 5";
      let start = time_us () |> int_of_float in
        t4#remove 2 5;
        let duration = (time_us () |> int_of_float) - start in
          printf "@.@;<0 15>(@.";
          printf "@;<0 15>%s@.@;<0 15>" "t4#print ()"; t4#print ();
          printf "@.@;<0 15>%s=%d@." "t4#get_alc_unq () " (t4#get_alc_unq ());
          printf "@.@;<0 15>(duration, us=%d)@." duration;
          printf "@;<0 15>)@.";
        printf "@;<0 12>)@.";
      printf "@;<0 9>@])@.@;<0 9>@[<v 0>(@.";
      printf "@;<0 9>%s@." "Now it should read '44 45 46 48 49 50 51 52 53 55 56':";
      printf "@;<0 12>(%s@." "t4#remove 1 2";
      let start = time_us () |> int_of_float in
        t4#remove 1 2;
        let duration = (time_us () |> int_of_float) - start in
          printf "@;<0 15>(@.";
          printf "@;<0 15>%s@.@;<0 15>" "t4#print ()"; t4#print ();
          printf "@.@;<0 15>%s=%d@." "t4#get_alc_unq () " (t4#get_alc_unq ());
          printf "@;<0 15>(duration, us=%d)@." duration;
          printf "@;<0 15>)@.";
      printf "@;<0 12>)@.@;<0 9>)@.";
      printf "@;<0 9>%s@." "Now it should read '44 46 48 53 55 56':";
      printf "@;<0 12>(%s@." "t4#remove 1 0";
      let start = (time_us () |> int_of_float) in
        t4#remove 1 0;
        let duration = (time_us () |> int_of_float) - start in
          printf "@;<0 15>(@.";
          printf "@;<0 15>%s@.@;<0 15>" "t4#print ()"; t4#print ();
          printf "@.@;<0 15>%s=%d@." "t4#get_alc_unq () " (t4#get_alc_unq ());
          printf "@.@;<0 15>(duration, us=%d)@." duration;
          printf "@;<0 15>)@.";
      printf "@;<0 12>)@.@;<0 9>@])@.";
      printf "@;<0 9>@[<v 0>(@.";
      printf "@;<0 9>%s@.@;<0 12>(%s@." "Now it should read '44 1.23 46 48 53 55 56':" "t4#insert 1.23 1 0";
      let start = (time_us () |> int_of_float) in
        t4#insert 1.23 1 0;
        let duration = (time_us () |> int_of_float) - start in
          printf "@;<0 15>(@.";
          printf "@;<0 15>%s@.@;<0 15>" "t4#print ()"; t4#print ();
          printf "@.@;<0 15>%s=%d@." "t4#get_alc_unq () " (t4#get_alc_unq ());
          printf "@.@;<0 15>(duration, us=%d)@." duration;
          printf "@;<0 15>)@.";
      printf "@;<0 12>)@.@;<0 9>@])@.";
      printf "@;<0 9>@[<v 0>(@.";
      printf "@;<0 9>%s@.@;<0 12>(%s@." "Now it should read '44 1.23 46 48 4.56 53 55 56':" "t4#insert 4.56 2 0";
      let start = (time_us () |> int_of_float) in
        t4#insert 4.56 2 0;
        let duration = (time_us () |> int_of_float) - start in
          printf "@;<0 15>(@.";
          printf "@;<0 15>%s@.@;<0 15>" "t4#print ()"; t4#print ();
          printf "@.@;<0 15>%s=%d@." "t4#get_alc_unq () " (t4#get_alc_unq ());
          printf "@.@;<0 15>(duration, us=%d)@." duration;
          printf "@;<0 15>)@.";
      printf "@;<0 12>)@.@;<0 9>@])@.";
      (*test remove*)
      printf "@;<0 9>@[<v 0>(@.";
      printf "@;<0 9>%s@.@;<0 12>(%s@." "Now it should read '44 1.23 46 48 4.56 55 56':" "t4#remove 2 4";
      let start = (time_us () |> int_of_float) in
        t4#remove 2 4;
        let duration = (time_us () |> int_of_float) - start in
          printf "@;<0 15>(@.";
          printf "@;<0 15>%s@.@;<0 15>" "t4#print ()"; t4#print ();
          printf "@.@;<0 15>%s=%d@." "t4#get_alc_unq () " (t4#get_alc_unq ());
          printf "@.@;<0 15>(duration, us=%d)@." duration;
          printf "@;<0 15>)@.";
      printf "@;<0 12>)@.@;<0 9>@])@.";
      (*test insert*)
      printf "@;<0 9>@[<v 0>(@.";
      printf "@;<0 9>%s@.@;<0 12>(%s@." "Now it should read '44 1.23 46 7.89 48 4.56 55 56':" "t4#fill 7.89";
      let start = (time_us () |> int_of_float) in
        t4#fill 7.89;
        let duration = (time_us () |> int_of_float) - start in
          printf "@;<0 15>(@.";
          printf "@;<0 15>%s@.@;<0 15>" "t4#print"; t4#print ();
          printf "@.@;<0 15>%s=%d@." "t4#get_alc_unq () " (t4#get_alc_unq ());
          printf "@.@;<0 15>(duration, us=%d)@." duration;
          printf "@;<0 15>)@.";
      printf "@;<0 12>)@.@;<0 9>@])@.";
      (*test alloc_by_bal*)
      printf "@;<0 9>@[<v 0>(@.";
      printf "@;<0 9>%s%d%s@.@;<0 12>(%s@." "alc_unq_ is " (t4#get_alc_unq ()) ", should be 13" "t4#alloc_by_bal";
      let start = (time_us () |> int_of_float) in
        t4#alloc_by_bal ();
        let duration = (time_us () |> int_of_float) - start in
          printf "@;<0 15>(@.";
          printf "@;<0 15>(duration, us=%d)@." duration;
          printf "@;<0 15>%s%d%s@." "The alc_unq_ now is " (t4#get_alc_unq()) ", it should be 21";
          printf "@;<0 15>%s@.@;<0 15>" "t4#print"; t4#print ();
          printf "@.@;<0 15>)@.";
      printf "@;<0 12>)@.@;<0 9>@])@.";

  in tree_test_code ()
