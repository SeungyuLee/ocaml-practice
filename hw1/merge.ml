let rec merge ((ilist1: int list), (ilist2: int list)): int list =
  match (ilist1, ilist2) with
  | (_, []) -> ilist1
  | ([], _) -> ilist2
  | (hd1::tl1, hd2::tl2) -> if (hd1 > hd2) then hd1::merge (tl1, hd2::tl2)
                            else hd2::merge (hd1::tl1, tl2)


let test (f : 'a -> 'b) (input : 'a) (output : 'b) : unit =
  if ((f input) = output)
    then ((print_string ("correct answer")); (print_newline ()))
    else ((print_string ("wrong answer")); (print_newline ()))

let _ =
  let test_merge = test merge in
  (test_merge ([3;2;1], [6;5;4]) [6;5;4;3;2;1]);
  (test_merge ([5;3;1], [6;4;2]) [6;5;4;3;2;1]);
  (test_merge ([1], [2]) [2;1]);
  (test_merge ([], [3;2]) [3;2]);
  (test_merge ([5;3], []) [5;3])

let _ =
  let print_bool x =
  print_endline (string_of_bool x) in
  print_bool ([7;5;4;3;2;1] = merge ([7; 2; 1], [5; 4; 3]));
  print_bool ([] = merge ([], []));
  print_bool ([9;2] = merge ([9; 2], []));
  print_bool ([7;3] = merge ([], [7; 3])); 
  print_bool ([5;5;4;4;3;3] = merge ([5; 4; 3], [5; 4; 3])); 
  print_bool ([8;6;5;4;3;2;1;0] = merge ([5; 3; 1], [8; 6; 4; 2; 0]))
