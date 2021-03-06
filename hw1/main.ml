let print_test test_name str_conv cmp answer result =
  let compare_result = cmp answer result in
  match test_name, compare_result with
  | "", compare_result -> (
      match compare_result with
      | true -> print_endline (
          string_of_bool compare_result
        )
      | false -> print_endline (
          (string_of_bool compare_result) ^
          " answer: " ^ (str_conv answer) ^
          " result: " ^ (str_conv result)
        )
    )
  | test_name, compare_result -> (
      match compare_result with
      | true -> print_endline (
          test_name ^ ": " ^ (string_of_bool compare_result)
        )
      | false -> print_endline (
          test_name ^ ": " ^
          (string_of_bool compare_result) ^
          " answer: " ^ (str_conv answer) ^
          " result: " ^ (str_conv result)
        )
    )

let print_test_equal ?test_name:(test_name="") str_conv answer result =
  print_test test_name str_conv (=) answer result

open Merge_final
let _ =
  let str_conv = fun int_list -> String.concat " " (List.map string_of_int int_list) in
  print_test_equal ~test_name:("merge1") str_conv [6; 5; 3; 2; 1; 1] (merge ([5; 3; 1], [6; 2; 1]))

open Sigma_final
let _ =
  let str_conv = string_of_int in
  print_test_equal ~test_name:("sigma1") str_conv 110 (sigma (1, 10, function x -> 2 * x));
  print_test_equal ~test_name:("sigma2") str_conv 385 (sigma (1, 10, fun x -> x * x));
  print_test_equal ~test_name:("sigma3") str_conv 0 (sigma (3, 1, fun x -> x * x));
  print_test_equal ~test_name:("sigma4") str_conv 27 (sigma(3, 3, fun x -> x * x * x));
  print_test_equal ~test_name:("sigma5") str_conv 385 (sigma(-10, -1, fun x -> x * x));
  print_test_equal ~test_name:("sigma6") str_conv 0 (sigma (11, 10, function x -> 2 * x));

open Iter_final
let _ =
  let str_conv = string_of_int in
  print_test_equal ~test_name:("iter1") str_conv 10 (iter (5, function x -> 2 + x) 0);
  print_test_equal ~test_name:("iter2") str_conv 32 (iter (5, function x -> 2 * x) 1);
  print_test_equal ~test_name:("iter3") str_conv 3 (iter (0, function x -> 2 * x) 3);

open Natop_final
let _ =
  let str_conv = string_of_int in

  let rec nat_to_int =
    fun n ->
      match n with
      | ZERO -> 0
      | SUCC n1 -> 1 + nat_to_int n1
  in
  let three = SUCC (SUCC (SUCC ZERO)) in
  let four = SUCC three in
  let five = SUCC four in

  print_test_equal ~test_name:("nat1") str_conv 7 (nat_to_int (natadd (three, four)));
  print_test_equal ~test_name:("nat2") str_conv 0 (nat_to_int (natadd (ZERO, ZERO)));
  print_test_equal ~test_name:("nat3") str_conv 3 (nat_to_int (natadd (ZERO, three)));
  print_test_equal ~test_name:("nat4") str_conv 4 (nat_to_int (natadd (four, ZERO)));

  print_test_equal ~test_name:("nat5") str_conv 15 (nat_to_int (natmul (three, five)));
  print_test_equal ~test_name:("nat6") str_conv 12 (nat_to_int (natmul (three, four)));
  print_test_equal ~test_name:("nat7") str_conv 0 (nat_to_int (natmul (ZERO, three)));
  print_test_equal ~test_name:("nat8") str_conv 0 (nat_to_int (natmul (four, ZERO)));
  print_test_equal ~test_name:("nat9") str_conv 0 (nat_to_int (natmul (ZERO, ZERO)));
  print_test_equal ~test_name:("nat10") str_conv 3 (nat_to_int (natmul (SUCC ZERO, three)));
  print_test_equal ~test_name:("nat11") str_conv 4 (nat_to_int (natmul (four, SUCC ZERO)));

  (* let _ = print_string "eval1: " in
     let eval_results =
     (eval TRUE)::
     (eval FALSE)::
     (eval (NOT FALSE))::
     (eval (ANDALSO (TRUE, TRUE)))::
     (eval (ANDALSO (TRUE, FALSE)))::
     (eval (ORELSE (TRUE, FALSE)))::
     (eval (ORELSE (FALSE, FALSE)))::
     (eval (IMPLY (LESS (PLUS (NUM 1, NUM 1), MINUS (NUM 3, NUM 1)), TRUE)))::
     [] in
     let ans =
     true::
     false::
     true::
     true::
     false::
     true::
     false::
     true::
     [] in
     if eval_results = ans
     then print_endline (
      "👍  " ^ (String.concat " " (List.map string_of_bool eval_results))
     )
     else print_endline (
      "❗\n answer = " ^
      (String.concat " " (List.map string_of_bool ans)) ^
      "\n result = " ^
      (String.concat " " (List.map string_of_bool eval_results))
     ) *)
