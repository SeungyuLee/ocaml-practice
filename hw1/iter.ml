let rec iter ((n: int), (f : 'a -> 'a)) (param: 'a) : 'a =
  match (n, f) with
  | (0, f) -> param
  | (n, f) -> f (iter(n-1, f) param)


let test (f : 'a -> 'b -> 'c) (input1 : 'a) (input2: 'b) (output: 'c) : unit =
  if ((f input1 input2) = output)
    then ((print_string ("correct")); (print_newline ()))
    else ((print_int ((f input1 input2))); (print_newline ()))

let double (a: int): int = a * a

let _ =
  let test_iter = test iter in
  (test_iter (3, double) 5 390625);
  (test_iter (5, function x -> 2+x) 0 10)
  

let _ =
  let print_bool x =
  print_endline (string_of_bool x) in
  print_bool (6 = iter (3, function x -> 2+x) 0 );
  print_bool (4 = iter (0, function x -> 2*x) 4 );
  print_bool (16383 = iter (11, function x -> 2*x+1) 7)
  
