let rec sigma ((a: int), (b: int), (f: (int -> int))): int = 
  if (a > b) then 0
  else match (a, b, f) with
       | (a, b, f) -> if (a = b) then f a
                      else f a + sigma (a+1, b, f)

let test (f : 'a -> 'b) (input : 'a) (output: 'b) : unit = 
  if ((f input) = output)
    then ((print_string ("correct")); (print_newline ()))
    else ((print_string ("wrong")); (print_newline ()))

let identity (a: int) : int = a
let double (a: int) : int = a*a

let _ =
  let test_sigma = test sigma in
  (test_sigma (5, 3, identity) 0);
  (test_sigma (3, 5, identity) 12);
  (test_sigma (5, 5, identity) 5);
  (test_sigma (1, 5, double) 55);
  (test_sigma (-5, 5, double) 110)

let _ =
  let print_bool x =
  print_endline (string_of_bool x) in
  print_bool (385 = sigma (1, 10, (fun x -> x*x)));
  print_bool (0 = sigma (3, 1, fun x -> x*x));
  print_bool (27 = sigma(3, 3, fun x -> x*x*x));
  print_bool (385 = sigma(-10, -1, fun x -> x*x))
