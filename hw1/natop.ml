type nat = ZERO | SUCC of nat

let succ (a: nat): nat =
  match a with
  | a -> SUCC a

let pred (a: nat): nat =
  match a with
  | ZERO -> ZERO
  | SUCC b -> b

let rec iter ((n: nat), (f: nat -> nat)) (param: nat): nat =
  match (n, f) with
  | (ZERO, f) -> param
  | (n, f) -> f (iter(pred n, f) param)


let rec natadd ((a: nat), (b: nat)): nat = 
  match (a, b) with
  | (ZERO, b) -> b
  | (a, ZERO) -> a
  | (a, b) -> natadd (succ a, pred b)

let rec natmul ((a: nat), (b: nat)): nat =
  match (a, b) with
  | (ZERO, b) -> ZERO
  | (a, ZERO) -> ZERO
  | (SUCC ZERO, b) -> b
  | (a, SUCC ZERO) -> a
  | (a, b) -> natadd (natmul(pred a, b), b)


let _ =
  let rec nat_to_int : nat -> int =
    fun n -> match n with
    | ZERO -> 0
    | SUCC n1 -> 1 + nat_to_int n1
    in

    let print_bool x =
    print_endline (string_of_bool x)
    in

    let three = SUCC (SUCC (SUCC ZERO))
    in
    let four = SUCC three
    in

    print_bool (7 = nat_to_int (natadd (three, four)));
    print_bool (0 = nat_to_int (natadd (ZERO, ZERO))); 
    print_bool (3 = nat_to_int (natadd (ZERO, three))); 
    print_bool (4 = nat_to_int (natadd (four, ZERO))); 

    print_bool (12 = nat_to_int (natmul (three, four))); 
    print_bool (0 = nat_to_int (natmul (ZERO, three))); 
    print_bool (0 = nat_to_int (natmul (four, ZERO))); 
    print_bool (0 = nat_to_int (natmul (ZERO, ZERO))); 
    print_bool (3 = nat_to_int (natmul (SUCC ZERO, three))); 
    print_bool (4 = nat_to_int (natmul (four, SUCC ZERO))); 
