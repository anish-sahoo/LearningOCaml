let v = 42
let float_num = 1.2
let float_num2 = 3.4
let int_to_float : float = float_of_int 42
let float_to_int : int = int_of_float 42.0

let f x = x + 1 (* this is a function *)
let add (f1:float) (f2:float) : float = f1 +. f2 (* inline type definitions*)

let () = Printf.printf "Result: %d\n" (f v)
let () = print_endline ("concatenating sum using ^ " ^ string_of_int (f 10))
let () = Printf.printf "Float Sum: %f\n" (add float_num float_num2)

(* only 4 primitive types in ocaml, conversions exist for int, float, bool to 
string and vice versa *)
let type1 : int = 10
let type2 : float = 10.1
let type3 : bool = true
let type4 : char = 'a'

let char_from_str : char = "hello".[1]
(* two equality operators: = (structural), == (physical)*)
(* corresponding inequality: <>, != *)

(* assertions *)
let () = assert (1 = 1)

(* if *)
let () = print_endline (if 1 = 1 then "true" else "false")

let this_works : int = 4 + (if 'a' = 'b' then 1 else 2) (* kinda like kotlin *)

let l_exp : int = 
  let x = 1 + 4 
  in x + 1

let () = Printf.printf "Local Expression: %d\n" l_exp

let this_evaluates_to = 
  let x = 5 in
  ((let y = f x in y) + x) (* this evaluates to f(x) + x = 11 *)

let () = Printf.printf "This evaluates to: %d\n" this_evaluates_to

let rec factorial n =
  if n = 0 then 1 
  else n * factorial (n - 1)

let rec fib n =
  if n = 0 then 0
  else if n = 1 then 1
  else fib (n - 1) + fib (n - 2)

let () = Printf.printf "Factorial of 5: %d\n" (factorial 5)
let () = Printf.printf "Fibonacci of 5: %d\n" (fib 5)

(* Apparently we can define mutually recursive functions *)
let rec even n = n = 0 || odd (n - 1)
  and odd n = n <> 0 && even (n - 1)  

let anonymous_fn = fun x-> x + 1

let () = Printf.printf "Anonymous Function: %d\n" (anonymous_fn 5)

let pow x = x * x
let decr x = x - 1
let incr x = x + 1

let () = 5 |> decr |> pow |> incr |> Printf.printf "Pipeline result 17: %d\n" 
let () = (incr (pow (decr 5))) |> Printf.printf "Non-pipeline result 17: %d\n"  

let labelled_args ~name_1:n1 : unit = Printf.printf "Name1: %s\n" n1

(* pattern matching *)
let rec sum xs =
  print_string ("Matching " ^ string_of_int (List.length xs) ^ "\n");
  match xs with
  | [] -> 0
  | x :: xs' -> x + sum xs'

let () = Printf.printf "Sum of 1, 2, 3: %d\n" (sum [1; 2; 3])

let rec append lst1 lst2 =
  match lst1 with
  | [] -> lst2
  | item :: rest_of_list -> item :: append rest_of_list lst2


let rec flip_list lst =
  match lst with
  | [] -> []
  | x :: xs -> append (flip_list xs) [x]

let () = Printf.printf "Flipped List: %s\n" 
  (String.concat ", " 
  (List.map string_of_int (flip_list [1; 2; 3])))

let rec fib_pattern n =
  match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> fib_pattern (n - 1) + fib_pattern (n - 2)

let () = Printf.printf "Fibonacci of 5 using pattern matching: %d\n" (fib_pattern 5)