(* Let's write a module to implement sin x n by using the Taylor's series (where n is the level of approximation, i.e., 1 only one item, 2 two items, 3 three items and so on). Do the same with cosine, tangent, logarithm and so on. *)
(* https://en.wikipedia.org/wiki/Taylor_series#Tayloronometric_functions *)

module type TAYLOR = sig
  val sin : float -> int -> float
  val cos : float -> int -> float
  val tan : float -> int -> float
  val arcsin : float -> int -> float
  val arccos : float -> int -> float
  val exp : float -> int -> float
end

module Taylor : TAYLOR = struct
  let rec pow x = function 0 -> 1 | n -> x * pow x (n - 1)
  let rec fpow x = function 0 -> 1. | n -> x *. fpow x (n - 1)
  let rec fact = function 0. -> 1. | 1. -> 1. | x -> x *. fact (x -. 1.)
  let pi = 3.141592653

  let rec sin x = function
    | 0 -> x
    | n ->
        let term =
          float_of_int (pow (-1) n)
          *. fpow x ((2 * n) + 1)
          /. fact ((2. *. float_of_int n) +. 1.)
        in
        sin x (n - 1) +. term

  let rec cos x = function
    | 0 -> 1.
    | n ->
        let term =
          float_of_int (pow (-1) n)
          *. fpow x (2 * n)
          /. fact (2. *. float_of_int n)
        in
        cos x (n - 1) +. term

  let tan x = function 0 -> x | n -> sin x n /. cos x n

  let rec arcsin x = function
    | 0 -> x
    | n ->
        let term =
          fact (2. *. float_of_int n)
          *. fpow x ((2 * n) + 1)
          /. (float_of_int (pow 4 n)
             *. fpow (fact (float_of_int n)) 2
             *. float_of_int ((2 * n) + 1))
        in
        arcsin x (n - 1) +. term

  let arccos x n = (pi /. 2.) -. arcsin x n

  let exp x n =
    let rec aux x acc = function
      | 0 -> 1. +. acc
      | n -> aux x (acc +. (fpow x n /. fact (float_of_int n))) (n - 1)
    in
    aux x 0. n
end
;;

Printf.printf "sin OG: %.6f Taylor: %.6f\n" (sin 3.) (Taylor.sin 3. 10);
Printf.printf "cos OG: %.6f Taylor: %.6f\n" (cos 3.) (Taylor.cos 3. 10);
Printf.printf "tan OG: %.6f Taylor: %.6f\n" (tan 3.) (Taylor.tan 3. 10);
Printf.printf "arcsin Taylor: %.6f\n" (Taylor.arcsin 0.5 10);
Printf.printf "arccos Taylor: %.6f\n" (Taylor.arccos 0.5 10);
Printf.printf "exp OG: %.6f Taylor: %.6f\n" (exp 3.) (Taylor.exp 3. 19)
