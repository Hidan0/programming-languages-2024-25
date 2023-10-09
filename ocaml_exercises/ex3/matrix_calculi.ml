(*
Write the matrix datatype with the following operations:

A function zeroes to construct a matrix of size n×m filled with zeros.
A function identity to construct the identity matrix (the one with all 0s but the 1s on the diagonal) of given size.
A function init to construct a square matrix of a given size n filled with the first n×n integers.
A function transpose that transposes a generic matrix independently of its size and content.
The basics operators + and * that adds and multiplies two matrices non necessarily squared.
*)

type matrix = int list list

let print_matrix (m : matrix) : unit =
  let rec aux = function
    | [] -> ()
    | l :: t ->
        List.iter (Printf.printf "%d ") l;
        print_newline ();
        aux t
  in
  aux m;
  print_newline ()

let zeros (n : int) (m : int) : matrix =
  List.init n (fun _ -> List.init m (fun _ -> 0))

let identity n m : matrix =
  List.init n (fun r -> List.init m (fun c -> if r == c then 1 else 0))

let init n : matrix =
  List.init n (fun r -> List.init n (fun c -> (r * n) + c + 1))

let rec transpose m =
  match m with
  | [] -> []
  | [] :: row -> transpose row
  | (hd :: tl) :: rest ->
      (hd :: List.map List.hd rest) :: transpose (tl :: List.map List.tl rest)

let ( ++ ) m1 m2 =
  List.map2 (fun r1 r2 -> List.map2 (fun c1 c2 -> c1 + c2) r1 r2) m1 m2

(* Dot product *)
let ( ** ) v1 v2 =
  let rec dotprod acc v1 v2 =
    match (v1, v2) with
    | hd1 :: tl1, hd2 :: tl2 -> dotprod (acc + (hd1 * hd2)) tl1 tl2
    | _, _ -> acc
  in
  dotprod 0 v1 v2

let ( *** ) m1 m2 =
  let matprod m1 m2 =
    let cols2 = transpose m2 in
    List.map (fun r1 -> List.map (fun c2 -> r1 ** c2) cols2) m1
  in
  matprod m1 m2

let () = print_matrix (zeros 2 4)
let () = print_matrix (identity 4 4)
let () = print_matrix (init 4)
let () = print_matrix (transpose (init 4))
let () = print_matrix (init 4 ++ identity 4 4)

(* let () = print_int ([ 1; 3; -5 ] ** [ 4; -2; -1 ]) *)
(* let () = print_int ([ 1; 2; 3 ] ** [ 0; 1; 0 ]) *)
let () = print_matrix (init 4 *** identity 4 4)
let () = print_matrix (init 4 *** zeros 4 4)
