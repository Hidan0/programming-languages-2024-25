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
  aux m

let zeros (n : int) (m : int) : matrix =
  List.init n (fun _ -> List.init m (fun _ -> 0))

let identity n m : matrix =
  List.init n (fun r -> List.init m (fun c -> if r == c then 1 else 0))

let init n : matrix =
  List.init n (fun r -> List.init n (fun c -> (r * n) + c + 1))

let () = print_matrix (zeros 2 4);;

print_newline ()

let () = print_matrix (identity 4 4);;

print_newline ()

let () = print_matrix (init 4);;

print_newline ()
