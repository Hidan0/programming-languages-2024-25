open Hamming

let test elem = hamming (fst elem) (snd elem)

let print_result (edits, ops) =
  let print_operation (a, b) = Printf.sprintf "'%c' <-> '%c'" a b in
  let ops_str = String.concat ", " (List.map print_operation ops) in
  Printf.printf "Number of edits: %d\nChanges: %s\n" edits ops_str

let main () =
  let test_cases =
    [
      ("kittens", "sitting");
      ("The cat jump down", "The Bats flies up");
      ("garden", "gordon");
      ("paris", "party");
    ]
  in
  List.iter print_result (List.map test test_cases)

let () = main ()
