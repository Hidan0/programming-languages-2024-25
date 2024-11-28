(*
Let's write a function (or a pool of functions) that given a quite large text (over than 2000 words) counts how frequent each word occurs in the text.

The text is read from a file (look at the pervasive module in the manual) and it is a real text with punctuation (i.e., commas, semicolons, ...) that should be counted (?????).

Note that words with different case should be considered the same.
*)

let input_file = "lorem.txt"
let read_line ic = try Some (input_line ic) with End_of_file -> None
let split_words line = String.split_on_char ' ' (String.lowercase_ascii line)

let read_words fname =
  let ic = open_in fname in
  let rec aux acc = function
    | None ->
        close_in ic;
        acc
    | Some l -> aux (acc @ split_words l) (read_line ic)
  in
  aux [] (read_line ic)

let frequencies : (string * float) list =
  let words = read_words input_file in
  let tot = List.length words in

  let rec aux acc words =
    match words with
    | [] -> acc
    | w :: ws -> (
        List.assoc_opt w acc |> function
        | None -> aux ((w, 1) :: acc) ws
        | Some n -> aux ((w, n + 1) :: List.remove_assoc w acc) ws)
  in
  let absolute = aux [] words in
  List.map (fun (w, n) -> (w, float_of_int n /. float_of_int tot)) absolute

let () = List.iter (fun (w, f) -> Printf.printf "[%s]: %f \n" w f) frequencies

let () =
  let rec aux acc = function [] -> acc | (_, f) :: fs -> aux (acc +. f) fs in
  aux 0. frequencies |> Printf.printf "Total: %f \n"
