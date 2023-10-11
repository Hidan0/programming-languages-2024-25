(*
Define the following functions/operators on strings:

1. is_palindrome: string → bool that checks if the string is palindrome, 
   a string is palindrome when the represented sentence can be read the same way in either 
   directions in spite of spaces, punctual and letter cases, e.g., detartrated, "Do geese see God?", "Rise to vote, sir.", ...
2. operator (-): string → string → string that subtracts the letters in a string from the letters in another string, e.g., 
   "Walter Cazzola"-"abcwxyz" will give "Wlter Col" note that the operator - is case sensitive
3. anagram : string → string list → boolean that given a dictionary of strings, checks if the input string is an anagram 
   of one or more of the strings in the dictionary
*)

let is_letter c =
  let code = Char.code c in
  (code >= 65 && code <= 90) || (code >= 97 && code <= 122)

let pre_process str =
  let rec aux acc = function
    | [] -> acc
    | c :: cs ->
        aux (if is_letter c then Char.lowercase_ascii c :: acc else acc) cs
  in
  aux [] (str |> String.to_seq |> List.of_seq)

let is_palindrome str =
  let str = pre_process str in
  let rec aux s e =
    if s >= e then true
    else if List.nth str s == List.nth str e then aux (s + 1) (e - 1)
    else false
  in
  aux 0 (List.length str - 1)

(* === TESTS === *)
let () = pre_process "Do geese see God?" |> List.iter (fun c -> print_char c)

let () =
  assert (
    let str = "Do geese see God?" in
    is_palindrome str);

  assert (
    let str = "Rise to vote, sir." in
    is_palindrome str);

  assert (not (is_palindrome "Walter Cazzola"));
  assert (is_palindrome "detartrated");
  assert (is_palindrome "anna")
