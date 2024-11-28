(* Allowed String.to_seq, List.of_seq, String.length *)

let str2list str = str |> String.to_seq |> List.of_seq

let reverse lst =
  let rec aux acc l = match l with [] -> acc | h :: t -> aux (h :: acc) t in
  aux [] lst

let hamming str1 str2 =
  let rec aux acc changes s1 s2 =
    match (s1, s2) with
    | c1 :: t1, c2 :: t2 when c1 = c2 -> aux acc changes t1 t2
    | c1 :: t1, c2 :: t2 -> aux ((c1, c2) :: acc) (changes + 1) t1 t2
    | [], [] -> (changes, reverse acc)
    | _, [] | [], _ -> invalid_arg "Strings have different sizes"
  in
  aux [] 0 (str2list str1) (str2list str2)
