let find_path (s : int) (e : int) : string =
  let rec aux acc s e =
    if s < e then
      if s * 4 > e then aux ('P' :: acc) (s + 1) e
      else if e mod 2 <> 0 || e / 4 * 4 <> e then aux ('P' :: acc) s (e - 1)
      else aux ('S' :: acc) s (e / 4)
    else acc
  in
  String.of_seq (List.to_seq (aux [] s e))

let () =
  assert (find_path 4 15 = "PPPPPPPPPPP");
  assert (find_path 5 81 = "SSP");
  assert (find_path 10 130 = "PPPPPPPPPPPPPPPPPPPPPPSPP");
  assert (find_path 5 24 = "PS");
  assert (find_path 5 34 = "PPPSPP");
  assert (find_path 4 19 = "SPPP")
