module type PolishCalculator = sig
  type expr

  val expr_of : string -> expr
  val eval : expr -> int
end

module PolishCalculator (S : Stack.IStack) = struct
  type tokens = Sum | Sub | Mul | Div | Val of int

  let expr_of str =
    let rec parse tks s =
      match tks with
      | h :: t ->
          (match h with
          | "+" -> S.push s Sum
          | "-" -> S.push s Sub
          | "*" -> S.push s Mul
          | "/" -> S.push s Div
          | value -> S.push s (Val (int_of_string value)));
          parse t s
      | [] -> s
    in
    let s = S.empty () in
    let tks = List.rev (String.split_on_char ' ' str) in
    parse tks s

  type value = Some of int | None

  let eval exp =
    let rec evaluate s v1 v2 =
      match S.is_empty s with
      | true -> 0
      | false -> (
          let tk = S.pop s in
          match tk with
          | Val va -> (
              if S.is_empty s then va
              else
                match (v1, v2) with
                | None, _ -> evaluate s (Some va) v2
                | _, None -> evaluate s v1 (Some va)
                | Some _, Some _ -> failwith "Value after 2 values")
          | Sum -> (
              match (v1, v2) with
              | Some vv1, Some vv2 ->
                  S.push s (Val (vv1 + vv2));
                  evaluate s None None
              | _, _ -> failwith "Invalid parameters for sum")
          | Sub -> (
              match (v1, v2) with
              | Some vv1, Some vv2 ->
                  S.push s (Val (vv1 - vv2));
                  evaluate s None None
              | _, _ -> failwith "Invalid parameters for sub")
          | Mul -> (
              match (v1, v2) with
              | Some vv1, Some vv2 ->
                  S.push s (Val (vv1 * vv2));
                  evaluate s None None
              | _, _ -> failwith "Invalid parameters for mul")
          | Div -> (
              match (v1, v2) with
              | Some vv1, Some vv2 ->
                  S.push s (Val (vv1 / vv2));
                  evaluate s None None
              | _, _ -> failwith "Invalid parameters for div"))
    in
    evaluate exp None None
end
