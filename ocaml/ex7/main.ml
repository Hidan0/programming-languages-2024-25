open PolishCalculator
open Stack
module MyPC = PolishCalculator (Stack)

let () =
  let e = MyPC.expr_of "10 6" in
  let result = MyPC.eval e in
  Printf.printf "The result is: %d\n" result;

  let e = MyPC.expr_of "3 4 +" in
  let result = MyPC.eval e in
  Printf.printf "The result is: %d\n" result;

  let e = MyPC.expr_of "3 4 + 5 *" in
  let result = MyPC.eval e in
  Printf.printf "The result is: %d\n" result
