module type IStack = sig
  exception Empty

  val is_empty : 'a list -> bool
  val push : 'a list -> 'a -> 'a list
  val pop : 'a list -> 'a * 'a list
end

module SafeStack : IStack = struct
  exception Empty

  (** [is_empty stk] returns [true] if [stk] is empty, otherwise [false]. *)
  let is_empty = function [] -> true | _ -> false

  (** [push stk x] returns [stk] with [x] pushed onto the top of the stack. *)
  let push stk x = x :: stk

  (** [pop stk] raises [Empty] if [stk] is empty, otherwise [(top, rest)]. *)
  let pop = function [] -> raise Empty | hd :: tl -> (hd, tl)
end
