module type Stack = sig
  val is_empty : 'a list -> bool
  val push : 'a list -> 'a -> 'a list
  val peek : 'a list -> 'a option
  val pop : 'a list -> 'a list option
end

module SafeStack : Stack = struct
  (** [SafeStack] is a stack data structure with option types. *)

  (** [is_empty stk] returns [true] if [stk] is empty, [false] otherwise. *)
  let is_empty = function [] -> true | _ -> false

  (** [push stk x] returns [stk] with [x] pushed onto it. *)
  let push stk x = x :: stk

  (** [peek stk] returns [None] if [stk] is empty, [Some x] otherwise, where [x] is the element on top. *)
  let peek = function [] -> None | x :: _ -> Some x

  (** [pop stk] returns [None] if [stk] is empty, [Some stk'] otherwise, where)
      [stk'] is [stk] with the top element removed. *)
  let pop = function [] -> None | _ :: tl -> Some tl
end
