module type IStack = sig
  type 'a stack

  exception EmptyStack

  val empty : unit -> 'a stack
  val is_empty : 'a stack -> bool
  val push : 'a stack -> 'a -> unit
  val pop : 'a stack -> 'a
  val top : 'a stack -> 'a
end

module Stack : IStack = struct
  type 'a stack = { mutable lst : 'a list }

  exception EmptyStack

  let empty () = { lst = [] }
  let is_empty s = List.length s.lst == 0
  let push s el = s.lst <- el :: s.lst
  let top s = match s.lst with h :: t -> h | [] -> raise EmptyStack

  let pop s =
    match s.lst with
    | h :: t ->
        s.lst <- t;
        h
    | [] -> raise EmptyStack
end
