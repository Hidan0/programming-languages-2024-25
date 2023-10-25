exception EmptyStack

let is_empty = function [] -> true | _ -> false
let push lst x = x :: lst
let peek = function [] -> raise EmptyStack | x :: _ -> x
let pop = function [] -> raise EmptyStack | _ :: tl -> tl
