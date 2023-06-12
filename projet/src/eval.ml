open Syntax

let rec eval expr =
  match expr with
  | Num n -> float_of_int n
  | FloatNum n -> n
  | Var v -> failwith "Impossible d'évaluer la variable"
  | App0 Pi -> Float.pi 
  | App0 E -> 2.71828182845904523536
  | App1 (Sqrt, e) -> sqrt (eval e) 
  | App1 (Exp, e) -> exp (eval e)
  | App1 (Log, e) -> log (eval e)
  | App1 (Sin, e) -> sin (eval e)
  | App1 (Cos, e) -> cos (eval e)
  | App1 (Tan, e) -> tan (eval e)
  | App1 (ASin, e) -> asin (eval e)
  | App1 (ACos, e) -> acos (eval e)
  | App1 (ATan, e) -> atan (eval e)
  | App1 (UMinus, e) -> -.(eval e)
  | App2 (Plus, e1, e2) -> eval e1 +. eval e2
  | App2 (Minus, e1, e2) -> eval e1 -. eval e2
  | App2 (Mult, e1, e2) -> eval e1 *. eval e2
  | App2 (Div, e1, e2) -> eval e1 /. eval e2
  | App2 (Expo, e1, e2) -> eval e1 ** eval e2
