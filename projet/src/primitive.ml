open Syntax

let rec primitive (expr : expr) (x : expr) = 
  match expr with
  (* k = k*x *)
  | Num k -> Light.((Num k)*x)
  | FloatNum k -> Light.((FloatNum k)*x)
  (* x *)
  | Var e when e=(Syntax.to_string x) ->
    primitive Light.(expr ^ Num 1) x
  (* x^n = (x^(n+1))/(n+1) *)
  | App2(Expo, e, n) when e = x && n <> App1(UMinus, Num 1) -> App2(Div, App2(Expo, x, App2(Plus, n, Num 1)), App2(Plus, n, Num 1))
  (* 1/x = ln|x| *)
  | App2(Div, Num 1, e) when e = x -> App1(Log, x) (* TODO ln|x|*)
  (* e^x = e^x *)
  | App2(Expo, App0(E), e) when e = x -> App2(Expo, App0(E), e)
  (* a^x = (a^x)/(ln(a)) *)
  | App2(Expo, a, e) when e = x -> App2(Div, App2(Expo, a, e), App1(Log, a))
  (* sin(x) = -cos(x) *)
  | App1(Sin, e) when e = x -> App1(UMinus, App1(Cos, e))
  (* cos(x) = sin(x) *)
  | App1(Cos, e) when e = x -> App1(Sin, e)
  (* 1/(cos(x)^2) = tg(x) *)
  | App2(Div, Num 1, App2(Expo, App1(Cos, e), Num 2)) when e = x -> App1(Tan, e)
  (* 1/(1+x^2) = arctg(x) *)
  | App2(Div, Num 1, App2(Plus, Num 1, App2(Expo, e, Num 2))) when e = x -> App1(ATan, e)
  (* 1/(sqrt(1-x^2)) *)
  | App2(Div, Num 1, App1(Sqrt, App2(Minus, Num 1, App2(Expo, e, Num 2)))) when e = x -> App1(ASin, e)
  | App1(UMinus, e) -> App1(UMinus, primitive e x)
  | _ -> expr