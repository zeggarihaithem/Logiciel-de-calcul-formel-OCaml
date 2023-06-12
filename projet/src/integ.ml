open Syntax
open Primitive
open Subst
open Eval
open Simpl
open Derive

(*
  The "formula" in the code refers to the Fundamental theorem of calculus,
  integ f(x)dx from a to b = F(b) - F(a)   
*)

(* 
  We use this type to store different kinds of expressions together 
  For example, this will allow to store an expression during integration by parts, 
  where one part must be calculated by the formula, and the other is an integral.
  The Number node is used when taking constants out of the integral
*)
type node =
  | Error of expr                   (* Expressions that cannot be evaluated *)
  | Number of expr                  (* Float or integer numbers *)
  | Formule of expr                 (* Expressions that must be calculated by the formula *)
  | Integral of expr * string       (* Integral, where the string stores a differentiable variable *)
  | Internal2 of op2 * node * node

(* This type is used for integration in parts *)
type trig = Sin | Cos | Tan | ASin | ACos | ATan

(* Local evaluation to eliminate arithmetic operations with constants *)
let rec local_eval expr = 
  match expr with
  | Num n -> expr
  | FloatNum n -> expr
  | Var v -> expr
  | App0 e -> expr
  | App1(op, e1) ->
    let e1localeval = local_eval e1 in
    (try FloatNum (eval (App1(op, e1localeval))) with | _ -> App1(op, e1))
  | App2(op, e1, e2) -> 
    let e1localeval = local_eval e1 in
    let e2localeval = local_eval e2 in
    (try
      FloatNum (eval (App2(op, e1localeval, e2localeval)))
    with | _ -> (try
                  FloatNum (eval (App2(op, e1localeval, e2)))
                with | _ -> (try
                              FloatNum (eval (App2(op, e1, e2localeval)))
                            with | _ -> App2(op, e1localeval, e2localeval))))


(* Building a tree to apply the linearity rules 
   (multiplication/division by a number and the sum of integrals) *)
let rec arith_tree expr x =
  let expr = replace_minus expr in
  match expr with
  | App2 (op, e1, e2) when op == Mult ->
    let e1type = (match e1 with | FloatNum _ | Num _ -> true | _ -> false) in
    let e2type = (match e2 with | FloatNum _ | Num _ -> true | _ -> false) in
    (match e1type, e2type with
    | true, true -> Number (local_eval (App2 (op, e1, e2)))
    | true, false -> Internal2 (op, Number e1, arith_tree e2 x)
    | false, true -> Internal2 (op, arith_tree e1 x, Number e2)
    | false, false -> Integral (App2 (op, e1, e2), x)
    )
  | App2 (op, e1, e2) when op == Div ->
    let e2type = (match e2 with | FloatNum _ | Num _ -> true | _ -> false) in
    (match e2type with
    | true -> Internal2 (op, arith_tree e1 x, Number e2)
    | false -> Integral (App2 (op, e1, e2), x)
    ) 
  | App2 (op, e1, e2) when op == Plus ->
    Internal2 (op, arith_tree e1 x, arith_tree e2 x)
  | App2 (op, e1, e2) -> Integral (expr, x)
  | App1 (op, e1) -> Integral (expr, x)
  | _ ->
    Integral (expr, x) 

and replace_minus expr =
  match expr with
  | App2 (Minus, e1, e2) -> App2 (Plus, replace_minus e1, App1 (UMinus, replace_minus e2))
  | _ -> expr

(* Application of the formula *)
let formule pexpr (x : string) a b = 
  let saexpr = subst pexpr x a in
  let sbexpr = subst pexpr x b in
  eval sbexpr -. eval saexpr 

(* Application of the antiderivative from the table *)
let eval_primitive expr x a b =
  match expr with
  | (App2(Expo, App0(E), e)) when (match e with | Var _ -> true | _ -> false)-> Some (formule expr x a b)
  | _ ->
  let pexpr = primitive expr (Light.(Var x)) in
  if expr <> pexpr || expr == Light.(e ^ Var "x") then Some (formule pexpr x a b) else None

(* When integrating in parts, u is usually taken as such a function,
   which after differentiation will become simpler. Therefore, we do
   not just apply the method, but consider various expressions. *)
let rec parts expr x =
  match expr with 
  (* expressions with log *)
  | App1 (Log, e) when Syntax.to_string e = x ->
    parts (App2 (Mult, Num 1, App1 (Log, e))) x
  | App2 (Mult, e1, App1 (Log, e2))
  | App2 (Mult, App1 (Log, e2), e1) ->
    parts_aux (App1 (Log, e2)) e1 x

  (* expressions with e^x *)
  | App2 (Mult, e1, App2(Expo, App0(E), e2))
  | App2 (Mult, App2(Expo, App0(E), e2), e1) ->
    parts_aux e1 (App2(Expo, App0(E), e2)) x
  
  (* expressions with trig *)
  | App1 (trig, e) when Syntax.to_string e = x ->
    parts (App2 (Mult, Num 1, App1 (trig, e))) x
  | App2 (Mult, e1, App1(trig, e2))
  | App2 (Mult, App1(trig, e2), e1) ->
    parts_aux e1 (App1(trig, e2)) x
    
  (* other expressions *)
  | App2 (Mult, e1, e2) ->
    parts_aux e1 e2 x
    
  | _ -> Error expr

and parts_aux u dv x = 
  let du = derive u x in
  let v = primitive dv (Light.(Var x)) in (*TODO integrate *)
  Internal2 (Minus, Formule (App2(Mult, u, v)), Integral (App2(Mult, v, du), x))

(* The midpoint rule, the segment is divided into 100 parts *)
let center_rectangles expr x a b = 
  let aeval = eval a in
  let beval = eval b in
  let n = 100 in
  let h = (beval -. aeval) /. (float_of_int n) in
  let rec aux i sum = 
    if i > n then sum else
      (let v = aeval +. ((float_of_int i) -. 0.5) *. h in
      let y = eval (subst expr x (FloatNum v)) in
      aux (i + 1) (sum +. y)) in
  let res = h *. (aux 1 0.0) in
  res

(* In this function, we consistently apply various integration methods, from the simplest ones.
   If the method does not return a value, the following applies.
   1 - application of the antiderivative from the table for the simplest integrals
   2 - application of linearity rules and recursive integration
   3 - integration by parts
   4 - application of midpoint rule
*)
let rec integ (expr : expr) (x : string) (a : expr) (b : expr) : float =
  let expr = simpl expr in
  let res = eval_primitive expr x a b in 
  match res with 
  | Some v -> v
  | None -> 
    let expr = local_eval expr in
    let expr' = simpl expr in
    let t = arith_tree expr' x in
    let restree = eval_tree t x a b true in 
    (match restree with
    | Some v -> v 
    | None -> 
    let p = parts expr x in
    let resp = eval_tree p x a b false in 
    (match resp with
    | Some v -> v 
    | None -> center_rectangles expr x a b))

and eval_tree tree x a b arith = 
  match tree with
  | Error _ -> None
  | Number e -> let l = eval_primitive e x a b in
    (match l with
    | Some v -> Some v
    | None -> Some (formule e x a b)
    ) (* TODO check *)
  | Formule e -> Some (formule e x a b)
  | Integral (e, x) -> if arith then eval_primitive e x a b else Some (integ e x a b)
  | Internal2 (op, n1, n2) ->
    let n1eval = eval_tree n1 x a b arith in 
    let n2eval = eval_tree n2 x a b arith in
    (match n1eval, n2eval with
    | Some x, Some y -> 
      Some (eval (App2(op, FloatNum x, FloatNum y)))
    | _ -> None 
    )
