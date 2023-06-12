open Syntax
open Eval
open Simpl


let rec subst e x e' =
  match e with
  (* e est une variable qui est égale à x *)
  |Var x' when x=x' -> e'
  (* e une variable qui n'est pas égale à x *)
  |Var v -> e
  (* e est un nombre *) 
  |Num n -> e
  (* e est un nombre *) 
  |FloatNum n -> e
  (* e est une application d'un opérateur n'ayant pas d'argument  *)
  |App0 op0 -> App0 op0
  (* e est une application d'un opérateur unaire *)
  |App1 (op1,e1) -> App1 (op1, subst e1 x e')
  (* e est une application d'un opérateur binaire *)
  |App2 (op2, e1, e2) -> App2 (op2, subst e1 x e', subst e2 x e')
