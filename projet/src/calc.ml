open Syntax
open Eval
open Simpl
open Subst
open Derive
open Integ
open Plot
open Graphics

type node = 
  | NodeF of cmd * float
  | NodeE of cmd * expr

let parse_expression str =
  Parser.expr Lexer.token (Lexing.from_channel str)

let menu_evaluate str =
  print_endline "Saisir l'expression à évaluer :";
  let e = parse_expression str in
  let result = eval e in
  print_float result; print_newline ();
  NodeF (Eval e, result)
  

let menu_simpl str =
  print_endline "Entrez l'expression à simplifier :";
  let e = parse_expression str in
  let result = simpl e in
  print_endline (to_string result);
  NodeE (Simpl e, result)


let menu_subst str =
  print_endline "Entrez l'expression à substituer :";
  let e = parse_expression str in
  print_endline "Entrez la variable à substituer :";
  let x = read_line () in
  print_endline "Entrez l'expression de substitution :";
  let e' = parse_expression str in
  let result = subst e x e' in
  print_endline (to_string result);
  NodeE (Subst (e, x, e'), result)


let menu_derive str =
  print_endline "Entrez l'expression à dériver :";
  let e = parse_expression str in
  print_endline "Entrer la variable par rapport à laquelle dériver :";
  let x = read_line () in
  let result = derive e x in
  print_endline (to_string result);
  NodeE (Derive (e, x), result)


let menu_integ str =
  print_endline "Entrez l'expression à intégrer : ";
  let e = parse_expression str in
  print_endline "Entrer la variable d'intégration :";
  let x = read_line () in
  print_endline "Entrer la borne inférieure :";
  let a = parse_expression str in
  print_endline "Entrer la borne supérieure :";
  let b = parse_expression str in
  let result = integ e x a b in
  print_float result; print_newline ();
  NodeF (Integ (e, x, a, b), result)
  

let menu_plot str =
  print_endline "Entrez l'expression à tracer :";
  let e = parse_expression str in
  print_endline "Entrez la variable :";
  let x = read_line () in
  print_endline "Voulez-vous entrer des intervalles personnalisés pour les axes ? (o/n)";
  let custom_intervals = read_line () in
  if custom_intervals = "o" || custom_intervals = "O" then (
    print_endline "Entrez la valeur minimale de l'axe des x :";
    let x_min = read_float () in
    print_endline "Entrez la valeur maximale de l'axe des x :";
    let x_max = read_float () in
    print_endline "Entrez la valeur minimale de l'axe des y :";
    let y_min = read_float () in
    print_endline "Entrez la valeur maximale de l'axe des y :";
    let y_max = read_float () in
    plot_expression e x x_min x_max y_min y_max
  ) else (
    plot_expression e x (-5.0) 5.0 (-5.0) 5.0
  )

let string_of_cmd cmd =
  match cmd with
  | Eval expr -> "Évaluation de " ^ Syntax.to_string expr
  | Subst (e1, var, e2) ->
      "Substitution de " ^ var ^ " par " ^ Syntax.to_string e2 ^ " dans " ^ Syntax.to_string e1
  | Simpl expr -> "Simplification de " ^ Syntax.to_string expr
  | Derive (expr, var) -> "Derive de " ^ Syntax.to_string expr ^ " avec " ^ var
  | Integ (e1, var, e2, e3) ->
      "Intégration de " ^ Syntax.to_string e1 ^ " avec " ^ var ^ " entre " ^
      Syntax.to_string e2 ^ " et " ^ Syntax.to_string e3
  | _ -> ""

let string_of_node node =
  match node with
  | NodeF (cmd, value) -> string_of_cmd cmd ^ " a donné " ^ string_of_float value
  | NodeE (cmd, expr) -> string_of_cmd cmd ^ " a donné " ^ Syntax.to_string expr

let rec menu_history lst = 
  match lst with
  | [] -> print_endline "Aucun enregistrement"
  | h::t -> print_endline (string_of_node h); if List.length t > 0 then menu_history t else ()
  

let print_menu lst () =
  print_endline "Choose an action:";
  print_endline "1. Évaluer";
  print_endline "2. Simplifier";
  print_endline "3. Substituer";
  print_endline "4. Dériver";
  print_endline "5. Intégrer";
  print_endline "6. Tracer";
  print_endline "7. Historique";
  print_endline "0. Quitter"

let handle_choice lst choice =
  match choice with
  | 0 -> print_endline "Quitter..."; []
  | 1 -> [menu_evaluate stdin]
  | 2 -> [menu_simpl stdin]
  | 3 -> [menu_subst stdin]
  | 4 -> [menu_derive stdin]
  | 5 -> [menu_integ stdin]
  | 6 -> menu_plot stdin; []
  | 7 -> menu_history lst; []
  | _ -> print_endline "Choix non valide"; []

let rec menu lst () =
  print_menu lst ();
  try
    let choice = read_int () in
    let lst = lst @ (handle_choice lst choice) in
    match choice with
    | 0 -> ()
    | _ -> menu lst ()
  with 
  | Failure s -> 
    (match s with
      | "int_of_string" -> print_endline "Choix non valide"
      | s -> print_endline s
    ); menu lst ()
  | Dune__exe__Lexer.Error s -> print_endline ("Impossible de traiter le symbole " ^ s); menu lst ()
  | Dune__exe__Parser.Error -> print_endline "Impossible de traiter l'expression"; menu lst ()
  | _ -> menu lst ()

let () = menu [] ()