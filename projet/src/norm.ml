open Syntax

(* Type for storing the expression. The list of nodes is necessary 
   to implement the commutativity of addition and multiplication *)
type node =
  | Leaf of expr
  | Internal1 of op1 * node
  | Internal2 of op2 * node list

(* Building an expression tree. For addition and multiplication, 
   a list is created in which nested summands or multipliers are stored *)
let rec build_tree expr =
  let expr = replace_minus expr in
  match expr with
  | App2 (op, e1, e2) when op == Plus || op == Mult->
    let node1 = build_tree e1 in
    let node2 = build_tree e2 in
    (match node1, node2 with
     | Internal2 (op1, children1), Internal2 (op2, children2)
       when op1 = op && op2 = op ->
       Internal2 (op, children1 @ children2)
     | Internal2 (op1, children1), _ when op1 = op ->
       Internal2 (op, children1 @ [build_tree e2])
     | _, Internal2 (op2, children2) when op2 = op ->
       Internal2 (op, build_tree e1 :: children2)
     | _, _ ->
       Internal2 (op, [build_tree e1; build_tree e2]))
  | App2 (op, e1, e2) -> Internal2 (op, [build_tree e1; build_tree e2])
  | App1 (op, e1) -> Internal1 (op, build_tree e1)
  | _ ->
    Leaf expr

and replace_minus expr =
  match expr with
  | App2 (Minus, e1, e2) -> App2 (Plus, replace_minus e1, App1 (UMinus, replace_minus e2))
  | _ -> expr

let rec count_nodes expr =
  match expr with
  | Var _ -> 1
  | Num _ -> 1
  | FloatNum _ -> 1
  | App0 _ -> 1
  | App1 (_, e) -> 1 + count_nodes e
  | App2 (_, e1, e2) -> 1 + count_nodes e1 + count_nodes e2

(* Comparison of expressions. 
   First, expressions are compared by the number of operands, 
   then by lexicographic comparison, and at the end by the sign *)
let cmp e1 e2 = 
  let e1_plus = 
    match e1 with
    | App1 (UMinus, e) -> e
    | _ -> e1
  in
  let e2_plus = 
    match e2 with
    | App1 (UMinus, e) -> e
    | _ -> e2
  in
  let compare_counts () =
    let e1_count = count_nodes e1_plus in
    let e2_count = count_nodes e2_plus in
    compare e1_count e2_count
  in
  match compare_counts () with
  | 0 -> 
    let compare_str () =
      let e1_str = Syntax.to_string e1_plus in
      let e2_str = Syntax.to_string e2_plus in
      compare e1_str e2_str
    in
    let e1_str = Syntax.to_string e1_plus in
    let e2_str = Syntax.to_string e2_plus in
    (match compare_str () with 
    | 0 -> 
      (
        match e1, e2 with
        | App1 (UMinus, e1_plus), App1 (UMinus, e2_plus) -> 0
        | App1 (UMinus, e1_plus), e2_plus -> -1
        | e1_plus, App1 (UMinus, e2_plus) -> 1
        | e1_plus, e2_plus -> 0
      )
    | n -> n) 
  | n -> n


(* When converting a tree into an expression, the summands and multipliers are sorted *)
let rec to_expr node =
  match node with
  | Leaf expr -> expr
  | Internal1 (op, node) -> App1 (op, to_expr node)
  | Internal2 (op, children) ->
    let exprs = to_expr_list children in
    let exprs = if op = Plus || op = Mult then List.sort cmp exprs else exprs in
    match op with
    | op -> appn_helper op exprs

and to_expr_list nodes =
  List.map (fun node -> to_expr node) nodes

and appn_helper op exprs =
  match exprs with
  | [] -> failwith "empty expression list"
  | [e] -> e
  | _ -> appn_helper_helper op exprs

and appn_helper_helper op remaining_exprs =
  match remaining_exprs with
  | [] -> failwith "empty expression list"
  | [e] -> e
  | e1::e2::[] -> App2 (op, e1, e2)
  | e1::e2::es -> appn_helper_helper op ((App2 (op, e1, e2))::es)


(* Function for normalization of expressions - reduction to one form *)
let norm expr = 
  let tree = build_tree expr in
  let res = to_expr tree in
  res
