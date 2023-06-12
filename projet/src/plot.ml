open Syntax
open Eval
open Subst
open Graphics

let plot_expression expr var x_min x_max y_min y_max=
  
  let eval_expr x =
    let subst_expr = subst expr var (FloatNum x) in
    eval subst_expr in

  (* Configuration de la fenêtre graphique *)
  let width = 700 in
  let height = 600 in
  open_graph (Printf.sprintf " %dx%d" width height);
  set_window_title "Plot";
  set_color black;

  let x_range = x_max -. x_min in
  let y_range = y_max -. y_min in

  (* Déterminer la distance entre deux graduations *)
  let x_tick_distance = x_range /. float_of_int (width / 50) in
  let y_tick_distance = y_range /. float_of_int (height / 50) in
  (* Dessiner les axes x et y *)
  moveto 0 (int_of_float ((-1.0 *. y_min /. y_range) *. float_of_int height));
  lineto width (int_of_float ((-1.0 *. y_min /. y_range) *. float_of_int height));
  moveto (int_of_float ((-1.0 *. x_min /. x_range) *. float_of_int width)) 0;
  lineto (int_of_float ((-1.0 *. x_min /. x_range) *. float_of_int width)) height;

 (* Dessiner une étiquette sur l'axe des x et des graduations *)
 moveto (width / 2) 10;
 draw_string "x";
 let rec draw_ticks_x i x =
   if x <= x_max then (
     let x' = int_of_float (((x -. x_min) /. x_range) *. float_of_int width) in
     moveto x' 0;
     lineto x' 5;
     moveto x' 20;
     draw_string (Printf.sprintf "%.2f" x);
     draw_ticks_x (i + 1) (x +. x_tick_distance)
   )
 in
 draw_ticks_x 0 x_min;

 (* Dessiner l'étiquette de l'axe y et les graduations *)
 moveto 10 (height / 2);
 draw_string "y";
 let rec draw_ticks_y i y =
   if y <= y_max then (
     let y' = int_of_float (((y -. y_min) /. y_range) *. float_of_int height) in
     moveto 0 y';
     lineto 5 y';
     moveto 15 y';
     draw_string (Printf.sprintf "%.2f" y);
     draw_ticks_y (i + 1) (y +. y_tick_distance)
   )
 in
 draw_ticks_y 0 y_min;

  
  (* Dessiner une courbe dans l'intervalle spécifié *)
  let num_points = 10000 in
  let delta_x = x_range /. float_of_int (num_points - 1) in
  let points = List.init num_points (fun i ->
    let x = x_min +. float_of_int i *. delta_x in
    if x >= x_min && x <= x_max then (
      let y = eval_expr x in
      let x' = int_of_float (((x -. x_min) /. x_range) *. float_of_int width) in
      let y' = int_of_float (((y -. y_min) /. y_range) *. float_of_int height) in
      (x', y')
    ) else
      (max_int, max_int) (* points en dehors de l'intervalle *)
  ) in
    let rec aux points = 
      (match points with
      | [] -> ()
      | (x,y)::t -> plot x y; aux t) in
    aux points;
    

  (* Attendre l'entrée de l'utilisateur *)
  ignore (read_key ());
  close_graph ()
