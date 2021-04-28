open Nottui
module W = Nottui_widgets

type in_port = In of int | ComponentOut of (int * int)

type out_port = Out of int | ComponentIn of (int * int)

type circuit = {
  name : string;
  inputs : string list;
  outputs : string list;
  components : subcircuit list;
  connections : (in_port * out_port list) list;
}

and subcircuit = Circuit of circuit | Nand

let drag_and_drop x_pos_abs y_pos_abs (ui : ui Lwd.t) : ui Lwd.t =
  let x = Lwd.var 0 in
  let y = Lwd.var 0 in

  let mouse_handler ~x:mouse_start_x ~y:mouse_start_y button =
    let drag_handler ~x:mouse_x ~y:mouse_y =
      let () = Lwd.set x (mouse_x - mouse_start_x - x_pos_abs) in
      let () = Lwd.set y (mouse_y - mouse_start_y - y_pos_abs) in
      ()
    in
    let release_handler ~x:_ ~y:_ = () in
    match button with
    | `Left -> `Grab (drag_handler, release_handler)
    | _ -> `Unhandled
  in

  Lwd.map2
    (fun (x, y) ui ->
      let space_x = Ui.space x 0 in
      let space_y = Ui.space 0 y in
      Ui.join_y space_y (Ui.join_x space_x (Ui.mouse_area mouse_handler ui)))
    (Lwd.pair (Lwd.get x) (Lwd.get y))
    ui

let binary_string_of_bool b = if b then "1" else "0"

let rec compute circuit inputs query =
  let connection =
    List.find_opt
      (fun (_, outs) -> List.exists (( = ) query) outs)
      circuit.connections
  in
  match connection with
  | Some (In i, _) -> List.nth inputs i
  | Some (ComponentOut (ci, oi), _) -> (
      let component = List.nth circuit.components ci in
      match component with
      | Nand ->
          if oi == 0 then
            let input0 = compute circuit inputs (ComponentIn (ci, 0)) in
            let input1 = compute circuit inputs (ComponentIn (ci, 1)) in
            Lwd.map2 (fun a b -> not (a && b)) input0 input1
          else Lwd.pure false
      | Circuit c ->
          compute c
            (List.mapi
               (fun ii _ -> compute circuit inputs (ComponentIn (ci, ii)))
               c.inputs)
            (Out oi))
  | None -> Lwd.pure false

let render circuit : ui Lwd.t =
  let inputs = List.map (fun _ -> Lwd.var false) circuit.inputs in
  let observed_inputs = List.map Lwd.get inputs in
  let compute query = compute circuit observed_inputs query in

  let status_line = Lwd.var "" in

  let render_input index name =
    let value = List.nth inputs index in
    let toggle_value () = Lwd.set value (not (Lwd.peek value)) in
    let observed = Lwd.get value in

    let render value =
      let value_button = W.button (binary_string_of_bool value) toggle_value in
      let color = if value then Notty.A.lightgreen else Notty.A.gray 5 in
      let dot =
        W.button ~attr:(Notty.A.fg color) "•" (fun () -> () (* TODO *))
      in
      let label = W.string name in
      Ui.join_y (Ui.join_x value_button dot) label
    in
    Lwd.map render observed
  in

  let render_component component =
    match component with
    | Nand -> Ui.hcat [ W.string "•\n•"; W.string "NAND"; W.string "•" ]
    | Circuit c ->
        let dot_lines l =
          W.string (String.concat "\n" (List.map (fun _ -> "•") l))
        in
        let input_dots = dot_lines c.inputs in
        let output_dots = dot_lines c.outputs in
        Ui.hcat [ input_dots; W.string c.name; output_dots ]
  in

  let render_component_area x_pos_abs y_pos_abs =
    Lwd.map Ui.zcat
      (Lwd_utils.flatten_l
         (List.map
            (fun it ->
              render_component it |> Lwd.pure
              |> drag_and_drop x_pos_abs y_pos_abs)
            circuit.components))
  in

  let render_output oi name =
    let computed = compute (Out oi) in
    let render value =
      let color = if value then Notty.A.lightgreen else Notty.A.gray 5 in
      let dot = W.string ~attr:(Notty.A.fg color) "•" in
      let label = W.string name in
      Ui.join_y dot label
    in
    Lwd.map render computed
  in

  let inputs =
    Lwd.map Ui.vcat
      (Lwd_utils.flatten_l (List.mapi render_input circuit.inputs))
  in

  (* TODO: Accurately compute pos x and pos y *)
  let components = render_component_area 8 1 in

  let status_bar = Lwd.map W.string (Lwd.get status_line) in

  let center = W.scroll_area (Lwd.map2 Ui.join_y status_bar components) in

  let outputs =
    Lwd.map Ui.vcat
      (Lwd_utils.flatten_l (List.mapi render_output circuit.outputs))
  in

  Lwd.map Ui.hcat (Lwd_utils.flatten_l [ inputs; center; outputs ])

(* A NAND B = NOT (A AND B)
 * A NAND A = NOT (A AND A) = NOT A *)
let not_gate =
  {
    name = "NOT";
    inputs = [ "a" ];
    outputs = [ "result" ];
    components = [ Nand ];
    connections =
      [
        (In 0, [ ComponentIn (0, 0); ComponentIn (0, 1) ]);
        (ComponentOut (0, 0), [ Out 0 ]);
      ];
  }

(* A NAND B = NOT (A AND B)
 * A AND B = NOT (A NAND B)
 *         = NAND (A NAND B, A NAND B) *)
let and_gate =
  {
    name = "AND";
    inputs = [ "a"; "b" ];
    outputs = [ "result" ];
    components = [ Nand; Nand ];
    connections =
      [
        (In 0, [ ComponentIn (0, 0) ]);
        (In 1, [ ComponentIn (0, 1) ]);
        (ComponentOut (0, 0), [ ComponentIn (1, 0); ComponentIn (1, 1) ]);
        (ComponentOut (1, 0), [ Out 0 ]);
      ];
  }

(* A OR B = NOT (AND (NOT A, NOT B))
 *        = NAND (NOT A, NOT B)
 *        = NAND (A NAND A, B NAND B) *)
let or_gate =
  {
    name = "OR";
    inputs = [ "a"; "b" ];
    outputs = [ "result" ];
    components = [ Nand; Nand; Nand ];
    connections =
      [
        (In 0, [ ComponentIn (0, 0); ComponentIn (0, 1) ]);
        (In 1, [ ComponentIn (1, 0); ComponentIn (1, 1) ]);
        (ComponentOut (0, 0), [ ComponentIn (2, 0) ]);
        (ComponentOut (1, 0), [ ComponentIn (2, 1) ]);
        (ComponentOut (2, 0), [ Out 0 ]);
      ];
  }

(* A XOR B = NOT (AND (A, B)) AND (A OR B)
 *         = NAND (A, B) AND (A OR B)
 *         = (NAND (A, B) AND A) OR (NAND (A, B) AND B)
 *         = NAND (NOT AND(NAND (A, B), A), NOT AND(NAND (A, B), B)) *)
let xor_gate =
  {
    name = "XOR";
    inputs = [ "a"; "b" ];
    outputs = [ "result" ];
    components = [ Nand; Nand; Nand; Nand ];
    connections =
      [
        (In 0, [ ComponentIn (0, 0); ComponentIn (1, 1) ]);
        (In 1, [ ComponentIn (0, 1); ComponentIn (2, 1) ]);
        (ComponentOut (0, 0), [ ComponentIn (1, 0); ComponentIn (2, 0) ]);
        (ComponentOut (1, 0), [ ComponentIn (3, 0) ]);
        (ComponentOut (2, 0), [ ComponentIn (3, 1) ]);
        (ComponentOut (3, 0), [ Out 0 ]);
      ];
  }

let half_adder =
  {
    name = "half_adder";
    inputs = [ "a"; "b" ];
    outputs = [ "sum"; "carry" ];
    components = [ Circuit xor_gate; Circuit and_gate ];
    connections =
      [
        (In 0, [ ComponentIn (0, 0); ComponentIn (1, 0) ]);
        (In 1, [ ComponentIn (0, 1); ComponentIn (1, 1) ]);
        (ComponentOut (0, 0), [ Out 0 ]);
        (ComponentOut (1, 0), [ Out 1 ]);
      ];
  }

let adder =
  {
    name = "adder";
    inputs = [ "a"; "b"; "cin" ];
    outputs = [ "sum"; "cout" ];
    components = [ Circuit half_adder; Circuit half_adder; Circuit or_gate ];
    connections =
      [
        (In 0, [ ComponentIn (0, 0) ]);
        (In 1, [ ComponentIn (0, 1) ]);
        (In 2, [ ComponentIn (1, 1) ]);
        (ComponentOut (0, 0), [ ComponentIn (1, 0) ]);
        (ComponentOut (0, 1), [ ComponentIn (2, 0) ]);
        (ComponentOut (1, 0), [ Out 0 ]);
        (ComponentOut (1, 1), [ ComponentIn (2, 1) ]);
        (ComponentOut (2, 0), [ Out 1 ]);
      ];
  }

let () = Ui_loop.run (render adder)
