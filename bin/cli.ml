open Minttea

let build_resume tags rm =
  let scores =
    List.map (Resmeta.score tags) (Resmeta.sections rm) |> List.concat |> Array.of_list
  in
  let free_space = Resmeta.available_space rm in
  let locations = Section.entry_positions (Resmeta.sections rm) in
  let entry_weights =
    Section.to_weights (Resmeta.sections rm) (Resmeta.formatting rm) (Resmeta.padding rm)
    |> Array.of_list
  in
  let _, sel_ind = Knapsack.solve free_space scores entry_weights in
  let mappings = Section.init_mask sel_ind locations (Resmeta.sections rm) in
  let resume =
    Section.render_sections (Resmeta.mandatory rm) mappings (Resmeta.sections rm)
  in
  let prepend_formatting s = Resmeta.formatting rm ^ s in
  let wrap s = "#resume(" ^ Resmeta.header rm ^ s ^ ")" in
  resume |> wrap |> prepend_formatting
;;

let builder_call tags rm output_file =
  let resume = build_resume tags rm in
  Io.string_to_file resume output_file;
  if not (Typst.compile output_file) then failwith "Typst Compilation Error";
  (* remove the output file if none is specified *)
  if output_file = "output.typ" then Sys.remove output_file
;;

type model =
  { choices : (string * int) array
  ; cursor : int
  ; points : int
  ; generating : bool
  ; fin : [ `no | `terminating | `terminated ] ref
  ; spinner : Leaves.Sprite.t
  ; output_file : string
  ; rm : Resmeta.res_meta
  }

let init_model tagset points output_file rm =
  { cursor = 0
  ; choices = tagset
  ; points
  ; generating = false
  ; fin = ref `no
  ; spinner = Leaves.Spinner.mini_dot
  ; output_file
  ; rm
  }
;;

let init _model = Command.Noop

let update event model =
  match event with
  | Event.Frame now ->
    let spinner = Leaves.Sprite.update ~now model.spinner in
    let cmd = if !(model.fin) = `terminated then Command.Quit else Command.Noop in
    let () = if !(model.fin) = `terminating then model.fin := `terminated in
    { model with spinner }, cmd
  | Event.KeyDown (Key "q" | Escape) -> model, Command.Quit
  | Event.KeyDown (Key "k" | Up) ->
    let cursor = max 0 (model.cursor - 1) in
    { model with cursor }, Command.Noop
  | Event.KeyDown (Key "j" | Down) ->
    let cursor = min (Array.length model.choices - 1) (model.cursor + 1) in
    { model with cursor }, Command.Noop
  | Event.KeyDown (Key "l" | Right | Space) ->
    if model.points > 0
    then
      model.choices.(model.cursor)
      <- (fun (t, v) -> t, v + 1) model.choices.(model.cursor);
    let points = max 0 (model.points - 1) in
    { model with points }, Command.Noop
  | Event.KeyDown (Key "h" | Left | Backspace) ->
    let can_dec = (fun (_, v) -> v) model.choices.(model.cursor) > 0 in
    if can_dec
    then
      model.choices.(model.cursor)
      <- (fun (t, v) -> t, max 0 (v - 1)) model.choices.(model.cursor);
    let points = model.points + Bool.to_int can_dec in
    { model with points }, Command.Noop
  | Event.KeyDown Enter ->
    let _ =
      if not model.generating
      then
        Some
          (Domain.spawn (fun _ ->
             builder_call (model.choices |> Array.to_list) model.rm model.output_file;
             model.fin := `terminating
             ))
      else None
    in
    { model with generating = true }, Command.Noop
  | _ -> model, Command.Noop
;;

let view model =
  let red = Spices.color "#EA3546" in
  let blue = Spices.color "#43BCCD" in
  let orange = Spices.color "#F86624" in
  let yellow = Spices.color "#F9C80E" in
  let white = Spices.color "#ffffea" in
  (* colors left over from previous implementations *)
  let _purple = Spices.color "#662E9B" in
  let unused = Spices.(default |> bg red |> fg white |> bold true) in
  let used = Spices.(default |> bg blue |> fg white |> bold true |> underline true) in
  let c = Spices.(default |> fg orange |> bold true) in
  let control = Spices.(default |> fg yellow |> bold true) in
  let def = Spices.(default |> fg white) in
  if model.generating
  then (
    let spin_text =
      Leaves.Sprite.view model.spinner
      |> Spices.build Spices.(default |> fg blue |> underline true) "%s"
    in
    if !(model.fin) = `terminating || !(model.fin) = `terminated
    then
      Format.sprintf
        "\n\n\nResume %s'ed - good luck on your future quests.\n\n\n"
        (Spices.build used "%s" "resspec")
    else "\n\n\n" ^ spin_text ^ " Generating... " ^ spin_text ^ "\n\n\n")
  else (
    let top = max (model.cursor - 2) 0 |> min (Array.length model.choices - 5) in
    let num_entries = 5 in
    let options =
      Array.sub model.choices top num_entries
      |> Array.to_list
      |> List.mapi (fun i (tag, value) ->
        let cursor = if model.cursor - top = i then Spices.build c "%s" ">" else " " in
        let v =
          if value = 0
          then Spices.build unused " %s " "_"
          else Int.to_string value |> Spices.build used " %s "
        in
        let t =
          if value = 0 then Spices.build def "%s" tag else Spices.build used "%s" tag
        in
        Format.sprintf "%s %s %s" cursor v t)
      |> String.concat "\n"
    in
    let make_control s = Spices.build control "%s" s in
    Format.sprintf
      {|
Welcome to resspec!

Navigate with %s %s
Invest points with
- %s: remove point
- %s: add point
-------------------------------------
You have %s skill points remaining...
%s
Press %s to quit, %s to kill.
Press %s to confirm generation.
    |}
      (make_control "j/Down")
      (make_control "k/Up")
      (make_control "h/Left/Backspace")
      (make_control "l/Right/Space")
      (model.points |> Spices.build c "%d")
      options
      (make_control "q")
      (make_control "ctrl+c")
      (make_control "enter"))
;;

let start_interactive_mode tagset level output_file rm =
    let cli = Minttea.app ~init ~update ~view () in
    Minttea.start cli ~initial_model:(init_model tagset (max 1 !level) !output_file rm)
