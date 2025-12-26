type section =
  { name : string
  ; entries : Entry.entry list
  }

let of_str l s =
  let title = Util.get_param "title" s in
  { name = title
  ; entries = Entry.of_sec_str (not (List.exists (fun v -> v = title) l)) s
  }
;;

let to_str sec =
  "section( title:\""
  ^ sec.name
  ^ "\",\n"
  ^ List.fold_left
      (fun acc v ->
          let s = Entry.to_str v in
          if acc <> "" then acc ^ ",\n" ^ s else acc ^ s)
      ""
      sec.entries
  ^ ")"
;;

let calc_available_space s sections mandatory ps padding =
  let formatting = Parsing.strip_formatting s in
  let header = Parsing.strip_header s in
  let stub =
    List.fold_left
      (fun acc sec ->
         if List.mem sec.name mandatory
         then acc ^ to_str sec ^ ",\n"
         else acc ^ "section(),")
      ""
      sections
  in
  let measured_space =
    match Typst.measure ("resume(" ^ header ^ stub ^ "\n)") formatting with
    | None -> 0.0
    (* TODO: the 4.0 number here accounts for lbars,
       which are not measured by Typst *)
    | Some x -> x +. (4.0 *. padding *. Float.of_int (List.length sections))
  in
  let m =
    match Parsing.get_margins s with
    | None -> 2.0
    | Some v -> 2.0 *. v
  in
  ps -. m -. measured_space
;;



let select_to_str l sec =
  "section( title:\""
  ^ sec.name
  ^ "\",\n"
  ^ List.fold_left
      (fun acc (sel, v) ->
          let s = Entry.to_str v in
          if not sel then acc else if acc <> "" then acc ^ ",\n" ^ s else acc ^ s)
      ""
      (List.combine l sec.entries)
  ^ ")"
;;

let combine_strs l =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> if acc = "" then aux h t else aux (acc ^ ",\n" ^ h) t
  in
  aux "" l
;;

let to_weights sections formatting padding =
  List.map
    (fun sec ->
        List.map
          (fun e ->
            Entry.measure e formatting
            |> Util.float_or_default
            |> ( +. ) padding
            |> (fun v -> v /. (Parsing.line_height formatting |> Util.float_or_default))
            |> Float.ceil
            |> Float.to_int)
          sec.entries)
    sections
  |> List.concat
;;

let entry_positions sections =
  List.mapi (fun i s -> List.mapi (fun j _ -> i, j) s.entries) sections |> List.concat
;;

let init_mask sel loc sec =
  let l = List.map (fun i -> List.nth loc i) sel in
  List.mapi (fun i s -> List.mapi (fun j _ -> List.mem (i, j) l) s.entries) sec
;;

let render_sections mandatory mappings sections =
  combine_strs
    (List.mapi
        (fun i s ->
          let l =
            if List.mem s.name mandatory
            then List.map (fun _ -> true) s.entries
            else List.nth mappings i
          in
          select_to_str l s)
        sections)
;;

(* let fold_list_to_match l sec = *)
(*   let rec aux acc l = function *)
(*     | [] -> acc *)
(*     | h :: t -> aux (List.take (h + 1) l :: acc) (List.drop h l) t *)
(*   in *)
(*   List.map (fun s -> List.length s.entries) sec |> aux [] l *)
(* ;; *)

let entries sec = sec.entries
