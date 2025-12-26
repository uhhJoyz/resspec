let knapsack cap va wa =
  let rec aux cap n memo =
    if n = 0 || cap = 0
    then 0
    else if memo.(n).(cap) <> -1
    then memo.(n).(cap)
    else (
      let take =
        if cap >= wa.(n - 1) then va.(n - 1) + aux (cap - wa.(n - 1)) (n - 1) memo else -1
      in
      let leave = aux cap (n - 1) memo in
      memo.(n).(cap) <- max take leave;
      memo.(n).(cap))
  in
  let n = min (Array.length va) (Array.length wa) in
  let memo = Array.make_matrix (n + 1) (cap + 1) (-1) in
  let o = aux cap n memo in
  let rec backtrack i j acc =
    if i <= 0 || j <= 0
    then acc
    else if memo.(i).(j) = memo.(i - 1).(j)
    then
      (* not taken *)
      backtrack (i - 1) j acc
    else
      (* taken *)
      backtrack
        (i - 1)
        (j - wa.(i - 1))
        (if j - wa.(i - 1) >= 0 then (i - 1) :: acc else acc)
  in
  o, backtrack n cap []
;;

let cap_cmd cmd =
  let input = Unix.open_process_in cmd in
  let v = In_channel.input_all input in
  In_channel.close input;
  v
;;

let float_or_default = function
  | None -> -1.0
  | Some x -> x
;;

let typst_query file id =
  let s = cap_cmd ("typst query " ^ file ^ " \"<" ^ id ^ ">\"") in
  if
    try Str.search_forward (Str.regexp "\"value\":\\(.*\\),\"label") s 0 >= 0 with
    | Not_found -> false
  then Some (Str.matched_group 1 s |> Float.of_string)
  else None
;;

let tags_of_str = function
  | "" -> []
  | tag_str -> String.split_on_char ',' tag_str
;;

let typst_compile = function
  | "" -> false
  | f -> Sys.command ("typst compile " ^ f) = 0
;;

let read_string fp =
  let ic = open_in_bin fp in
  let s_val = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s_val
;;

let string_to_file s o =
  let oc = open_out o in
  Printf.fprintf oc "%s" s;
  close_out oc
;;

module CharSet = Set.Make (Char)

let extract_func f s =
  let ss = String.to_seq s in
  let fs = String.to_seq f in
  let len_pref = Seq.length fs in
  let rec find_prefix i seq =
    match Seq.uncons seq with
    | Some (_, iseq) ->
      if Seq.equal ( = ) fs (Seq.take len_pref seq)
      then Some (i, seq)
      else find_prefix (i + 1) iseq
    | _ -> None
  in
  let openers = CharSet.of_list [ '('; '['; '{' ] in
  let closers = CharSet.of_list [ ')'; ']'; '}' ] in
  let rec find_start i seq =
    match Seq.uncons seq with
    | None -> None
    | Some (x, iseq) ->
      if CharSet.mem x openers then Some (i, seq) else find_start (i + 1) iseq
  in
  let rec find_end i acc seq =
    match Seq.uncons seq with
    | None -> None
    | Some (x, iseq) ->
      let exit =
        match acc, i with
        | 0, 0 -> false
        | a, _ -> if a <= 0 then true else false
      in
      if exit
      then Some (i, iseq)
      else (
        let fe = find_end (i + 1) in
        if CharSet.mem x closers
        then fe (acc - 1) iseq
        else if CharSet.mem x openers
        then fe (acc + 1) iseq
        else fe acc iseq)
  in
  let rec to_func_list b acc seq =
    match find_prefix 0 seq with
    | None -> Some (List.rev acc)
    | Some (str_start, fseq) ->
      (match find_start 0 fseq with
       | None -> None
       | Some (fst, sseq) ->
         (match find_end 0 0 sseq with
          | None -> None
          | Some (fend, eseq) ->
            to_func_list
              (b + str_start + fst + fend + 1)
              (String.sub s (b + str_start) (fst + fend) :: acc)
              eseq))
  in
  match to_func_list 0 [] ss with
  | None -> []
  | Some l -> l
;;

let split_res_to_sec_opt s =
  let find i = String.index_from_opt s i '#' in
  let rec get_res_opt = function
    | "" -> None
    | str ->
      if String.starts_with ~prefix:"resume" str
      then Some str
      else (
        let p = find 0 in
        match p with
        | None -> None
        | Some p -> get_res_opt (String.sub str (p + 1) (String.length str - p - 1)))
  in
  match get_res_opt s with
  | None -> None
  | Some res_str -> Some (extract_func "section" res_str)
;;

(* let split_sec_by_ent_opt l = List.map (extract_func_opt "entry") l *)

let _read_to_sections dir =
  if Sys.is_directory dir
  then (
    match Sys.readdir dir with
    | [||] -> []
    | files ->
      let rec aux acc = function
        | [] -> acc
        | None :: l -> aux acc l
        | Some t :: l -> aux (t :: acc) l
      in
      Array.to_list
        (Array.map
           (fun file ->
              let df = dir ^ "/" ^ file in
              if Sys.is_directory df
              then None
              else (
                let ic = open_in_bin df in
                let s_val = really_input_string ic (in_channel_length ic) in
                close_in ic;
                Some s_val))
           files)
      |> aux [])
  else (
    match split_res_to_sec_opt (read_string dir) with
    | None -> []
    | Some l -> l)
;;

(* List.concat_map (split_sec_by_ent_opt) l) *)

let _sep_tag_and_mod str =
  if str = ""
  then [], ""
  else (
    let find i =
      match String.index_from_opt str i '#' with
      | None -> 0
      | Some i -> i
    in
    let part_l = find 0 in
    let part_r = find (part_l + 1) in
    let tag_str =
      match part_l with
      | 0 -> ""
      | p -> String.sub str (p + 1) (part_r - p - 1)
    in
    String.split_on_char ',' tag_str, str)
;;

(* at this point, I decided to use regexes to save developer effort and time *)
let get_param param f =
  let o, c =
    match param with
    | "tags" -> {|\[|}, {|\]|}
    | _ -> {|"|}, {|"|}
  in
  let i =
    try
      Str.search_forward
        (Str.regexp (param ^ {|:\(.*\)|} ^ o ^ {|\(.*\)|} ^ c ^ {|,|}))
        f
        0
    with
    | Not_found -> -1
  in
  if i < 0
  then Printf.sprintf "Failed to gather function %s, with input: %s\n" param f |> failwith
  else Str.matched_group 2 f
;;

(*TODO: move this temporary file to /tmp/*)
let typst_measure s formatting =
  (* typst_query the size of a particluar item 
     after applying formatting and rendering  *)
  let tf = "___measuring.typ" in
  let () =
    string_to_file
      (formatting
       ^ "#context [ \n#let t = "
       ^ s
       ^ "\n#let size = measure(t)\n#metadata(size.height.inches())<demo>]\n")
      tf
  in
  let v = typst_query tf "demo" in
  let () = Sys.remove tf in
  v
;;

let calc_padding formatting =
  let sec_size = typst_measure "section()" formatting |> float_or_default in
  let ent_size = typst_measure "entry()" formatting |> float_or_default in
  let both = typst_measure "section(entry())" formatting |> float_or_default in
  both -. (sec_size +. ent_size)
;;

let line_height formatting = typst_measure "[temp]" formatting

type entry =
  { tags : string list
  ; contents : string
  }

type section =
  { name : string
  ; entries : entry list
  }

module Entry = struct
  let of_str t s =
    { tags = (if t then tags_of_str (get_param "tags" s) else []); contents = s }
  ;;

  let to_str e = e.contents
  let of_sec_str t s = List.map (of_str t) (extract_func "entry" s)
  let measure e formatting = typst_measure e.contents formatting
end

module Section = struct
  let of_str l s =
    let title = get_param "title" s in
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
              |> float_or_default
              |> ( +. ) padding
              |> (fun v -> v /. (line_height formatting |> float_or_default))
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
end

let strip_formatting s = Str.search_forward (Str.regexp {|#resume|}) s 0 |> String.sub s 0

let strip_header s =
  (* |> String.map (fun c -> if c = '\n' then ' ' else c) in *)
  let start =
    (try Str.search_forward (Str.regexp {|#resume|}) s 0 with
     | Not_found -> -9)
    |> ( + ) 8
  in
  if start >= 0
  then
    Str.search_forward (Str.quote "section" |> Str.regexp) s start
    |> ( + ) (-start)
    |> String.sub s start
  else ""
;;

let get_margins s =
  if
    try Str.search_forward (Str.regexp {|page(margin:\(.*\)in)|}) s 0 >= 0 with
    | Not_found -> false
  then Some (Str.matched_group 1 s |> Float.of_string)
  else None
;;

let calc_available_space s sections mandatory ps padding =
  let formatting = strip_formatting s in
  let header = strip_header s in
  let stub =
    List.fold_left
      (fun acc sec ->
         if List.mem sec.name mandatory
         then acc ^ Section.to_str sec ^ ",\n"
         else acc ^ "section(),")
      ""
      sections
  in
  let measured_space =
    match typst_measure ("resume(" ^ header ^ stub ^ "\n)") formatting with
    | None -> 0.0
    (* TODO: the 4.0 number here accounts for lbars,
       which are not measured by Typst *)
    | Some x -> x +. (4.0 *. padding *. Float.of_int (List.length sections))
  in
  let m =
    match get_margins s with
    | None -> 2.0
    | Some v -> 2.0 *. v
  in
  ps -. m -. measured_space
;;

type _res_meta =
  { contents : string
  ; formatting : string
  ; header : string
  ; mandatory : string list
  ; line_height : float
  ; available_space : int
  ; page_height : float
  ; padding : float
  ; sections : section list
  }

module ResMeta = struct
  let of_file f mandatory page_height =
    let s = read_string f in
    let formatting = strip_formatting s in
    let padding = calc_padding formatting in
    let sections =
      match split_res_to_sec_opt s with
      | None -> []
      | Some l -> l |> List.map (Section.of_str mandatory)
    in
    let line_height = line_height formatting |> float_or_default in
    { contents = s
    ; formatting
    ; header = strip_header s
    ; mandatory
    ; line_height
    ; available_space =
        (calc_available_space s sections mandatory page_height padding
         |> fun v -> v /. line_height |> Float.to_int)
    ; page_height
    ; padding
    ; sections
    }
  ;;
end

let score tags sec =
  sec.entries
  |> List.mapi (fun i e ->
    List.map
      (fun tag ->
         List.mem tag e.tags |> Bool.to_int |> ( * ) (List.length sec.entries - i))
      tags
    |> List.fold_left ( + ) 0)
;;

(* let of_file f mandatory page_height = *)
let build_resume res tags mandatory p =
  let rm = ResMeta.of_file res mandatory p in
  let scores = List.map (score tags) rm.sections |> List.concat |> Array.of_list in
  let free_space = rm.available_space in
  let locations = Section.entry_positions rm.sections in
  let entry_weights =
    Section.to_weights rm.sections rm.formatting rm.padding |> Array.of_list
  in
  let _, sel_ind = knapsack free_space scores entry_weights in
  let mappings = Section.init_mask sel_ind locations rm.sections in
  let resume = Section.render_sections mandatory mappings rm.sections in
  let prepend_formatting s = rm.formatting ^ s in
  let wrap s = "#resume(" ^ rm.header ^ s ^ ")" in
  resume |> wrap |> prepend_formatting
;;

let output_file = ref "output.typ"
let args = ref []
let tag_str = ref ""
let page_height = ref 11.0
let usage_msg = "resspec <source-dir-or-file> -o <output-filename> -t <tag1>,...,<tagN>"
let anon_fun filename = args := filename :: !args

let speclist =
  [ "-o", Arg.Set_string output_file, "Set output filename."
  ; "-t", Arg.Set_string tag_str, "Set tags of built resume."
  ; "-p", Arg.Set_float page_height, "Page height (inches, default 11)."
  ]
;;

let () =
  let () = Printf.printf "Time: %fs \n" (Sys.time ()) in
  Arg.parse speclist anon_fun usage_msg;
  (* write resume data to temporary output file *)
  let () = Printf.printf "Time: %fs \n" (Sys.time ()) in
  let resume =
    build_resume
      (List.nth !args 0)
      (tags_of_str !tag_str)
      [ "Education"; "Skills" ]
      !page_height
  in
  let () = Printf.printf "Time: %fs \n" (Sys.time ()) in
  string_to_file resume !output_file;
  let () = Printf.printf "Time: %fs \n" (Sys.time ()) in
  if not (typst_compile !output_file) then failwith "Typst Compilation Error";
  let () = Printf.printf "Time: %fs \n" (Sys.time ()) in
  (* remove the output file if none is specified *)
  if !output_file = "output.typ" then Sys.remove !output_file;
  let () = Printf.printf "Time: %fs \n" (Sys.time ()) in
  Printf.printf
    "Successfully compiled and exported resume to: %s\n"
    (String.sub !output_file 0 (String.length !output_file - 3) ^ "pdf")
;;
