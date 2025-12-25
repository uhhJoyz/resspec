type _res_meta =
  { contents : string
  ; formatting : string
  ; header : string
  ; mandatory_sec : string list
  ; line_height : float
  ; available_space : float
  ; page_height : float
  }

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
  let memo = Array.init (n + 1) (fun _ -> Array.init (cap + 1) (fun _ -> -1)) in
  let o = aux cap n memo in
  let rec backtrack i j acc =
    if i = 0 || j = 0
    then acc
    else if memo.(i).(j) = memo.(i - 1).(j)
    then
      (* not taken *)
      backtrack (i - 1) j acc
    else
      (* taken *)
      backtrack (i - 1) (j - wa.(i - 1)) ((i - 1) :: acc)
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

let read_to_sections dir =
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

  let to_weights sections formatting =
    List.map
      (fun sec ->
         List.map
           (fun e ->
              Entry.measure e formatting
              |> float_or_default
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
end

let strip_formatting f =
  let s = read_string f in
  Str.search_forward (Str.regexp {|#resume|}) s 0 |> String.sub s 0
;;

let strip_header f =
  (* |> String.map (fun c -> if c = '\n' then ' ' else c) in *)
  let s = read_string f in
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

let get_margins f =
  let s = read_string f in
  if
    try Str.search_forward (Str.regexp {|page(margin:\(.*\)in)|}) s 0 >= 0 with
    | Not_found -> false
  then Some (Str.matched_group 1 s |> Float.of_string)
  else None
;;

let contains v l = List.exists (fun e -> v = e) l

let calc_available_space f sections mandatory ps =
  let formatting = strip_formatting f in
  let header = strip_header f in
  let stub =
    List.fold_left
      (fun acc sec ->
         if contains sec.name mandatory
         then acc ^ Section.to_str sec ^ ",\n"
         else acc ^ "section(),")
      ""
      sections
  in
  let measured_space =
    match typst_measure ("resume(" ^ header ^ stub ^ "\n)") formatting with
    | None -> 0.0
    | Some x -> x
  in
  let m =
    match get_margins f with
    | None -> 1.0
    | Some v -> v
  in
  ps -. m -. measured_space
;;

let score tags sec =
  sec.entries
  |> List.mapi (fun i e ->
    List.map
      (fun tag ->
         contains tag e.tags |> Bool.to_int |> ( * ) (List.length sec.entries - i))
      tags
    |> List.fold_left ( + ) 0)
;;

let build_resume res tags mandatory p =
  let sections = read_to_sections res |> List.map (Section.of_str mandatory) in
  let formatting = strip_formatting res in
  let scores = List.map (score tags) sections |> List.concat |> Array.of_list in
  let free_space =
    calc_available_space res sections mandatory p
    |> fun v -> v /. (line_height formatting |> float_or_default) |> Float.to_int
  in
  let locations = Section.entry_positions sections |> Array.of_list in
  let entry_weights = Section.to_weights sections formatting |> Array.of_list in
  let () = Printf.printf "avail. lines: %d" free_space in
  let () =
    Array.combine scores locations
    |> Array.combine entry_weights
    |> Array.iter (fun (a, (b, (c, d))) ->
      Printf.printf
        "free space: %d, weight: %d, score: %d, location: %d, %d\n"
        free_space
        a
        b
        c
        d)
  in
  let rating, sel_ind = knapsack free_space scores entry_weights in
  let () = Printf.printf "\n\nselections:" in
  let () = List.iter (Printf.printf "%d ") sel_ind in
  let selections = Array.make (Array.length entry_weights) false in
  let () = Printf.printf "rating: %d\n" rating in
  let () = List.iter (fun v -> selections.(v) <- true) sel_ind in
  let () = Array.iter (fun b -> Printf.printf "(%B) " b) selections in
  let resume =
    Section.combine_strs
      (List.map
         (fun s ->
            Section.select_to_str (List.mapi (fun i _ -> selections.(i)) s.entries) s)
         sections)
  in
  let () =
    Printf.printf
      "%f\n"
      (match
         Entry.measure (List.nth (List.nth sections 0).entries 0) (strip_formatting res)
       with
       | None -> 0.0
       | Some x -> x)
  in
  let attach_formatting s = strip_formatting res ^ s in
  let wrap s = "#resume(" ^ strip_header res ^ s ^ ")" in
  resume |> wrap |> attach_formatting
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
  Arg.parse speclist anon_fun usage_msg;
  (* write resume data to temporary output file *)
  let resume =
    build_resume
      (List.nth !args 0)
      (tags_of_str !tag_str)
      [ "Education"; "Skills" ]
      !page_height
  in
  string_to_file resume !output_file;
  if not (typst_compile !output_file) then failwith "Typst Compilation Error";
  (* remove the output file if none is specified *)
  if !output_file = "output.typ" then Sys.remove !output_file;
  knapsack 10 [| 10; 4; 25; 3; 9 |] [| 9; 1; 4; 2; 5 |]
  |> fun (o, b) ->
  let () = Printf.printf "knapsack val: %d\n" o in
  List.iter (Printf.printf ", %d") b;
  typst_query (List.nth !args 0) "demo"
  |> (fun v ->
  match v with
  | None -> -1.0
  | Some x -> x)
  |> Printf.printf "this is the version: %f!";
  get_margins (List.nth !args 0)
  |> (fun v ->
  match v with
  | None -> -1.0
  | Some x -> x)
  |> Printf.printf "this is the margin: %f..."
;;
