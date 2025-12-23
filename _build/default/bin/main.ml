open Graph
open Yojson.Basic.Util

let get_tags = function
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

let sep_tag_and_mod str =
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
  then (
    let err = Printf.sprintf "Failed to gather function %s, with input: %s\n" param f in
    failwith err)
  else Str.matched_group 2 f
;;

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
    { tags = (if t then get_tags (get_param "tags" s) else []); contents = s }
  ;;

  let get_tags e = e.tags
  let to_str e = e.contents
  let of_sec_str t s = List.map (of_str t) (extract_func "entry" s)
end

module Section = struct
  let of_str l s =
    let title = get_param "title" s in
    { name = title; entries = Entry.of_sec_str (not (List.exists (fun v -> v = title) l)) s }
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
           if sel then acc else if acc <> "" then acc ^ ",\n" ^ s else acc ^ s)
        ""
        (List.mapi (fun i v -> List.nth l i, v) sec.entries)
    ^ ")"
  ;;
end

let build_resume dir tags =
    let sections = read_to_sections dir |> List.map (Section.of_str ["Education"; "Skills"]) in
  let _ =
    List.map
      sep_tag_and_mod
      (List.map
         (fun s -> Section.select_to_str (List.map (fun _ -> false) s.entries) s)
         sections)
  in
  let () = List.iter (fun s -> Printf.printf "%s" (Section.to_str s)) sections in
  let () =
    List.iter
      (fun s ->
         Printf.printf
           "%s"
           (List.fold_left
              (fun acc e ->
                 acc
                 ^ ", "
                 ^
                 if List.length (Entry.get_tags e) > 0
                 then List.nth (Entry.get_tags e) 0
                 else "")
              ""
              s.entries))
      sections
  in
  (* List.iter *)
  (*   (fun (t, m) -> *)
  (*      Printf.printf "module contents: %s\n\ttags: " m; *)
  (*      List.iter (Printf.printf "%s ") t; *)
  (*      Printf.printf "\n") *)
  (*   tm; *)
  List.iter print_endline tags;
  List.map Section.to_str sections |> List.fold_left ( ^ ) ""
;;

let string_to_file s o =
  let oc = open_out o in
  Printf.fprintf oc "%s" s;
  close_out oc
;;

let output_file = ref "output.typ"
let args = ref []
let tag_str = ref ""
let usage_msg = "resspec <source-dir-or-file> -o <output-filename> -t <tag1>,...,<tagN>"
let anon_fun filename = args := filename :: !args

let speclist =
  [ "-o", Arg.Set_string output_file, "Set output filename."
  ; "-t", Arg.Set_string tag_str, "Set tags of built resume."
  ]
;;

let () =
  Arg.parse speclist anon_fun usage_msg;
  (* write resume data to temporary output file *)
  let _ = build_resume (List.nth !args 0) (get_tags !tag_str) in
  string_to_file "test" !output_file;
  if not (typst_compile !output_file) then failwith "Typst Compilation Error";
  (* remove the output file if none is specified *)
  if !output_file = "output.typ" then Sys.remove !output_file
;;

module NodeName = struct
  type t = string

  let compare = compare
  let hash = Hashtbl.hash
  let equal = ( = )
  (* let default = "N/A" *)
end

module Int = struct
  type t = int

  let compare = compare

  (* let hash = Hashtbl.hash *)
  (* let equal = ( = ) *)
  let default = 0
end

module G = Imperative.Graph.ConcreteLabeled (NodeName) (Int)
module H = Hashtbl.Make (NodeName)

let g = G.create ()
let nodes = H.create 16

let add_node s =
  let v = G.V.create s in
  H.add nodes v s;
  G.add_vertex g v
;;

let tagstrings = [ "test"; "test 2"; "test 3" ]
let () = List.iter add_node tagstrings
let () = Printf.printf "%d nodes added.\n" (G.nb_vertex g)

let () =
  List.iter (fun s -> H.find nodes s |> G.add_edge g (H.find nodes "test")) tagstrings
;;

let () = Printf.printf "%d edges added.\n" (G.nb_edges g)

(* let () = *)
(*   G.iter_vertex *)
(*     (fun v1 -> *)
(*        G.iter_vertex *)
(*          (fun v2 -> *)
(*             if G.V.label v1 <> G.V.label v2 then G.add_edge_e g (G.E.create v1 v2 16)) *)
(*          g) *)
(*     g *)
(* ;; *)

(* let () = G.iter_edges_e (fun e -> Printf.printf "%d" (G.E.label e)) g *)

let () =
  let json = Yojson.Basic.from_string (read_string "./demo/meta.json") in
  let sec = json |> member "folders" |> to_list |> filter_string in
  List.iter
    (fun d ->
       let dl = Sys.readdir ("./demo/" ^ d) in
       Array.iter (fun f -> Printf.printf "%s " f) dl)
    sec
;;
