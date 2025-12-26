open Io

module CharSet = Set.Make (Char)

let float_or_default = function
  | None -> -1.0
  | Some x -> x
;;

let tags_of_str = function
  | "" -> []
  | tag_str -> String.split_on_char ',' tag_str
;;

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

(* archaic code preserved from a bygone version, left here for sentimental reasons *)
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

let calc_padding formatting =
  let sec_size = Typst.measure "section()" formatting |> float_or_default in
  let ent_size = Typst.measure "entry()" formatting |> float_or_default in
  let both = Typst.measure "section(entry())" formatting |> float_or_default in
  both -. (sec_size +. ent_size)
;;
