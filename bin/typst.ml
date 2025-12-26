open Io

let cap_cmd cmd =
  let input = Unix.open_process_in cmd in
  let v = In_channel.input_all input in
  In_channel.close input;
  v
;;

let query file id =
  let s = cap_cmd ("typst query " ^ file ^ " \"<" ^ id ^ ">\"") in
  if
    try Str.search_forward (Str.regexp "\"value\":\\(.*\\),\"label") s 0 >= 0 with
    | Not_found -> false
  then Some (Str.matched_group 1 s |> Float.of_string)
  else None
;;

let compile = function
  | "" -> false
  | f -> Sys.command ("typst compile " ^ f) = 0
;;

(*TODO: move this temporary file to /tmp/*)
let measure s formatting =
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
  let v = query tf "demo" in
  let () = Sys.remove tf in
  v
;;

