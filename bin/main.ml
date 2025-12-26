(* open Minttea *)
open Util

(* let of_file f mandatory page_height = *)
let build_resume res tags mandatory p =
  let rm = Resmeta.of_file res mandatory p in
  let scores = List.map (Resmeta.score tags) (Resmeta.sections rm) |> List.concat |> Array.of_list in
  let free_space = Resmeta.available_space rm in
  let locations = Section.entry_positions (Resmeta.sections rm) in
  let entry_weights =
    Section.to_weights (Resmeta.sections rm) (Resmeta.formatting rm) (Resmeta.padding rm) |> Array.of_list
  in
  let _, sel_ind = Knapsack.solve free_space scores entry_weights in
  let mappings = Section.init_mask sel_ind locations (Resmeta.sections rm) in
  let resume = Section.render_sections mandatory mappings (Resmeta.sections rm) in
  let prepend_formatting s = (Resmeta.formatting rm) ^ s in
  let wrap s = "#resume(" ^ (Resmeta.header rm) ^ s ^ ")" in
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
  Arg.parse speclist anon_fun usage_msg;
  (* write resume data to temporary output file *)
  let resume =
    build_resume
      (List.nth !args 0)
      (tags_of_str !tag_str)
      [ "Education"; "Skills" ]
      !page_height
  in
  Io.string_to_file resume !output_file;
  if not (Typst.compile !output_file) then failwith "Typst Compilation Error";
  (* remove the output file if none is specified *)
  if !output_file = "output.typ" then Sys.remove !output_file;
  Printf.printf
    "Successfully compiled and exported resume to: %s\n"
    (String.sub !output_file 0 (String.length !output_file - 3) ^ "pdf")
;;
