let output_file = ref "output.typ"
let interactive = ref false
let level = ref 10
let args = ref []
let tag_str = ref ""
let page_height = ref 11.0

let usage_msg =
  "resspec <source-dir-or-file> -o <output-filename> -t <tag1>,...,<tagN> -i -p 11.0 -l \
   10"
;;

let anon_fun filename = args := filename :: !args

let speclist =
  [ "-o", Arg.Set_string output_file, "Set output filename."
  ; "-t", Arg.Set_string tag_str, "Set tags of target resume."
  ; "-p", Arg.Set_float page_height, "Manually specifyg page height (inches, default 11)."
  ; "-i", Arg.Set interactive, "Enter interactive builder mode."
  ; ( "-l"
    , Arg.Set_int level
    , "Set your current level (number of skill points in interactive mode, default 10)." )
  ]
;;

let () =
  Arg.parse speclist anon_fun usage_msg;
  if List.length !args <= 0
  then Printf.printf "Must specify at least one command line argument.\n\t%s \n" usage_msg
  else if String.length !tag_str <= 0 && not !interactive
  then
    Printf.printf
      "resspec must be run with either the -t <tag1,tag2,...> flag or using the \
       interactive mode with -i.\n\
      \ \t %s\n"
      usage_msg
  else (
    (* write resume data to temporary output file *)
    let rm = Resmeta.of_file (List.nth !args 0) [ "Education"; "Skills" ] !page_height in
    if !interactive
    then (
      let tagset = Resmeta.tagset rm |> Array.map (fun t -> t, 0) in
      Cli.start_interactive_mode tagset level output_file rm)
    else (
      (* assumes even weighting *)
      let tagset = Resmeta.tagset rm |> Array.to_list |> List.map (fun t -> t, 1) in
      let () = Cli.builder_call tagset rm !output_file in
      Printf.printf
        "Successfully compiled and exported resume to: %s\n"
        (String.sub !output_file 0 (String.length !output_file - 3) ^ "pdf")))
;;
