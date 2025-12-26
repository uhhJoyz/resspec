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

let line_height formatting = Typst.measure "[temp]" formatting
