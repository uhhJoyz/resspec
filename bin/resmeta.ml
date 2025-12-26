open Section

type res_meta =
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

let of_file f mandatory page_height =
  let s = Io.read_string f in
  let formatting = Parsing.strip_formatting s in
  let padding = Util.calc_padding formatting in
  let sections =
    match Util.split_res_to_sec_opt s with
    | None -> []
    | Some l -> l |> List.map (Section.of_str mandatory)
  in
  let line_height = Parsing.line_height formatting |> Util.float_or_default in
  { contents = s
  ; formatting
  ; header = Parsing.strip_header s
  ; mandatory
  ; line_height
  ; available_space =
      (Section.calc_available_space s sections mandatory page_height padding
       |> fun v -> v /. line_height |> Float.to_int)
  ; page_height
  ; padding
  ; sections
  }
;;

let score tags sec =
  Section.entries sec
  |> List.mapi (fun i e ->
    List.map
      (fun (tag, point) ->
         List.mem tag (Entry.tags e)
         |> Bool.to_int
         |> Float.of_int
         |> ( *. )
              (List.length sec.entries - i
               |> Float.of_int
               |> fun v -> 1.0 +. (v /. (List.length sec.entries |> Float.of_int)))
         |> Float.to_int
         |> ( * ) point)
      tags
    |> List.fold_left ( + ) 0)
;;

module StringSet = Set.Make (String)

let tagset rm =
  List.map (fun s -> List.map (fun e -> Entry.tags e) s.entries) rm.sections
  |> List.flatten
  |> List.flatten
  |> StringSet.of_list
  |> StringSet.to_list
  |> Array.of_list
;;

let sections rm = rm.sections
let contents rm = rm.contents
let formatting rm = rm.formatting
let header rm = rm.header
let mandatory rm = rm.mandatory
let line_height rm = rm.line_height
let available_space rm = rm.available_space
let page_height rm = rm.page_height
let padding rm = rm.padding
