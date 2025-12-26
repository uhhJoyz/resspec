type entry =
  { tags : string list
  ; contents : string
  }

let of_str t s =
  { tags = (if t then Util.tags_of_str (Util.get_param "tags" s) else []); contents = s }
;;

let to_str e = e.contents
let of_sec_str t s = List.map (of_str t) (Util.extract_func "entry" s)
let measure e formatting = Typst.measure e.contents formatting

let tags e = e.tags
