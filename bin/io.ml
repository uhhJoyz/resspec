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
