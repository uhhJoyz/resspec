let solve cap va wa =
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
  let memo = Array.make_matrix (n + 1) (cap + 1) (-1) in
  let o = aux cap n memo in
  let rec backtrack i j acc =
    if i <= 0 || j <= 0
    then acc
    else if memo.(i).(j) = memo.(i - 1).(j)
    then
      (* not taken *)
      backtrack (i - 1) j acc
    else
      (* taken *)
      backtrack
        (i - 1)
        (j - wa.(i - 1))
        (if j - wa.(i - 1) >= 0 then (i - 1) :: acc else acc)
  in
  o, backtrack n cap []
;;
