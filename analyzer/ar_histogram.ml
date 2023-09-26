open Common

let xy_log_histogram nbins contribs =
  let reduce z (+) proj =
    List.fold_left (fun mx (_name,r) -> mx + (proj r)) z contribs
  in
  let x r = r.AR.Vect.author in
  let y r = r.AR.Vect.review in
  let transf x = log (0.1 +. x) in
  let rev x = (exp x -. 0.1) in
  let max_p p = transf (reduce 0. max p) in
  let min_p p = transf (reduce Float.max_float min p) in
  let max_x = max_p x and max_y = max_p y in
  let min_x = min_p x and min_y = min_p y in
  let lbin mn mx n =
    let imax = nbins - 1 in
    let i = int_of_float (float imax *. (transf n -. mn) /. (mx -. mn)) in
    min i imax
  in
  let bin r =
    lbin min_x max_x (x r),
    lbin min_y max_y (x r)
  in
  let h = Array.make_matrix nbins nbins 0 in
  let add (_name,r) =
    let ix, iy = bin r in
    h.(ix).(iy) <- h.(ix).(iy) + 1
  in
  let pos mn mx ix =
    let width = mx -. mn in
    rev (mn +. (float ix *. width) /. float nbins)
  in
  let pos_x = pos min_x max_x in
  let pos_y = pos min_y max_y in
  let interval pos i = pos i, pos (i+1) in
  List.iter add contribs;
  let h2 = Array.mapi (fun i r ->
      Array.mapi (fun j v -> interval pos_x i, interval pos_y j, v) r
  ) h
  in
  Array.to_list (Array.map Array.to_list h2)


let () =
  let filename = Sys.argv.(1) in
  let changelog = changelog_from_file filename in
  let contributions = AR.sorted_contributions changelog in
  let hist_contribs = xy_log_histogram 20 contributions in
  let pp_interval ppf (l,_r) = Fmt.pf ppf "%g" l in
  let pp_hist_point  ppf (x,y,z) = Fmt.pf ppf "%a %a %d"  pp_interval x pp_interval y z in
  let pp_line ppf x =
    List.iter (Fmt.pf ppf "%a@," pp_hist_point) x
  in
  Fmt.pr "@[<v># Authored Reviewed Contributors@,%a@]@."
    (Fmt.list pp_line) hist_contribs
