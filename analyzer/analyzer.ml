[@@@warning "-32"]
module Name = struct
  type t = string list
  let compare = Stdlib.compare
end

module Name_set = Set.Make(Name)
module Dict = Map.Make(String)
let (.!()) d x = Dict.find x d

let (.%()) d x = Option.value ~default:[] (Dict.find_opt x d)


let fold_field ~f:inner start l =
  List.fold_left (fun acc (k,x) -> inner acc k x)
    start l

let fold_entry_by_section ~f acc release s =
      fold_field ~f:(fun acc section es ->
          List.fold_left (fun acc e -> f acc release section e) acc es
        ) acc s

let fold_entry ~f start (x:Changelog.Def.t) =
  fold_field ~f:(fold_entry_by_section ~f) start x

let count_entry x =
  fold_entry ~f:(fun n _ _ _ -> 1 + n) 0 x

module Name_map = Map.Make(Name)

module Count(Monoid:sig
    type t
    val (+): t -> t -> t
    val zero: t
  end) = struct
  let add map key x =
    let y = Option.value ~default:Monoid.zero (Name_map.find_opt key map) in
    Name_map.add key Monoid.(x+y) map
end

module AR = struct
  type t = { author:int; review:int }
  let compare x y =
    let d = x.review + x.author - y.review - y.author in
    if d = 0 then x.author - y.author else d
  let (+) x y = {author=x.author + y.author; review=x.review + y.review}
  let zero = { author=0; review=0 }
end

module Count_AR= Count(AR)


let add cat set = function
  | Changelog.Def.Doc _ -> set
  | Entry e ->
    let s = Dict.of_list e.Changelog.Def.sapients in
    List.fold_left (fun s x -> Name_set.add x s)
      set
      s.%(cat)

let add_ar map = function
  | Changelog.Def.Doc _ -> map
  | Entry e ->
    let s = Dict.of_list e.Changelog.Def.sapients in
    let add_cat cat x map  =
      List.fold_left (fun map name -> Count_AR.add map name x)
        map
        s.%(cat)
    in
    map
    |> add_cat "authors" AR.{author=1; review=0}
    |> add_cat "review" AR.{review=1; author =1 }

let count_category cat x =
  fold_entry ~f:(fun set _ _ x -> add cat set x) Name_set.empty x

let count_category_by_release cat (x:Changelog.Def.t) =
  let by_release (history, previous) release x =
    let set =
      fold_entry_by_section ~f:(fun set _ _ x -> add cat set x)
        Name_set.empty release x
    in
    let all = Name_set.union previous set in
    let diff = Name_set.diff set previous in
    (release, diff, Name_set.cardinal set, Name_set.cardinal diff) :: history , all
  in
  let history, _ = fold_field ~f:by_release ([], Name_set.empty) (List.rev x) in
  history

let count_contributions changelog =
  fold_entry ~f:(fun map _ _ x -> add_ar map x) Name_map.empty changelog

let xy_log_histogram nbins contribs =
  let reduce z (+) proj =
    List.fold_left (fun mx (_name,r) -> mx + (proj r)) z contribs
  in
  let x r = r.AR.author in
  let y r = r.AR.review in
  let transf n = log (0.1 +. float n) in
  let rev x = (exp x -. 0.1) in
  let max_p p = transf (reduce 0 max p) in
  let min_p p = transf (reduce Int.max_int min p) in
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
  Format.printf "Analyzing %s@." filename;
  let json = Yojson.Safe.from_file filename in
  let changelog = Changelog.Def.from_yojson json in
  let n_entries = count_entry changelog in
  let authors = count_category "authors" changelog in
  let reviewers = count_category "review" changelog in
  let author_history = List.rev @@ count_category_by_release "authors" changelog in
  let reviewer_history = List.rev @@ count_category_by_release "review" changelog in
  let contributions =
    let compare (namex,x) (namey, y) =
      let d = AR.compare x y in
      if d = 0 then Stdlib.compare namex namey else d
    in
    List.sort compare @@ Name_map.bindings @@ count_contributions changelog
  in
  let hist_contribs = xy_log_histogram 20 contributions in
  let pp_author_info ppf (r,diff,any,news) =
    Fmt.pf ppf "%S %d %d @[<h>{%a}@]"
      r any news Fmt.(list ~sep:Fmt.comma Changelog.Def.Pp.name) (Name_set.elements diff)
  in
  let pp_contrib  ppf(name, {AR.author; review}) =
    Fmt.pf ppf "@[%a (%d,%d)@]" Changelog.Def.Pp.name name author review
  in
  let pp_interval ppf (l,_r) = Fmt.pf ppf "%g" l in
  let pp_hist_point  ppf (x,y,z) = Fmt.pf ppf "%a %a %d"  pp_interval x pp_interval y z in
  let pp_line ppf x =
    List.iter (Fmt.pf ppf "%a@," pp_hist_point) x
  in
  Fmt.pr
  "@[<v>Changelog: %d entry@,%d Authors@,%a@,\
    ---------------------------------------------@,\
     %d reviewer@,%a@,\
    ---------------------------------------------@,\
    Author history@,\
    %a@,\
    ---------------------------------------------@,\
    Reviewer history@,\
    %a@,\
    ---------------------------------------------@,\
    Contributions @,\
    %a@,\
    Histograms @,\
    %a@,\
     @]@."
    n_entries
    (Name_set.cardinal authors)
    Fmt.(list Changelog.Def.Pp.name) (Name_set.elements authors)
    (Name_set.cardinal reviewers)
    Fmt.(list Changelog.Def.Pp.name) (Name_set.elements reviewers)
    (Fmt.list pp_author_info) author_history
    (Fmt.list pp_author_info) reviewer_history
    (Fmt.list pp_contrib) contributions
    (Fmt.list pp_line) hist_contribs
