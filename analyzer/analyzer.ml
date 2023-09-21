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

let add cat set = function
  | Changelog.Def.Doc _ -> set
  | Entry e ->
    let s = Dict.of_list e.Changelog.Def.sapients in
    List.fold_left (fun s x -> Name_set.add x s)
      set
      s.%(cat)

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
    (release, Name_set.cardinal set, Name_set.cardinal diff) :: history , all
  in
  let history, _ = fold_field ~f:by_release ([], Name_set.empty) (List.rev x) in
  history




let () =
  let filename = Sys.argv.(1) in
  Format.printf "Analyzing %s@." filename;
  let json = Yojson.Safe.from_file filename in
  let changelog = Changelog.Def.from_yojson json in
  let authors = count_category "authors" changelog in
  let reviewers = count_category "review" changelog in
  let author_history = count_category_by_release "authors" changelog in
  let pp_author_info ppf (r,any,news) =
    Fmt.pf ppf "%S %d %d"
      r any news
  in
  Fmt.pr
    "@[<v>%d Authors@,%a@,\
    ---------------------------------------------@,\
     %d reviewer@,%a@,\
    ---------------------------------------------@,\
    History@,\
    %a@,\
     @]@."
    (Name_set.cardinal authors)
    Fmt.(list Changelog.Def.Pp.name) (Name_set.elements authors)
    (Name_set.cardinal reviewers)
    Fmt.(list Changelog.Def.Pp.name) (Name_set.elements reviewers)
    (Fmt.list pp_author_info) author_history
