open Common
module Def = Changelog.Def


let (.%()) d x = Option.value ~default:[] (List.assoc_opt x d)
let add x authors = match x with
  | Def.Doc _ -> authors
  | Def.Entry e ->
    Name_set.add_seq (List.to_seq e.sapients.%("authors")) authors

let authors_by_release (x:Def.t) =
  fold_by_release
    ~entry:(fun authors _ _ x -> add x authors)
    ~entry_start:Name_set.empty
    ~release_start:[]
    ~release:(fun release cohorts author_set ->
         (release, author_set) :: cohorts
      )
    x

let diff (global,l) (release, authors) =
  let diff = Name_set.diff authors global in
  let global = Name_set.union authors global in
  (global, (release,diff)::l)

let () =
  let filename = Sys.argv.(1) in
  let changelog = changelog_from_file filename in
  let authors = authors_by_release changelog in
  let _, cohorts = List.fold_left diff (Name_set.empty,[]) authors in
  let pp_cohort ppf (release, authors) =
    let break ppf () = Format.fprintf ppf "@ " in
    Format.fprintf ppf "@[<v 2>Release %s, %d new authors:@ %a@]@."
      release
      (Name_set.cardinal authors)
      (Format.pp_print_seq ~pp_sep:break Def.Pp.name) (Name_set.to_seq authors)
  in
  List.iter (pp_cohort Format.std_formatter) cohorts
