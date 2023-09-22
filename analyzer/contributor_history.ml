open Common

let contribution_by_release (x:Changelog.Def.t) =
  fold_by_release
    ~entry:(fun map _ _ x -> AR.add map x)
    ~entry_start:Name_map.empty
    ~release_start:[]
    ~release:(fun release history map -> (release, map) :: history)
    x

let () =
  let filename = Sys.argv.(1)  in
  let changelog = changelog_from_file filename in
  let history = contribution_by_release changelog in
  let pp_author ppf (name, {AR.Vect.author; review}) =
    Fmt.pf ppf "%a %d %d" Changelog.Def.Pp.name name author review
  in
  let pp_release ppf (r,l) =
    Fmt.pf ppf "# %s@,#@,@,%a" r
      (Fmt.list pp_author)
      (List.sort AR.compare @@ Name_map.bindings l)
  in
  Fmt.pr "@[<v># Contributor history@,%a@]@."
    (Fmt.list pp_release) history
