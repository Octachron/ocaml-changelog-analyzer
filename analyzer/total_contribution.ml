open Common

let total_contributions_by_release (x:Changelog.Def.t) =
  fold_by_release
    ~entry:(fun c _ _ _ -> succ c)
    ~entry_start:0
    ~release_start:[]
    ~release:(fun r history c -> (r,c) :: history)
    x

let () =
  let filename = Sys.argv.(1) in
  let changelog = changelog_from_file filename in
  let contributions = total_contributions_by_release changelog in
  let total = List.fold_left (fun n (_,x) -> x + n) 0 contributions in
  let pp_vc ppf (v,x) = Fmt.pf ppf "%S %d" v x in
  Fmt.pr "@[<v># Release Contributions @,%a@,Total %d@]@."
    Fmt.(list pp_vc) contributions
    total
