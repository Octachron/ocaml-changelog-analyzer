open Common


let pp_contrib ids  ppf (name, {AR.Vect.author; review}) =
  let id = Name_map.find name ids in
  Fmt.pf ppf "@[%g %g %d %a@]" author review id
    Changelog.Def.Pp.name name

let () =
  let filename = Sys.argv.(1) in
  let changelog = changelog_from_file filename in
  let ids = ids changelog in
  let contributions = AR.sorted_contributions changelog in
  Fmt.pr "@[<v># Contributor Authored Reviewed@,%a@]@."
    (Fmt.list @@ pp_contrib ids) contributions
