open Common


let pp_contrib  ppf (name, {AR.Vect.author; review}) =
  Fmt.pf ppf "@[%a %d %d@]" Changelog.Def.Pp.name name author review


let () =
  let filename = Sys.argv.(1) in
  let changelog = changelog_from_file filename in
  let contributions = AR.sorted_contributions changelog in
  Fmt.pr "@[<v># Contributor Authored Reviewed@,%a@]@."
    (Fmt.list pp_contrib) contributions
