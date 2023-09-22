open Common

let () =
  let filename = Sys.argv.(1) in
  let changelog = changelog_from_file filename in
  let contributions = AR.sorted_contributions changelog in
  Fmt.pr "@[<v># Contributor Authored Reviewed@,%a@]@."
    (Fmt.list AR.pp_contrib) contributions
