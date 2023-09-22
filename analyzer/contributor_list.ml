open Common


let () =
  let filename = Sys.argv.(1) and cat = Sys.argv.(2) in
  let changelog = changelog_from_file filename in
  let list = Cat.count_category cat changelog in
  Fmt.pr "@[<v>#%s@,%a@]@." cat
    (Fmt.list Changelog.Def.Pp.name) (Name_set.elements list)
