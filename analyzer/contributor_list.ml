open Common


let () =
  let filename = Sys.argv.(1) in
  let rec read_cats pos =
    if pos >= Array.length Sys.argv then []
    else Sys.argv.(pos)::read_cats (pos+1)
  in
  let cats = read_cats 2 in
  let changelog = changelog_from_file filename in
  let list =
    List.fold_left (fun set cat ->
        let new_set = Cat.count_category cat changelog in
        Name_set.union new_set set
      ) Name_set.empty
      cats
  in
  Fmt.pr "@[<v>#@[<h>%a@]@,%a@]@." Fmt.(list ~sep:comma string) cats
    (Fmt.list Changelog.Def.Pp.name) (Name_set.elements list)
