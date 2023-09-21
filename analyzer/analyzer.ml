[@@@warning "-32"]
module Name = struct
  type t = string list
  let compare = Stdlib.compare
end

module Name_set = Set.Make(Name)
module Dict = Map.Make(String)
let (.!()) d x = Dict.find x d

let (.%()) d x = Option.value ~default:[] (Dict.find_opt x d)


let fold_field start inner l =
  List.fold_left (fun acc (k,x) -> inner acc k x)
    start l


let fold_entry start f (x:Changelog.Def.t) =
  fold_field start (fun acc release s ->
      fold_field acc (fun acc section es ->
          List.fold_left (fun acc e -> f acc release section e) acc es
        )
        s
    )
    x

let authors x =
  let add_author set _ _ = function
    | Changelog.Def.Doc _ -> set
    | Entry e ->
      let s = Dict.of_list e.Changelog.Def.sapients in
      List.fold_left (fun s x -> Name_set.add x s)
        set
        s.%("authors")
  in
  fold_entry Name_set.empty
   add_author x


let () =
  let filename = Sys.argv.(1) in
  Format.printf "Analyzing %s@." filename;
  let json = Yojson.Safe.from_file filename in
  let changelog = Changelog.Def.from_yojson json in
  let authors = authors changelog in
  Fmt.pr "@[<v>%d Authors@,%a@]@."
    (Name_set.cardinal authors)
    Fmt.(list Changelog.Def.Pp.name) (Name_set.elements authors)
