module Edge = struct
  type t = Changelog.Def.name * Changelog.Def.name
  let compare (x:t) (y:t) = Stdlib.compare x y
  let make x y = if x <= y then (x,y) else (y,x)
end

module Edge_map = Map.Make(Edge)

open Common

let cat_set s cat set =
  List.fold_left (fun s x -> Name_set.add x s)
    set
    s.%(cat)


let add_one edge_map xy =
  let new_value = 1 + Option.value ~default:0 (Edge_map.find_opt xy edge_map) in
  Edge_map.add xy new_value edge_map

let add_edge edge_map = function
    | Changelog.Def.Doc _ -> edge_map
    | Entry e ->
      let s = Dict.of_list e.Changelog.Def.sapients in
      let set =
        Name_set.empty |> cat_set s "authors" |> cat_set s "review"
      in
      Name_set.fold (fun name edge_map ->
          Name_set.fold (fun name' edge_map ->
              if name = name' then edge_map else
                add_one edge_map (Edge.make name name')
            )
            set
            edge_map
        )
        set edge_map

let count changelog =
  fold_entry
    ~f:(fun edge_map _ _ x -> add_edge edge_map x)
    Edge_map.empty
    changelog

let pp_name = Changelog.Def.Pp.name'
let pp_edge ppf ((x,y), _weight) =
  Fmt.pf ppf {|%a -- %a|}
     pp_name x
     pp_name y

let pp_dot ppf edge_map =
  Fmt.pf ppf
    "@[<v 2>graph {@,\
     %a@;<0 -2>\
     }@]"
    (Fmt.list pp_edge)
    (Edge_map.bindings edge_map)

let () =
  let filename = Sys.argv.(1) in
  let changelog = changelog_from_file filename in
  let graph = count changelog in
  Fmt.pr "%a@." pp_dot graph
