module Name = struct
  type t = string list
  let compare = Stdlib.compare
end

module Name_set = Set.Make(Name)
module Dict = Map.Make(String)
let (.!()) d x = Dict.find x d

let (.%()) d x = Option.value ~default:[] (Dict.find_opt x d)


let fold_field ~f:inner start l =
  List.fold_left (fun acc (k,x) -> inner acc k x)
    start l

let fold_entry_by_section ~f acc release s =
      fold_field ~f:(fun acc section es ->
          List.fold_left (fun acc e -> f acc release section e) acc es
        ) acc s

let fold_entry ~f start (x:Changelog.Def.t) =
  fold_field ~f:(fold_entry_by_section ~f) start x

let count_entry x =
  fold_entry ~f:(fun n _ _ _ -> 1 + n) 0 x

module Name_map = Map.Make(Name)

module Count(Monoid:sig
    type t
    val (+): t -> t -> t
    val zero: t
  end) = struct
  let add map key x =
    let y = Option.value ~default:Monoid.zero (Name_map.find_opt key map) in
    Name_map.add key Monoid.(x+y) map
end

let changelog_from_file filename =
  Format.printf "Analyzing %s@." filename;
  let json = Yojson.Safe.from_file filename in
  Changelog.Def.from_yojson json

module AR = struct
  module Vect = struct
    type t = { author:int; review:int }
    let compare x y =
      let d = x.review + x.author - y.review - y.author in
      if d = 0 then x.author - y.author else d
    let (+) x y = {author=x.author + y.author; review=x.review + y.review}
    let zero = { author=0; review=0 }
  end

  module Count= Count(Vect)

  let add map = function
    | Changelog.Def.Doc _ -> map
    | Entry e ->
      let s = Dict.of_list e.Changelog.Def.sapients in
      let add_cat cat x map  =
        List.fold_left (fun map name -> Count.add map name x)
          map
          s.%(cat)
      in
      map
      |> add_cat "authors" Vect.{author=1; review=0}
      |> add_cat "review" Vect.{review=1; author =1 }

  let count changelog =
    fold_entry ~f:(fun map _ _ x -> add map x) Name_map.empty changelog

  let sorted_contributions changelog  =
    let compare (namex,x) (namey, y) =
      let d = compare x y in
      if d = 0 then Stdlib.compare namex namey else d
    in
    List.sort compare @@ Name_map.bindings @@ count changelog

  let pp_contrib  ppf (name, {Vect.author; review}) =
    Fmt.pf ppf "@[%a (%d,%d)@]" Changelog.Def.Pp.name name author review

end

module Cat = struct
  let add cat set = function
    | Changelog.Def.Doc _ -> set
    | Entry e ->
      let s = Dict.of_list e.Changelog.Def.sapients in
      List.fold_left (fun s x -> Name_set.add x s)
        set
        s.%(cat)

  let count_category cat x =
    fold_entry ~f:(fun set _ _ x -> add cat set x) Name_set.empty x
end
