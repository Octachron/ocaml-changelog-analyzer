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

let fold_by_release
    ~release
    ~release_start
    ~entry
    ~entry_start
    changelog =
  let by_release r_acc r x =
    let release_info =
      fold_entry_by_section ~f:entry entry_start r x
    in
    release r r_acc release_info
  in
  fold_field ~f:by_release release_start changelog


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
  let json = Yojson.Safe.from_file filename in
  Changelog.Def.from_yojson json


module AR = struct
  module Vect = struct
    type t = { author:float; review:float }
    let linear alpha x y = x +. alpha *. (y -. x)
    let interpolate alpha x y =
      { author = linear alpha x.author y.author;
        review = linear alpha x.review y.review;
      }

    let compare x y =
      let d = x.review -. y.review in
      if d = 0. then x.author -. y.author else d
    let (+) x y = {author=x.author +. y.author; review=x.review +. y.review}
    let (/.) x n = {author= x.author /. n; review = x.review /. n  }
    let zero = { author=0.; review=0. }
  end

  module Count= Count(Vect)

  let add map = function
    | Changelog.Def.Doc _ -> map
    | Entry e ->
      let s = Dict.of_list e.Changelog.Def.sapients in
      let add_cat cat x map  =
        let set = s.%(cat) in
        let many = float @@ List.length @@ set in
        List.fold_left (fun map name -> Count.add map name Vect.(x/. many))
          map set
      in
      map
      |> add_cat "authors" Vect.{author=1.; review=0. }
      |> add_cat "review" Vect.{review=1.; author =0. }

  let count changelog =
    fold_entry ~f:(fun map _ _ x -> add map x) Name_map.empty changelog

  let compare (namex,x) (namey, y) =
    let d = Vect.compare x y in
    if d = 0. then Stdlib.compare namex namey
    else if d >= 0. then 1
    else -1


  let sort x = List.sort compare @@ Name_map.bindings x
  let sorted_contributions changelog  =
    sort @@ count changelog

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



let ids changelog =
    let authors = Cat.count_category "authors" changelog in
    let reviewers = Cat.count_category "review" changelog in
    Name_set.union reviewers authors
    |> Name_set.to_seq
    |> Seq.mapi (fun i x -> x, i)
    |> Name_map.of_seq

type version =
  | Working_version
  | Normal of { major:int; minor:int; patch:int; date:string * string * string}
  | Maintenance of { major:int; minor:int}


module Scanning = struct


  module Scanf_pattern = struct
    type ('a,'i,'b,'c,'d) scan =
      ('a, 'i, 'b, 'c, 'a -> 'd option, 'd) format6
    type ('i,'b, 'x) t =
      | []
      | (::): (('a, 'i,'b, 'u -> 'x option, 'x) scan * 'u) * ('i,'b,'x) t -> ('i,'b,'x) t
  end

  let rec one_of: type x. string -> (_,_,x) Scanf_pattern.t -> x option =
    fun s pats -> match pats with
      | [] -> None
      | (fmt,k) :: rest ->
        match Scanf.sscanf_opt s fmt k with
        | None -> one_of s rest
        | Some _ as x -> x

  let release_name = function
    | "Working version" -> Some Working_version
    | s ->
      let mk major minor patch day month year  =
        Normal {major;minor;patch;date=day, month, year} in
      let mkm major minor = Maintenance {major;minor} in
      one_of s [
          "OCaml %d.%02d.%d (%s %s %s@)", mk;
          "OCaml %d.%d maintenance branch", mkm;
          "OCaml %d.%d, maintenance version", mkm;
          "OCaml %d.%d maintenance version", mkm;
        ]
end
