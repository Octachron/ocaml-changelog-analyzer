open Common

let contribution_by_release (x:Changelog.Def.t) =
  fold_by_release
    ~entry:(fun map _ _ x -> AR.add map x)
    ~entry_start:Name_map.empty
    ~release_start:[]
    ~release:(fun release history map -> (release, map) :: history)
    x

type version =
  | Working_version
  | Normal of { major:int; minor:int; patch:int; date:string * string * string}
  | Maintenance of { major:int; minor:int}


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

let parse_release_name = function
  | "Working version" -> Working_version
  | s ->
    let mk major minor patch day month year  =
      Normal {major;minor;patch;date=day, month, year} in
    let mkm major minor = Maintenance {major;minor} in
    match one_of s [
      "OCaml %d.%02d.%d (%s %s %s@)", mk;
      "OCaml %d.%d maintenance branch", mkm;
      "OCaml %d.%d, maintenance version", mkm
    ]
    with
    | Some x -> x
    | None -> Fmt.epr "Cannot parse:%s@." s; exit 2

let () =
  let filename = Sys.argv.(1) in
  let template = Scanf.format_from_string Sys.argv.(2) "%d%d%d" in
  let changelog = changelog_from_file filename in
  let history = contribution_by_release changelog in
  let pp_author ppf (name, {AR.Vect.author; review}) =
    Fmt.pf ppf "%a %d %d" Changelog.Def.Pp.name name author review
  in
  let output_release (r,l) =
    let version = parse_release_name r in
    match version with
    | Working_version | Maintenance _ -> ()
    | Normal v ->
      let filename = Fmt.str template v.major v.minor v.patch in
      let contributions = AR.sort l in
      Out_channel.with_open_bin filename (fun f ->
          let ppf = Format.formatter_of_out_channel f in
          Fmt.pf ppf "@[<v># OCaml %d.%d.%d@;%a@]@." v.major v.minor v.patch
            (Fmt.list pp_author) contributions
        )
    in
    List.iter output_release history
