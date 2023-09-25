open Common

let contribution_by_release (x:Changelog.Def.t) =
  fold_by_release
    ~entry:(fun map _ _ x -> AR.add map x)
    ~entry_start:Name_map.empty
    ~release_start:[]
    ~release:(fun release history map -> (release, map) :: history)
    x

let parse_release_name x =
  match Scanning.release_name x with
    | Some x -> x
    | None -> Fmt.epr "Cannot parse:%s@." x; exit 2

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
