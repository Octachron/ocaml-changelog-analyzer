open Common

let contribution_by_release (x:Changelog.Def.t) =
  fold_by_release
    ~entry:(fun map _ _ x -> AR.add map x)
    ~entry_start:Name_map.empty
    ~release_start:[]
    ~release:(fun release history map -> (release, map) :: history)
    x

let parse_release_name s =
  match Scanning.release_name s with
  | Some x -> x
  | None -> Fmt.epr "Cannot parse:%s@." s; exit 2


let normal_non_patch (r,m)= match parse_release_name r with
  | Normal p -> if p.patch = 0 then Some ((p.major, p.minor), m) else None
  | Maintenance _ | Working_version -> None

let interpolate i versions time =
  let nm (_,x) = x in
  let before = Name_map.map (fun x -> (x,AR.Vect.zero)) (nm versions.(i)) in
  let next = Name_map.map (fun x -> (AR.Vect.zero,x)) (nm versions.(i+1)) in
  let merge _k x y = match x, y with
    | None, None -> None
    | Some _ as x, None | None, (Some _ as x)  -> x
    | Some (b,_), Some (_,a) -> Some (b,a)
  in
  let shared = Name_map.merge merge before next in
  Name_map.map (fun (before,next) -> AR.Vect.interpolate time before next) shared

let rec repeat_last = function
  | [] -> []
  | [a] -> [a;a]
  | x :: q -> x :: repeat_last q


let with_fmt filename  f =
  Out_channel.with_open_bin filename (fun ch ->
      f (Format.formatter_of_out_channel ch)
    )


let (.!()) map x = match Name_map.find_opt x map with
  | Some x -> x
  | None -> Fmt.epr "Not found: %a@." Changelog.Def.Pp.name x; exit 2

let pp_author ids ppf (name, {V2.x; y}) =
  Fmt.pf ppf "%f %f %d %a" x y ids.!(name) Changelog.Def.Pp.name name

let pp_version ppf (mj,mn) =
  if mj >= 5 then
    Fmt.pf ppf "%d.%d" mj mn
  else
    Fmt.pf ppf "%d.%02d" mj mn

let pp_trans ppf (before,after) =
  Fmt.pf ppf "%a->%a" pp_version before pp_version after

let cmd script versions time  =
  let s =
    Fmt.str
      {|OCAML_VERSION="%a" OCAML_TIME=%04d gnuplot %s|}
      pp_trans versions
      time script
  in
  Fmt.epr "%s@." s;
  let r = Sys.command s in
  if r <> 0 then
    Fmt.epr "Failing to plot with %d@." r

let () =
  let filename = Sys.argv.(1) in
  let template = Scanf.format_from_string Sys.argv.(2) "%d" in
  let plot = cmd Sys.argv.(3) in
  let template d = Fmt.str template d in
  let changelog = changelog_from_file filename in
  let ids =
    let authors = Cat.count_category "authors" changelog in
    let reviewers = Cat.count_category "review" changelog in
    Name_set.union reviewers authors
    |> Name_set.to_seq
    |> Seq.mapi (fun i x -> x, i)
    |> Name_map.of_seq
  in
  let history =
    changelog
    |> contribution_by_release
    |> List.filter_map normal_non_patch
    |> repeat_last
    |> Array.of_list
  in
  Fmt.pr "Interpolation %d versions@." (Array.length history);
  let interpolation_steps = 100 in
  for i = 0 to Array.length history - 2 do
    for time = 0 to interpolation_steps - 1 do
      let step = i*interpolation_steps + time in
      let before, _ = history.(i) in
      let next, _ = history.(i+1) in
      let intv = (before,next) in
      with_fmt (template step) (fun ppf ->
          let time = float time /. float interpolation_steps in
          let hi =
            interpolate i history time
          in
          Fmt.pf ppf "@[<v># OCaml%a %g@,%a@]@." pp_trans intv
            time
            (Fmt.list @@ pp_author ids) (Name_map.bindings hi)
        );
      plot intv step
    done;
  done
