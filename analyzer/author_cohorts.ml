open Common
module Def = Changelog.Def


let (.%()) d x = Option.value ~default:[] (List.assoc_opt x d)
let add x authors = match x with
  | Def.Doc _ -> authors
  | Def.Entry e ->
    Name_set.add_seq (List.to_seq e.sapients.%("authors")) authors

let authors_by_release (x:Def.t) =
  fold_by_release
    ~entry:(fun authors _ _ x -> add x authors)
    ~entry_start:Name_set.empty
    ~release_start:[]
    ~release:(fun release cohorts author_set ->
         (release, author_set) :: cohorts
      )
    x

let stat (global,l) (release, authors, future) =
  let diff = Name_set.diff authors global in
  let ephemeral = Name_set.diff diff future in
  let lasting = Name_set.inter diff future in
  let global = Name_set.union authors global in
  (global, (release, authors, ephemeral,lasting)::l)

let keep_large (_,a,e) =
  Iarray.fold_left (+) 0 a + e >= 20

let classify cohorts (r,authors, eph, _lasting) =
  r,
  Iarray.map (fun c -> Name_set.(cardinal (inter c authors))) cohorts,
  Name_set.cardinal eph

let _basic () =
  let filename = Sys.argv.(1) in
  let changelog = changelog_from_file filename in
  let authors = authors_by_release changelog in
  let _, with_future = List.fold_right (fun (r,authors) (last,l) ->
      let future = Name_set.union authors last in
      future, (r,authors,last) :: l
    ) authors (Name_set.empty, [])
  in
  let _, cohorts = List.fold_left stat (Name_set.empty,[]) with_future in
  let pp_cohort ppf (release, _, ephemeral, authors) =
    let break ppf () = Format.fprintf ppf "@ " in
    Format.fprintf ppf "@[<v 2>Release %s, %d ephemeral authors, %d new authors:@ %a@]@."
      release
      (Name_set.cardinal ephemeral)
      (Name_set.cardinal authors)
      (Format.pp_print_seq ~pp_sep:break Def.Pp.name) (Name_set.to_seq authors)
  in
  List.iter (pp_cohort Format.std_formatter) cohorts

let short_name r = match String.split_on_char ' ' r with
  | "OCaml" :: v :: _ -> v
  | "Working" :: _ :: _ -> "dev"
  | _ -> "?"

let hist () =
  let filename = Sys.argv.(1) in
  let changelog = changelog_from_file filename in
  let authors = authors_by_release changelog in
  let authors = List.map (fun (r,a) -> short_name r, a) authors in
  let _, with_future = List.fold_right (fun (r,authors) (last,l) ->
      let future = Name_set.union authors last in
      future, (r,authors,last) :: l
    ) authors (Name_set.empty, [])
  in
  let _, cohorts = List.fold_left stat (Name_set.empty,[]) with_future in
  let cohorts = List.rev cohorts in
  let cohort_classes =
    Iarray.of_seq @@ Seq.map (fun (_r,_a,_e,l) -> l ) @@ List.to_seq cohorts
  in
  let hist = List.map (classify cohort_classes) cohorts in
  let filtered_hist = List.filter keep_large hist in
  let pp_header ppf r =
    Format.fprintf ppf "@[<h>Release" ;
    List.iter (fun (r,_,_) -> Format.fprintf ppf " %s" r) r;
    Format.fprintf ppf " ephemere";
    Format.fprintf ppf "@]@,"
  in
  let pp_release ppf (release, cohorts, e) =
    Format.fprintf ppf "@[<v>@[<h>%S" release;
    Iarray.iter (Format.fprintf ppf " %d") cohorts;
    Format.fprintf ppf " %d" e;
    Format.fprintf ppf "@]@,"
  in
  let ppf = Format.std_formatter in
  Format.fprintf ppf "@[<v>";
  pp_header ppf hist;
  List.iter (pp_release ppf) filtered_hist;
  Format.fprintf ppf "@]@."

let () = hist ()
