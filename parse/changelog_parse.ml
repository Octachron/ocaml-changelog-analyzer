[@@@warning "-32"]

module Changelog = Changelog.Def

open Angstrom
open Helpers

let is_empty = String.for_all empty_char

let (let*) = (>>=)

let pp_raw_entry ppf x =
  Format.fprintf ppf
    "@[<hv 2>Entry@ {%a}@]"
    Fmt.(list string) x


let pp_release_entry ppf = function
  | `Section x ->
    Format.fprintf ppf "Section [%s]" x
  | `Doc x ->
    Format.fprintf ppf "Doc [%s]" x
  | `Entry x ->
    pp_raw_entry ppf x

let line ppf () = Format.fprintf ppf "@,"

let summary_section ppf (title,items) =
  Format.fprintf ppf "@[<v 2>section %s=%d items@,%a@]"
    title (List.length items)
    (Fmt.list Changelog.Pp.any) items

let summary_release ppf (r,sections) =
  Format.fprintf ppf
    "@[<v>Release %s@,\
     --------------------------------------@,\
     %a@,\
     --------------------------------------@,\
     @]"
    r
    (Format.pp_print_list ~pp_sep:line summary_section) sections

let summary ppf x =
  Format.fprintf ppf "@[<v>%a@]"
  (Format.pp_print_list ~pp_sep:line summary_release) x


let group_all l =
  l
  |> List.filter (fun x -> String.trim x <> "")
  |> Release.group_by
    ~and_then:(fun x ->
      let s = Section.group_by ~and_then:Entry.group_by x in
      List.filter (fun (_title,entries) -> not (List.is_empty entries)) s
    )

let () =
  let file = Sys.argv.(1) in
  let lines =
    In_channel.with_open_bin file (fun chan ->
      In_channel.input_lines chan
      )
  in
  let groups = group_all lines in
  let json = Changelog.to_yojson groups in
  Format.printf "@[<v>%a@]@."
   (Yojson.Safe.pretty_print ~std:false)  json
