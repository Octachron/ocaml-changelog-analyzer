open Angstrom
open Helpers
module Changelog = Changelog.Def
let (let*) = (>>=)



let optional_refs = option [] Refs.parse_many

let bullet =
  ((char '*' *> return true) <|> (char '-' *> return false)) <?> "bullet"

let start = (bullet <* optional_refs) <?> "entry prefix"

let by_entry x =
  match parse_string ~consume:Prefix start x with
  | Error _ -> Group_by.Elt x
  | Ok _ -> Sep (`Start x)

let parse_flat_entry sapients =
  let* bullet in
  let* refs = optional_refs in
  let* () = space *> skip_many (char ':') *> space in
  let* rest = all <?> "rest of entry" in
  return { Changelog.references=refs;
           sapients;
           text= rest;
           breaking=bullet;
         }

let parse_entry lines =
  let {Sapient.main; refs; sapients} = Sapient.split lines in
  match parse_string ~consume:All (parse_flat_entry sapients <?> "entry")  main with
  | Ok x ->
    let x = { x with text = String.trim x.text; references = x.references @ refs } in
    Changelog.Entry x
  | Error err ->
    Format.eprintf "@[<v> error = %s@, main=%S@]@." err main;
    Doc main

let debug ppf s = Format.fprintf ppf "(group by entry)%s" s
let group_by x =
  Group_by.list ~parent:`Doc ~debug ~and_then:Fun.id by_entry x
  |> List.filter_map (function
      | `Doc, (_ :: _  as lines) ->
        Some (Changelog.Doc (String.concat "" @@ List.rev lines))
      | `Doc, [] -> None
      | `Start x, lines -> Some (parse_entry (x::lines))
    )
