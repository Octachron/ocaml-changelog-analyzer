open Angstrom
open Helpers
module Changelog = Changelog.Def
let (let*) = (>>=)


let ref = (string "#" <|> string "fix #" <|> string "RFC" <|> string "ocaml/RFCs") *> nums
let refs = (space *> sep_by1 comma ref) <?> "refs"

let optional_refs = option [] refs

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

let known_non_authors = function
  | "no cross ref to class after dump+load"
  | "changes the generated sequences"
  | "error message in french"
    as raw ->
    Fmt.epr "Ignored non author parentheses: %s@." raw;
    true
  | _ -> false

let parse_entry lines =
  let {Sapient.main; raw; sapients} = Sapient.split (List.rev lines) in
  let refs, authors =
    match parse_string ~consume:Prefix refs raw with
    | Ok x -> x, []
    | Error _ ->
      if known_non_authors raw then [], []
      else [], sapients
  in
  match parse_string ~consume:All (parse_flat_entry authors <?> "entry")  main with
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
