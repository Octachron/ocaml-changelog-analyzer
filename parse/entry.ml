open Angstrom
open Helpers
module Changelog = Changelog.Def
let (let*) = (>>=)


let ref = (string "#" <|> string "RFC" <|> string "ocaml/RFCs") *> nums
let refs = (space *> sep_by1 comma ref) <?> "refs"

let bullet =
  (space *> (char '*' *> return true) <|> (char '-' *> return false)) <?> "bullet"

let start = (bullet <* refs) <?> "entry prefix"

let by_entry x =
  match parse_string ~consume:Prefix start x with
  | Error _ -> Group_by.Elt x
  | Ok _ -> Sep (`Start x)

let parse_flat_entry sapients =
  let* bullet in
  let* refs in
  let* () = space *> skip_many (char ':') *> space in
  let* rest = all <?> "rest of entry" in
  return { Changelog.references=refs;
           sapients;
           text= rest;
           breaking=bullet;
         }

let parse_entry lines =
  let main, authors = Sapient.split (List.rev lines) in
  match parse_string ~consume:All (parse_flat_entry authors <?> "entry")  main with
  | Ok x -> Changelog.Entry x
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
