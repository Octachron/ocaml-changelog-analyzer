open Angstrom
open Helpers
let parse = (string "#" <|> string "fix #" <|> string "RFC" <|> string "ocaml/RFCs") *> nums
let parse_many = (space *> sep_by1 comma parse) <?> "refs"
