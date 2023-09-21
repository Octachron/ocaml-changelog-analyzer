open Angstrom
module H = Helpers
let (let*) = (>>=)

let markdown_header =
  let* _h = many1 (char '#') in
  let* all = H.all in
  return all


let is_capital = function
  | 'A'..'Z' -> true
  | _ -> false
let is_alpha_or_space = function
  | 'A'..'Z' | 'a' .. 'z' | ' ' -> true
  | _ -> false

let old_style_header =
  let* start = satisfy is_capital in
  let* rest = take_while is_alpha_or_space <* char ':' in
  return (String.init 1 (H.const start) ^ rest)

let section_header = markdown_header <|> old_style_header

let sep_section x = match parse_string section_header ~consume:All x with
  | Ok x -> Group_by.Sep x
  | Error _ -> Elt x

let debug ppf s = Format.fprintf ppf "(section)%s" s
let group_by ~and_then x =
  Group_by.list ~debug ~parent:"Release documentation" sep_section x
    ~and_then
