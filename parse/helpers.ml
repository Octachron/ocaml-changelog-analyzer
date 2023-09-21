
open Angstrom

let empty_char = function
  | '\n' | '\r' | '\t' | ' ' -> true
  | _ -> false
let is_num = function '0'..'9' -> true | _ -> false
let nums = take_while is_num >>| (fun s -> if String.length s > 0 then int_of_string s else -1)
let const x _y = x
let space = skip_while empty_char
let comma = space *> char ',' <* space
let all = take_while (const true)
