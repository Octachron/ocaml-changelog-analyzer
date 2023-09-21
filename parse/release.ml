
let is_separator s = String.length s > 0 && String.for_all ((=) '-') s
let[@tail_mod_cons] rec pregroup = function
  | x :: y :: z when is_separator y ->
    Group_by.Sep x :: pregroup z
  | [] -> []
  | x :: y -> Elt x :: pregroup y

let debug ppf s = Format.fprintf ppf "(release)%s" s
let group_by lines = Group_by.list ~debug Fun.id @@ pregroup lines
