
module Pp = struct
  [@@@warning "-32"]
  let string =  Format.pp_print_string
  let int = Format.pp_print_int
  let comma ppf () = Format.fprintf ppf ",@ "
  let semi ppf () = Format.fprintf ppf ";@ "
  let visible_space ppf () = Format.fprintf ppf "⍽"
  let list ?(sep=comma)= Format.pp_print_list ~pp_sep:sep
end


module Group_by = struct
  type ('a,'b) split =
    | Sep of 'a
    | Elt of 'b

  let fold_by_separator ?(debug=(fun _ _ -> ())) sep (left,state) x =
    match sep x, state with
    | Sep y, Some (title,x) ->
      (title, List.rev x) :: left, Some (y,[])
    | Sep y, None ->
      left, Some (y,[])
    | Elt x, None ->
      Format.eprintf "@[Orphan elements=@ @[%a@]@]@." debug x ;
      left, None
    | Elt x, Some (title,l) ->
      left, Some (title, x :: l)

  let commit (left,x) = match x with
    | None -> List.rev left
    | Some (title, x) ->
      List.rev @@ (title, List.rev x) :: left

  let group_by fold_left ?parent ?debug sep x =
    let start = match parent with
      | None -> None
      | Some p -> Some (p, [])
    in
    commit @@ fold_left (fold_by_separator ?debug sep) ([],start) x

   let list ?parent ?debug sep x = group_by List.fold_left ?parent ?debug sep x
   let seq ?parent ?debug sep x = group_by Seq.fold_left ?parent ?debug sep x

end

open Angstrom

module Parsing_misc = struct
  let is_num = function '0'..'9' -> true | _ -> false
  let nums = take_while is_num >>| int_of_string
  let const x _y = x
  let space = skip_many (char ' ')
  let comma = space *> char ',' <* space
  let all = take_while (const true)
end open Parsing_misc

module Sapient = struct

  type sep =
    | Comma
    | And

  type name = string list
  type t = (string * name list) list

  let by_connector = function
    | "," -> Group_by.Sep Comma
    | "and" -> Group_by.Sep And
    | x -> Elt x

  let split_section = function
    | x :: "by" :: (_ :: _ as q) -> [Group_by.Sep x; Elt q]
    | q -> [Elt q]


  let parse authors =
    let split_comma s =
      let len = String.length s in
      if len > 1 && s.[len-1] = ',' then
        Seq.cons (String.sub s 0 (len-1))  (Seq.return ",")
      else
        Seq.return s
    in
    let words =
      String.split_on_char ' ' authors
      |> List.to_seq
      |> Seq.filter (function "" -> false | _ -> true)
      |> Seq.concat_map split_comma
      |> Group_by.seq ~parent:Comma by_connector
      |> List.map snd
      |> List.concat_map split_section
      |> Group_by.list ~parent:"authors" Fun.id
    in words

  let rec split after = function
    | [] -> `Doc (String.concat "" (List.rev after))
    | next :: before ->
      match parse_string ~consume:Prefix (space *> char '(' *> all ) next with
      | Error _ -> split (next::after) before
      | Ok x ->
        let authors = parse (String.concat "" (x::after)) in
        `Entry (String.concat "" (List.rev before), authors)

     let pp_sapient ppf x =
       Format.fprintf ppf "(%a)" Pp.(list ~sep:visible_space string) x

     let pp ppf (title,x) =
       Format.fprintf ppf "%s={@[%a@]}" title (Pp.list pp_sapient) x

end


let empty_char = function
  | '\n' | '\r' | '\t' | ' ' -> true
  | _ -> false

let is_empty = String.for_all empty_char

let is_separator =
  String.for_all ((=) '-')


let (let*) = (>>=)

let markdown_header =
  let* h = many1 (char '#') in
  let* all in
  return (List.length h, all)

type entry =
  { references: int list;
    text:string;
    breaking:bool;
    sapients: Sapient.t
  }


let start =
  (char '*' *> return true)
  <|> (char '-' *> return false)


let ref = char '#' *> nums
let refs = space *> sep_by comma ref




let parse_flat_entry sapients =
  let* start in
  let* refs in
  let* () = space *> skip_many (char ':') *> space in
  let* rest = all in
  return { references=refs;
           sapients;
           text= rest;
           breaking=start;
         }

let parse_entry lines =
  match Sapient.split [] (List.rev lines) with
  | `Doc _ as x -> x
  | `Entry (main, authors) ->
    match parse_string ~consume:All (parse_flat_entry authors)  main with
    | Ok x -> `Entry x
    | Error x -> `Doc x

let classify = function
  | [] -> None
  | [x] ->
    let x = String.trim x in
    begin match parse_string ~consume:All markdown_header x with
      | Ok (_,rest) -> Some (`Section rest)
      | Error _ ->
        begin match parse_string ~consume:All (parse_flat_entry []) x with
          | Ok x -> Some (`Oneliner_entry x)
          | Error _ -> Some (`Doc x)
        end
    end
  | [x;y] when is_separator y -> Some (`Release x)
  | xs -> Some (`Entry xs)




let by_newline x =
  if is_empty x then
    Group_by.Sep ()
  else
    Elt x

let by_release = function
  | `Release x -> Group_by.Sep x
  | `Doc _ | `Oneliner_entry _ | `Entry _ | `Section _ as x -> Elt x

let by_section = function
  | `Section x -> Group_by.Sep x
  | `Oneliner_entry x -> Elt (`Entry x)
  | `Doc d as x ->
      Format.eprintf
        "@[Doc? [%s]@]@." d;
      Elt x
  | `Entry t ->
    match parse_entry t with
    | `Entry _ as x -> Elt x
    | `Doc d as x ->
      Format.eprintf
        "@[Doc? [%s]@]@." d;
      Elt x



let pp_section_entry ppf x =
    Format.fprintf ppf
      "@[<hv 2>Entry@ {@,\
       breaking=%B;@,\
       references=@[%a@];@,\
       text=\"%s\"@,\
       %a;@,\
       }@]"
      x.breaking
      (Pp.list Pp.int) x.references
      x.text
      (Pp.list Sapient.pp) x.sapients

let pp_section_item ppf = function
  | `Doc x -> Format.fprintf ppf "Doc [%s]" x
  | `Entry x -> pp_section_entry ppf x

let pp_raw_entry ppf x =
    Format.fprintf ppf
      "@[<hv 2>Entry@ {%a}@]"
      (Pp.list Pp.string) x


let pp_release_entry ppf = function
  | `Section x ->
    Format.fprintf ppf "Section [%s]" x
  | `Doc x ->
    Format.fprintf ppf "Doc [%s]" x
  | `Oneliner_entry x ->
    pp_section_entry ppf x
  | `Entry x ->
    pp_raw_entry ppf x


let line ppf () = Format.fprintf ppf "@,"

let summary_section ppf (title,items) =
  Format.fprintf ppf "@[<v 2>section %s=%d items@,%a@]"
    title (List.length items)
    (Pp.list pp_section_item) items

let summary_release ppf (r,sections) =
  Format.fprintf ppf "@[<v>%s@,%a@]"
    r
    (Format.pp_print_list ~pp_sep:line summary_section) sections

let summary ppf x =
  Format.fprintf ppf "@[<v>%a@]"
  (Format.pp_print_list ~pp_sep:line summary_release) x


let group_all l =
  l
  |> Group_by.list ~parent:() ~debug:Pp.string by_newline
  |> List.map snd
  |> List.filter_map classify
  |> Group_by.list ~debug:pp_release_entry by_release
  |> List.map (fun (t,x) -> t, Group_by.list ~debug:pp_section_item by_section x)

let () =
  let file = Sys.argv.(1) in
  let lines =
    In_channel.with_open_bin file (fun chan ->
      In_channel.input_lines chan
      )
  in
  let groups = group_all lines in
  Format.printf "@[summary=%a@]@."
   summary groups
