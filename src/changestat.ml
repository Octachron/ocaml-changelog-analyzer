[@@@warning "-32"]

module Pp = struct
  [@@@warning "-32"]
  let string =  Format.pp_print_string
  let int = Format.pp_print_int
  let comma ppf () = Format.fprintf ppf ",@ "
  let semi ppf () = Format.fprintf ppf ";@ "
  let visible_space ppf () = Format.fprintf ppf "⍽"
  let list ?(sep=comma)= Format.pp_print_list ~pp_sep:sep
end

module Json = struct
  let string x = `String x
  let list inner x = `List (List.map inner x)
end


type 'a fields = (string * 'a) list

let fields to_json fs =
    List.map (fun (x,l) -> x, to_json l) fs

let fields_to_yojson to_json fs =
  `Assoc (fields to_json fs)

module Group_by = struct
  type ('a,'b) split =
    | Sep of 'a
    | Elt of 'b

  let fold_by_separator k ~debug sep (left,state) x =
    match sep x, state with
    | Sep y, Some (title,x) ->
      (title, k (List.rev x)) :: left, Some (y,[])
    | Sep y, None ->
      left, Some (y,[])
    | Elt x, None ->
      Format.eprintf "@[Orphan elements=@ [@[%a@]]@]@." debug x ;
      left, None
    | Elt x, Some (title,l) ->
      left, Some (title, x :: l)

  let commit k (left,x) = match x with
    | None -> List.rev left
    | Some (title, x) ->
      List.rev @@ (title, k (List.rev x)) :: left

  let group_by ~and_then ~debug fold_left ?parent  sep x =
    let start = match parent with
      | None -> None
      | Some p -> Some (p, [])
    in
    commit and_then @@ fold_left (fold_by_separator and_then ~debug sep) ([],start) x

   let stop = Fun.id
   let list ~and_then ?parent ~debug sep x = group_by ~and_then List.fold_left ?parent ~debug sep x
   let seq ~and_then ?parent ~debug sep x = group_by ~and_then Seq.fold_left ?parent ~debug sep x

end

open Angstrom

module Parsing_misc = struct

  let empty_char = function
    | '\n' | '\r' | '\t' | ' ' -> true
    | _ -> false
  let is_num = function '0'..'9' -> true | _ -> false
  let nums = take_while is_num >>| (fun s -> if String.length s > 0 then int_of_string s else -1)
  let const x _y = x
  let space = skip_while empty_char
  let comma = space *> char ',' <* space
  let all = take_while (const true)
end open Parsing_misc

module Sapient = struct

  type sep =
    | Comma
    | And
    | Semi
  type name = string list
  let name_to_yojson =Json.(list string)
  type t = name list fields
  let to_yojson (fs:t): (string * Yojson.Safe.t) list =
    fields (Json.list name_to_yojson) fs

  let rec ligature = function
    | ("report"| "review" as x) :: "and" :: q ->
      x :: "&&&" :: ligature q
    | x :: q -> x :: ligature q
    | [] -> []

  let by_connector = function
    | "," -> Group_by.Sep Comma
    | "and" -> Group_by.Sep And
    | ";" -> Sep Semi
    | x -> Elt x

  let strip_postfix q = match List.rev q with
    | ("Coq" | "testing") :: "for" :: q -> List.rev q
    | _ :: "XCode" :: "against" :: q -> List.rev q
    | _ :: "in" :: q -> List.rev q
    | _ -> q


  let normalize_name = function
    (* groups *)
    | ["the"; "Tarides" ; "multicore";  "team"] -> ["Tarides multicore team"]
    | ["the"; "OCaml"; "core"; "development"; "team"] -> ["OCaml core development team"]
    | ["many"; "other"; "valued"; "reviewers"] | ["Many"; "fine"; "eyes"] -> ["many"]
    | ["reviewing"; "each"; "other"; "without"; "self-loops";] ->
      ["Author group review"]
   (* typo *)
    | (["Nicolas"; "Ojeda"; "Bar"])  -> ["Nicolás"; "Ojeda"; "Bär"]
    | ["Jacques-"; "Henri"; "Jourdan"] -> ["Jacques-Henri"; "Jourdan"]
  (* Long names*)
    | (["Paul-Elliot"; "Anglès"; "d'Auriac"] as q)
    | (["Antonio"; "Nuno"; "Monteiro"] as q)
    | ( [ _; ("von"|"De"|"Van"); _] as q )
    | (["Richard"; "L"; "Ford"] as q)
    | (["Fabrice"; "Le"; "Fessant"] as q)
    | (["Nicolás"; "Ojeda"; "Bär"] as q)
    | (["Demi"; "Marie"; "Obenour"] as q)
    | (["Raphael"; "Sousa"; "Santos"] as q)
    | (["Fu"; "Yong"; "Quah"] as q)
    | (["San"; "Vũ"; "Ngọc"] as q)
    | (["Isaac"; {|"Izzy"|}; "Avram"] as q)
    | (["Gabriel"; "de"; "Perthuis"] as q)
    | (["John"; "Christopher"; "McAlpine"] as q)
    | (["Peter"; "Michael"; "Green"] as q)
    | (["Khoo"; "Yit"; "Phang"] as q)
    | ("Github"|"github") :: "user" :: q
    | (["Xavier"; "Van"; "de"; "Woestyne" ] as q) ->  q
    | x ->
      Format.eprintf "Complex name or error:%s@." (String.concat " " x);
      x

  let split_section x = match strip_postfix (List.filter ((<>) "") x) with
    | "additional" :: "testing" :: "by" :: q ->
       [Group_by.Sep "tests"; Elt q]
   (* alternative name for authors *)
    | ("code"|"patch") :: "by" :: q
    | "final" :: "fix" :: "by" :: q ->
      [Group_by.Sep "authors"; Elt q]
  (* factorizations *)
    | "review" :: "&&&" :: "final" :: "fix" :: "by" :: name ->
      [Sep "review"; Elt name; Sep "authors"; Elt name]
    | "report" :: "&&&" :: "fix" :: "by" :: name ->
      [Sep "report"; Elt name; Sep "authors"; Elt name]
    | x :: "&&&" :: y :: "by" :: name ->
      [Sep x; Elt name; Sep y; Elt name]
    | x :: "&&&" :: y :: z :: "by" :: name ->
      [Sep x; Elt name; Sep (String.concat " " [y;z]); Elt name]
    | x :: "&&&" :: y :: z :: w :: "by" :: name ->
      [Sep x; Elt name; Sep (String.concat " " [y;z;w]); Elt name]
    (* author section "header"*)
    | ("help"|"review" | "thought" | "feedback" | "contributions" | "advice" | "design" | "discussion" as x)
      :: ("from"|"with") :: q
    | "patch" :: ("review" as x) :: "by" :: q
    | "with" :: ("help" | "inspiration" as x) :: "from" :: q
    | ("inspiration" as x) :: "from" :: q
    | ("fix" as x) :: "suggested" :: "by" :: q
    | _ :: ("design" as x) :: "by" :: q
    | ("light"|"code") :: ("review" as x) :: "by" :: q
    | _ :: ("fix" as x) :: "by" :: q
    | _ :: ("report" as x) :: "by" :: q
    | ("thanks" as x) :: "to" :: q
    | "following" :: ("discussion" as x) :: "with" :: q
    | "with" :: ("thanks" | "feedback" | "contributions" as x) :: ("to"|"from") :: q
    | ("report" as x) :: "par" :: q
    | "additional" :: x :: "by" :: q
    | ("fix" as x) :: "in" :: _ :: "by" :: q
    | "from" :: ("an"|"a") :: x :: "by" :: q
    | "initial" :: ("debugging" as x) :: "by" :: q
    | "design":: ("advice" as x) :: "by" :: q
    | ("compatibility" as x) :: "hacking":: "by" :: q
    | ("report" as x) :: "on" :: "the" :: q
    | "additional" :: x :: "with" :: q
    | "superior" :: "implementation" :: x :: "by" :: q
    | ("debugging" as x) :: "&" :: "test" :: ("cases" | "case") :: "by" :: q
    | x :: "by" :: q
    | ("review" | "report" as x) :: q
      ->
      if List.is_empty q then
        [Group_by.Sep x]
      else
        [Group_by.Sep x; Elt q]
    | "designed" :: "with" :: q -> [Group_by.Sep "design"; Elt q]

    | ("initial"|"first") :: ("PR"|"patch") :: "by" :: q ->
       [Group_by.Sep "initial PR"; Elt q]
    | "based" :: "on" :: "an" :: "initial" :: "work" :: "by" :: q ->
       [Group_by.Sep "initial work"; Elt q]
    | "feedbacks" :: "from" :: q ->
       [Group_by.Sep "feedback"; Elt q]
    | "reports" :: "from" :: q ->
       [Group_by.Sep "report"; Elt q]
    | ("feature" | "original") :: "request" :: ("from"|"by") :: q ->
       [Group_by.Sep "feature request"; Elt q]
    | "bug" :: "reported" :: q
    | "regression" :: "spotted" :: q ->
       [Group_by.Sep "report"; Elt q]
    | "stealth" :: "commit" :: "by" :: q ->
      [ Sep "stealth commit"; Elt q]
    | x :: y :: ("review" | "reviewed") :: "by" :: q ->
      [Elt [x;y]; Sep "review"; Elt q]
    | x :: y :: z :: ("review" | "reviewed") :: "by" :: q ->
      [Elt [x;y;z]; Sep "review"; Elt q]
    | _ :: "bug" :: "report" :: "by" :: q ->
       [Group_by.Sep "bug report"; Elt q]

    | "split" :: "off" :: "from" :: _ :: "by" :: q ->
       [Group_by.Sep "PR editing"; Elt q]
  (* with grouping *)
    | [x;y;"with";w;z] ->
      [Elt [x;y]; Elt [w;z]]
    (* typo fixes *)
    | ["Xavier"; "Leroy"; "Guillaume"; "Munch-Maccagnoni";] ->
      [Elt  ["Xavier"; "Leroy"]; Elt ["Guillaume"; "Munch-Maccagnoni";]]
    | [] -> []
    | q ->
      if List.length q > 2 then
        [Elt (normalize_name q)]
     else
       [Elt q]

  module Dict = Map.Make(String)
  let merge l =
    l |> Dict.of_list |> Dict.bindings

  let parse authors =
    let split_punct s =
      let len = String.length s in
      let last = s.[len-1] in
      if len > 1 && (last = ',' || last = ';' || last = '.') then
        Seq.cons (String.sub s 0 (len-1))  (Seq.return ",")
      else
        Seq.return s
    in
    let words =
      String.split_on_char ' ' authors
      |> List.to_seq
      |> Seq.filter (function "" -> false | _ -> true)
      |> Seq.concat_map split_punct
      |> List.of_seq
      |> ligature
      |> Group_by.list ~debug:Pp.string ~and_then:Fun.id ~parent:Comma by_connector
      |> List.map snd
      |> List.concat_map split_section
      |> Group_by.list ~debug:Pp.(list string) ~and_then:Fun.id ~parent:"authors" Fun.id
      |> merge
    in words


  let split_line s =
    match String.rindex_opt s '(' with
    | Some start ->
      Some (String.sub s 0 start, String.sub s (start + 1) (String.length s - start -1))
    | None -> None

  let rec split after = function
    | [] -> String.concat "" after, []
    | next :: before ->
      match split_line next with
      | None -> split (next::after) before
      | Some (b, a) ->
        let authors = parse (String.concat "" (a::after)) in
        String.concat "\n" (List.rev (b::before)), authors

  let no_authors t expl  =
    Format.eprintf "Entry without authors %t @." expl;
    (String.concat "\n" (List.rev  t)), []

  let check_tail close last  =
    let right = String.sub last (close + 1) (String.length last - close - 1) in
    String.for_all (fun x -> x = '.' || empty_char x) right, right

   let split = function
     | [] -> no_authors [] (Format.dprintf "no text")
     | last :: q as rev_lines ->
       match String.rindex_opt last ')', String.rindex_opt last '(' with
       | Some close, Some open_ when open_ + 1 < close ->
         let before = String.concat "\n" (List.rev @@ String.sub last 0 open_ :: q) in
         let after = String.sub last (open_+1) (close - open_ - 1) in
         let empty_tail, tail = check_tail close last in
         if empty_tail then
           before, parse after
         else
           no_authors rev_lines (Format.dprintf "Closing ) is not last char in %S|%S|2" last tail)
       | Some close, None ->
           let empty_tail, tail = check_tail close last in
           if empty_tail then
           split [] (String.sub last 0 close :: q)
         else
           no_authors rev_lines (Format.dprintf "Closing ) is not last char in %S|%S" last tail)
       | Some _, Some _ -> no_authors rev_lines (Format.dprintf "Ill matched ( and )")
       | None, _ -> no_authors rev_lines (Format.dprintf "No closing ) in %s" last)

     let pp_sapient ppf x =
       Format.fprintf ppf "[%a]" Pp.(list ~sep:visible_space string) x

     let pp ppf (title,x) =
       Format.fprintf ppf "%s={@[%a@]}" title (Pp.list pp_sapient) x

end



let is_empty = String.for_all empty_char


let (let*) = (>>=)

let pp_raw_entry ppf x =
  Format.fprintf ppf
    "@[<hv 2>Entry@ {%a}@]"
    (Pp.list Pp.string) x


let pp_release_entry ppf = function
  | `Section x ->
    Format.fprintf ppf "Section [%s]" x
  | `Doc x ->
    Format.fprintf ppf "Doc [%s]" x
  | `Entry x ->
    pp_raw_entry ppf x


module Release = struct


  let is_separator s = String.length s > 0 && String.for_all ((=) '-') s
  let[@tail_mod_cons] rec pregroup = function
    | x :: y :: z when is_separator y ->
      Group_by.Sep x :: pregroup z
    | [] -> []
    | x :: y -> Elt x :: pregroup y

  let debug ppf s = Format.fprintf ppf "(release)%s" s
  let group_by lines = Group_by.list ~debug Fun.id @@ pregroup lines

end

module Section = struct
  let markdown_header =
    let* _h = many1 (char '#') in
    let* all in
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
    return (String.init 1 (const start) ^ rest)

  let section_header = markdown_header <|> old_style_header

  let sep_section x = match parse_string section_header ~consume:All x with
    | Ok x -> Group_by.Sep x
    | Error _ -> Elt x

  let debug ppf s = Format.fprintf ppf "(section)%s" s
  let group_by ~and_then x =
    Group_by.list ~debug ~parent:"Release documentation" sep_section x
      ~and_then
end

module Entry = struct

  type t =
    { references: int list;
      text:string;
      breaking:bool;
      sapients: Sapient.t
    }
    let to_yojson x: Yojson.Safe.t =
      `Assoc ([
        "references", `List (List.map (fun n -> `Int n) x.references);
        "text", `String x.text;
        "breaking change", `Bool x.breaking
      ] @ Sapient.to_yojson x.sapients
    )

  type any =
    | Doc of string
    | Entry of t
  let any_to_yojson = function
    | Doc x -> `Assoc ["doc", Json.string x ]
    | Entry x -> to_yojson x

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
    return { references=refs;
             sapients;
             text= rest;
             breaking=bullet;
           }

  let parse_entry lines =
    let main, authors = Sapient.split (List.rev lines) in
    match parse_string ~consume:All (parse_flat_entry authors <?> "entry")  main with
    | Ok x -> Entry x
    | Error err ->
      Format.eprintf "@[<v> error = %s@, main=%S@]@." err main;
      Doc main

  let debug ppf s = Format.fprintf ppf "(group by entry)%s" s
  let group_by x =
    Group_by.list ~parent:`Doc ~debug ~and_then:Fun.id by_entry x
    |> List.filter_map (function
        | `Doc, (_ :: _  as lines) -> Some (Doc (String.concat "" @@ List.rev lines))
        | `Doc, [] -> None
        | `Start x, lines -> Some (parse_entry (x::lines))
      )


  let pp ppf x =
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

  let pp_any ppf = function
    | Doc x -> Format.fprintf ppf "Doc [%s]" x
    | Entry x -> pp ppf x

end


let line ppf () = Format.fprintf ppf "@,"

let summary_section ppf (title,items) =
  Format.fprintf ppf "@[<v 2>section %s=%d items@,%a@]"
    title (List.length items)
    Pp.(list Entry.pp_any) items

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

type t = Entry.any list fields fields
let to_yojson: t -> Yojson.Safe.t =
  fields_to_yojson (fields_to_yojson (Json.list Entry.any_to_yojson))

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
  let json = to_yojson groups in
  Format.printf "@[<v>%a@]@."
   (Yojson.Safe.pretty_print ~std:false)  json
