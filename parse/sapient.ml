type sep =
  | Comma
  | And
  | Semi

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


let normalize_name ~warn = function
  (* groups *)
  | ["the"; "Tarides" ; "multicore";  "team"] -> ["Tarides multicore team"]
  | ["the"; "OCaml"; "core"; "development"; "team"] -> ["OCaml core development team"]
  | ["many"; "other"; "valued"; "reviewers"] | ["Many"; "fine"; "eyes"] -> ["many"]
  | ["reviewing"; "each"; "other"; "without"; "self-loops";] ->
    ["Author group review"]
  (* typo *)
  | (["Nicolas"; "Ojeda"; "Bar"])  -> ["Nicolás"; "Ojeda"; "Bär"]
  | ["Jacques-"; "Henri"; "Jourdan"] -> ["Jacques-Henri"; "Jourdan"]
  | ["Florian"; "Angetti"] -> ["Florian"; "Angeletti"]
  | ["Francois"; "Berenger"] -> ["François"; "Berenger"]
  | ["Nathanaël"; "Courant"] -> ["Nathanaëlle"; "Courant"]
  (* pseudo*)
  | ["octachron@"] -> ["Florian"; "Angeletti"]
  | ["Daniel"; "C."; "Bünzli"] ->  ["Daniel"; "Bünzli"]
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
    if warn then Format.eprintf "Complex name or error:%s@." (String.concat " " x);
    x


let elt ?(warn=false)  x= Group_by.Elt (normalize_name ~warn x)

let split_section x = match strip_postfix (List.filter ((<>) "") x) with
  | "additional" :: "testing" :: "by" :: q ->
    [Group_by.Sep "tests"; elt q]
  (* alternative name for authors *)
  | ("code"|"patch") :: "by" :: q
  | "final" :: "fix" :: "by" :: q ->
    [Group_by.Sep "authors"; elt q]
  (* factorizations *)
  | "review" :: "&&&" :: "final" :: "fix" :: "by" :: name ->
    [Sep "review"; Elt name; Sep "authors"; Elt name]
  | "report" :: "&&&" :: "fix" :: "by" :: name ->
    [Sep "report"; Elt name; Sep "authors"; Elt name]
  | x :: "&&&" :: y :: "by" :: name ->
    [Sep x; Elt name; Sep y; Elt name]
  | x :: "&&&" :: y :: z :: "by" :: name ->
    [Sep x; Elt name; Sep (String.concat " " [y;z]); elt name]
  | x :: "&&&" :: y :: z :: w :: "by" :: name ->
    [Sep x; Elt name; Sep (String.concat " " [y;z;w]); elt name]
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
      [Group_by.Sep x; elt q]
  | "designed" :: "with" :: q -> [Group_by.Sep "design"; elt q]

  | ("initial"|"first") :: ("PR"|"patch") :: "by" :: q ->
    [Group_by.Sep "initial PR"; elt q]
  | "based" :: "on" :: "an" :: "initial" :: "work" :: "by" :: q ->
    [Group_by.Sep "initial work"; elt q]
  | "feedbacks" :: "from" :: q ->
    [Group_by.Sep "feedback"; elt q]
  | "reports" :: "from" :: q ->
    [Group_by.Sep "report"; elt q]
  | ("feature" | "original") :: "request" :: ("from"|"by") :: q ->
    [Group_by.Sep "feature request"; elt q]
  | "bug" :: "reported" :: q
  | "regression" :: "spotted" :: q ->
    [Group_by.Sep "report"; elt q]
  | "stealth" :: "commit" :: "by" :: q ->
    [ Sep "stealth commit"; elt q]
  | x :: y :: ("review" | "reviewed") :: "by" :: q ->
    [Elt [x;y]; Sep "review"; elt q]
  | x :: y :: z :: ("review" | "reviewed") :: "by" :: q ->
    [Elt [x;y;z]; Sep "review"; elt q]
  | _ :: "bug" :: "report" :: "by" :: q ->
    [Group_by.Sep "bug report"; elt q]

  | "split" :: "off" :: "from" :: _ :: "by" :: q ->
    [Group_by.Sep "PR editing"; elt q]
  (* with grouping *)
  | [x;y;"with";w;z] ->
    [Elt [x;y]; Elt [w;z]]
  (* typo fixes *)
  | ["Xavier"; "Leroy"; "Guillaume"; "Munch-Maccagnoni";] ->
    [Elt  ["Xavier"; "Leroy"]; Elt ["Guillaume"; "Munch-Maccagnoni";]]
  | [] -> []
  | q ->
    let warn = List.length q > 2 in
      [Elt (normalize_name ~warn q)]

module Dict = Map.Make(String)
let merge l =
  l |> Dict.of_list |> Dict.bindings

let parse authors =
  let split_punct s =
    let len = String.length s in
    let last = s.[len-1] in
    if (len > 1 && (last = ',' || last = ';')) ||
       (len > 2 && last = '.') then
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
    |> Group_by.list ~debug:Fmt.string ~and_then:Fun.id ~parent:Comma by_connector
    |> List.map snd
    |> List.concat_map split_section
    |> Group_by.list ~debug:Fmt.(list string) ~and_then:Fun.id ~parent:"authors" Fun.id
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
  String.for_all (fun x -> x = '.' || Helpers.empty_char x) right, right

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
