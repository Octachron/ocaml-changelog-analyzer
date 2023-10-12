type t = string list list
type groups = (string * t) list

type sep =
  | Comma
  | And
  | Semi

let rec ligature = function
  | ("report"| "review" | "suggestions" as x) :: "and" :: q ->
    x :: "&&&" :: ligature q
  | "request" :: "and" :: q ->
    "feature request" :: "&&&" :: ligature q
  | "with" :: "help" :: "and" :: "review" :: "from":: q ->
    "help" :: "by" :: q
  | "with" :: "help" :: "and" :: "advice" :: q ->
    "help" :: q
  | "much" :: "input" :: "and" :: "thought" :: q ->
    "thought" ::  q
  | "testing" :: "and" :: "regression" :: "fix" :: q ->
    "regression" :: "fix" :: q
  | "advice" :: "and" :: "review" :: q ->
    "review" :: q
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
  | [ "skills"; "MSDN"; "impressive"; "displaying"] -> []
  | _ -> q


let normalize_name ~warn = function
  (* groups *)
  | ["the"; "Tarides" ; "multicore";  "team"] -> ["Tarides multicore team"]
  | ["the"; "OCaml"; "core"; "development"; "team"] -> ["OCaml core development team"]
  | ["others"] | ["many"; "other"; "valued"; "reviewers"] | [("Many"|"many"); "fine"; "eyes"] ->
    ["Many fine eyes"]
  | ["same"] | ["the"; "same"] | ["reviewing"; "each"; "other"; "without"; "self-loops";] ->
    ["Author group review"]
  (* typo *)
  | (["Nicolas"; "Ojeda"; "Bar"])  -> ["Nicolás"; "Ojeda"; "Bär"]
  | ["Jacques-"; "Henri"; "Jourdan"] -> ["Jacques-Henri"; "Jourdan"]
  | ["Florian"; "Angetti"] -> ["Florian"; "Angeletti"]
  | ["Francois"; "Berenger"] -> ["François"; "Berenger"]
  | [("Nathanaël"|"Naëla"); "Courant"] -> ["Nathanaëlle"; "Courant"]
  | ["Frederic"; "Bour"] -> ["Frédéric"; "Bour"]
  | ["Sebastien"; "Hinderer"] ->  ["Sébastien"; "Hinderer"]
  (* pseudo*)
  | ["octachron@"] -> ["Florian"; "Angeletti"]
  | ["Daniel"; "C."; "Bünzli"] ->  ["Daniel"; "Bünzli"]
  | ["David"; ("Alsopp" | "Allsop") ] -> ["David"; "Allsopp"]
  | ["Oliver"; "Andrieu"] -> ["Olivier"; "Andrieu"]
  | ["Gabiel"; "Scherer"] -> ["Gabriel"; "Scherer"]
  | ["San"; "Vu"; "Ngoc"] -> ["San";"Vũ";"Ngọc"]
  | ["Stephen"; "DOlan"] -> ["Stephen"; "Dolan"]
  | ["Demi"; "Obenour"] -> ["Demi"; "Marie"; "Obenour"]
  | ["Fuyong"; "Quah"] -> ["Fu"; "Yong"; "Quah"]
  | ["Mekhrubon"; "Tuarev"] -> ["Mekhrubon"; "Turaev"]
  (* Long names*)
  | (["Perry"; "E."; "Metzger"] as q)
  | (["Hezekiah"; "M."; "Carty"] as q)
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
  | "thanks" :: "to" :: "a" :: ("report" as x) :: "of" :: q
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
  | ["caml-list"; "discussion"] ->
    [Sep "review"; elt ["caml-list"]]
  | ["reported"; "on"; "caml-list"] | ["reported"; "on"; "caml-list"; "on"; _]  ->
    [Sep "report"; elt ["caml-list"]]
  | [] -> []
  | q ->
    let warn = List.length q > 2 in
      [Elt (normalize_name ~warn q)]

let known_non_authors = function
  | "no cross ref to class after dump+load"
  | "changes the generated sequences"
  | "error message in french"
  | "partially"
  | "Windows"
  | "PREFIX"
  | "reported in private"
  | "bytecode"
  | "genprintval.ml"
  | "camlp4 revised syntax"
  | "ocamlopt, x86"
  | "mingw"
  | "crash"
  | "less formatting in html code"
  | "i.e. non-contractive"
  | "duplicate of #6686"
  | "CC0 1.0"
  | "GADTs"
  | "PACKLD is more complete"
  | "with -g"
  | "interface with Unix DBM key-value stores"
  | "armhf" | "armel"
  | "MAKE"
  | "lazy"
  | "Random.self_init()"
  | "from #5318"
  | "set|clear"
  | "probably fixed"
  | "" | "_"  -> true
  | s ->
    if List.exists (fun prefix -> String.starts_with ~prefix s)
      [ "module ";
        "val ";
        "fun ";
        "i.e. ";
        "e.g. ";
        "flag ";
        "More information in";
        "Allow easy retrieval";
        "ocamlc ";
        "but does not";
        "-runtime-";
        "-with-debug";
        "(val ";

      ]
    then
      (
        Format.eprintf "Prefix non author detection: %s@." s;
        true
      )
    else
      false

module Dict = Map.Make(String)
let merge l =
  l |> Dict.of_list |> Dict.bindings


let validate_author s =
  if known_non_authors s then Error [] else
  match Angstrom.(parse_string ~consume:Prefix) Refs.parse_many s with
  | Ok x ->
    Format.eprintf "Old reference syntax detected: %s=%a@."
       s Fmt.(Dump.list int) x;
    Error x
  | Error _ -> Ok ()

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
  match validate_author authors with
  | Error _ as x -> x
  | Ok () ->
  let words =
    authors
    |> String.map (function '\n' -> ' ' | c -> c )
    |> String.split_on_char ' '
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
  in Ok words


let rec seq_parentheses stack pos s () =
  if pos < 0 then Seq.Nil
  else match s.[pos] with
    | ')' ->
      seq_parentheses (pos :: stack) (pos-1) s ()
    | '(' ->
      begin match stack with
      | last :: stack ->
        Seq.cons (pos,last) (seq_parentheses stack (pos-1) s) ()
      | [] ->
        Seq.cons (pos, String.length s) (seq_parentheses stack (pos-1) s) ()
      end
    | _ ->
      seq_parentheses stack (pos-1) s ()

let seq_parentheses s () = seq_parentheses [] (String.length s - 1) s ()


let cut_at_char s pos =
  let gap = 1 in
  String.sub s 0 pos,
  String.sub s (pos + gap) (String.length s - pos - gap )

let split_middle s start stop =
  let rest, last = cut_at_char s stop in
  let before, mid = cut_at_char rest start in
  before, mid, last

let validate_parenthese s (first,last) =
  let before, mid, last = split_middle s first last in
  match parse mid with
  | Error _ as x -> x
  | Ok authors -> Ok (before,authors,last)

type split = { main:string; refs: int list; sapients: groups}

let no_authors refs main expl  =
  Format.eprintf "Entry without authors %t: %s @." expl main;
  { refs; main; sapients= []}

let rec punct_trim_right ~blank s pos =
  if pos < 0 then ""
  else match s.[pos] with
    | '.' -> punct_trim_right ~blank:false s (pos-1)
    | ' ' | '\t' | '\n' -> punct_trim_right ~blank:true s (pos-1)
    | _ ->
      if blank then
        String.sub s 0 (1+pos)
      else
        String.sub s 0 (2+pos)

let trim_right s = punct_trim_right ~blank:true s (String.length s - 1)

let (+?) s l = if String.trim s = "" then l else s :: l

let rec find_sapients refs s seq =
  match seq () with
  | Seq.Nil -> no_authors refs s (Format.dprintf "No parenthesis group found")
  | Seq.Cons(x, seq) ->
    match validate_parenthese s x with
    | Ok (before,sapients,after) ->
      { refs; main = before ^ trim_right after; sapients }
    | Error rmore ->
      find_sapients (rmore@refs) s seq

let split lines =
  let text = String.concat "\n" lines in
  find_sapients [] text (seq_parentheses text)
