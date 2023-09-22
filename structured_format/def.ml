
exception Parse_error of string
module Json = struct
  let string x = `String x
  let from_string  =
    function
    | `String x -> x
    | _ -> raise (Parse_error ("expected a string"))
 let from_int  =
    function
    | `Int x -> x
    | _ -> raise (Parse_error ("expected an int"))

 let from_bool  =
    function
    | `Bool x -> x
    | _ -> raise (Parse_error ("expected a bool"))

  let list inner x = `List (List.map inner x)
  let from_list inner = function
    | `List l -> List.map inner l
    | _ -> raise (Parse_error ("expected a list"))
end

module Dict = Map.Make(String)
let (.?()) map x = Dict.find_opt x map
let (.!()) map x = Dict.find x map


type 'a fields = (string * 'a) list


let fields to_json fs =
    List.map (fun (x,l) -> x, to_json l) fs

let fields_to_yojson to_json fs =
  `Assoc (fields to_json fs)


let fields_from_yojson_list inner l =
  List.map (fun (label, x) -> label, inner x) l


let fields_from_yojson inner : Yojson.Safe.t -> _ = function
  | `Assoc l -> fields_from_yojson_list inner l
  | _ -> raise (Parse_error ("object expected"))

type name = string list
let name_to_yojson =Json.(list string)
let name_from_yojson = Json.(from_list from_string)

type sapients = name list fields
let sapients_to_yojson (fs:sapients): (string * Yojson.Safe.t) list =
  fields (Json.list name_to_yojson) fs

let sapients_from_yojson j =
  fields_from_yojson_list (Json.from_list name_from_yojson) j

type entry =
  { references: int list;
    text:string;
    breaking:bool;
    sapients: sapients
  }

let entry_to_yojson x: Yojson.Safe.t =
  `Assoc ([
      "references", `List (List.map (fun n -> `Int n) x.references);
      "text", `String x.text;
      "breaking change", `Bool x.breaking
    ] @ sapients_to_yojson x.sapients
    )
let entry_from_yojson: Yojson.Safe.t -> entry = function
  | `Assoc l ->
    let map = Dict.of_list l in
    begin match map.!("references"), map.!("text"), map.!("breaking change") with
    | references, text, breaking_change ->
      let rest =
        List.fold_left (fun map key -> Dict.remove key map)
          map
          ["references"; "text"; "breaking change"]
      in
      { references = Json.(from_list from_int) references;
        text = Json.from_string text;
        breaking = Json.from_bool breaking_change;
        sapients = sapients_from_yojson (Dict.bindings rest)
      }
    | exception Not_found -> raise (Parse_error "missing key in changelog entry")
    end
  | _ -> raise (Parse_error "expected assoc")

type extended_entry =
  | Doc of string
  | Entry of entry

let extended_entry_to_yojson = function
  | Doc x -> `Assoc ["doc", Json.string x ]
  | Entry x -> entry_to_yojson x

let extended_entry_from_yojson: Yojson.Safe.t -> extended_entry = function
  | `Assoc ["doc", `String x] -> Doc x
  | x -> Entry (entry_from_yojson x)


type t = extended_entry list fields fields
let to_yojson: t -> Yojson.Safe.t =
  fields_to_yojson (fields_to_yojson (Json.list extended_entry_to_yojson))
let from_yojson: Yojson.Safe.t -> t =
  fields_from_yojson
    (fields_from_yojson
       (Json.from_list extended_entry_from_yojson)
    )



module Pp = struct
  let escaped_string ppf s = Fmt.pf ppf "%S" s
  let comma ppf () = Format.fprintf ppf ",@ "
  let semi ppf () = Format.fprintf ppf ";@ "
  let visible_space ppf () = Format.fprintf ppf "⍽"
  let insecable_space ppf () = Format.fprintf ppf " "

  let name ppf x =  Fmt.(list ~sep:visible_space string) ppf x
  let name' ppf x =
    let escape =
      String.map (function '"' -> ' ' | x -> x) (String.concat " " x)
    in
    Fmt.pf ppf {|"%s"|} escape

  let sapient ppf x =
    Format.fprintf ppf "[%a]" name x

  let sapients ppf (title,x) =
    Format.fprintf ppf "%s={@[%a@]}" title (Fmt.list sapient) x


  let entry ppf x =
    Format.fprintf ppf
      "@[<hv 2>Entry@ {@,\
       breaking=%B;@,\
       references=@[%a@];@,\
       text=\"%s\"@,\
       %a;@,\
       }@]"
      x.breaking
      Fmt.(list int) x.references
      x.text
      (Fmt.list sapients) x.sapients

  let any ppf = function
    | Doc x -> Format.fprintf ppf "Doc [%s]" x
    | Entry x -> entry ppf x

  end
