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
