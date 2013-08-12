(* Des fonctions utilitaires toutes betes pour travailler avec des options *)
let iter_opt f x = match x with
    None -> ()
    | Some v -> f v

let def_opt x d = match x with
    None -> d
    | Some v -> v

let string_of_list f l = "[" ^ String.concat "," (List.map f l) ^ "]"
let string_of_slist = string_of_list (fun x -> x)
let string_of_ilist = string_of_list string_of_int

module SSet = Set.Make(
    struct
        let compare = Pervasives.compare
        type t = string
    end )

let pad_to_16 s =
    let n = String.length s in
    (String.make (16-n) '0') ^ s

let to_hex64 v =
    let s = String.uppercase (Uint64.to_string_hex v) in
    let n = String.length s in
    if n > 2 && s.[1] = 'X'
    then "0x" ^ pad_to_16 (String.sub s 2 (n-2))
    else "0x" ^ pad_to_16 s

let unique l =
    let rec aux l a =
        match l with
        [] -> a
        | t::q -> 
                aux q (if List.mem t a then a else t::a)
    in aux l []

let choice l =
    let i = Random.int (List.length l) in
    List.nth l i

let rec buildlist f n =
    match n with
    0 -> []
    | _ -> f (n-1) :: buildlist f (n-1)
