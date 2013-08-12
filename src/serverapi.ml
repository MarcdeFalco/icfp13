
let list_of_strings_to_json l =
    let rec aux l =
        match l with
        [] -> []
        | t::q -> `String t :: aux q in
    `List (aux l)

exception DecodeError

let rec get_from_assoc conv l s =
    match l with
    [] -> raise DecodeError
    | (t,v)::q -> if t = s
            then conv v
            else get_from_assoc conv q s

let conv_string v = match v with
    `String s -> s
    | _ -> raise DecodeError
let assoc_string = get_from_assoc conv_string

let conv_bool v = match v with
    `Bool n -> n
    | _ -> raise DecodeError
let assoc_bool = get_from_assoc conv_bool

let conv_int v = match v with
    `Int n -> n
    | _ -> raise DecodeError
let assoc_int = get_from_assoc conv_int

let conv_list_of_strings v =
    let rec aux l = match l with
    [] -> []
    | `String s::q -> s::aux q
    | _ -> raise DecodeError in
    match v with
    `List l -> aux l
    | _ -> raise DecodeError
let assoc_list_of_strings = get_from_assoc conv_list_of_strings

let decode_training_problem res = match res with
    `Assoc l ->
        ( 
            assoc_string l "challenge",
            assoc_string l "id",
            assoc_int l "size",
            assoc_list_of_strings l "operators"
        )
    | _ -> raise DecodeError

let make_train_bounded size ops =
    let arg = `Assoc [
        ("size", `Int size);
        ("operators",
            list_of_strings_to_json ops)
        ] in
    let e, res = Netcode.access "train" (Some arg) in
    Printf.fprintf stdout "[TRAIN] arg:%s code:%d res:%s\n"
        (Yojson.Basic.pretty_to_string arg) e
        (Yojson.Basic.pretty_to_string res);
    flush stdout;
    decode_training_problem res

let decode_eval_response res = match res with
    `Assoc l -> 
        List.map Uint64.of_string
            (assoc_list_of_strings l "outputs")
    | _ -> raise DecodeError

let make_eval id al =
    let rec convert al = match al with
        [] -> []
        | a::q -> (`String (Util.to_hex64 a))::convert q
    in
    let arg = `Assoc [
        ("id", `String id);
        ("arguments", `List (convert al))
    ] in
    let e, res = Netcode.access "eval" (Some arg) in
    Printf.fprintf stdout "[EVAL] arg: code:%d res:\n"
        (*(Yojson.Basic.pretty_to_string arg)*) e
        (*(Yojson.Basic.pretty_to_string res)*);
    flush stdout;
    decode_eval_response res


type guess_response =
    | Win
    | Mismatch of Uint64.t * Uint64.t
    | Error of string
exception Solved

let decode_guess_response res = match res with
    `Assoc l ->
        let status = assoc_string l "status" in
        Printf.printf "%s\n" status;
        begin
        match status with
        "win" -> Win
        | "mismatch" -> 
            let [a;c;_] = List.map Uint64.of_string
                (assoc_list_of_strings l "values") in
            Mismatch (a,c)
        | "error" ->  Error (assoc_string l "message")
        end
    | _ -> raise DecodeError

let make_guess id p =
    let arg = `Assoc [
        ("id", `String id);
        ("program", `String (Lambda.to_string p))
    ] in
    let e, res = Netcode.access "guess" (Some arg) in
    Printf.fprintf stdout "[Guess] arg:%s code:%d res:%s\n"
        (Yojson.Basic.pretty_to_string arg) e
        (Yojson.Basic.pretty_to_string res);
    flush stdout;
    decode_guess_response res

type problem_status = Success | Failure | Undone

let rec extract_myproblems l =
    let rec aux l = 
    match l with
    [] -> []
    | (`Assoc t) :: q ->
            let status = 
                    try
                        if assoc_bool t "solved"
                        then Success
                        else Failure
                    with DecodeError -> Undone in
            ( assoc_string t "id",
              assoc_int t "size",
              assoc_list_of_strings t "operators",
              status ) :: aux q
    | _ -> raise DecodeError
    in match l with
    `List l -> aux l
    | _ -> raise DecodeError

let make_myproblems () =
    let e, res = Netcode.access "myproblems" None in
    Printf.fprintf (open_out "myproblems.json")
        "%s" (Yojson.Basic.pretty_to_string res);
    Printf.fprintf stdout 
        "[MYPROBLEMS] code:%d\n" e;
    extract_myproblems res
