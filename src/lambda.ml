type expr = Zero | One
    | Var of string
    | If of expr * expr * expr
    | Fold of expr * expr * string * string * expr
    | Not of expr | Shl1 of expr
    | Shr1 of expr | Shr4 of expr | Shr16 of expr
    | And of expr * expr | Or of expr * expr
    | Xor of expr * expr | Plus of expr * expr
type program = Entry of string * expr

let op1 = [ "not"; "shl1"; "shr1"; "shr4"; "shr16" ]
let op2 = [ "and"; "or"; "xor"; "plus" ]

let rec size_expr e = match e with
    Zero -> 1
    | One -> 1
    | Var _ -> 1
    | If (e0,e1,e2) -> 1 + size_expr e0 + size_expr e1 + size_expr e2
    | Fold (e0,e1,_,_,e2) -> 2 + size_expr e0 + size_expr e1 + size_expr e2
    | Not e -> 1 + size_expr e
    | Shl1 e -> 1 + size_expr e
    | Shr1 e -> 1 + size_expr e
    | Shr4 e -> 1 + size_expr e
    | Shr16 e -> 1 + size_expr e
    | And (e0,e1) -> 1 + size_expr e0 + size_expr e1
    | Or (e0,e1) -> 1 + size_expr e0 + size_expr e1
    | Xor (e0,e1) -> 1 + size_expr e0 + size_expr e1
    | Plus (e0,e1) -> 1 + size_expr e0 + size_expr e1

let rec size_program (Entry (_,e)) = 1 + size_expr e

let rec ops_expr e = match e with
    Zero -> Util.SSet.empty
    | One -> Util.SSet.empty
    | Var _ -> Util.SSet.empty
    | If (e0,e1,e2) -> Util.SSet.union (Util.SSet.singleton "if0")
        (Util.SSet.union (ops_expr e0) 
        (Util.SSet.union (ops_expr e1) (ops_expr e2)))
    | Fold (e0,e1,_,_,e2) -> Util.SSet.union (Util.SSet.singleton "fold")
        (Util.SSet.union (ops_expr e0) 
        (Util.SSet.union (ops_expr e1) (ops_expr e2)))
    | Not e -> Util.SSet.union (Util.SSet.singleton "not") (ops_expr e)
    | Shl1 e -> Util.SSet.union (Util.SSet.singleton "shl1") (ops_expr e)
    | Shr1 e -> Util.SSet.union (Util.SSet.singleton "shr1") (ops_expr e)
    | Shr4 e -> Util.SSet.union (Util.SSet.singleton "shr4") (ops_expr e)
    | Shr16 e -> Util.SSet.union (Util.SSet.singleton "shr16") (ops_expr e)
    | And (e0,e1) -> Util.SSet.union (Util.SSet.singleton "and") 
        (Util.SSet.union (ops_expr e0) (ops_expr e1))
    | Or (e0,e1) -> Util.SSet.union (Util.SSet.singleton "or") 
        (Util.SSet.union (ops_expr e0) (ops_expr e1))
    | Xor (e0,e1) -> Util.SSet.union (Util.SSet.singleton "xor") 
        (Util.SSet.union (ops_expr e0) (ops_expr e1))
    | Plus (e0,e1) -> Util.SSet.union (Util.SSet.singleton "plus") 
        (Util.SSet.union (ops_expr e0) (ops_expr e1))

let ops_program p = match p with
    Entry (x, Fold (Var xp, Zero, _, _, e)) when x=xp
        -> Util.SSet.union (Util.SSet.singleton "tfold") (ops_expr e)
    | Entry(_, e) -> ops_expr e

let get_ops p = 
    let s = ops_program p in
    Util.SSet.elements s

module Env = Map.Make(
    struct
        let compare = Pervasives.compare
        type t = string
    end )

let rec evale d e = match e with
    Zero -> Uint64.zero
    | One -> Uint64.one
    | Var x -> 
        let a,b,c = d in
        if x = "a" then a else if x = "b" then b else c
    | If (e0,e1,e2) -> let c = evale d e0 in
        if c = Uint64.zero
        then evale d e1
        else evale d e2
    | Fold (e0,e1,x,y,e2) ->
        let v = evale d e0 in
        let s = evale d e1 in
        let a1 = Uint64.logand  (Uint64.of_int 255) (Uint64.shift_right v 56) in
        let a2 = Uint64.logand  (Uint64.of_int 255) (Uint64.shift_right v 48) in
        let a3 = Uint64.logand  (Uint64.of_int 255) (Uint64.shift_right v 40) in
        let a4 = Uint64.logand  (Uint64.of_int 255) (Uint64.shift_right v 32) in
        let a5 = Uint64.logand  (Uint64.of_int 255) (Uint64.shift_right v 24) in
        let a6 = Uint64.logand  (Uint64.of_int 255) (Uint64.shift_right v 16) in
        let a7 = Uint64.logand  (Uint64.of_int 255) (Uint64.shift_right v 8) in
        let a8 = Uint64.logand  (Uint64.of_int 255) v in
        List.fold_left
            (fun c b ->  let a, _, _ = d in evale (a,b,c) e2) s
            [a8;a7;a6;a5;a4;a3;a2;a1]
    | Not e -> Uint64.lognot (evale d e)
    | Shl1 e -> Uint64.shift_left (evale d e) 1
    | Shr1 e -> Uint64.shift_right (evale d e) 1
    | Shr4 e -> Uint64.shift_right (evale d e) 4
    | Shr16 e -> Uint64.shift_right (evale d e) 16
    | And (e0,e1) -> Uint64.logand (evale d e0) (evale d e1)
    | Or (e0,e1) -> Uint64.logor (evale d e0) (evale d e1)
    | Xor (e0,e1) -> Uint64.logxor (evale d e0) (evale d e1)
    | Plus (e0,e1) -> Uint64.add (evale d e0) (evale d e1)

let eval (Entry (x,e)) v = evale (v,Uint64.zero,Uint64.zero) e

let rec expr_to_string e = match e with
    Zero -> "0"
    | One -> "1"
    | Var x -> x
    | If (e0,e1,e2) -> "(if0 " ^ expr_to_string e0
        ^ " " ^ expr_to_string e1
        ^ " " ^ expr_to_string e2 ^ ")"
    | Fold (e0,e1,x,y,e2) -> "(fold " ^ expr_to_string e0
        ^ " " ^ expr_to_string e1
        ^ " (lambda (" ^ x ^ " " ^ y ^ ") "
        ^ expr_to_string e2 ^ "))"
    | Not e -> "(not " ^ expr_to_string e ^ ")"
    | Shl1 e -> "(shl1 " ^ expr_to_string e ^ ")"
    | Shr1 e -> "(shr1 " ^ expr_to_string e ^ ")"
    | Shr4 e -> "(shr4 " ^ expr_to_string e ^ ")"
    | Shr16 e -> "(shr16 " ^ expr_to_string e ^ ")"
    | And (e0,e1) -> "(and " ^ expr_to_string e0 ^ " "
        ^ expr_to_string e1 ^ ")"
    | Or (e0,e1) -> "(or " ^ expr_to_string e0 ^ " "
        ^ expr_to_string e1 ^ ")"
    | Xor (e0,e1) -> "(xor " ^ expr_to_string e0 ^ " "
        ^ expr_to_string e1 ^ ")"
    | Plus (e0,e1) -> "(plus " ^ expr_to_string e0 ^ " "
        ^ expr_to_string e1 ^ ")"

let to_string (Entry (x,e)) = "(lambda (" ^ x ^ ") "
    ^ expr_to_string e ^ ")"

