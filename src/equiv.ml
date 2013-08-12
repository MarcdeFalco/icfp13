open Lambda

let rec zero e = match e with
    | Zero -> true
    | Not x -> one x
    | And (x,y) -> zero x || zero y
    | Or (x,y) -> zero x && zero y
    | Plus (x,y) -> zero x && zero y
    | Shl1 x -> zero x
    | Shr1 x -> zero x
    | Shr4 x -> zero x
    | Shr16 x -> zero x
    | _ -> false

and one e = match e with
    | One -> true
    | Plus(x,y) -> 
            (one x && zero y) || (one y && zero x)
    | Xor (x,y) -> 
            (zero x && one y) || (zero y && one x)
    | Or (x,y) -> one x || one y
    | And (x,y) -> one x && one y
    | Not x -> zero x
    | _ -> false
