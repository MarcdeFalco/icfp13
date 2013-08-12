open Lambda

let stupid = false

let op1 = [ "not"; "shl1"; "shr1"; "shr4"; "shr16" ]
let op2 = [ "and"; "xor"; "or"; "plus" ]

let fop1 = List.filter (fun o -> List.mem o op1)
let fop2 = List.filter (fun o -> List.mem o op2)

exception GenError

let apply1 o e = match o with
    "not" -> Not e
    | "shl1" -> Shl1 e
    | "shr1" -> Shr1 e
    | "shr4" -> Shr4 e
    | "shr16" -> Shr16 e
    | _ -> raise GenError

let apply2 o e1 e2 = match o with
    "and" -> And (e1, e2)
    |"or" -> Or (e1, e2)
    |"xor" -> Xor (e1, e2)
    |"plus" -> Plus (e1, e2)
    | _ -> raise GenError

type gen_options = {
    partial : bool;
    ops1 : string list;
    ops2 : string list;
    if0 : bool;
    fold : bool;
    folded : bool;
    freevars : int;
    last : string ;
    heavyrandom : bool
    }

let extract_nth l n = 
    let rec aux l n a =
    match l with
    [] -> raise Not_found
    | t::q -> if n = 0
        then (t,(a@q))
        else aux q (n-1) (t::a)
    in aux l n []

let rec shuffle l =
    match l with
    [] -> []
    | _ -> let t, q = extract_nth l 
        (Random.int (List.length l)) in
        t :: shuffle q

let global_count = ref 0

let rec gen_expr size options cont =
    incr global_count;
    if !global_count > 50
    then global_count := 0;
    let doshuffle = false in (*options.heavyrandom || !global_count = 1 in*)
    let l = [ 1; 2; 3; 4 ] in
    List.iter
        (fun i -> match i with
            1 -> dos1 size options cont
            | 2 -> dos2 size options cont
            | 3 -> dos3 size options cont
            | _ -> dos4 size options cont) 
        (if doshuffle then shuffle l else l)

and dos5 size options cont =
    if options.fold && not options.folded && size >= 5 
    then do_fold size options cont;

and dos3 size options cont =
    if size >= 3
    then begin
            List.iter (fun o -> binary o size options  cont)
            options.ops2
    end

and dos4 size options cont =
    if options.if0 && size >= 4 
    then ternary_if0 size options cont

and dos1 size options cont =
    if size = 1 || (size >= 1 && options.partial)
    then begin
        cont Lambda.Zero;
        cont Lambda.One;
        match options.freevars with
          3 -> cont (Lambda.Var "a");
            cont (Lambda.Var "b");
            cont (Lambda.Var "c")
        | 2 -> cont (Lambda.Var "b");
            cont (Lambda.Var "c")
        | _ -> cont (Lambda.Var "a");
    end

and dos2 size options cont =
     if size >= 2
    then begin 
        List.iter
        (fun o -> unary o size options cont)
        options.ops1
    end

and unary o size options cont =
    if (options.last = "not" && o = "not")
    then ()
    else
        gen_expr (size-1) 
            { options with last = o }
            (fun e -> 
                if not stupid
                    && (Equiv.zero e
                        || (o <> "shl1" && Equiv.one e))
                then ()
                else cont (apply1 o e))

and binary o size options cont =
    gen_expr ((size-1)/2) 
        { options with last = o }
        (fun el -> 
            if stupid or not ( (o = "and" || o = "plus" || o = "or")
                    && Equiv.zero el)
            then
            let sel = Lambda.size_expr el in
                gen_expr (size-1-sel) { options with last = o }
                    (fun er -> 
            if stupid or (not ( (o = "and" || o = "plus" || o = "or")
                    && Equiv.zero er)
                && not (size = 3 && el < er))
                        then cont (apply2 o el er)))

and ternary_if0 size options cont =
    let noptions = { options with last = "" } in
    gen_expr (size-3) noptions
        (fun e0 -> 
            if stupid or not (Equiv.zero e0 || Equiv.one e0)
            then let se0 = Lambda.size_expr e0 in
                    gen_expr (max 1 ((size-2-se0)/2)) noptions
                        (fun e1 -> let se1 = Lambda.size_expr e1 in
                            gen_expr (size-1-se0-se1) noptions
                                (fun e2 -> cont (If (e0,e2,e1));
                                    cont (If (e0,e1,e2)))))

and do_fold size options cont =
    let noptions = { options with folded = true; last = "" } in
    gen_expr (max 1 ((size-3)/2)) noptions
        (fun e0 -> let se0 = Lambda.size_expr e0 in
            gen_expr (size-3-se0) noptions
                (fun e1 -> let se1 = Lambda.size_expr e1 in
                    gen_expr (size-2-se0-se1) 
                        { noptions with freevars = 3 }
                        (fun e2 -> 
                            cont (Fold(e1,e0,"b","c",e2));
                            cont (Fold(e0,e1,"b","c",e2)))))

exception Restart

let second_batch_found id egood econd good bad reverse tests e =
    let p = Lambda.Entry("a",e) in
    let ebad = e in

    if List.for_all (fun (a,r) -> Lambda.eval p a = r) bad
    then begin

        let ifp = 
            if reverse
            then Lambda.Entry("a",Lambda.If(econd,ebad,egood))
            else Lambda.Entry("a",Lambda.If(econd,egood,ebad))
        in
        
        match Serverapi.make_guess id ifp with
            Serverapi.Win -> raise Serverapi.Solved
            | Serverapi.Mismatch (a,r) -> 
                    Printf.printf "mismatch restarting\n";
                    flush stdout;
                    tests := (a,r) :: !tests;
                    Printf.printf "%d\n" (List.length !tests);
                    raise Restart
            | Serverapi.Error e -> ()
    end

let condition_found id size ops1 ops2 if0 egood good bad tests e =
    let p = Lambda.Entry("a",e) in
    let goodzero = List.for_all (fun (a,r) -> Lambda.eval p a = Uint64.zero) good in
    let badnzero = List.for_all (fun (a,r) -> Lambda.eval p a <> Uint64.zero) bad in
    let goodnzero = List.for_all (fun (a,r) -> Lambda.eval p a <> Uint64.zero) good in
    let badzero = List.for_all (fun (a,r) -> Lambda.eval p a = Uint64.zero) bad in


    if (goodzero && badnzero) || (goodnzero && badzero)
    then begin
        Printf.printf "%d condition found\n" size;
        flush stdout;

        gen_expr size
        {  
            partial = true; heavyrandom = false;
           ops1 = ops1; ops2 = ops2; if0 = if0; fold = false;
           folded = false; freevars = 1; last = ""
        }
        (second_batch_found id egood e good bad (goodnzero && badzero) tests)

    end
 
(*
let condition_found id egood ebad good bad tests e =
    let p = Lambda.Entry("a",e) in
    let goodzero = List.for_all (fun (a,r) -> Lambda.eval p a = Uint64.zero) good in
    let badnzero = List.for_all (fun (a,r) -> Lambda.eval p a <> Uint64.zero) bad in
    let goodnzero = List.for_all (fun (a,r) -> Lambda.eval p a <> Uint64.zero) good in
    let badzero = List.for_all (fun (a,r) -> Lambda.eval p a = Uint64.zero) bad in

        Printf.printf "final batch\n";
        flush stdout;

    if (goodzero && badnzero) || (goodnzero && badzero)
    then begin
        
        let ifp = 
            if goodzero && badnzero
            then Lambda.Entry("a",Lambda.If(e,egood,ebad))
            else Lambda.Entry("a",Lambda.If(e,ebad,egood))
        in
        
        match Serverapi.make_guess id ifp with
            Serverapi.Win -> raise Serverapi.Solved
            | Serverapi.Mismatch (a,r) -> 
                    Printf.printf "mismatch restarting\n";
                    flush stdout;
                    tests := (a,r) :: !tests;
                    Printf.printf "%d\n" (List.length !tests);
                    raise Restart
            | Serverapi.Error e -> ()
    end

let second_batch_found id size ops1 ops2 egood good bad tests e =
    let p = Lambda.Entry("a",e) in
    if List.for_all (fun (a,r) -> Lambda.eval p a = r) bad
    then begin
        Printf.printf "%d second batch\n" size;
        flush stdout;

        gen_expr (size-1)
        {  
            partial = true; heavyrandom = false;
           ops1 = ops1; ops2 = ops2; if0 = false; fold = false;
           folded = false; freevars = 1; last = ""
        }
        (condition_found id egood e good bad tests)
    end
    *)

let first_batch_found id size ops1 ops2 if0 tests verbose e =
try
    let p = Lambda.Entry("a",e) in

    if verbose then begin
        Printf.printf "%s\n" (Lambda.to_string p);
        flush stdout
    end;

    let ntests = List.length !tests in
    let good, bad = List.partition (fun (a,r) -> Lambda.eval p a = r) !tests in

    let ratio = (float_of_int (List.length good)) /. (float_of_int ntests) in

    if ratio > 0.5
    then begin
        gen_expr size
        {  
           partial = true; heavyrandom = false;
           ops1 = ops1; ops2 = ops2; if0 = if0; fold = false;
           folded = false; freevars = 1; last = ""
        }
        (condition_found id size ops1 ops2 if0 e good bad tests)
    end
with Restart -> ()

let gen id size ops tests partial verbose heavyrandom =
    let if0 = List.mem "if0" ops in
    let tfold = List.mem "tfold" ops in
    let fold = List.mem "fold" ops in
    let ops1, ops2 = fop1 ops, fop2 ops in
    gen_expr size
    {  partial = size = 5; heavyrandom = false;
       ops1 = ops1; ops2 = ops2; if0 = if0; fold = fold;
       folded = tfold; freevars = if tfold then 2 else 1;
       last = ""}

    (first_batch_found id size ops1 ops2 if0 tests verbose)
