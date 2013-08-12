let load_tests () =
    let ts = Scanf.Scanning.open_in_bin "tests.txt" in

    let read_int () = 
        Scanf.bscanf ts " %d " (fun x -> x) in
    let read_string () = 
        Scanf.bscanf ts " %s " (fun x -> x) in

    let id = read_string () in
    let size = read_int () in
    let noperators = read_int () in
    
    let operators = ref [] in
    for i = 1 to noperators do
        operators := (read_string ()) :: !operators
    done;

    let ntests = read_int () in
    let tl = ref [] in

    for i = 1 to ntests do
        let a = Uint64.of_string (read_string ()) in
        let r = Uint64.of_string (read_string ()) in
        tl := (a,r) :: !tl
    done;

    Scanf.Scanning.close_in ts;

    (id, size, !operators, List.rev !tl)

let make_tests id size operators =
    let gl = Discriminate.generate_bonus_tests_1 () in
    let rl = Serverapi.make_eval id gl in
    let tl = List.combine gl rl in

    let gl = Discriminate.generate_bonus_tests_1 () in
    let rl = Serverapi.make_eval id gl in
    let tl2 = List.combine gl rl in

    let tests = tl @ tl2 (*@ tl3 @ tl4 *)in

    let tests_file  = open_out "tests.txt" in

    Printf.fprintf tests_file "%s %d %d\n" id size
        (List.length operators);
    List.iter (fun s -> Printf.fprintf tests_file "%s\n" s)
        operators;
    Printf.fprintf tests_file "%d\n" (List.length tests);
    List.iter (fun (a,r) ->
        Printf.fprintf tests_file "%s %s\n"
            (Uint64.to_string_hex a)
            (Uint64.to_string_hex r)) tests;
    close_out tests_file;
    tests

let solve8b id size operators =
    try
        let tests = ref (make_tests id size operators) in
        (*
        Printf.printf "Going for 8.\n";
        flush stdout;
        Bonus.gen id 8 operators tests true false false;
        Printf.printf "NO SOLUTION FOUND.\n"
        *) ()
    with Serverapi.Solved -> Printf.fprintf stderr "WIN\n"

let followb n heavyrandom =
    try
        let id, size, operators, tests = load_tests ()  in
        Printf.printf "Going for %d (%s).\n" n
            (if heavyrandom then "HEAVY" else "LOW");
        flush stdout;
        Bonus.gen id n operators (ref tests) false false heavyrandom;
        Printf.printf "NO SOLUTION FOUND.\n"
    with Serverapi.Solved -> Printf.fprintf stderr "WIN\n"


let solve8 id size operators =
    try
        let tests = ref (make_tests id size operators) in
        Printf.printf "Going for 8.\n";
        flush stdout;
        Generator.gen id 8 operators tests true false false;
        Printf.printf "NO SOLUTION FOUND.\n"
    with Serverapi.Solved -> Printf.fprintf stderr "WIN\n"

let follow n heavyrandom =
    try
        let id, size, operators, tests = load_tests ()  in
        Printf.printf "Going for %d (%s).\n" n
            (if heavyrandom then "HEAVY" else "LOW");
        flush stdout;
        Generator.gen id n operators (ref tests) false false heavyrandom;
        Printf.printf "NO SOLUTION FOUND.\n"
    with Serverapi.Solved -> Printf.fprintf stderr "WIN\n"

exception Stop
let _ = 
let uniqueid = (Unix.getpid ()) in
Random.init uniqueid;
Printf.printf "UID:%d\n" uniqueid;
   (* Printexc.record_backtrace false;*)

try begin
    let action =
        if Array.length Sys.argv < 2
        then "myproblems"
        else Sys.argv.(1) in
    match action with
      "myproblems" -> let problems = Serverapi.make_myproblems () in
        let count_solved = ref 0 in
        let count_lost = ref 0 in
        let problems_txt = open_out "problems.txt" in
        List.iter
            (fun (id,size,operators,solved) ->
                if solved = Serverapi.Success
                then incr count_solved;
                if solved = Serverapi.Failure
                then incr count_lost;
                Printf.fprintf problems_txt
                    "%s %d %s %s\n"
                    id size (Util.string_of_slist operators)
                    (match solved with
                    Serverapi.Success -> "S"
                    | Serverapi.Failure -> "B"
                    | _ -> "U")
            ) problems;
        close_out problems_txt;
        Printf.fprintf stdout "%d/%d resolus %d points perdus\n"
            !count_solved (List.length problems) !count_lost

    | "trainb" ->
            let challenge, id, size, operators =
                Serverapi.make_train_bounded 137 [] in 
            solve8b id size operators;
  
    | "train" ->
        let size = int_of_string Sys.argv.(2) in
            let challenge, id, size, operators =
            Serverapi.make_train_bounded size 
                (if Array.length Sys.argv = 3
                then [] else [Sys.argv.(3)]) in
            solve8 id size operators;

    | "followb" -> followb
            (int_of_string Sys.argv.(2))
            (if Array.length Sys.argv > 3 then true else false)

    | "solveb" ->
        let id = Sys.argv.(2) in
        let problems =  Serverapi.extract_myproblems
            (Yojson.Basic.from_file "myproblems.json") in
        let rec find l = match l with
            [] -> raise Not_found
            | (pid,size,operators,_) :: q ->
                    if pid = id then (size, operators)
                                else find q
        in 
        let size, operators = find problems in
        Printf.fprintf stdout "Solving %s (size=%d operators=%s)\n"
            id size (Util.string_of_slist operators);
        solve8b id size operators

    | "follow" -> follow 
            (int_of_string Sys.argv.(2))
            (if Array.length Sys.argv > 3 then true else false)
    | "solve" ->
        let id = Sys.argv.(2) in
        let problems =  Serverapi.extract_myproblems
            (Yojson.Basic.from_file "myproblems.json") in
        let rec find l = match l with
            [] -> raise Not_found
            | (pid,size,operators,_) :: q ->
                    if pid = id then (size, operators)
                                else find q
        in 
        let size, operators = find problems in
        Printf.fprintf stdout "Solving %s (size=%d operators=%s)\n"
            id size (Util.string_of_slist operators);
        solve8 id size operators
    | "gen" -> Generator.gen "" 
        (int_of_string Sys.argv.(2))
        ["if0";"shl1";"and";"not"] (ref []) true true true;
    | _ -> Printf.fprintf stderr "Error: Unknown action."
end with Yojson.Json_error s -> 
        Printexc.print_backtrace stderr;
        Printf.fprintf stderr "JSON %s\n" s
    | Failure f -> 
        Printexc.print_backtrace stderr;
        Printf.fprintf stderr "%s\n" f
    | e -> 
        Printf.printf "%s\n" (Printexc.to_string e);
        Printexc.print_backtrace stderr
(*
 * Du code rattrapge que je garde au cas ou
        Yojson.Json_error s -> 
            Printexc.print_backtrace stderr;
            Printf.fprintf stderr "JSON %s\n" s
        | Failure f -> 
            Printexc.print_backtrace stderr;
            Printf.fprintf stderr "%s\n" f
        | _ -> Printexc.print_backtrace stderr
*)
