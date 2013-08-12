let rec apply_test_case a r l =
    let f p =
        let v = Lambda.eval p a in
        v = r 
    in List.filter f l

let generate_tests () =
    let rec gen n = if n = 0 then []
        else (Uint64.of_int64 (Random.int64 Int64.max_int))
            :: gen (n-1) in
    gen 240

let generate_bonus_tests_1 () =
    let rec gen n = if n = 0 then []
        else (Uint64.of_int64 (Random.int64 Int64.max_int))
            :: gen (n-1) in
    gen 256

let generate_bonus_tests_2 () =
    Util.buildlist (fun i -> 
        Uint64.logor 
            (Uint64.shift_left (Uint64.of_int (i/16)) 32) 
            (Uint64.of_int (i mod 16)))
        256

let generate_bonus_tests_3 () =
    Util.buildlist (fun i -> 
        Uint64.logor 
            (Uint64.of_string "0xffffffff00000000")
            (Uint64.of_int i))
        256

let generate_bonus_tests_4 () =
    let rec gen n = if n = 0 then []
        else (Uint64.of_int64 (Random.int64 Int64.max_int))
            :: gen (n-1) in
    gen 256
