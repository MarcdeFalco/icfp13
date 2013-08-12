let auth_token = "0397vZKrYz2AKtKVHH9mI7VRtFXYGcqSEOK43cAW"

let build_url path = 
    "http://icfpc2013.cloudapp.net/" ^ path ^ "?auth=" ^ auth_token ^ "vpsH1H"

let writer accum data =
  Buffer.add_string accum data;
  String.length data

let showContent content =
  Printf.printf "%s" (Buffer.contents content);
  flush stdout

let showInfo connection =
  Printf.printf "Time: %f\nURL: %s\n"
    (connection#get_totaltime)
    (connection#get_effectiveurl)

exception AccessError
exception Retry

let rec access path arg =
    (*Printexc.record_backtrace true;*)
    let url = build_url path in
    let result = Buffer.create 16384 in
    let errorBuffer = ref "" in
    let json_arg = match arg with
        None -> ""
        | Some v -> Yojson.Basic.to_string v in
    try
        let connection = new Curl.handle in
        connection#set_errorbuffer errorBuffer;
        connection#set_writefunction (writer result);
        connection#set_url url;
        connection#set_postfields json_arg;
        connection#perform;
        connection#cleanup;
        let br = Buffer.contents result in
        if String.sub br 0 3 = "Too"
        then raise Retry;
        (*Printexc.record_backtrace false;*)
        (0, Yojson.Basic.from_string br)
    with
    | Retry -> access path arg
    | Curl.CurlException (reason, code, str) ->
        Printf.fprintf stderr "Error: %s\n" !errorBuffer;
        raise AccessError
    | Failure s ->
        Printf.fprintf stderr "Caught exception: %s\n" s;
        raise AccessError
