module J = Decoders_yojson.Basic
module R = Jsonrpc2_sync

let debug = ref (try ignore (Sys.getenv "DEBUG"); true with _ -> false)

let get_err = function
  | Ok x -> x
  | Error e ->
    if !debug then Printf.eprintf "got error: %s\n%!" (Printexc.to_string e);
    raise e

module Server = struct
  let sum (c:R.t) (l:int list) : (int,_) result =
    R.send_request_with
      ~encode_params:(fun l -> `List (List.map (fun x->`Int x) l))
      ~decode_res:(function (`Int i) -> Ok i | _ -> Error "expected int")
      c ~meth:"sum" ~params:(Some l)

  let set_offset (c:R.t) (x: int option) : (unit,_) result =
    R.send_notify_with
      ~encode_params:(fun i -> `List [`Int i])
      c ~meth:"set-offset" ~params:x

  let run () =
    if !debug then Printf.eprintf "server: start subprocess\n%!";
    let ic, oc =
      Unix.open_process
        (Printf.sprintf "%s -sub %s" Sys.executable_name (if !debug then "-debug" else "")) in
    let c = R.create ~ic ~oc () in
    let thread = Thread.create R.run c in
    if !debug then Printf.eprintf "server: initialize test\n%!";
    (* first sum *)
    let sum1 = sum c [1;2] |> get_err in
    if !debug then Printf.eprintf "server: sum1: %d\n%!" sum1;
    assert (sum1 = 3);

    (* set offset *)
    set_offset c (Some 42) |> get_err;

    (* second sum *)
    let sum2 = sum c [1;2] |> get_err in
    if !debug then Printf.eprintf "server: sum2: %d\n%!" sum2;
    assert (sum2 = 42 +3);

    (* set offset to 0 *)
    set_offset c None |> get_err;

    (* third sum *)
    let sum3 = sum c [1;2;3] |> get_err in
    if !debug then Printf.eprintf "server: sum2: %d\n%!" sum3;
    assert (sum3 = 6);

    ignore (Unix.close_process (ic,oc) : Unix.process_status);
    Thread.join thread
end

module Sub = struct
  let decode_ dec j = match J.Decode.decode_value dec j with
    | Ok x -> Ok x
    | Error e -> Error (J.Decode.string_of_error e)

  type state = {
    c: R.t;
    mutable cur_offset: int;
  }

  let declare_meths (self:state) : unit =
    R.declare_blocking_method_with self.c
      ~decode_arg:(decode_ J.Decode.(list int))
      ~encode_res:J.Encode.(fun x -> int x)
      "sum"
      (function
        | None ->
          if !debug then Printf.eprintf "client: sum None (offset %d)\n%!" self.cur_offset;
          self.cur_offset 
        | Some l ->
          if !debug then Printf.eprintf "client: sum [%s] (offset %d)\n%!"
            (String.concat";" @@ List.map string_of_int l) self.cur_offset; 
          List.fold_left (+) self.cur_offset l);
    R.declare_blocking_method_with self.c
      ~decode_arg:(function
          | (`List [`Int i]) -> Ok i
          | _ -> Error "expected an int")
      ~encode_res:(fun b -> `Bool b)
      "set-offset"
      (function
        | None ->
          if !debug then Printf.eprintf "client: set-offset None\n%!"; 
          self.cur_offset <- 0; true
        | Some i ->
          if !debug then Printf.eprintf "client: set-offset %d\n%!" i; 
          self.cur_offset <- i; true);
    ()

  let run () =
    let c = R.create ~ic:stdin ~oc:stdout () in
    let st = {c; cur_offset=0} in
    if !debug then Printf.eprintf "client: declare meths\n%!";
    declare_meths st;
    if !debug then Printf.eprintf "client: run\n%!";
    get_err @@ R.run c
end

let () =
  let sub = ref false in
  Arg.parse [
    "-debug", Arg.Set debug, " enable debug";
    "-sub", Arg.Set sub, " start in sub mode";
  ] (fun _ -> raise (Arg.Bad "no arg expected")) "test-server";
  if !sub
  then Sub.run()
  else Server.run()

