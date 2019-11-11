
module J = Yojson.Basic

module Make(IO : Jsonrpc2_intf.IO)
    : Jsonrpc2_intf.S with module IO = IO
= struct
  module IO = IO
  type json = J.t

  let code_parse_error = (-32700)
  let code_invalid_request = (-32600)
  let code_method_not_found = (-32601)
  let code_invalid_param = (-32602)
  let code_internal_error = (-32603)

  module Id = struct
    type t =
      | Int of int
      | String of string
      | Null

    let equal = (=)
    let hash = Hashtbl.hash
    let to_string = function
      | Int i -> string_of_int i
      | String s -> s
      | Null -> "null"
    let pp out id = Format.pp_print_string out (to_string id)

    let to_json = function
      | Int i -> `Int i
      | String s -> `String s
      | Null -> `Null

    module Tbl = Hashtbl.Make(struct
        type nonrec t = t
        let equal = equal
        let hash = hash
      end)
  end

  type t = {
    methods: (string, method_) Hashtbl.t;
    by_id: (json, string) result IO.Future.promise Id.Tbl.t; (* promises to fullfill *)
    ic: IO.in_channel;
    oc: IO.out_channel;
    send_lock: IO.lock; (* avoid concurrent writes *)
    mutable id_ : int;
  }

  and method_ = 
    (t -> params:json -> (json, string) result IO.Future.t)
  (** A method available through JSON-RPC *)

  let create ~ic ~oc () : t =
    { ic; oc; by_id=Id.Tbl.create 32; methods=Hashtbl.create 16;
      id_=0; send_lock=IO.create_lock(); }

  (* Get a fresh ID for this connection *)
  let fresh_id_ (self:t) : Id.t =
    let i = self.id_ in
    self.id_ <- i + 1;
    Id.Int i

  (* Build the JSON message to send for the given {b request} *)
  let mk_request_ ~id ~meth ~params =
    `Assoc [
      "method", `String meth;
      "jsonrpc", `String "2.0";
      "params", params;
      "id", Id.to_json id;
    ]

  (* Build the JSON message to send for the given {b notification} *)
  let mk_notify_ ~meth ~params =
    `Assoc [
      "method", `String meth;
      "jsonrpc", `String "2.0";
      "params", params;
    ]

  (* Build a response message *)
  let mk_response (id:Id.t) msg : json =
    `Assoc [
      "jsonrpc", `String "2.0";
      "result", msg;
      "id", Id.to_json id;
    ]

  (* Build an error message *)
  let mk_error_ ?id code msg : json =
    let l = [
      "jsonrpc", `String "2.0";
      "error", `Assoc [
        "code", `Int code;
        "message", `String msg;
      ]
    ] in
    let l = match id with
      | None -> l
      | Some id -> ("id", Id.to_json id) :: l
    in
    `Assoc l

  let declare_method (self:t) name meth : unit =
    Hashtbl.replace self.methods name meth

  (* Is the message a proper request? *)
  let valid_request_ msg =
    match msg with
    | `Assoc l ->
      (List.mem_assoc "method" l) &&
      (try List.assoc "id" l <> `Null with Not_found -> false)
    | _ -> false

  (* Is the message a proper notification? *)
  let valid_notify_ msg =
    match msg with
    | `Assoc l ->
      (List.mem_assoc "method" l) &&
      (not (List.mem_assoc "id" l))
    | _ -> false

  (* Is the message a valid batch request? *)
  let valid_batch_ msg =
    match msg with
    | `List l ->
      List.for_all (fun msg -> valid_request_ msg || valid_notify_ msg) l
    | _ -> false

  (** {2 Client side} *)

  type message = json

  (* Remove the handler of a request *)
  let remove_request (self:t) id : unit =
    Id.Tbl.remove self.by_id id

  let request (self:t) ~meth ~params : message * _ IO.Future.t =
    let id = fresh_id_ self in
    let msg = mk_request_ ~id ~meth ~params in
    (* future response, with sender associated to ID *)
    let future, promise =
      IO.Future.make ~on_cancel:(fun () -> remove_request self id)
        () in
    Id.Tbl.add self.by_id id promise;
    msg, future

  (* Notify the remote server *)
  let notify (_self:t) ~meth ~params : message =
    let msg = mk_notify_ ~meth ~params in
    msg

  let send_msg_ (self:t) (s:string) : _ IO.t =
    IO.with_lock self.send_lock
      (fun () -> IO.write_string self.oc s)

  let send (self:t) (m:message) : _ result IO.t =
    let json = J.to_string m in
    let full_s =
      Printf.sprintf "Content-Length: %d\r\n\r\n%s"
        (String.length json) json
    in
    send_msg_ self full_s

  let send_batch (self:t) (l:message list) : _ result IO.t =
    let json = J.to_string (`List l) in
    let full_s =
      Printf.sprintf "Content-Length: %d\r\n\r\n%s"
        (String.length json) json
    in
    send_msg_ self full_s

  let is_response_ msg =
    match msg with
    | `Assoc l ->
      List.mem_assoc "id" l &&
      List.mem_assoc "result" l &&
      (try List.assoc "jsonrpc" l = `String "2.0" with Not_found -> false)
    | _ -> false

  let is_error_ msg =
    match msg with
    | `Assoc l ->
      List.mem_assoc "id" l &&
      List.mem_assoc "result" l &&
      (try List.assoc "jsonrpc" l = `String "2.0" with Not_found -> false)
    | _ -> false

  let is_batch_response_ msg =
    match msg with
    | `List l ->
      List.for_all (fun msg -> is_response_ msg || is_error_ msg) l
    | _ -> false

  (*

  let handle_response client msg =
    failwith "not implemented"  (* TODO *)

  let handle_error client msg =
    failwith "not implemented"  (* TODO *)

  let handle_batch client msg =
    failwith "not implemented"  (* TODO *)

  (** Handle a JSON message from the server *)
  let handle_response client msg =
    if is_response msg then handle_response client msg
    else if is_error_ msg then handle_error client msg
    else if is_batch_response msg then handle_batch client msg
    else begin
      Lwt_log.ign_error_f "could not interpret message %s" (`to_string msg);
      Lwt.return ()
    end

  (** Thread that listens for incoming messages, and `handles` them *)
  let rec serve client =
    lwt continue =
      try_lwt
        match_lwt `read client.input with
        | None -> Lwt.return false  (* stop *)
        | Some msg ->
          lwt _ = handle_response client msg in
          Lwt.return true
      with `Error e ->
        Lwt_log.ign_error "error while reading an incoming value";
        Lwt.return false  (* stop listening *)
    in
    (* shall we wait for another response? *)
    if continue then serve client else Lwt.return ()
     *)

  (*
  (** Try to call the given method *)
  let call_method_ (self:t) ?id ~method_ ~params =
    let command =
      try Some (Hashtbl.find method_ self.methods)
      with Not_found -> None
    in
    match command, id with
    | None, _ ->
      Lwt.return (Some (mk_error_ code_method_not_found
                        ("method " ^ method_ ^ " not found")))
    | Some command, Some id ->
      let future, send = IO.Future.make() in
      (* call command (with provision for an answer) *)
      Lwt_log.ign_debug_f "handle command %s" method_;
      command ~params
        ~success:(fun json -> Lwt.wakeup send (Some (mk_response id json)))
        ~fail:(fun msg -> Lwt.wakeup send (Some (mk_error_ code_invalid_param msg)));
      future  (* future answer *)
    | Some command, None ->
      (* call command *)
      command ~params ~success:(fun json -> ()) ~fail:(fun msg -> ());
      Lwt.return None

  (* Handle a request message *)
  let handle_request_ server msg =
    match msg with
    | `Assoc l ->
      let id = List.assoc "id" l in
      (match List.assoc "method" l with
      | `String method_ -> (* good method *)
        Lwt_log.ign_debug_f "handle request %s" method_;
        let params = try List.assoc "params" l with Not_found -> `List [] in
        call_method ~id ~method_ ~params server
      | _ ->  (* bad method name *)
        Lwt_log.ign_debug_f "bad request %s" (`to_string msg);
        Lwt.return (Some (mk_error_ code_invalid_request "invalid method name")))
    | _ -> assert false

  (* Handle a notification message *)
  let handle_notify_ server msg =
    match msg with
    | `Assoc l ->
      begin match List.assoc "method" l with
      | `String method_ -> (* good method *)
        let params = try List.assoc "params" l with Not_found -> `List [] in
        call_method ~method_ ~params server
      | _ ->  (* bad method name *)
        Lwt.return (Some (mk_error_ code_invalid_request "invalid method name"))
      end
    | _ -> assert false

  (** Give a JSON request to the server; it returns either [None] (no response)
      or [Some j'] where [j'] is an answer for the client *)
  let rec handle server msg : json option IO.Future.t =
    Lwt_log.ign_debug_f "handle message %s" (`to_string msg);
    if is_valid_request msg then handle_request server msg
    else if is_valid_notify msg then handle_notify server msg
    else if is_valid_batch msg then handle_batch server msg
    else Lwt.return (Some
      (mk_error code_invalid_request "not a proper JSONRPC request"))
  and handle_batch_ server msg =
    match msg with
    | `List l ->
      (* process every request/notification in parallel *)
      let answers = List.map (handle server) l in
      lwt answers = Lwt_list.map_p (fun x -> x) answers in
      if List.for_all (fun x -> x = None) answers then Lwt.return None  (* no answer *)
      else Lwt.return  (* return answers *)
          (Some (`List (List.flatten
            (List.map (function | None -> [] | Some x -> [x]) answers))))
    | _ -> assert false

    (** Listen on a socket *)

    (** Handles incoming messages, by invoking methods (or signaling errors) and
        returning responses on the output channel *)
    let rec handle_messages server input output =
      try_lwt
        match_lwt `read input with
        | None -> Lwt.return ()  (* done *)
        | Some msg -> begin
          (* deal with incoming message in another thread *)
          Lwt.async (fun () -> match_lwt handle server msg with
          | None -> Lwt.return ()
          | Some response ->
            Lwt_log.ign_debug_f "response: %s" (`to_string response);
            `write output response >>
            Lwt_io.write_char output '\n' >>
            Lwt_io.flush output);
            (* read next message *)
            handle_messages server input output
          end
      with `Error e ->
        Lwt_log.ign_debug_f
          "cut connection because of JSON error: %s" (`pp_error e);
        Lwt_io.close input >>
        Lwt_io.close output

    let serve server sockaddr =
      (* how to handle an incoming connection *)
      let handle_conn (input, output) =
        Lwt_log.ign_debug "incoming connection";
        Lwt.async (fun () -> handle_messages server input output)
      in
      (* establish the server *)
      Lwt_io.establish_server sockaddr handle_conn

    let serve_addr server inet_addr port =
      let addr = Lwt_unix.ADDR_INET (inet_addr, port) in
      serve server addr

    let serve_str server str port =
      let addr = Unix.inet_addr_of_string str in
      serve_addr server addr port
  end

  (* TODO: add that to sublibraries
  let connect addr =
    try_lwt
      (* open connection *)
      lwt (input,output) = Lwt_io.open_connection addr in
      (* build client *)
      let client = {
        input;
        output;
        requests = Hashtbl.create 3;
        id = 0;
      } in
      Lwt.return (Some client)
    with Unix.Unix_error (e, _, _) ->
      let str = Unix.error_message e in
      Lwt_log.ign_error_f "could not connect: %s" str;
      Lwt.return None

    let connect_addr (inet_addr : Lwt_unix.inet_addr) port =
      let addr = Unix.ADDR_INET (inet_addr, port) in
      connect addr

    let connect_str s port = connect_addr (Unix.inet_addr_of_string s) port
     *)
     *)

  exception Err_ of int * string

  (* bind on IO+result *)
  let (>>=?) x f =
    let open IO.Infix in
    x >>= function
    | Error _ as err -> IO.return err
    | Ok x -> f x

  (* read a full message *)
  let read_msg (self:t) : ((string * string) list * json, exn) result IO.t =
    let open IO.Infix in
    let rec read_headers acc =
      IO.read_line self.ic >>=? function
      | "\r" -> IO.return (Ok acc) (* last separator *)
      | line ->
        begin match
            if String.get line (String.length line-1) <> '\r' then raise Not_found;
            let i = String.index line ':' in
            if i<0 || String.get line (i+1) <> ' ' then raise Not_found;
            String.sub line 0 i, String.sub line (i+1) (String.length line-i-1)
          with
          | pair -> read_headers (pair :: acc)
          | exception _ ->
            IO.return (Error (Err_ (code_parse_error, "invalid header: " ^ line)))
        end
    in
    read_headers [] >>=? fun headers ->
    begin match int_of_string (List.assoc "Content-Length" headers) with
      | n ->
        let buf = Bytes.make n '\000' in
        IO.read_exact self.ic buf n >>=? fun () ->
        begin match J.from_string (Bytes.unsafe_to_string buf) with
          | j -> IO.return @@ Ok (headers, j)
          | exception _ ->
            IO.return (Error (Err_ (code_parse_error, "cannot decode json")))
        end
      | exception _ ->
        IO.return @@ Error (Err_ (code_parse_error, "missing Content-Length' header"))
    end

  let j_id (j:json) : Id.t =
    match j with
    | `Int i -> Id.Int i
    | `String s -> Id.String s
    | `Null -> Id.Null
    | _ -> raise (Err_ (code_invalid_request, "invalid id " ^ J.to_string j))

  let j_obj 

  type incoming_msg =
    | I_request of Id.t * string * json
    | I_notify of string * json
    | I_response of Id.t * (json, string) result

  let j_message (j:json) : incoming_msg =
    let rpcver =
      try (match List.assoc "jsonrpc" 
    try

    with
    | Error


  let run (self:t) : _ IO.t =
    let open IO.Infix in
    let rec loop() : _ IO.t =
      read_msg self >>= function
      | Error End_of_file ->
        IO.return (Ok ()) (* done! *)
      | Error (Err_ (code, msg)) ->
        send self (mk_error_ code msg) >>= fun _ -> loop ()
      | Error e ->
        send self
          (mk_error_ code_internal_error ("unknown error: " ^ Printexc.to_string e))
        >>= fun _ -> loop ()
      | Ok (_hd, `List batch) ->
        begin try

          with
          | Err_ (code,msg) ->
            send self
              (mk_error_ code msg)
        end
        >>= fun () -> loop ()
      | Ok (_hd, `Obj msg) ->
        begin try
        assert false
        (* TODO;
        begin try
            let rpcver =
              try (match List.assoc "jsonrpc" 
           *)

        end
        >>= fun () -> loop ()
      | Ok (_, j) ->
        send self
          (mk_error_ code_invalid_request "expected array of toplevel object")
    in
    loop ()
end
