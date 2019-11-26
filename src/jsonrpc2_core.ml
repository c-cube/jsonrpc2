
module J = Yojson.Basic

type 'a printer = Format.formatter -> 'a -> unit

type code = int
let code_parse_error : code = (-32700)
let code_invalid_request : code = (-32600)
let code_method_not_found : code = (-32601)
let code_invalid_param : code = (-32602)
let code_internal_error : code = (-32603)

let opt_map_ f = function None -> None | Some x -> Some (f x)

(** {2 The protocol part, independent from IO and Transport} *)
module Protocol : sig
  type json = J.t

  type t
  (** A jsonrpc2 connection. *)

  val create : unit -> t
  (** Create a state machine for Jsonrpc2 *)

  val clear : t -> unit
  (** Clear all internal state. *)

  module Id : sig
    type t

    val equal : t -> t -> bool
    val hash : t -> int
    val pp : t printer

    module Tbl : Hashtbl.S with type key = t
  end

  (** {3 Send requests and notifications to the other side} *)

  type message = json
  (** Message sent to the other side *)

  val error : t -> code -> string -> message

  val request : t -> meth:string -> params:json option -> message * Id.t
  (** Create a request message, for which an answer is expected. *)

  val notify : t -> meth:string -> params:json option -> message
  (** Create a notification message, ie. no response is expected. *)

  (** Actions to be done next. This includes sending messages out
      on the connection, calling a method, or finishing a local request. *)
  type action =
    | Send of message
    | Send_batch of message list
    | Start_call of (Id.t * string * json option)
    | Notify of string * json option
    | Fill_request of (Id.t * (json,int * string) result)
    | Error_without_id of int * string

  val process_msg : t -> message -> (action list, code*string) result
  (** Process incoming message *)

  val process_call_reply : t -> Id.t -> (json, string) result -> action list
  (** Send the response for the given call to the other side *)
end = struct
  type json = J.t

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

  type message = json

  type to_reply =
    | TR_single
    | TR_batch of {
        mutable missing: int;
        mutable done_: message list;
      }

  type t = {
    mutable id_ : int;
    active: unit Id.Tbl.t; (* active requests *)
    to_reply: to_reply Id.Tbl.t; (* active calls to which we shall answer *)
  }

  let create () : t =
    { id_=0; active=Id.Tbl.create 24; to_reply=Id.Tbl.create 24; }

  let clear (self:t) : unit =
    self.id_ <- 0;
    Id.Tbl.clear self.active;
    Id.Tbl.clear self.to_reply

  (* Get a fresh ID for this connection *)
  let fresh_id_ (self:t) : Id.t =
    let i = self.id_ in
    self.id_ <- i + 1;
    Id.Int i

  (* Build the JSON message to send for the given {b request} *)
  let mk_request_ ~id ~meth ~params =
    let l = [
      "method", `String meth;
      "jsonrpc", `String "2.0";
      "id", Id.to_json id;
    ] in
    let l = match params with None -> l | Some x -> ("params",x) :: l in
    `Assoc l

  (* Build the JSON message to send for the given {b notification} *)
  let mk_notify_ ~meth ~params =
    let l = [
      "method", `String meth;
      "jsonrpc", `String "2.0";
    ] in
    let l = match params with None -> l | Some x -> ("params", x) :: l in
    `Assoc l

  (* Build a response message *)
  let mk_response (id:Id.t) msg : json =
    `Assoc [
      "jsonrpc", `String "2.0";
      "result", msg;
      "id", Id.to_json id;
    ]

  (* Build an error message *)
  let error_ _self ~id code msg : json =
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

  let error self code msg = error_ ~id:None self code msg

  let request (self:t) ~meth ~params : message * Id.t =
    let id = fresh_id_ self in
    Id.Tbl.add self.active id ();
    let msg = mk_request_ ~id ~meth ~params in
    msg, id

  (* Notify the remote server *)
  let notify (_self:t) ~meth ~params : message =
    mk_notify_ ~meth ~params

  module P_ : sig
    type +'a t
    val return : 'a -> 'a t
    val fail : string -> _ t
    val is_list : bool t
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    val (>|=) : 'a t -> ('a -> 'b) -> 'b t
    val field : string -> 'a t -> 'a t
    val field_opt : string -> 'a t -> 'a option t
    val json : json t
    val one_of : string -> 'a t list -> 'a t
    val int : int t
    val string : string t
    val null : unit t
    val list : 'a t -> 'a list t
    val run : 'a t -> json -> ('a, string lazy_t) result
  end = struct
    type +'a t = json -> ('a, (string * json) list) result
    let return x _ = Ok x
    let error_ ?(ctx=[]) j e = Error ((e,j)::ctx)
    let errorf_ ?ctx j fmt = Printf.ksprintf (error_ ?ctx j) fmt
    let fail s j = Error [s,j]
    let (>>=) x f j = match x j with
      | Ok x -> f x j
      | Error e -> Error e
    let (>|=) x f j = match x j with
      | Ok x -> Ok (f x)
      | Error e -> Error e
    let json j = Ok j
    let is_list = function `List _ -> Ok true | _ -> Ok false
    let int = function
      | `Int i -> Ok i
      | `String s as j ->
        (try Ok (int_of_string s) with _ -> errorf_ j "expected int")
      | j -> error_ j "expected int"
    let string = function
      | `Int i -> Ok (string_of_int i)
      | `String s -> Ok s
      | j -> error_ j "expected string"
    let null = function `Null -> Ok () | j -> error_ j "expected null"
    let field name f : _ t = function
      | `Assoc l as j ->
        (match List.assoc name l with
         | x -> f x
         | exception Not_found -> errorf_ j "no field '%s' found in object" name)
      | j -> error_ j "expected object"
    let field_opt name f : _ t = function
      | `Assoc l ->
        (match List.assoc name l with
         | x -> (match f x with Ok x -> Ok (Some x) | Error e -> Error e)
         | exception Not_found -> Ok None)
      | j -> error_ j "expected object"
    let rec one_of what l j =
      match l with
      | [] -> errorf_ j "expected %s, none matched the given list" what
      | x :: tl ->
        match x j with
        | Ok x -> Ok x
        | Error _ -> one_of what tl j

    let list f : _ t = function
      | `List l ->
        let rec aux acc = function
          | [] -> Ok (List.rev acc)
          | x :: tl ->
            match f x with
            | Error ctx -> error_ ~ctx x "in list"
            | Ok x -> aux (x::acc) tl
        in
        aux [] l
      | j -> error_ j "expected list"

    let run (p:_ t) (j:json) : _ result =
      match p j with
      | Ok x -> Ok x
      | Error l ->
        let msg = lazy (
          String.concat "\n" @@
          List.rev_map (fun (e,j) -> e ^ " in " ^ J.to_string j) l
        ) in
        Error  msg
  end

  type incoming = 
    | I_error of Id.t * code * string
    | I_request of Id.t * string * json option
    | I_notify of string * json option
    | I_response of Id.t * json

  type incoming_full =
    | IF_one of incoming
    | IF_batch of incoming list

  let parse_id : Id.t P_.t =
    let open P_ in
    one_of "id" [
      (int >|= fun x -> Id.Int x);
      (string >|= fun x -> Id.String x);
      (null >|= fun () -> Id.Null);
    ]

  let parse_error : (int*string) P_.t =
    let open P_ in
    field "code" int >>= fun code ->
    field "message" string >|= fun msg -> (code,msg)

  let parse_incoming : incoming P_.t =
    let open P_ in
    field "jsonrpc" string >>= function
    | "2.0" ->
      one_of "incoming message" [
        (field "error" parse_error >>= fun (c,e) ->
         field "id" parse_id >|= fun id ->
         I_error (id,c,e));
        (field "result" json >>= fun j ->
         field "id" parse_id >|= fun id ->
         I_response(id,j));
        (field "method" string >>= fun name ->
         field_opt "params" json >>= fun params ->
         field_opt "id" parse_id >|= function
         | Some id -> I_request (id, name, params)
         | None -> I_notify (name, params))
      ]
    | _ -> fail "expected field 'jsonrpc' to contain '2.0'"

  let parse_incoming_full : incoming_full P_.t =
    let open P_ in
    is_list >>= function
    | true ->
      list parse_incoming >>= fun l ->
      if l=[] then fail "batch must be non-empty"
      else return (IF_batch l)
    | false -> parse_incoming >|= fun x -> IF_one x

  (** Actions to be done next. This includes sending messages out
      on the connection, calling a method, or finishing a local request. *)
  type action =
    | Send of message
    | Send_batch of message list
    | Start_call of (Id.t * string * json option)
    | Notify of string * json option
    | Fill_request of (Id.t * (json,int * string) result)
    | Error_without_id of int * string

  let acts_of_inc self ~tr (i:incoming) : action =
    match i with
    | I_notify (s,m) -> Notify (s,m)
    | I_request (id,s,m) ->
      if Id.Tbl.mem self.to_reply id then (
        Send (error_ self ~id:None code_internal_error "ID already used in a request")
      ) else (
        Id.Tbl.add self.to_reply id tr;
        (* update count of messages in this batch to answer to *)
        (match tr with TR_single -> () | TR_batch r -> r.missing <- r.missing + 1);
        Start_call (id,s,m)
      )
    | I_response (id,m) ->
      if Id.Tbl.mem self.active id then (
        Id.Tbl.remove self.active id;
        Fill_request (id,Ok m)
      ) else (
        Send (error_ self ~id:None code_internal_error "no request with given ID")
      )
    | I_error (Id.Null,code,msg) ->
      Error_without_id (code,msg)
    | I_error (id,code,msg) ->
      if Id.Tbl.mem self.active id then (
        Id.Tbl.remove self.active id;
        Fill_request (id, Error (code, msg))
      ) else (
        Send (error_ self ~id:None code_internal_error "no request with given ID")
      )

  let process_msg (self:t) (m:message) : (action list, _) result =
    match P_.run parse_incoming_full m with
    | Error (lazy e) -> Error (code_invalid_request, e)
    | Ok (IF_one m) -> Ok [acts_of_inc ~tr:TR_single self m]
    | Ok (IF_batch l) ->
      let tr = TR_batch {missing=0; done_=[]} in
      Ok (List.map (acts_of_inc ~tr self) l)

  let process_call_reply self id res : _ list =
    let msg_of_res = function
      | Ok res ->
        mk_response id res
      | Error e ->
        error_ self ~id:(Some id) code_invalid_param e
    in
    match Id.Tbl.find self.to_reply id with
    | exception Not_found ->
      invalid_arg (Printf.sprintf "already replied to id %s" (Id.to_string id))
    | TR_single ->
      Id.Tbl.remove self.to_reply id;
      [Send (msg_of_res res)]
    | TR_batch r ->
      Id.Tbl.remove self.to_reply id;
      r.done_ <- msg_of_res res :: r.done_;
      r.missing <- r.missing - 1;
      if r.missing = 0 then (
        [Send_batch r.done_]
      ) else []
end

module Make(IO : Jsonrpc2_intf.IO)
    : Jsonrpc2_intf.S with module IO = IO
= struct
  module IO = IO
  module Id = Protocol.Id
  type json = J.t

  type t = {
    proto: Protocol.t;
    methods: (string, method_) Hashtbl.t;
    reponse_promises:
      (json, code*string) result IO.Future.promise Id.Tbl.t; (* promises to fullfill *)
    ic: IO.in_channel;
    oc: IO.out_channel;
    send_lock: IO.lock; (* avoid concurrent writes *)
  }

  and method_ = 
    (t -> params:json option -> return:((json, string) result -> unit) -> unit)
  (** A method available through JSON-RPC *)

  let create ~ic ~oc () : t =
    { ic; oc; reponse_promises=Id.Tbl.create 32; methods=Hashtbl.create 16;
      send_lock=IO.create_lock(); proto=Protocol.create(); }

  let declare_method (self:t) name meth : unit =
    Hashtbl.replace self.methods name meth

  let declare_method_with self ~decode_arg ~encode_res name f : unit =
    declare_method self name
      (fun self ~params ~return ->
         match params with
         | None ->
           (* pass [return] as a continuation to {!f} *)
           f self ~params:None ~return:(fun y -> return (Ok (encode_res y)))
         | Some p ->
           match decode_arg p with
           | Error e -> return (Error e)
           | Ok x ->
             (* pass [return] as a continuation to {!f} *)
             f self ~params:(Some x) ~return:(fun y -> return (Ok (encode_res y))))

  let declare_blocking_method_with self ~decode_arg ~encode_res name f : unit =
    declare_method self name
      (fun _self ~params ~return ->
         match params with
         | None -> return (Ok (encode_res (f None)))
         | Some p ->
           match decode_arg p with
           | Error e -> return (Error e)
           | Ok x -> return (Ok (encode_res (f (Some x)))))

  (** {2 Client side} *)

  exception Jsonrpc2_error of int * string
  (** Code + message *)

  type message = json

  let request (self:t) ~meth ~params : message * _ IO.Future.t =
    let msg, id = Protocol.request self.proto ~meth ~params in
    (* future response, with sender associated to ID *)
    let future, promise =
      IO.Future.make
        ~on_cancel:(fun () -> Id.Tbl.remove self.reponse_promises id)
        ()
    in
    Id.Tbl.add self.reponse_promises id promise;
    msg, future

  (* Notify the remote server *)
  let notify (self:t) ~meth ~params : message =
    Protocol.notify self.proto ~meth ~params

  let send_msg_ (self:t) (s:string) : _ IO.t =
    IO.with_lock self.send_lock
      (fun () -> IO.write_string self.oc s)

  (* send a single message *)
  let send (self:t) (m:message) : _ result IO.t =
    let json = J.to_string m in
    let full_s =
      Printf.sprintf "Content-Length: %d\r\n\r\n%s"
        (String.length json) json
    in
    send_msg_ self full_s

  let send_request self ~meth ~params : _ IO.t =
    let open IO.Infix in
    let msg, res = request self ~meth ~params in
    send self msg >>= function
    | Error e -> IO.return (Error e)
    | Ok () ->
      IO.Future.wait res >|= fun r ->
      match r with
      | Ok x -> Ok x
      | Error (code,e) -> Error (Jsonrpc2_error (code,e))

  let send_notify self ~meth ~params : _ IO.t =
    let msg = notify self ~meth ~params in
    send self msg

  let send_request_with ~encode_params ~decode_res self ~meth ~params : _ IO.t =
    let open IO.Infix in
    send_request self ~meth ~params:(opt_map_ encode_params params)
    >>= function
    | Error _ as e -> IO.return e
    | Ok x ->
      let r = match decode_res x with
        | Ok x -> Ok x
        | Error s -> Error (Jsonrpc2_error (code_invalid_request, s))
      in
      IO.return r

  let send_notify_with ~encode_params self ~meth ~params : _ IO.t =
    send_notify self ~meth ~params:(opt_map_ encode_params params)

  (* send a batch message *)
  let send_batch (self:t) (l:message list) : _ result IO.t =
    let json = J.to_string (`List l) in
    let full_s =
      Printf.sprintf "Content-Length: %d\r\n\r\n%s"
        (String.length json) json
    in
    send_msg_ self full_s

  (* bind on IO+result *)
  let (>>=?) x f =
    let open IO.Infix in
    x >>= function
    | Error _ as err -> IO.return err
    | Ok x -> f x

  (* read a full message *)
  let read_msg (self:t) : ((string * string) list * json, exn) result IO.t =
    let rec read_headers acc =
      IO.read_line self.ic >>=? function
      | "\r" -> IO.return (Ok acc) (* last separator *)
      | line ->
        begin match
            if String.get line (String.length line-1) <> '\r' then raise Not_found;
            let i = String.index line ':' in
            if i<0 || String.get line (i+1) <> ' ' then raise Not_found;
            String.sub line 0 i, String.trim (String.sub line (i+1) (String.length line-i-2))
          with
          | pair -> read_headers (pair :: acc)
          | exception _ ->
            IO.return (Error (Jsonrpc2_error (code_parse_error, "invalid header: " ^ line)))
        end
    in
    read_headers [] >>=? fun headers ->
    let ok = match List.assoc "Content-Type" headers with
      | "utf8" | "utf-8" -> true
      | _ -> false
      | exception Not_found -> true
    in
    if ok then (
      match int_of_string (List.assoc "Content-Length" headers) with
      | n ->
        let buf = Bytes.make n '\000' in
        IO.read_exact self.ic buf n >>=? fun () ->
        begin match J.from_string (Bytes.unsafe_to_string buf) with
          | j -> IO.return @@ Ok (headers, j)
          | exception _ ->
            IO.return (Error (Jsonrpc2_error (code_parse_error, "cannot decode json")))
        end
      | exception _ ->
        IO.return @@ Error (Jsonrpc2_error(code_parse_error, "missing Content-Length' header"))
    ) else (
      IO.return @@ Error (Jsonrpc2_error(code_invalid_request, "content-type must be 'utf-8'"))
    )

  (* execute actions demanded by the protocole *)
  let rec exec_actions (self:t) l : _ result IO.t =
    let open IO.Infix in
    match l with
    | [] -> IO.return (Ok ())
    | a :: tl ->
      begin match a with
        | Protocol.Send msg -> send self msg
        | Protocol.Send_batch l -> send_batch self l
        | Protocol.Start_call (id, name, params) ->
          begin match Hashtbl.find self.methods name with
            | m ->
              let fut, promise = IO.Future.make () in
              m self ~params
                ~return:(fun r -> IO.Future.fullfill promise r);
              (* now wait for the method's response, and reply to protocol *)
              IO.Future.wait fut >>= fun res ->
              let acts' = Protocol.process_call_reply self.proto id res in
              exec_actions self acts'
            | exception Not_found ->
              send self
                (Protocol.error self.proto code_method_not_found "method not found")
          end
        | Protocol.Notify (name,params) ->
          begin match Hashtbl.find self.methods name with
            | m ->
              (* execute notification, do not process response *)
              m self ~params ~return:(fun _ -> ());
              IO.return (Ok ())
            | exception Not_found ->
              send self
                (Protocol.error self.proto code_method_not_found "method not found")
          end
        | Protocol.Fill_request (id, res) ->
          begin match Id.Tbl.find self.reponse_promises id with
            | promise ->
              IO.Future.fullfill promise res;
              IO.return (Ok ())
            | exception Not_found ->
              send self @@ Protocol.error self.proto code_internal_error "no such request"
          end
        | Protocol.Error_without_id (code,msg) ->
          IO.return (Error (Jsonrpc2_error (code,msg)))
      end
      >>=? fun () ->
      exec_actions self tl

  let run (self:t) : _ IO.t =
    let open IO.Infix in
    let rec loop() : _ IO.t =
      read_msg self >>= function
      | Error End_of_file ->
        IO.return (Ok ()) (* done! *)
      | Error (Jsonrpc2_error (code, msg)) ->
        send self (Protocol.error self.proto code msg) >>=? fun () -> loop ()
      | Error _ as err -> IO.return err (* exit now *)
      | Ok (_hd, msg) ->
        begin match Protocol.process_msg self.proto msg with
          | Ok actions ->
            exec_actions self actions
          | Error (code,msg) ->
            send self (Protocol.error self.proto code msg)
        end
        >>=? fun () -> loop ()
    in
    loop ()
end
