
(** {1 Main Interface} *)

module J = Yojson.Basic

type 'a printer = Format.formatter -> 'a -> unit

module type FUTURE = sig
  type 'a t
  (** Future value *)

  type 'a promise
  (** How to fill an existing future with a value *)

  val fullfill : 'a promise -> 'a -> unit
  (** Fill a promise with a value. Behavior is not specified if this
      is called several times *)

  val cancel : _ t -> unit
  (** Cancel a future. Does nothing if the promise is filled already
      or if there's no meaningful notion of cancellation. *)

  val make :
    ?on_cancel:(unit -> unit) ->
    unit ->
    'a t * 'a promise
    (** Make a future with the accompanying promise to fullfill it.
        @param on_cancel if provided, call this function upon cancellation. *)

  type 'a wait

  val wait : 'a t -> 'a wait
  (** Wait for the future to be filled *)
end

module type IO = sig
  type 'a t

  val return : 'a -> 'a t

  module Infix : sig
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  end
  include module type of Infix

  module Future : FUTURE with type 'a wait = 'a t

  type lock

  val create_lock : unit -> lock
  val with_lock : lock -> (unit -> 'a t) -> 'a t

  type in_channel
  type out_channel

  val read_line : in_channel -> (string, exn) result t
  (** Read a full line, including the trailing '\n' *)

  val read_exact : in_channel -> bytes -> int -> (unit, exn) result t
  (** [read_exact ic buf n] reads exactly [n] bytes into [buf], starting
      at index 0. *)

  val write_string : out_channel -> string -> (unit, exn) result t
  (** write to the channel. *)
end

module type S = sig
  module IO : IO

  type json = J.t

  type t
  (** A jsonrpc2 connection. *)

  val create :
    ic:IO.in_channel ->
    oc:IO.out_channel ->
    unit ->
    t
  (** Create a connection from the pair of channels *)

  (** {3 Declare methods available from the other side} *)

  type method_ = 
    (t -> params:json option -> return:((json, string) result -> unit) -> unit)

  val declare_method : t -> string -> method_ -> unit
  (** Add a method that can be called from the other side.
      The method, when called, {b must} at some point call its [return] paramter
      with a result. *)

  (** {3 Send requests and notifications to the other side} *)

  exception Jsonrpc2_error of int * string
  (** Code + message *)

  type message
  (** Message sent to the other side *)

  val request :
    t -> meth:string -> params:json ->
    message * (json, string) result IO.t
  (** Create a request message, for which an answer is expected. *)

  val notify : t -> meth:string -> params:json -> message
  (** Create a notification message, ie. no response is expected. *)

  val send : t -> message -> (unit, exn) result IO.t
  (** Send the message. *)

  val send_batch : t -> message list -> (unit, exn) result IO.t
  (** Send a batch of messages. *)

  val run : t -> (unit, exn) result IO.t
  (** Listen for incoming messages and responses *)
end
