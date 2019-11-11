
(** {1 Blocking API for jsonrpc2} *)

module IO
  : Jsonrpc2.IO
    with type 'a t = 'a
     and type in_channel = in_channel
     and type out_channel = out_channel
= struct
  type 'a t = 'a
  let return x = x
  module Infix = struct
    let (>|=) x f = f x
    let (>>=) x f = f x
  end
  include Infix

  module Future = struct
    type 'a t = {
      mutable res: 'a option;
      on_cancel: (unit -> unit);
    }
    type 'a promise = 'a t
    let fullfill (p:_ promise) x = p.res <- Some x
    let cancel p = p.on_cancel ()
    let make ?(on_cancel=fun () -> ()) () =
      let r = {res=None; on_cancel;} in
      r, r
  end

  type lock = Mutex.t
  let create_lock = Mutex.create
  let with_lock lock f =
    Mutex.lock lock;
    try
      let x = f() in
      Mutex.unlock lock;
      x
    with e ->
      Mutex.unlock lock;
      raise e

  type nonrec in_channel = in_channel
  type nonrec out_channel = out_channel

  let read_line ic =
    try Ok (input_line ic)
    with e -> Error e

  let read_exact ic buf n : _ result =
    try
      let n' = input ic buf 0 n in
      if n=n' then Ok ()
      else (
        let msg = Printf.sprintf "incomplete read: %d bytes when %d expected" n' n in
        Error (Failure msg)
      )

    with e -> Error e

  let write_string oc (s:string) : _ result =
    try
      output_string oc s;
      flush oc;
      Ok ()
    with e -> Error e
end

include Jsonrpc2.Make(IO)
          
