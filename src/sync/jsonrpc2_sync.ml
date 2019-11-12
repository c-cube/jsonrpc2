
(** {1 Blocking API for jsonrpc2} *)

module IO_sync
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

  let with_lock lock f =
    Mutex.lock lock;
    try
      let x = f() in
      Mutex.unlock lock;
      x
    with e ->
      Mutex.unlock lock;
      raise e

  module Future = struct
    type 'a state =
      | Waiting
      | Cancelled
      | Done of 'a

    type 'a t = {
      mutable st: 'a state;
      mutex: Mutex.t;
      cond: Condition.t;
      on_cancel: (unit -> unit);
    }
    type 'a wait = 'a
    type 'a promise = 'a t

    let make ?(on_cancel=fun () -> ()) () =
      let r = {
        st=Waiting;
        on_cancel;
        mutex=Mutex.create();
        cond=Condition.create();
      } in
      r, r

    let cancel p =
      let call_f = with_lock p.mutex
        (fun () ->
           if p.st = Waiting then (
             p.st <- Cancelled;
             true
           ) else false) in

      if call_f then p.on_cancel ()

    let fullfill (p:_ promise) x =
      with_lock p.mutex
        (fun () ->
           if p.st <> Waiting then failwith "promise already fullfilled";
           p.st <- Done x;
           Condition.broadcast p.cond)

    let rec wait r =
      let x =
        with_lock r.mutex
          (fun () ->
             if r.st = Waiting then Condition.wait r.cond r.mutex;
             r.st)
      in
      match x with
      | Done y -> y
      | Cancelled -> failwith "cancelled"
      | Waiting -> wait r
  end

  type lock = Mutex.t
  let create_lock = Mutex.create

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

include Jsonrpc2.Make(IO_sync)
          
