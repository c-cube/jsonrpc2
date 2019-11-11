
module IO
  : Jsonrpc2.IO
    with type 'a t = 'a Lwt.t
     and type 'a Future.t = 'a Lwt.t
     and type 'a Future.promise = 'a Lwt.u
     and type in_channel = Lwt_io.input_channel
     and type out_channel = Lwt_io.output_channel
= struct
  type 'a t = 'a Lwt.t
  let return = Lwt.return
  module Infix = Lwt.Infix
  include Infix

  module Future = struct
    type 'a t = 'a Lwt.t
    type 'a promise = 'a Lwt.u
    let fullfill = Lwt.wakeup
    let cancel = Lwt.cancel
    let make ?on_cancel () : _ t * _ promise =
      let fut, promise = Lwt.wait () in
      (match on_cancel with Some f -> Lwt.on_cancel fut f | None -> ());
      fut, promise
  end

  type lock = Lwt_mutex.t
  let create_lock() : lock = Lwt_mutex.create()
  let with_lock = Lwt_mutex.with_lock

  type in_channel = Lwt_io.input_channel
  type out_channel = Lwt_io.output_channel

  let read_line ic =
    Lwt_io.read_line_opt ic >|= function
    | Some x -> Ok x
    | None -> Error End_of_file

  let read_exact ic buf n : _ result t =
    Lwt.catch
      (fun () -> Lwt_io.read_into_exactly ic buf 0 n >|= fun () -> Ok ())
      (fun e -> return (Error e))

  let write_string oc (s:string) : _ result t =
    Lwt.catch
      (fun () ->
         Lwt_io.write_from_string_exactly oc s 0 (String.length s) >|= fun () -> Ok ())
      (fun e -> return (Error e))
end

include Jsonrpc2.Make(IO)
          
