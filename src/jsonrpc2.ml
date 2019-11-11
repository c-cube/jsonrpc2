
(** {1 Simple JSON-RPC2 implementation} 

    See {{: https://www.jsonrpc.org/specification} the spec} *)

module type IO = Jsonrpc2_intf.IO
module type S = Jsonrpc2_intf.S

module Make = Jsonrpc2_core.Make
