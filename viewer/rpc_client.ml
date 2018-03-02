open Base
open Async_kernel
module Rpc = Mud_common.Rpc

let error_s = Core_kernel.error_s

let request rpc arg =
  let (sexp_arg,decode) = Rpc.encode rpc arg in
  let%map result =
    Async_js.Http.get
      (Sexp.to_string sexp_arg)
  in
  Or_error.bind result ~f:(fun response ->
    match Core_kernel.Sexp.of_string response with
    | exception exn ->
      error_s [%message "Couldn't parse as s-expression" (response:string) (exn:Exn.t)]
    | response ->
      match decode response with
      | exception exn ->
        error_s [%message "Decoding of response failed" (response:Sexp.t) (exn:Exn.t)]
      | response -> response)
