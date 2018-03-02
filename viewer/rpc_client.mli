open Base
open Async_kernel
open Mud_common

val request
  :  ('a, 'b) Rpc.t
  -> 'a -> 'b Or_error.t Deferred.t
