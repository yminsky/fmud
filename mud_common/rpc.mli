(** A simple, s-expression based RPC system, intended for use over HTTP requests. *)

open! Base

type ('a,'b) t
type ('a,'b) rpc = ('a,'b) t

val create 
  :  string 
  -> (module Sexpable.S with type t = 'a) 
  -> (module Sexpable.S with type t = 'b)
  -> ('a,'b) t

(** Given an RPC and a query, returns the encoded query, and a function for 
    decoding the response. *)
val encode : ('a,'b) t -> 'a -> Sexp.t * (Sexp.t -> 'b)

module Implementation : sig
  type t
  val create : ('a,'b) rpc -> ('a -> 'b) -> t
end

module Decoder : sig
  type t

  val create : Implementation.t list -> t
  val eval : t -> Sexp.t -> Sexp.t Or_error.t
end