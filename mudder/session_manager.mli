open! Core

type t

val create
  :  Host_and_port.t
  -> on_new_session:
       (nick:string -> password:string -> Session_id.t -> string)
  -> on_input:(string -> string)
  -> 