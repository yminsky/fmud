open! Core
open! Async

type action =
  | Send_message of { nick: string; message: string }
  | Kill_client  of { nick: string }
[@@deriving sexp]

type 'world handlers =
  { init         : 'world
  ; description  : string
  ; handle_line  : 'world -> string -> string -> 'world * action list
  ; nick_added   : 'world -> string -> 'world * action list
  ; nick_removed : 'world -> string -> 'world * action list
  }
[@@deriving sexp]

val start_mud : 'a handlers -> port:int -> unit
