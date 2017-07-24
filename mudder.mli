open! Core

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

(** Start the MUD server on the port in question. Never returns *)
val start_mud : 'a handlers -> unit
