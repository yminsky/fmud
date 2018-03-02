(** A purely functional MUD engine, designed for teaching 

    The engine is designed to allow the specific MUD to be written 
    entirely using pure functions. The MUD designer decides on a
    "world" type that contains the full state of the mud, and provides
    an initial world and functions for handling events. Each handler
    returns a new world and a list of actions.
*)
open! Core

(** The actions that can be taken by the MUD engine. *)
type action =
  (** Send a message to a particular player, by nick *)
  | Send_message of { nick: string; message: string }
  (** Kick a client off of the system, closing the connection *)
  | Kill_client  of { nick: string }
[@@deriving sexp]

module Handlers : sig
  type 'world t =
    { init         : 'world
    ; description  : string
    ; handle_line  : 'world -> string -> string -> 'world * action list
    ; nick_added   : 'world -> string -> 'world * action list
    ; nick_removed : 'world -> string -> 'world * action list
    }
end 

(** Start the MUD server on the port in question. Never returns. *)
val start_mud : 'a Handlers.t -> unit
