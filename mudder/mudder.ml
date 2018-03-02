open Core
open Async
module P = Mud_common.Protocol
module Rpc = Mud_common.Rpc
module Hashtbl = Base.Hashtbl
module Map = Base.Map
module Nick = P.Nick
module Password = P.Password
open Cohttp_async

type action =
  | Send_message of { nick: string; message: string }
  | Kill_client  of { nick: string }
[@@deriving sexp]

module Nick_event = struct
  type t = | Message of string
           | Kill
end

module Nick_state = struct
  type t =
    { nick : Nick.t
    ; password : Password.t
    ; pending : Nick_event.t Fqueue.t
    ; nonce : Int32.t option
    }
end

module Handlers = struct
  type 'world t =
    { init : 'world
    ; description  : string
    ; handle_line  : 'world -> string -> string -> 'world * action list
    ; nick_added   : 'world -> string -> 'world * action list
    ; nick_removed : 'world -> string -> 'world * action list
    }
end

module State = struct
  type 'world t =
    { world     : 'world
    ; nicks     : Nick_state.t Map.M(Nick).t
    ; handlers  : 'world Handlers.t
    } [@@deriving fields]
end

let run_action (s:_ State.t) action =
  print_s [%message "run_action" (action : action)];
  match action with
  | Send_message { nick; message } ->
    let nick = Nick.of_string nick in
    let nicks =
      Map.change s.nicks nick ~f:(function
        | None -> None
        | Some nick_state ->
          let pending =
            Fqueue.enqueue nick_state.pending (Message message)
          in
          Some { nick_state with pending })
    in
    { s with nicks }
  | Kill_client { nick } ->
    let nicks =
      Map.change s.nicks (Nick.of_string nick) ~f:(function
        | None -> None
        | Some nick_state ->
          let pending = Fqueue.enqueue nick_state.pending Kill in
          Some { nick_state with pending })
    in
    { s with nicks }

let impl = Rpc.Implementation.create

let check_nick (state_ref : _ State.t ref) =
  impl P.check_nick (fun {nick} ->
    let state = !state_ref in
    if Map.mem state.nicks nick then Known_nick else New_nick)

let login (state_ref : _ State.t ref) =
  impl P.login (fun { nick; password } ->
    let state = !state_ref in
    match Map.find state.nicks nick with
    | None -> Unknown_nick
    | Some ns ->
      if Password.(<>) ns.password password then Wrong_password
      else
        let nonce = Random.int32 Int32.max_value in
        let ns = { ns with nonce = Some nonce } in
        let nicks = Map.set state.nicks ~key:nick ~data:ns in
        state_ref := { state with nicks };
        Login_accepted { nonce }
  )

let input (state_ref : _ State.t ref) =
  impl P.input (fun { nonce; input } ->
    let state = !state_ref in
    match Map.find state.nicks
  )

let main handlers ~port =
  let%bind _server =
    Server.create
      ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port port)
      (fun ~body:_ _ _ ->
         Server.respond_string "<html><body><b>WooT!</b></body></html>")
  in
  Deferred.never ()

let start_mud handlers =
  let open Command.Let_syntax in
  Command.async
    ~summary:"Start a hello world Async server"
    [%map_open
      let port =
        flag "-p" (optional_with_default 8080 int)
          ~doc:"int Source port to listen on"
      in
      fun () -> main handlers port ]
  |> Command.run
