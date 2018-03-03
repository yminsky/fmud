open Core
open Async
module Hashtbl  = Base.Hashtbl
module Map      = Base.Map

module P        = Mud_common.Protocol
module Rpc      = Mud_common.Rpc
module Nick     = P.Nick
module Password = P.Password
module Nonce    = P.Nonce
open Cohttp_async

type action =
  | Send_message of { nick: string; message: string }
  | Kick         of { nick: string }
[@@deriving sexp]

module Player = struct
  type t =
    { nick : Nick.t
    ; password : Password.t
    ; pending : action Fqueue.t
    ; nonce : Nonce.t option
    ; last_heartbeat : Time.t
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
    { world          : 'world
    ; players        : Player.t Map.M(Nick).t
    ; handlers       : 'world Handlers.t
    ; rstate         : Random.State.t
    } [@@deriving fields]
end

(** An RPC that can update the state *)
let impl rpc f =
  (fun (state_ref : _ State.t ref) ->
     Rpc.Implementation.create rpc (fun input ->
       let state = !state_ref in
       let (state,response) = f state input in
       state_ref := state;
       response))

(** An RPC that leaves the state unchanged. *)
let impl' rpc f =
  impl rpc (fun state input -> (state, f state input))

let check_nick () =
  impl' P.check_nick (fun state {nick} ->
    if Map.mem state.players nick then Known_nick else New_nick)

let login () =
  impl P.login (fun state { nick; password } ->
    match Map.find state.players nick with
    | None -> (state, Unknown_nick)
    | Some player ->
      if Password.(<>) player.password password then (state, Wrong_password)
      else
        let nonce = Nonce.random state.rstate in
        let player = { player with nonce = Some nonce } in
        let players = Map.set state.players ~key:nick ~data:player in
        let state = { state with players } in
        (state, Login_accepted { nonce })
  )

let register () =
  impl P.register (fun state { nick; password } ->
    match Map.find state.players nick with
    | Some _ ->
      (state, Nick_taken)
    | None ->
      let nonce = Nonce.random state.rstate in
      let players = 
        Map.set state.players ~key:nick
          ~data:{ nick; password; pending = Fqueue.empty
                ; nonce = Some nonce; last_heartbeat = Time.now () }
      in
      ({ state with players },
       Registered { nonce }))

(** apply_action adds the action to the appropriate player's
    pending action queue. If no player is found, the action is
    ignored. *)
let apply_action (state: _ State.t) (action:action) =
  let nick =
    Nick.of_string (
      match action with
      | Send_message {nick;_} -> nick
      | Kick {nick} -> nick)
  in
  let players =
    Map.change state.players nick ~f:(function
      | None -> None
      | Some player ->
        Some { player with pending = Fqueue.enqueue player.pending action })
  in
  { state with players }

let unknown_nonce nonce =
  raise_s [%message "Unknown nonce" (nonce:Nonce.t)]

let find_nonce_exn (state:_ State.t) nonce =
  let player =
    Map.to_sequence state.players
    |> Sequence.find ~f:(fun (_,player) ->
      Option.equal Nonce.equal player.nonce (Some nonce))
  in
  match player with
  | Some x -> x
  | None -> unknown_nonce nonce

let input () =
  impl P.input (fun state { nonce; input } ->
    let (_,player) = find_nonce_exn state nonce in
    let (world,actions) =
      state.handlers.handle_line state.world (Nick.to_string player.nick) input
    in
    let state = List.fold actions ~init:{ state with world } ~f:apply_action in
    (state, ())
  )

let heartbeat () =
  impl P.heartbeat (fun state { nonce } ->
    let state =
      let (nick,_) = find_nonce_exn state nonce in
      { state with
        players =
          Map.change state.players nick ~f:(function
            | None -> None
            | Some player ->
              Some { player with last_heartbeat = Time.now () })}
    in
    (state,()))

let poll () =
  impl P.poll (fun state { nonce } ->
    let (nick,player) = find_nonce_exn state nonce in
    let pending = player.pending in
    let player = { player with pending = Fqueue.empty } in
    let players = Map.set state.players ~key:nick ~data:player in
    let state = { state with players } in
    let result =
      Fqueue.to_list pending
      |> List.fold_until ~init:[] ~f:(fun messages action ->
        match action with
        | Kick _                           -> Stop messages
        | Send_message { nick=_; message } -> Continue (message :: messages))
    in
    let (responses, kicked) =
      match result with
      | Finished messages      -> (messages, false)
      | Stopped_early messages -> (messages, true)
    in
    (state, { responses; kicked })
  )

let rpc_decoder state_ref =
  [ input ()
  ; login ()
  ; check_nick ()
  ; heartbeat ()
  ; poll ()
  ; register ()
  ]
  |> List.map ~f:(fun f -> f state_ref)
  |> Rpc.Decoder.create

let main (handlers : _ Handlers.t) ~port =
  let state_ref =
    ref { State.
          world = handlers.init
        ; players = Map.empty (module Nick)
        ; handlers = handlers
        ; rstate = Random.State.make_self_init ()
        }
  in
  let decoder = rpc_decoder state_ref in
  let%bind (_:_ Server.t) =
    Server.create
      ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port port)
      (fun ~body _addr _request ->
         let%bind body = Body.to_string body in
         let response =
           let open Or_error.Let_syntax in
           let%bind request = Or_error.try_with (fun () -> Sexp.of_string body) in
           Rpc.Decoder.eval decoder request
         in
         let response = [%sexp (response : Sexp.t Or_error.t)] in
         print_s [%message
           "Request" (body : string) (response : Sexp.t)];
         Server.respond_string
           ~headers:(Cohttp.Header.of_list ["Access-Control-Allow-Origin", "*"])
           (Sexp.to_string response))
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
      fun () -> main handlers ~port ]
  |> Command.run
