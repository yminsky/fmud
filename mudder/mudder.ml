open Core
open Async
module Hashtbl  = Base.Hashtbl
module Map      = Base.Map
module Set      = Base.Set

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
  type activity = 
    | Inactive
    | Active of { pending: action Fqueue.t;  nonce: Nonce.t }
  [@@deriving sexp]

  type t =
    { nick : Nick.t
    ; password : Password.t
    ; activity : activity
    ; last_poll : Time_ns.t sexp_opaque
    }
    [@@deriving sexp]

    let active t =
      match t.activity with
      | Inactive -> false
      | Active _ -> true
end

module Handlers = struct
  type 'world t =
    { init : 'world
    ; description   : string
    ; handle_line   : 'world -> string -> string -> 'world * action list
    ; nick_added    : 'world -> string -> 'world * action list
    ; nick_removed  : 'world -> string -> 'world * action list
    ; sexp_of_world : 'world -> Sexp.t
    }
end

module State = struct
  type 'world t =
    { world          : 'world
    ; players        : Player.t Map.M(Nick).t
    ; handlers       : 'world Handlers.t sexp_opaque
    ; rstate         : Random.State.t sexp_opaque
    } [@@deriving sexp, fields]

  let set_player t (player:Player.t) =
    { t with 
      players = Map.set t.players ~key:player.nick ~data:player }
    
  let active_nicks t =
    Map.filter t.players ~f:(fun player ->
      match player.activity with Inactive -> false | Active _ -> true)
    |> Map.keys
    |> Set.of_list (module Nick)
end

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
        match player.activity with
        | Inactive -> Some player
        | Active { pending; nonce } ->
          let pending = Fqueue.enqueue pending action in
          Some { player with activity = Active { nonce ; pending }})
  in
  { state with players }

let apply_actions state actions = 
  List.iter actions ~f:(fun action ->
    print_s [%message "Action" (action : action)]);
  List.fold actions ~init:state ~f:apply_action

(** Update the state according to the provided function [f], handling
   resulting addition and removal of nicks *)
let update_state (type world) ~context state_ref f =
  let state : world State.t = !state_ref in
  let (state,rval,added,removed) =
    let (state',rval) = f state in
    let old_nicks = State.active_nicks state in
    let new_nicks = State.active_nicks state' in
    let added   = Set.diff new_nicks old_nicks in
    let removed = Set.diff old_nicks new_nicks in
    (state', rval, added, removed)
  in
  let apply_changes (state:_ State.t) nicks update_world =
    Set.fold ~init:state nicks ~f:(fun state nick ->
      let (world,actions) = update_world state.world (Nick.to_string nick) in
      apply_actions { state with world } actions)
  in
  let state = apply_changes state removed state.handlers.nick_removed in
  let state = apply_changes state added   state.handlers.nick_added   in
  begin
    let none_if_empty s = if Set.is_empty s then None else Some s in
    let added = none_if_empty added in
    let removed = none_if_empty removed in
    print_s [%message
      "Update state" 
        (context:Sexp.t)
        (added : Set.M(Nick).t sexp_option)
        (removed : Set.M(Nick).t sexp_option)];
  end;
  state_ref := state;
  rval

(** An RPC that can update the state *)
let impl rpc f =
  (fun (state_ref : _ State.t ref) ->
     Rpc.Implementation.create rpc (fun input ->
       update_state
         ~context:[%message "RPC" ~_:(Rpc.name rpc : string)]
         state_ref (fun state -> f state input)))

let login () =
  impl P.login (fun state { nick; password } ->
    match Map.find state.players nick with
    | None -> (state, Unknown_nick)
    | Some player ->
      if Password.(<>) player.password password then (state, Wrong_password)
      else 
        (* blindly overwrite any existing session. *)
        let nonce = Nonce.random state.rstate in
        let state = 
          State.set_player state
            { player with activity = Active { nonce; pending = Fqueue.empty }}
        in
        (state, Login_accepted { nonce })
  )

let register () =
  impl P.register (fun state { nick; password } ->
    match Map.find state.players nick with
    | Some _ ->
      (state, Nick_taken)
    | None ->
      let nonce = Nonce.random state.rstate in
      let state = 
        State.set_player state
          { nick; password
          ; activity = Active { pending = Fqueue.empty; nonce }
          ; last_poll = Time_ns.now () }
      in
      (state,Registered { nonce }))

let unknown_nonce nonce =
  raise_s [%message "Unknown nonce" (nonce:Nonce.t)]

let find_nonce_exn (state:_ State.t) nonce =
  let player =
    Map.to_sequence state.players
    |> Sequence.find ~f:(fun (_,player) ->
      match player.activity with
      | Inactive -> false
      | Active {nonce = nonce'; _ } -> Nonce.equal nonce' nonce)
  in
  match player with
  | Some x -> x
  | None -> unknown_nonce nonce

let input () =
  impl P.input (fun state { nonce; input } ->
    let (_,player) = find_nonce_exn state nonce in
    let (world, actions) =
      state.handlers.handle_line state.world (Nick.to_string player.nick) input
    in
    let state = apply_actions { state with world } actions in
    (state, ())
  )

let poll () =
  impl P.poll (fun state { nonce } ->
    let (_,player) = find_nonce_exn state nonce in
    let pending =
      match player.activity with
      | Inactive -> Fqueue.empty
      | Active active -> active.pending
    in
    let result =
      Fqueue.to_list pending
      |> List.fold_until ~init:[] ~f:(fun messages action ->
        match action with
        | Kick { nick } -> 
          assert (Nick.(=) (Nick.of_string nick) player.nick);
          Stop messages
        | Send_message { nick; message } -> 
          assert (Nick.(=) (Nick.of_string nick) player.nick);
          Continue (message :: messages))
    in
    let (responses, kicked) =
      match result with
      | Finished messages      -> (messages, false)
      | Stopped_early messages -> (messages, true)
    in
    let responses = List.rev responses in
    let player = 
      { player with activity = Active { pending = Fqueue.empty; nonce }
                  ; last_poll = Time_ns.now () }
    in
    let state = State.set_player state player in
    let state =
      if not kicked then state
      else State.set_player state { player with activity = Inactive }
    in
    (state, { responses; kicked })
  )

let rpc_decoder state_ref =
  [ input ()
  ; login ()
  ; poll ()
  ; register ()
  ]
  |> List.map ~f:(fun f -> f state_ref)
  |> Rpc.Decoder.create

let expire_old_players (state:_ State.t) =
  let now = Time_ns.now () in
  let to_expire =
    Map.data state.players
    |> List.filter_map ~f:(fun player ->
      let should_expire =
        Player.active player
        && Time_ns.Span.(>)
             (Time_ns.diff now player.last_poll) 
             (Time_ns.Span.of_sec 60.)
      in
      if should_expire then Some player else None)
  in
  let state =
    List.fold to_expire ~init:state ~f:(fun state player ->
      State.set_player state { player with activity = Inactive })
  in
  (state,())

let rpc_respond decoder body =
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
    (Sexp.to_string response)

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
  let () =
    every (Time.Span.of_sec 1.0) (fun () ->
      update_state ~context:[%message "expiration loop"]
        state_ref expire_old_players)
  in
  let%bind (_:_ Server.t) =
    Server.create
      ~on_handler_error:`Ignore
      (Tcp.Where_to_listen.of_port port)
      (fun ~body _addr request ->
         let uri = Request.uri request in
         let load_local local = 
           print_s [%message
             "Loading local file" (local : string) (uri : Uri.t)];
           Server.respond_with_file local
         in
         match Uri.path uri with
         | "/rpc" -> rpc_respond decoder body
         | "/" -> load_local "site/index.html"
         | _ ->
           load_local (Server.resolve_local_file ~docroot:"site" ~uri))
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
