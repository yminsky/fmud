open Core
open Async

module Nick : Identifiable = String
module Password : Identifiable = String

type action =
  | Send_message of { nick: string; message: string }
  | Kill_client  of { nick: string }
[@@deriving sexp]

type client =
  { nick: Nick.t
  ; r: Reader.t
  ; w: Writer.t
  }

type 'world handlers =
  { init : 'world
  ; description : string
  ; handle_line : 'world -> string -> string -> 'world * action list
  ; nick_added : 'world -> string -> 'world * action list
  ; nick_removed : 'world -> string -> 'world * action list
  }
[@@deriving sexp]

type 'world state =
  { mutable world : 'world
  ; clients       : client Nick.Table.t
  ; passwords     : Password.t Nick.Table.t
  }

let rec remove_client (s:_ state) (h:_ handlers) (c:client) =
  let%bind () = Writer.close c.w in
  let%bind () = Reader.close c.r in
  Hashtbl.remove s.clients c.nick;
  let (new_world,actions) = h.nick_removed s.world (Nick.to_string c.nick) in
  s.world <- new_world;
  run_actions s h actions

and run_actions (s:_ state) (h:_ handlers) actions : unit Deferred.t =
  Deferred.List.iter actions ~f:(fun action ->
    (* Debug output *)
    print_endline (Sexp.to_string_hum (sexp_of_action action));
    begin match action with
    | Send_message { nick; message } ->
      let nick = Nick.of_string nick in
      begin match Hashtbl.find s.clients nick with
      | None -> ()
      | Some c -> Writer.write_line c.w (String.strip message)
      end;
      Deferred.unit
    | Kill_client { nick } ->
      let nick = Nick.of_string nick in
      begin match Hashtbl.find s.clients nick with
      | None -> Deferred.unit
      | Some c -> remove_client s h c
      end
    end)

let input_loop (s:_ state) (h:_ handlers) (c:client) =
  let rec loop () =
    match%bind Monitor.try_with (fun () -> Reader.read_line c.r) with
    | Ok `Eof | Error _ -> remove_client s h c
    | Ok (`Ok line) ->
      let (new_world,actions) = h.handle_line s.world (Nick.to_string c.nick) line in
      s.world <- new_world;
      let%bind () = run_actions s h actions in
      loop ()
  in
  loop ()

let close r w =
  let%bind () = Reader.close r in
  Writer.close w

let prompt r w prompt =
  Writer.write w (prompt ^ ": ");
  match%map Monitor.try_with (fun () -> Reader.read_line r) with
  | Ok `Eof | Error _ -> None
  | Ok (`Ok response) -> Some response
;;


let start_client s handlers r w nick =
  let c = { nick; r; w } in
  Hashtbl.set s.clients ~key:nick ~data:c;
  let (new_world, actions) = 
    handlers.nick_added s.world (Nick.to_string nick) 
  in
  s.world <- new_world;
  let%bind () = run_actions s handlers actions in
  input_loop s handlers c
;;

let build_connection_handler handlers =
  let s = { world = handlers.init
          ; clients = Nick.Table.create ()
          ; passwords = Nick.Table.create ()
          }
  in
  stage (fun r w ->
    Writer.write_line w handlers.description;
    match%bind prompt r w "nick" with
    | None -> close r w
    | Some nick ->
      let nick = Nick.of_string nick in
      match Hashtbl.find s.clients nick with
      | Some _ ->
        Writer.write_line w "Someone is already logged in with that nick.";
        let%bind () = Writer.flushed w in
        close r w
      | None ->
        match Hashtbl.find s.passwords nick with
        | None ->
          Writer.write_line w "Hey, you're new here! Please pick a password.";
          let%bind () = Writer.flushed w in
          begin match%bind prompt r w "new password" with
          | None -> close r w
          | Some password ->
            let password = Password.of_string password in
            Hashtbl.set s.passwords ~key:nick ~data:password;
            start_client s handlers r w nick
          end
        | Some expected_password ->
          begin match%bind prompt r w "pw" with
          | None -> close r w
          | Some password ->
            let password = Password.of_string password in
            if not (Password.equal expected_password password) then (
              Writer.write_line w "Wrong password. Bye.";
              let%bind () = Writer.flushed w in
              close r w
            ) else (
              start_client s handlers r w nick
            )
          end
  )

let start_mud h =
  Command.async'
    ~summary:"A mud!"
    (let open Command.Let_syntax in
     [%map_open
       let port = flag "-port" (optional_with_default 12321 int)
                    ~doc:"PORT the port to listen on" 
       in
       fun () ->
         let open Deferred.Let_syntax in
         let handle_connection = unstage (build_connection_handler h) in
         printf "Starting accept loop.\n";
         let%bind server = 
           Tcp.Server.create
             ~on_handler_error:(`Call (fun _addr exn ->
               print_endline @@ Sexp.to_string_hum @@
               [%message "Unexpected exception" ~_:(exn:Exn.t)]))
             (Tcp.on_port port) 
             (fun _addr -> handle_connection)
         in
         Tcp.Server.close_finished server])
  |> Command.run
