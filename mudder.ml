open! Core
open! Async

type action =
  | Send_message of { nick: string; message: string }
  | Kill_client  of { nick: string }
[@@deriving sexp]

type client =
  { nick: string
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
  { mutable world: 'world
  ; clients: client String.Table.t
  }

let rec remove_client (s:_ state) (h:_ handlers) (c:client) =
  Writer.close c.w >>= fun () ->
  Reader.close c.r >>= fun () ->
  Hashtbl.remove s.clients c.nick;
  let (new_world,actions) = h.nick_removed s.world c.nick in
  s.world <- new_world;
  run_actions s h actions

and run_actions (s:_ state) (h:_ handlers) actions : unit Deferred.t =
  Deferred.List.iter actions ~f:(fun action ->
      (* Debug output *)
      print_endline (Sexp.to_string_hum (sexp_of_action action));
      begin match action with
      | Send_message { nick; message } ->
        begin match Hashtbl.find s.clients nick with
          | None -> ()
          | Some c -> Writer.write_line c.w (String.strip message)
        end;
        Deferred.unit
      | Kill_client { nick } ->
        begin match Hashtbl.find s.clients nick with
          | None -> Deferred.unit
          | Some c -> remove_client s h c
        end
      end)

let input_loop (s:_ state) (h:_ handlers) (c:client) =
  let rec loop () =
    Monitor.try_with (fun () -> Reader.read_line c.r) >>= function
    | Ok `Eof | Error _ -> remove_client s h c
    | Ok (`Ok line) ->
      let (new_world,actions) = h.handle_line s.world c.nick line in
      s.world <- new_world;
      run_actions s h actions
      >>= fun () ->
      loop ()
  in
  loop ()

let mud handlers =
  let s = { world = handlers.init
          ; clients = String.Table.create ()
          }
  in
  (fun r w ->
     Writer.write_line w handlers.description;
     Writer.write w "nick: ";
     let close () =
       Reader.close r >>= fun () ->
       Writer.close w
     in
     Reader.read_line r >>= function
     | `Eof ->  close ()
     | `Ok nick ->
       match Hashtbl.find s.clients nick with
       | Some _ ->
         Writer.write_line w "Nick already taken. Sorry.";
         close ()
       | None ->
         let c = { nick; r; w } in
         Hashtbl.set s.clients ~key:nick ~data:c;
         let (new_world, actions) = handlers.nick_added s.world nick in
         s.world <- new_world;
         run_actions s handlers actions >>= fun () ->
         input_loop s handlers c
  )


let start_mud h ~port =
  Command.async'
    ~summary:"A mud!"
    (Command.Param.return
       (fun () ->
          let handler = mud h in
          printf "Starting accept loop.\n";
          let%bind server = 
            Tcp.Server.create
              ~on_handler_error:(`Call (fun _addr exn ->
                  print_endline @@ Sexp.to_string_hum @@
                  [%message "Unexpected exception" ~_:(exn:Exn.t)]))
              (Tcp.on_port port) 
              (fun _addr -> handler)
          in
          Tcp.Server.close_finished server))
  |> Command.run
