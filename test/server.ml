open Core
open Async
open Cohttp_async

let error_handler _ ?request:_ error start_response =
  let response_body = start_response Headers.empty in
  begin match error with
  | `Exn exn ->
    Body.write_string response_body (Exn.to_string exn);
    Body.write_string response_body "\n";
  | #Status.standard as error ->
    Body.write_string response_body (Status.default_reason_phrase error)
  end;
  Body.close response_body
;;

let request_handler _ reqd =
  match Reqd.request reqd  with
  | { meth = `POST; headers; _ } ->
    let response =
      let content_type =
        match Headers.get headers "content-type" with
        | None   -> "application/octet-stream"
        | Some x -> x
      in
      let headers = Headers.of_list ["content-type", content_type; "connection", "close"] in
      Response.create ~headers `OK
    in
    let request_body  = Reqd.request_body reqd in
    let response_body = Reqd.respond_with_streaming reqd response in
    let rec on_read buffer ~off ~len =
      Body.write_bigstring response_body buffer ~off ~len;
      Body.schedule_read request_body ~on_eof ~on_read;
    and on_eof () =
      print_endline "eof";
      Body.close response_body
    in
    Body.schedule_read (Reqd.request_body reqd) ~on_eof ~on_read
  | { meth = `GET; _} ->
    let response =
      Response.create `OK
        ~headers:(Headers.of_list ["content-type","application/octet-stream"])
    in
    Reqd.respond_with_string reqd response "Egad, a message!"
  | _ -> Reqd.respond_with_string reqd (Response.create `Method_not_allowed) ""
;;

let main port max_accepts_per_batch =
  let%bind _server =
    Tcp.Server.create_sock
      ~backlog:10_000
      ~max_connections:10_000
      ~max_accepts_per_batch
      ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port port)
      (Httpaf_async.Server.create_connection_handler
         ~request_handler ~error_handler)
  in
  Deferred.never ()

let () =
  let open Command.Let_syntax in
  Command.async
    ~summary:"Start a hello world Async server"
    [%map_open
      let port =
        flag "-p" (optional_with_default 8080 int)
          ~doc:"int Source port to listen on"
      and accepts =
        flag "-a" (optional_with_default 1 int)
          ~doc:"int Maximum accepts per batch"
      in
      fun () ->
        main port accepts]
  |> Command.run
