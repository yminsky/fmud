open! Core
open! Async
open Httpaf

let error_handler _ ?request:_ error start_response =
  let response_body = start_response Headers.empty in
  begin match error with
  | `Exn exn ->
    Response.Body.write_string response_body (Exn.to_string exn);
    Response.Body.write_string response_body "\n";
  | #Status.standard as error ->
    Response.Body.write_string response_body (Status.default_reason_phrase error)
  end;
  Response.Body.close response_body
;;

let request_handler _ reqd =
  match Reqd.request reqd  with
  | { Request.meth = `POST; headers; _ } ->
    let response =
      let content_type = Headers.get_exn headers "content-type" in
      Response.create ~headers:(Headers.of_list ["content-type", content_type; "connection", "close"]) `OK
    in
    let request_body  = Reqd.request_body reqd in
    let response_body = Reqd.respond_with_streaming reqd response in
    let rec on_read buffer ~off ~len =
      Response.Body.write_bigstring response_body buffer ~off ~len;
      Request.Body.schedule_read request_body ~on_eof ~on_read;
      0
    and on_eof () =
      print_endline "eof";
      Response.Body.close response_body
    in
    Request.Body.schedule_read (Reqd.request_body reqd) ~on_eof ~on_read
  | _ -> Reqd.respond_with_string reqd (Response.create `Method_not_allowed) ""
;;

let main port max_accepts_per_batch =
  let%bind _server =
    Tcp.Server.create_sock
      ~backlog:10_000
      ~max_connections:10_000 
      ~max_accepts_per_batch 
      ~on_handler_error:`Ignore
      (Tcp.Where_to_listen.of_port port)
      (Httpaf_async.create_connection_handler ~request_handler ~error_handler)
  in  
  Deferred.never ()

let () =
  let open Command.Let_syntax in
  Command.async
    ~summary:"Start a hello world Async server"
    [%map_open
      let port = 
        flag "-p" (optional_with_default 8080 int)
          ~doc:"PORT Source port to listen on"
      and max_accepts = 
        flag "-a" (optional_with_default 1 int)
          ~doc:"# Maximum accepts per batch"
      in 
      fun () ->
        main port max_accepts
    ]
  |> Command.run