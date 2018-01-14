open Core
open Async

let response_handler finished (response:Httpaf.Response.t) response_body =
  match response with
  | { status = `OK; _ } ->
    let on_eof () = Ivar.fill finished () in
    let rec on_read bs ~off ~len =
      Httpaf.Bigstring.to_string ~off ~len bs |> print_endline;
      Httpaf.Body.schedule_read response_body ~on_read ~on_eof
    in
    Httpaf.Body.schedule_read response_body ~on_read ~on_eof;
  | _ -> assert false
;;

let error_handler _ = assert false

let main host port =
  let where_to_connect =
    Tcp.Where_to_connect.of_host_and_port (Host_and_port.create ~host ~port)
  in
  let finished = Ivar.create () in
  let%bind socket = Tcp.connect_sock where_to_connect in
  let request_body =
    Httpaf_async.Client.request
      ~error_handler
      ~response_handler:(response_handler finished)
      socket
      (Httpaf.Request.create `GET "/")
  in
  Httpaf.Body.close request_body;
  Ivar.read finished
;;

let () =
  let open Command.Let_syntax in
  Command.async
    ~summary:"Start a hello world Async server"
    [%map_open
      let port = 
        flag "-p" (optional_with_default 8080 int)
          ~doc:"PORT destination port"
      and host =
        flag "-h" (optional_with_default "localhost" string)
          ~doc:"HOST destination host"
      in    
      fun () -> main host port]
  |> Command.run