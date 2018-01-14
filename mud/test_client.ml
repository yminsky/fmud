open Core
open Async

open Httpaf
open Httpaf_async

let response_handler finished response response_body =
  match response with
  | { Response.status = `OK; _ } ->
    let rec on_read bs ~off ~len =
      Bigstring.to_string ~off ~len bs |> print_endline;
      Body.schedule_read response_body ~on_read ~on_eof
    and on_eof () = Ivar.fill finished () in
    Body.schedule_read response_body ~on_read ~on_eof;
  | _ -> assert false
;;

let error_handler _ = assert false

let main host port =
  let where_to_connect =
    Tcp.Where_to_connect.of_host_and_port (Host_and_port.create ~host ~port)
  in
  let finished = Ivar.create () in
  Tcp.connect_sock where_to_connect
  >>= fun socket ->
  let request_body =
    Client.request
      ~error_handler
      ~response_handler:(response_handler finished)
      socket
      (Request.create `GET "/")
  in
  Body.close request_body;
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