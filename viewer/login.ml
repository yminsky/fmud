open! Base
open! Import

module Model = struct

  type status = Awaiting_input | Logging_in
  [@@deriving sexp, compare]

  type error = | String of string
               | Ordinary of Error.t
  [@@deriving sexp, compare]

  type t = 
    { nick : string
    ; password : string
    ; status : status
    ; errors : error list
    } [@@deriving sexp, fields, compare]

  let cutoff t1 t2 = compare t1 t2 = 0

  let empty = { nick = "" 
              ; password = "" 
              ; status = Awaiting_input
              ; errors = []
              }
end

module Action = struct
  type t =
    | Update_nick of string
    | Update_password of string
    | Report_error of Model.error
    | Login
    | Register
  [@@deriving sexp]
end

include struct
  open Async_kernel 

  let valid_nick_and_password (model:Model.t) =
    let nick     = String.strip model.nick     in
    let password = String.strip model.password in
    if String.is_empty nick
    || String.is_empty password 
    then Error "Password and nickname must both be non-empty"
    else Ok (Nick.of_string nick,Password.of_string password)
    

  let login (model : Model.t) ~(schedule: Action.t -> unit) ~report_nonce =
    begin match valid_nick_and_password model with
    | Error reason -> schedule (Report_error (String reason))
    | Ok (nick,password) -> 
      don't_wait_for begin 
        match%map Rpc_client.request P.login { nick ; password } with
        | Error error -> 
          schedule (Report_error (Ordinary error))
        | Ok Wrong_password -> 
          schedule (Report_error (String "Wrong password. Try again!"))
        | Ok Unknown_nick ->
          let msg = "Unknown nick. Maybe you want to register instead?" in
          schedule (Report_error (String msg))
        | Ok Login_accepted { nonce } ->
          report_nonce nonce
      end;
    end;
    { model with status = Logging_in }

  let register (model : Model.t) ~(schedule: Action.t -> unit) ~report_nonce =
    begin match valid_nick_and_password model with
    | Error reason -> schedule (Report_error (String reason))
    | Ok (nick,password) ->
      don't_wait_for begin
        match%map Rpc_client.request P.register { nick; password } with
        | Error error ->
          schedule (Report_error (Ordinary error))
        | Ok Nick_taken ->
          schedule (Report_error (String "That nickname was taken. Try another!"))
        | Ok Registered { nonce } ->
          report_nonce nonce
      end;
    end;
    { model with status = Logging_in }

  let apply_action (action:Action.t) (model : Model.t) ~schedule ~report_nonce =
    match action with
    | Update_nick nick -> { model with nick }
    | Update_password password -> { model with password }
    | Report_error error -> { Model.empty with errors = error :: model.errors }
    | Register -> register model  ~schedule ~report_nonce
    | Login -> login model ~schedule ~report_nonce

end

open Vdom

let view  (m : Model.t) ~(inject : Action.t -> Vdom.Event.t) =
  let { Model.nick; password; status; errors } = m in
  let input name ~current  ?(autofocus=false) update = 
    [ Node.text (name ^ " ")
    ; Node.input
      [ Attr.type_ "text"
      ; Attr.string_property "value" current
      ; Attr.autofocus autofocus
      ; Attr.on_input (fun _ev text -> inject (update text))
      ; Attr.create "size" "20"
      ] []
    ]
  in
  let inputs =
    List.concat
      [ input "nickname" ~current:nick (fun x -> Update_nick x) ~autofocus:true
      ; [Node.create "br" [] []]
      ; input "password" ~current:password (fun x -> Update_password x)
      ; [Node.create "br" [] []]
      ; [Node.button
           [Attr.on_click (fun _ -> inject Action.Login)]
           [Node.text "Log in"]]
      ; [Node.button
           [Attr.on_click (fun _ -> inject Action.Register)]
           [Node.text "Register"]]
      ]
  in
  let errors =
    List.bind (List.rev errors) ~f:(fun error ->
      let text =
        match error with
        | String s -> s
        | Ordinary e -> Error.to_string_hum e
      in
      [ Node.text text; Node.create "br" [] [] ])
  in
  let status =
    match status with
    | Awaiting_input -> []
    | Logging_in -> [ Node.create "br" [] []; Node.text "Logging in..."]
  in
  Node.div [] (List.concat [ errors; inputs; status ])
  
