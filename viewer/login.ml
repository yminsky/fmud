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
    ; error : (unit,error) Result.t
    } [@@deriving sexp, fields, compare]

  let cutoff t1 t2 = compare t1 t2 = 0

  let empty = { nick = "" ; password = "" ; status = Awaiting_input; error = Ok () }
end

module Action = struct
  type t = 
    | Update_nick of string
    | Update_password of string
    | Report_error of Model.error
  [@@deriving sexp]
end

include struct
  open Async_kernel 

  let apply_action (action:Action.t) (model : Model.t) _state =
    match action with
    | Update_nick nick -> { model with nick }
    | Update_password password -> { model with password }
    | Report_error error -> { Model.empty with error = Error error }

  let submit_input (model : Model.t) ~(schedule: Action.t -> unit) ~report_nonce =
    don't_wait_for begin match%map
        Rpc_client.request P.login
          { nick = Nick.of_string model.nick
          ; password = Password.of_string model.password }
      with
      | Error error -> 
        schedule (Report_error (Ordinary error))
      | Ok Wrong_password -> 
        schedule (Report_error (String "Wrong password. Try again!"))
      | Ok Unknown_nick ->
        let msg = "Unknown nick. Update the code to register this nick!" in
        schedule (Report_error (String msg))
      | Ok Login_accepted { nonce } ->
        report_nonce nonce
    end;
    { model with status = Logging_in }
end

open Vdom

let view (m : Model.t) ~(inject : Action.t -> Vdom.Event.t) =
  let { Model.nick; password; status; error } = m in
  let input name ~current update = 
    [ Node.text name
    ; Node.input
      [ Attr.type_ "text"
      ; Attr.string_property "value" current
      ; Attr.on_input (fun _ev text -> inject (update text))
      ; Attr.create "size" "20"
      ] []
    ]
  in
  let inputs =
    List.concat
      [ input "password" ~current:password (fun x -> Update_password x)
      ; input "nick" ~current:nick (fun x -> Update_nick x) ]
  in
  let error =
    match error with
    | Ok () -> []
    | Error (String text) -> 
      [ Node.text ("ERROR: " ^ text) 
      ; Node.create "br" [] [] ]
    | Error (Ordinary err) ->
      [ Node.text ("ERROR:\n" ^ Error.to_string_hum err)
      ; Node.create "br" [] [] ]
  in
  let status =
    match status with
    | Awaiting_input -> []
    | Logging_in -> [ Node.create "br" [] []; Node.text "Logging in..."]
  in
  Node.div [] (List.concat [ error; inputs; status ])
  
