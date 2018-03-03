open! Base
open! Stdio
module Time = Core_kernel.Time
open! Import
let log_s = Async_js.log_s

module Interaction = struct
  type t =
    | Input of { text: string; posted: bool }
    | Response of string
  [@@deriving sexp, compare]
end

module Model = struct
  type t =
    { interactions : Interaction.t Map.M(Int).t
    ; current_input : string
    ; nonce : Nonce.t
    ; last_error : Error.t option
    ; kicked : bool
    ; poll_in_flight : bool
    } [@@deriving sexp, fields, compare]

  let next_interaction t =
    match Map.max_elt t.interactions with
    | None -> 0
    | Some (i,_) -> i + 1

  let submit_input t =
    let interactions =
      Map.set t.interactions
        ~key:(next_interaction t)
        ~data:(Input { text = t.current_input; posted = false })
    in
    { t with interactions ; current_input = "" }

  let update_input t current_input =
    { t with current_input }

  let add_response t response =
    let interactions =
      Map.set t.interactions
        ~key:(next_interaction t)
        ~data:(Response response)
    in
    { t with interactions }

  let poll_response t (resp : P.Poll_response.t Or_error.t) =
    let t = { t with poll_in_flight = false } in
    match resp with
    | Error err -> { t with last_error = Some err }
    | Ok { responses; kicked } ->
      let t = 
        List.fold responses ~init:t ~f:(fun t response ->
          add_response t response)
      in
      { t with kicked }

  let create nonce =
    { interactions = Map.empty (module Int)
    ; current_input = ""
    ; nonce
    ; last_error = None
    ; kicked = false
    ; poll_in_flight = false
    }
end

module Action = struct
  type t =
    | Submit_input
    | Poll_response of P.Poll_response.t Or_error.t
    | Update_input of string
    | Poll
  [@@deriving sexp, variants]
end

open Async_kernel

let poll (model:Model.t) ~(schedule:Action.t -> unit) =
  let nonce = model.nonce in
  if not model.poll_in_flight then (
    don't_wait_for (
      let%map response = Rpc_client.request P.poll { nonce } in
      schedule (Poll_response response));
    { model with poll_in_flight = true })
  else
    model
;;

let apply_action 
      (action:Action.t)
      (model:Model.t)
      ~(schedule:Action.t -> unit)
  =
  if model.kicked then model
  else match action with
    | Submit_input       -> Model.submit_input model
    | Poll_response resp -> Model.poll_response model resp
    | Update_input text  -> Model.update_input model text
    | Poll               -> poll model ~schedule

open Async_kernel

let on_startup ~(schedule : Action.t -> unit) =
  log_s [%message "Starting up polling loop"];
  let rec loop () =
    schedule Poll;
    upon (Async_js.sleep 0.5) (fun () -> loop ())
  in
  loop ()

let view (m : Model.t) ~(inject : Action.t -> Vdom.Event.t) =
  let open Vdom in
  let input =
    [Node.input
      [ Attr.type_ "text"
      ; Attr.string_property "value" m.current_input
      ; Attr.on_input (fun _ev text -> inject (Update_input text))
      ; Attr.create "size" "80"
      ] []]
  in
  let entries =
    let interactions =
      Map.data m.interactions
      |> List.bind ~f:(function
        | Input {text;posted} ->
          [ Node.text text
          ; Node.text (if posted then ":" else "...")
          ; Node.create "br" [] []]
        | Response text ->
          [ Node.create "em" [] [Node.text text]
          ; Node.create "br" [] [] ])
    in
    [Node.div [] interactions]
  in
  let kicked =
    if m.kicked then
      [ Node.create "br" [] []
      ; Node.text "You have been kicked!" ]
    else []
  in
  Node.div [ ] 
    (List.concat
       [ entries
       ; input
       ; kicked
       ])
