open! Base
open! Stdio
module Time_ns = Core_kernel.Time_ns
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
    ; last_error : (Time_ns.t * Error.t) option
    ; kicked : bool
    ; in_flight : bool
    } [@@deriving fields, compare]

  let can_send t =
    not t.in_flight
    && (match t.last_error with
      | None -> true
      | Some (last_error_time,_) ->
        let now = Incr.now () in
        Time_ns.Span.(>)
          (Time_ns.diff now last_error_time)
          (Time_ns.Span.of_sec 2.))

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
    let previous_response = 
      Option.bind (Map.max_elt t.interactions) ~f:(fun (max_i,max_interaction) ->
        match max_interaction with
        | Input _ -> None
        | Response previous -> Some (max_i,previous))
    in
    let interactions =
      match previous_response with
      | None ->
        Map.set t.interactions
          ~key:(next_interaction t)
          ~data:(Response response)
      | Some (max_i,max_response) ->
        Map.set t.interactions
          ~key:max_i
          ~data:(Response (max_response ^ "\n" ^ response))
    in
    { t with interactions }

  let poll_response t (resp : P.Poll_response.t Or_error.t) =
    let t = { t with in_flight = false } in
    match resp with
    | Error err -> { t with last_error = Some (Incr.now (), err) }
    | Ok { responses; kicked } ->
      let t = 
        List.fold responses ~init:t ~f:(fun t response ->
          add_response t response)
      in
      { t with kicked }

  let input_response t resp =
    let t = { t with in_flight = false } in
    match resp with
    | Error err -> { t with last_error = Some (Incr.now (), err) }
    | Ok id ->
      { t with 
        interactions = 
          Map.change t.interactions id ~f:(function
            | Some (Input { text; _ }) ->
              Some (Input { text; posted = true })
            | x -> x) }

  let create nonce =
    { interactions = Map.empty (module Int)
    ; current_input = ""
    ; nonce
    ; last_error = None
    ; kicked = false
    ; in_flight = false
    }
end

module Action = struct
  type t =
    | Submit_input
    | Poll_response of P.Poll_response.t Or_error.t
    | Input_response of int Or_error.t
    | Update_input of string
    | Poll
  [@@deriving sexp, variants]
end

open Async_kernel

let poll (model:Model.t) ~(schedule:Action.t -> unit) =
  let nonce = model.nonce in
  if not (Model.can_send model) then model
  else (
    don't_wait_for (
      let%map response = Rpc_client.request P.poll { nonce } in
      schedule (Poll_response response));
    { model with in_flight = true })

let maybe_send_input (model:Model.t) ~(schedule:Action.t -> unit) =
  if not (Model.can_send model) then model
  else
    let to_send =
      Map.to_alist model.interactions
      |> List.find_map ~f:(fun (id,interaction) ->
        match interaction with
        | Response _ -> None
        | Input { text; posted } ->
          if posted then None
          else Some (id,text))
    in
    match to_send with
    | None -> model
    | Some (id,input) ->
      let nonce = model.nonce in
      don't_wait_for (
        let%map response = Rpc_client.request P.input { nonce; input } in        
        schedule (Input_response (Or_error.map response ~f:(fun () -> id))));
      { model with in_flight = true }


let apply_action 
      (action:Action.t)
      (model:Model.t)
      ~(schedule:Action.t -> unit)
  =
  if model.kicked then model
  else (
    let model = 
      match action with
      | Submit_input        -> Model.submit_input model
      | Poll_response resp  -> Model.poll_response model resp
      | Input_response resp -> Model.input_response model resp
      | Update_input text   -> Model.update_input model text
      | Poll                -> poll model ~schedule
    in
    maybe_send_input model ~schedule)

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
    if m.kicked then
      [ Node.create "br" [] []
      ; Node.text "You have been kicked! Try reloading the page to log in again." ]
    else
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
          [ Node.create "pre" [] [Node.text text]
          ; Node.create "br" [] [] ])
    in
    [Node.div [] interactions]
  in
  Node.div [ ] 
    (List.concat
       [ entries
       ; input
       ])
