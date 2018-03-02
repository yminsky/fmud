open! Base
open Incr_dom
module Rpc = Mud_common.Rpc

module P = Mud_common.Protocol
module Nick = P.Nick
module Nonce = P.Nonce
module Password = P.Password

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
    { interactions ; current_input = "" }

  let add_response t response =
    let interactions =
      Map.set t.interactions
        ~key:(next_interaction t)
        ~data:(Response response)
    in
    { t with interactions }

  let update_input t current_input =
    { t with current_input }

  let cutoff t1 t2 =
    compare t1 t2 = 0

  let empty =
    { interactions = Map.empty (module Int)
    ; current_input = ""
    }
end

module Action = struct
  type t =
    | Submit_input
    | Add_response of string
    | Update_input of string
    | Poll
  [@@deriving sexp]

  let should_log (_:t) = true
end

module State = struct
  type t =
    { schedule : Action.t -> unit
    ; mutable poll_in_flight: bool
    }
end

let apply_action action model state =
  match (action:Action.t) with
  | Submit_input      -> Model.submit_input model
  | Add_response text -> Model.add_response model text
  | Update_input text -> Model.update_input model text
  | Poll ->
    ignore state;
    assert false

let update_visibility m = m

open Async_kernel

let login nick password =
  Rpc_client.request P.login { nick; password }

let poll ~schedule =
  let rec loop () =
    schedule Action.Poll;
    upon (Async_js.sleep 0.5) (fun () -> loop ())
  in
  loop ()

let on_startup ~schedule (_ : Model.t) =
  poll ~schedule;
  return { State. schedule; poll_in_flight = false }

let on_display ~old:_ _ _ = ()

let key_handler ~(inject : Action.t -> _) =
  let open Vdom in
  Attr.on_keypress (fun ev ->
    match Dom_html.Keyboard_code.of_event ev with
    | Enter ->  inject Submit_input
    | _ -> Event.Ignore
  )


let view (m : Model.t Incr.t) ~(inject : Action.t -> Vdom.Event.t) =
  let open Incr.Let_syntax in
  let open Vdom in
  let%map m = m in
  let input =
    Node.input
      [ Attr.type_ "text"
      ; Attr.string_property "value" m.current_input
      ; Attr.on_input (fun _ev text -> inject (Update_input text))
      ; Attr.create "size" "80"
      ] []
  in
  let entries =
    let interactions =
      Map.data m.interactions
      |> List.bind ~f:(function
        | Input {text;posted} ->
          [ Node.text text
          ; Node.text (if posted then "..." else ":")
          ; Node.create "br" [] []]
        | Response text ->
          [ Node.create "em" [] [Node.text text]
          ; Node.create "br" [] [] ])
    in
    Node.div [] interactions
  in
  Node.body [ key_handler ~inject ] [ entries ; input ]
