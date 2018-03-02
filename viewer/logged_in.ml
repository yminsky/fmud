open! Base
open! Import

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

  let add_response t response =
    let interactions =
      Map.set t.interactions
        ~key:(next_interaction t)
        ~data:(Response response)
    in
    { t with interactions }

  let update_input t current_input =
    { t with current_input }

  let create nonce =
    { interactions = Map.empty (module Int)
    ; current_input = ""
    ; nonce
    }
end

module Action = struct
  type t =
    | Submit_input
    | Add_response of string
    | Update_input of string
    | Poll
  [@@deriving sexp]
end

let apply_action action model =
  match (action:Action.t) with
  | Submit_input      -> Model.submit_input model
  | Add_response text -> Model.add_response model text
  | Update_input text -> Model.update_input model text
  | Poll -> assert false

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

let on_startup ~schedule =
  poll ~schedule

let view (m : Model.t) ~(inject : Action.t -> Vdom.Event.t) =
  let open Vdom in
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
  Node.div [ ] [ entries ; input ]
