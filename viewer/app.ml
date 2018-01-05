open! Core_kernel
open Incr_dom

module Interaction = struct
  type t = 
    { input: string
    ; response: string }
  [@@deriving sexp, compare]
end

module Model = struct
  type t = 
    { interactions: Interaction.t Int.Map.t
    ; current_input: string
    } [@@deriving sexp, fields, compare]

  let submit_input t =
    let index =
      match Map.max_elt t.interactions with
      | None -> 0
      | Some (i,_) -> i + 1
    in  
    let new_interaction =
      { Interaction.input = t.current_input; response = "No response yet..." }
    in  
    { interactions = Map.set t.interactions ~key:index ~data:new_interaction
    ; current_input = "" }

  let update_input t current_input =
    { t with current_input }

  let cutoff t1 t2 =
    compare t1 t2 = 0

  let empty = 
    { interactions = Int.Map.empty
    ; current_input = "Type Something Here" 
    }
end

module Action = struct
  type t =
    | Submit_input
    | Update_input of string
  [@@deriving sexp]

  let should_log (_:t) = true
end

module State = struct
  type t = unit
end

let apply_action action model _state =
  match (action:Action.t) with
  | Submit_input -> Model.submit_input model
  | Update_input text -> Model.update_input model text

let update_visibility m = m

let on_startup ~schedule:_ _ =
  Async_kernel.return ()

let on_display ~old:_ _ _ = ()

let view (m : Model.t Incr.t) ~(inject : Action.t -> Vdom.Event.t) =
  let open Incr.Let_syntax in
  let open Vdom in
  let%map m = m in
  let button label action =
    Node.button
      [ Attr.on_click (fun _ev -> inject action) ]
      [ Node.text label ]
  in  
  let submit = button "Submit" Submit_input in
  let input =
    Node.input
      [ Attr.type_ "text"
      ; Attr.string_property "value" m.current_input
      ; Attr.on_input (fun _ev text -> inject (Update_input text))
      ] []
  in  
  let entries =  
    let interactions = 
      Map.data m.interactions
      |> List.bind ~f:(fun entry -> 
        [ Node.text entry.input
        ; Node.create "br" [] []
        ; Node.text entry.response
        ; Node.create "br" [] []
        ])
    in  
    Node.div [] interactions
  in  
  Node.body [] [ entries; input; submit ]
;;


