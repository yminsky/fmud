open! Base
open Incr_dom

module Interaction = struct
  type t = 
    { input: string
    ; response: string }
  [@@deriving sexp, compare]
end

module Model = struct
  type t = 
    { interactions : Interaction.t Map.M(Int).t
    ; current_input : string
    ; counter : int
    } [@@deriving sexp, fields, compare]

  let submit_input t =
    let index =
      match Map.max_elt t.interactions with
      | None -> 0
      | Some (i,_) -> i + 1
    in  
    let new_interaction =
      { Interaction.input = t.current_input
      ; response = Printf.sprintf {|You said "%s" at counter %d|} t.current_input t.counter
      } 
    in  
    { t with
      interactions = Map.set t.interactions ~key:index ~data:new_interaction
    ; current_input = "" }

  let update_input t current_input =
    { t with current_input }

  let click_counter t = { t with counter = t.counter + 1}

  let cutoff t1 t2 =
    compare t1 t2 = 0

  let empty = 
    { interactions = Map.empty (module Int)
    ; current_input = ""
    ; counter = 0
    }
end

module Action = struct
  type t =
    | Submit_input
    | Update_input of string
    | Click_counter
  [@@deriving sexp]

  let should_log (_:t) = true
end

module State = struct
  type t = { schedule : Action.t -> unit }
end

let apply_action action model _state =
  match (action:Action.t) with
  | Submit_input -> Model.submit_input model
  | Update_input text -> Model.update_input model text
  | Click_counter -> Model.click_counter model

let update_visibility m = m

open Async_kernel

let on_startup ~schedule (_ : Model.t) =
  let rec loop () =
    schedule Action.Click_counter;
    upon (Async_js.sleep 0.1) (fun () -> loop ())
  in  
  loop ();
  return { State. schedule }

let on_display ~old:_ _ _ = ()


let key_handler ~(inject : Action.t -> _) =
  let open Vdom in
  Attr.on_keypress (fun ev ->
    match Dom_html.Keyboard_code.of_event ev with
    | Enter ->  inject Submit_input
    | _ -> Event.Ignore
  )
;;  

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
      |> List.bind ~f:(fun entry -> 
        [ Node.text entry.input
        ; Node.create "br" [] []
        ; Node.create "em" [] [Node.text entry.response]
        ; Node.create "br" [] []
        ; Node.create "hr" [] []
        ])
    in  
    Node.div [] interactions
  in  
  Node.body [ key_handler ~inject ] 
    [ Node.text (Int.to_string m.counter)
    ; entries
    ; input ]
;;


