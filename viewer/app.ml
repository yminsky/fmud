open! Base
open! Import

module Model = struct
  type t =
    | Login of Login.Model.t
    | Main_page of Main_page.Model.t
  [@@deriving sexp, compare]
                 
  let empty = Login Login.Model.empty
  let cutoff t1 t2 =
    compare t1 t2 = 0
end

module Action = struct
  type t =
    | Submit_input
    | Report_nonce of Nonce.t
    | Main_page of Main_page.Action.t
    | Login of Login.Action.t
  [@@deriving sexp]
  
  let should_log (_:t) = true
end

module State = struct
  type t = { schedule : Action.t -> unit }
end

let submit_input (model:Model.t) (state:State.t) =
  match model with
  | Login m -> 
    Model.Login (
      Login.submit_input m
        ~schedule:(fun action -> state.schedule (Login action))
        ~report_nonce:(fun nonce -> state.schedule (Report_nonce nonce)))
  | Main_page m ->
    Model.Main_page (Main_page.apply_action Submit_input m)

let apply_action (action:Action.t) (model:Model.t) (state:State.t) : Model.t=
  match action with
  | Submit_input -> submit_input model state
  | Main_page action ->
    (match model with
     | Main_page model -> Main_page (Main_page.apply_action action model)
     | _ -> model)
  | Login action ->
    (match model with
     | Login model -> Login (Login.apply_action action model state)
     | _ -> model)
  | Report_nonce nonce ->
    Main_page (Main_page.Model.create nonce)

let on_startup ~schedule (_ : Model.t) =
  Async_kernel.return { State. schedule }
    
let on_display ~old:_ _ _ = ()
let update_visibility m = m
                            
let key_handler ~(inject : Action.t -> Vdom.Event.t) =
  let open Vdom in
  Attr.on_keypress (fun ev ->
    match Dom_html.Keyboard_code.of_event ev with
    | Enter ->  inject Submit_input
    | _ -> Event.Ignore)
    
let view (m : Model.t Incr.t) ~(inject : Action.t -> Vdom.Event.t) =
  let open Incr.Let_syntax in 
  let open Vdom in
  let%map m = m in
  let inner =
    match m with
    | Login m -> Login.view m ~inject:(fun a -> inject (Login a))
    | Main_page m -> Main_page.view m ~inject:(fun a -> inject (Main_page a))
  in
  Node.body [ key_handler ~inject ] [ inner ]

let init_model = Model.empty
