open! Base
open! Import

module Model = struct
  type t =
    | Login of Login.Model.t
    | Main_page of Main_page.Model.t
  [@@deriving compare]

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
    | Scroll
  [@@deriving sexp, variants]

  let should_log (_:t) = true
end

module State = struct
  type t = { schedule : Action.t -> unit }

  let schedule_as t inj action = t.schedule (inj action)
  let schedule t action = t.schedule action
end

let login_apply_action model state action =
  Model.Login (
    Login.apply_action action model
      ~schedule:(State.schedule_as state Action.login)
      ~report_nonce:(State.schedule_as state Action.report_nonce))

let main_page_apply_action model state action =
  Model.Main_page (
    Main_page.apply_action action model
      ~schedule:(State.schedule_as state Action.main_page))

let submit_input (model:Model.t) (state:State.t) =
  match model with
  | Login m     -> login_apply_action     m state Login
  | Main_page m -> main_page_apply_action m state Submit_input

let apply_action (action:Action.t) (model:Model.t) (state:State.t) : Model.t=
  match action with
  | Submit_input -> submit_input model state
  | Login action ->
    (match model with
     | Login model -> login_apply_action model state action
     | _ -> model)
  | Main_page action ->
    (match model with
     | Main_page model -> main_page_apply_action model state action
     | _ -> model)
  | Report_nonce nonce ->
    Main_page.on_startup ~schedule:(State.schedule_as state Action.main_page);
    Main_page (Main_page.Model.create nonce)
  | Scroll ->
    Js_misc.scroll ~id:"prompt" ();
    model

let on_startup ~schedule (_ : Model.t) =
  Async_kernel.return { State. schedule }

let on_display ~(old:Model.t) (current:Model.t) state =
  let scroll =
    match old, current with
    | Main_page old, Main_page current ->
      not (Map.equal [%compare.equal: Main_page.Interaction.t]
             old.interactions current.interactions)
      || not (String.equal old.current_input current.current_input)
    | _ -> false
  in
  let set_focus =
    match old, current with
    | Main_page _, Main_page _ -> false
    | _, Main_page _ -> true
    | _ -> false
  in
  if scroll then State.schedule state Scroll;
  if set_focus then
    match Dom_html.getElementById_coerce "prompt" Dom_html.CoerceTo.input  with
    | None -> ()
    | Some i -> i##select

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
