open! Base
open! Import

module Model = struct
  type t =
    | Login of Login.Model.t
    | Logged_in of Logged_in.Model.t
  [@@deriving sexp, compare]
                 
  let empty = Login Login.Model.empty
  let cutoff t1 t2 =
    compare t1 t2 = 0
end

module Action = struct
  type t =
    | Submit_input
    | Logged_in of Logged_in.Action.t
    | Login of Login.Action.t
  [@@deriving sexp]
  
  let should_log (_:t) = true
end

module State = struct
  type t = { schedule : Action.t -> unit }
end

let apply_action = assert false

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
    | Logged_in m -> Logged_in.view m ~inject:(fun a -> inject (Logged_in a))
  in
  Node.body [ key_handler ~inject ] [ inner ]

let init_model = Model.empty
