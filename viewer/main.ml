open! Core_kernel
open! Incr_dom
open! Js_of_ocaml

let () =
  Start_app.simple
    (module Logged_in)
    ~initial_model:Logged_in.Model.empty
