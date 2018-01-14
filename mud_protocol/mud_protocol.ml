open! Base

module Session_id : Identifiable.S = String

module Login = struct
  type t = 
    { nick: string
    ; password: string
    ; session: Session_id.t
    } [@@deriving sexp]
end

module Login_response = struct
  type t = | Login_accepted
           | Wrong_password
  [@@deriving sexp]
end

let login = 
  Rpc.create "login" (module Login) (module Login_response)

module  Input = struct
  type t = 
    { session: Session_id.t
    ; input: string
    } [@@deriving sexp]
end

module Input_response = struct
  type t = { response: string} [@@deriving sexp]
end

let input = 
  Rpc.create "input" (module Input) (module Input_response)

module Heartbeat = struct 
  type t = { session: Session_id.t } [@@deriving sexp]
end  

let heartbeat =
  Rpc.create "heartbeat" (module Heartbeat) (module Unit)

module Poll = struct
  type t = { session: Session_id.t } [@@deriving sexp]
end

module Poll_response = struct
  type t = { responses : string list } [@@deriving sexp]
end

let poll = Rpc.create "poll" (module Poll) (module Poll_response)