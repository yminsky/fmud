open! Base

module Nick : Identifiable.S = String
module Password : Identifiable.S = String

module Check_nick = struct
  type t = { nick : Nick.t } [@@deriving sexp]
end

module Nonce : sig
  type t
  include Identifiable.S with type t := t
  val random : Random.State.t -> t
end = struct
  let random rstate =
    Random.State.int32 rstate Int32.max_value
  include Int32
end

module Nick_status = struct
  type t = Known_nick | New_nick [@@deriving sexp]
end

let check_nick =
  Rpc.create "check-nick" (module Check_nick) (module Nick_status)

module Login = struct
  type t =
    { nick: Nick.t
    ; password: Password.t
    } [@@deriving sexp]
end

module Login_response = struct
  type t = | Login_accepted of { nonce : Nonce.t }
           | Wrong_password
           | Unknown_nick
  [@@deriving sexp]
end

let login =
  Rpc.create "login" (module Login) (module Login_response)

module Input = struct
  type t =
    { nonce: Nonce.t
    ; input: string
    } [@@deriving sexp]
end

let input =
  Rpc.create "input" (module Input) (module Unit)

module Heartbeat = struct
  type t = { nonce: Nonce.t } [@@deriving sexp]
end

let heartbeat =
  Rpc.create "heartbeat" (module Heartbeat) (module Unit)

module Poll = struct
  type t = { nonce: Nonce.t } [@@deriving sexp]
end

module Poll_response = struct
  type t = { responses : string list } [@@deriving sexp]
end

let poll = Rpc.create "poll" (module Poll) (module Poll_response)
