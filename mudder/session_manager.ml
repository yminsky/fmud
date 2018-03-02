open! Core

type t = { decoder : Mud_common.Rpc.Decoder.t }

let create decoder = { decoder }