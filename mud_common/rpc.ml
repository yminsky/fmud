open! Base

type ('a,'b) t =
  { name: string
  ; request: (module Sexpable.S with type t = 'a)
  ; response: (module Sexpable.S with type t = 'b)
  }

let name t = t.name

let create name request response =
  { name; request; response }

type ('a,'b) rpc = ('a,'b) t

module Implementation = struct
  type t = T : ('a,'b) rpc * ('a -> 'b) -> t

  let create rpc f = T (rpc,f)
end

module Generic_request = struct
  type t = (string * Sexp.t) [@@deriving sexp]
end

let encode (type request) (type response)
      { name
      ; request = (module Request : Sexpable.S with type t = request)
      ; response = (module Response : Sexpable.S with type t = response)
      }
      (request : request)
  =
  let sexp = Generic_request.sexp_of_t (name,Request.sexp_of_t request) in
  let response_decoder = Or_error.t_of_sexp Response.t_of_sexp in
  (sexp,response_decoder)
;;


module Decoder = struct
  type t = Implementation.t Map.M(String).t

  let create implementations =
    List.map implementations ~f:(fun impl ->
      let Implementation.T (rpc,_) = impl in
      (rpc.name,impl))
    |> Map.of_alist_exn (module String)

  let error_s sexp = Error (Error.create_s sexp)

  let eval t sexp =
    match Generic_request.t_of_sexp sexp with
    | exception _ -> error_s [%message "Malformed request" (sexp : Sexp.t)]
    | (name,body) ->
      match Map.find t name with
      | None ->
        error_s [%message "Unknown RPC" (name:string)]
      | Some (Implementation.T
                ({request = (module Request)
                 ; response = (module Response); _},f))
        ->
        match Request.t_of_sexp body with
        | exception exn ->
          error_s [%message
            "Couldn't parse body of request"
              (name:string) (body:Sexp.t) (exn:Exn.t)]
        | request ->
          match f request with
          | exception exn ->
            error_s [%message
              "Evaluation of request failed"
                (name:string) (request:Request.t) (exn:Exn.t)]
          | response ->
            Ok (Response.sexp_of_t response)
end
