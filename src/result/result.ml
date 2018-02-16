include struct
  [@@@warning "-33"]
  open Result_compat
  open Pervasives

  type ('a, 'error) t = ('a, 'error) result =
    | Ok    of 'a
    | Error of 'error
end

let is_ok = function
  | Ok    _ -> true
  | Error _ -> false

let is_error = function
  | Ok    _ -> false
  | Error _ -> true

let map x ~f =
  match x with
  | Ok x -> Ok (f x)
  | Error _ as x -> x

let map_error x ~f =
  match x with
  | Ok _ as res -> res
  | Error x -> Error (f x)

type ('a, 'error) result = ('a, 'error) t
