open Import

module Entry = struct
  type t =
    | Path of Path.t
    | Alias of Path.t
    | Library of Path.t * string
    | Preprocess of string list

  let jbuild_file_in ~dir = Path (Utils.jbuild_file_in ~dir)

  let to_string = function
    | Path p -> Utils.describe_target p
    | Alias p -> "alias " ^ Utils.describe_target p
    | Library (_, s) -> sprintf "library %S" s
    | Preprocess l -> Sexp.to_string (List [Atom "pps"; Sexp.To_sexp.(list string) l])

  let pp ppf x =
    Format.pp_print_string ppf (to_string x)
end

type 'a t =
  { data : 'a
  ; required_by : Entry.t list
  }

let prepend_one t entry = { t with required_by = entry :: t.required_by }
let append t entries = { t with required_by = t.required_by @ entries }

exception E of exn * Entry.t list

let reraise exn entry =
  reraise (
    match exn with
    | E (exn, entries) -> E (exn, entry :: entries)
    | exn -> E (exn, [entry]))

let unwrap_exn = function
  | E (exn, entries) -> (exn, Some entries)
  | exn -> (exn, None)

let () =
  Printexc.register_printer (function
    | E (exn, _) -> Some (Printexc.to_string exn)
    | _ -> None)
