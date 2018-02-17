open Import

let map_fname = ref (fun x -> x)

let pp_required_by ppf required_by =
  Format.fprintf ppf "@[<v>%a@]@\n"
    (Format.pp_print_list
       (fun ppf x ->
          Format.fprintf ppf "-> required by %a" With_required_by.Entry.pp x))
    required_by

    | Library_not_available        of Library_not_available.t
    | No_solution_found_for_select of No_solution_found_for_select.t
    | Dependency_cycle             of (Path.t * string) list
    | Conflict                     of Conflict.t

let report_lib_error ppf (e : Lib.Error.t) ~required_by =
  match e with
  | Library_not_available { name; reason } ->
    Format.fprintf ppf
      "@{<error>Error@}: Library %S %s.@\n" package
      (match reason with
       | Not_found  -> "not found"
       | Hidden (path, msg) ->\
         sprintf "in %s is hidden (%s)" (Path.to_string_maybe_quoted path) msg);
    pp_required_by ppf required_by;
    (match !Clflags.external_lib_deps_hint with
     | [] -> (* during bootstrap *) ()
     | l ->
       Format.fprintf ppf
         "Hint: try: %s\n"
         (List.map l ~f:quote_for_shell |> String.concat ~sep:" "));
    false
  | Conflict { lib1 = (lib1, rb1); lib2 = (lib2, rb2) } ->
    Format.fprintf ppf
      "@{<error>Error@}: Conflict between the following libaries:\n\
       - %S in %s\n\
      \    %a\
       - %S in %s\n\
      \    %a\
       %a\
       This cannot work.\n"
      (Lib.name lib1) (Lib.src_dir lib1)
      pp_required_by r1
      (Lib.name lib2) (Lib.src_dir lib2)
      pp_required_by r2
      pp_required_by required_by;
    false
  | No_solution_found_for_select { loc } ->
    Format.fprintf ppf
      "%a@{<error>Error@}: No solution found for this select form.\n"
      Loc.print loc;
    false
  | Dependency_cycle cycle ->
    Format.fprintf ppf
      "@{<error>Error@}: Dependency cycle detected between the \
       following libraries:\n\
       @[<v>%a@]\n\
       Required by:\n\
       %a"
      (Format.pp_print_list (fun ppf (name, path) ->
         Format.fprintf ppf "-> %S in %s"
           name (Path.to_string_maybe_quoted path)))
      cycle
      pp_required_by required_by;
    false

(* Return [true] if the backtrace was printed *)
let report_with_backtrace ppf exn ~backtrace =
  match exn with
  | Loc.Error (loc, msg) ->
    let loc =
      { loc with
        start = { loc.start with pos_fname = !map_fname loc.start.pos_fname }
      }
    in
    Format.fprintf ppf "%a@{<error>Error@}: %s\n" Loc.print loc msg;
    false
  | Usexp.Parser.Error e ->
    let pos = Usexp.Parser.Error.position e in
    let msg = Usexp.Parser.Error.message e in
    let pos = { pos with pos_fname = !map_fname pos.pos_fname } in
    let loc = { Loc. start = pos; stop = pos } in
    Format.fprintf ppf "%a@{<error>Error@}: %s\n" Loc.print loc msg;
    false
  | Fatal_error msg ->
    if msg.[String.length msg - 1] = '\n' then
      Format.fprintf ppf "%s" msg
    else
      Format.fprintf ppf "%s\n" (String.capitalize_ascii msg);
    false
  | Lib.Error { data; required_by } ->
    report_lib_error ppf data ~required_by
  | Code_error msg ->
    let bt = Printexc.raw_backtrace_to_string backtrace in
    Format.fprintf ppf "@{<error>Internal error, please report upstream \
                        including the contents of _build/log.@}\n\
                        Description: %s\n\
                        Backtrace:\n\
                        %s" msg bt;
    true
  | Unix.Unix_error (err, func, fname) ->
    Format.fprintf ppf "@{<error>Error@}: %s: %s: %s\n"
      func fname (Unix.error_message err);
    false
  | _ ->
    let s = Printexc.to_string exn in
    let bt = Printexc.raw_backtrace_to_string backtrace in
    if String.is_prefix s ~prefix:"File \"" then
      Format.fprintf ppf "%s\nBacktrace:\n%s" s bt
    else
      Format.fprintf ppf "@{<error>Error@}: exception %s\nBacktrace:\n%s" s bt;
    true

let report_aux ppf ?dependency_path exn =
  let backtrace = Printexc.get_raw_backtrace () in
  let backtrace_printed = report_with_backtrace ppf exn ~backtrace in
  if !Clflags.debug_dep_path then
    Option.iter dependency_path ~f:(fun dep_path ->
      Format.fprintf ppf "Dependency path:\n    %s\n"
        (String.concat ~sep:"\n--> "
           (List.map dep_path ~f:With_required_by.Entry.to_string)));
  if not backtrace_printed && !Clflags.debug_backtraces then
    Format.fprintf ppf "Backtrace:\n%s"
      (Printexc.raw_backtrace_to_string backtrace);
  Format.pp_print_flush ppf ()


let reported = ref String_set.empty

let report exn =
  let exn, dependency_path = With_required_by.unwrap_exn exn in
  match exn with
  | Already_reported -> ()
  | _ ->
    report_aux err_ppf ?dependency_path exn;
    Format.pp_print_flush err_ppf ();
    let s = Buffer.contents err_buf in
    Buffer.clear err_buf;
    (* To avoid keeping huge errors in memory *)
    let hash = Digest.string s in
    if not (String_set.mem hash !reported) then begin
      reported := String_set.add hash !reported;
      print_to_console s
    end
