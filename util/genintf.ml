open Core.Std
open Async.Std

module Regex = Re2.Regex

let const_rex = Regex.create_exn "^const "
  
type test_params =
  { fdef_rex      : Regex.t
  ; module_prefix : string
  ; test_module   : string
  ; test_instance : string
  }
  
let type_conversion ?(is_return=false) ~params ts =
  let ts = String.strip ts in
  let ts = Re2.Regex.replace ~f:(fun _ -> "") const_rex ts |> Or_error.ok_exn in
  let wrap_return s = if is_return then "(" ^ s ^ ")" else s in
  if ts = "struct " ^ params.module_prefix ^ " *"
  then wrap_return "ptr t"
  else
    match String.strip ts with
    | "struct nl_cache **" -> wrap_return "ptr (ptr Cache.t)"
    | "struct rtnl_addr *" -> wrap_return "ptr RTAddress.t"
    | "struct nl_addr *"   -> wrap_return "ptr Address.t"
    | "struct nl_sock *"   -> wrap_return "ptr Socket.t"
    | "char *"             -> "string"
    | "unsigned int"       -> "uint"
    | "void *"             -> wrap_return "ptr void"
    | other                ->
      if String.is_substring ~substring:" " other
      then wrap_return other
      else other
;;

let emit_lib_funs ~params ~return_type ~fname ~args =
  let make_args args =
    String.split ~on:',' args
    |> List.map ~f:(type_conversion ~params)
    |> String.concat ~sep:" @-> "
  in
  let fname = String.strip fname in
  let return_type = String.strip return_type in
  printf "let %s = foreign \"%s\"\n(%s @-> returning %s)\n\n"
    fname fname (make_args args) (type_conversion ~is_return:true ~params return_type)
;;

let getter_type_handlers return_type =
  match String.strip return_type with
  | "struct nl_addr *"   -> Some ("%s", "Netlink.Address.to_string")
  | "char *"             -> Some ("%s", "id")
  | "const char *"       -> Some ("%s", "id")
  | "unsigned int"       -> Some ("%d", "Unsigned.UInt32.to_int")
  | "uint8_t"            -> Some ("%d", "Unsigned.UInt8.to_int")
  | "uint16_t"           -> Some ("%d", "Unsigned.UInt16.to_int")
  | "uint32_t"           -> Some ("%d", "Unsigned.UInt32.to_int")
  | "uint64_t"           -> Some ("%d", "Unsigned.UInt64.to_int")
  | "int"                -> Some ("%d", "id")
  | other                ->
    eprintf "Ignoring unknown getter return type: %s\n" other;
    None
;;

let emit_test_getters ~params ~return_type ~fname ~args =
  if List.length (String.split ~on:',' args) = 1
  then
    let fname = String.strip fname in
    match getter_type_handlers return_type with
    | None -> ()
    | Some (format, converter) ->
      if String.is_prefix ~prefix:"get" fname
      then
        printf "printf \"\\t%s : %s\\n%s\" (%s (%s.%s %s));\n"
          fname format "%!" converter params.test_module fname params.test_instance
;;

let function_definition_handler ~make_test ~params fdef =
  match Regex.find_submatches params.fdef_rex fdef with
  | Ok [| Some _; Some return_type; Some fname; Some args |] ->
    if make_test
    then emit_test_getters ~params ~return_type ~fname ~args
    else emit_lib_funs ~params ~return_type ~fname ~args
  | Error e ->
    eprintf "Ignoring non-matching extern statement: %s, %s\n" (Error.to_string_hum e) fdef
  | l ->
    let sexp_of_t =
      Tuple2.sexp_of_t
        String.sexp_of_t
        (Or_error.sexp_of_t (Array.sexp_of_t (Option.sexp_of_t String.sexp_of_t)))
    in
    failwiths "Function definition did not match the expected pattern" (fdef,l) sexp_of_t
;;

let handle_line ~make_test ~params i fdef_in_progress l =
  let l = String.strip l in
  if String.is_prefix ~prefix:"extern \"C\"" l
  then None
  else if String.is_prefix ~prefix:"extern" l
  then
    match fdef_in_progress with
    | Some _  -> failwithf "Unexpected extern at %d" i ()
    | None ->
      if String.is_suffix ~suffix:";" l
      then
        begin
          function_definition_handler ~make_test ~params l;
          None
        end
      else Some [l]
  else
    match fdef_in_progress with
    | None -> None
    | Some fdef_in_progress ->
      if String.is_suffix ~suffix:";" l
      then
        let fdef = List.rev (l::fdef_in_progress) |> String.concat ~sep:" " in
        begin
          function_definition_handler ~make_test ~params fdef;
          None
        end
      else Some (l::fdef_in_progress)
;;

let () =
  Command.async
    ~summary:"Hacky netlink interface generator"
    Command.Spec.(
      empty
      +> flag "-make-test" no_arg ~doc:" output test code"
      +> flag "-test-module-name"
        (optional_with_default "Test_module" string) ~doc:" test module name"
      +> flag "-test-instance-name"
        (optional_with_default "test_instance" string) ~doc:" test instance variable name"
      +> anon ( "FILENAME" %: string)
      +> anon ( "MODULE_PREFIX" %: string)
    )
    (fun make_test test_module test_instance fname module_prefix () ->
       let fdef_rex_str = "^extern\\s(.+)" ^ module_prefix ^ "_([^\\(]+)\\(([^\\)]+)" in
       let fdef_rex = Regex.create_exn fdef_rex_str in
       Reader.file_lines fname
       >>| fun lines ->
       if not make_test
       then printf "let foreign fname = foreign ~from:libnl_route (\"%s_\"^fname)\n\n" module_prefix;
       let params = { fdef_rex; module_prefix; test_module; test_instance } in
       List.foldi lines ~init:None ~f:(handle_line ~make_test ~params)
       |> function
       | Some fdef_in_progress ->
         failwiths "Unexpected left over function definition"
           fdef_in_progress (List.sexp_of_t String.sexp_of_t)
       | None -> ())
  |> Command.run
;;
