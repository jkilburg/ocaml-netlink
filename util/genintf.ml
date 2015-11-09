open Core.Std
open Async.Std

module Regex = Re2.Regex

let const_rex = Regex.create_exn "^const "
  
let type_conversion ?(is_return=false) ~module_prefix ts =
  let ts = String.strip ts in
  let ts = Re2.Regex.replace ~f:(fun _ -> "") const_rex ts |> Or_error.ok_exn in
  let wrap_return s = if is_return then "(" ^ s ^ ")" else s in
  if ts = "struct " ^ module_prefix ^ " *"
  then wrap_return "ptr t"
  else
    match String.strip ts with
    | "struct rtnl_addr *" -> wrap_return "ptr Address.t"
    | "struct nl_addr *"   -> wrap_return "ptr Netlink.Address.t"
    | "struct nl_sock *"   -> wrap_return "ptr Socket.t"
    | "char *"             -> "string"
    | "unsigned int"       -> "int"
    | other                ->
      if String.is_substring ~substring:" " other
      then wrap_return other
      else other
;;

let fdef_to_ocaml ~fdef_rex ~module_prefix fdef =
  match Regex.find_submatches fdef_rex fdef with
  | Ok [| Some _; Some return_type; Some fname; Some args |] ->
    let make_args args =
      String.split ~on:',' args
      |> List.map ~f:(type_conversion ~module_prefix)
      |> String.concat ~sep:" @-> "
    in
    let fname = String.strip fname in
    let return_type = String.strip return_type in
    printf "let %s = foreign \"%s\"\n(%s @-> returning %s)\n\n"
      fname fname (make_args args) (type_conversion ~is_return:true ~module_prefix return_type)
  | Error e ->
    eprintf "Ignoring non-matching extern statement: %s\n" (Error.to_string_hum e)
  | l ->
    let sexp_of_t =
      Tuple2.sexp_of_t
        String.sexp_of_t
        (Or_error.sexp_of_t (Array.sexp_of_t (Option.sexp_of_t String.sexp_of_t)))
    in
    failwiths "Function definition did not match the expected pattern" (fdef,l) sexp_of_t
;;

let () =
  Command.async
    ~summary:"Hacky netlink interface generator"
    Command.Spec.(
      empty
      +> anon ( "FILENAME" %: string)
      +> anon ( "MODULE_PREFIX" %: string)
    )
    (fun fname module_prefix () ->
       let fdef_rex_str = "^extern\\s(.+)" ^ module_prefix ^ "_([^\\(]+)\\(([^\\)]+)" in
       let fdef_rex = Regex.create_exn fdef_rex_str in
       Reader.file_lines fname
       >>| fun lines ->
       printf "let foreign fname = foreign ~from:libnl_route (\"%s_\"^fname)\n\n" module_prefix;
       List.foldi lines ~init:None
         ~f:(fun i fdef_in_progress l ->
             let l = String.strip l in
             if String.is_prefix ~prefix:"extern \"C\"" l
             then None
             else if String.is_prefix ~prefix:"extern" l
             then
               match fdef_in_progress with
               | None ->
                 if String.is_suffix ~suffix:";" l
                 then
                   begin
                     fdef_to_ocaml ~fdef_rex ~module_prefix l;
                     None
                   end
                 else Some [l]
               | Some _  -> failwithf "Unexpected extern at %d" i ()
             else
               match fdef_in_progress with
               | None -> None
               | Some fdef_in_progress ->
                 if String.is_suffix ~suffix:";" l
                 then
                   let fdef = List.rev (l::fdef_in_progress) |> String.concat ~sep:" " in
                   begin
                     fdef_to_ocaml ~fdef_rex ~module_prefix fdef;
                     None
                   end
                 else Some (l::fdef_in_progress))
       |> function
       | Some fdef_in_progress ->
         failwiths "Unexpected left over function definition" fdef_in_progress (List.sexp_of_t String.sexp_of_t)
       | None -> ())
  |> Command.run
;;
