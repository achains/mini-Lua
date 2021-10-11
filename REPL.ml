open Lua_lib.Interpreter
open Lua_lib.Parser

(* REMOVE THIS MADNESS *)
open Eval (Result)

let rec print_lst = function
  | [] -> print_endline "[]\n=======\n"
  | hd :: tl ->
      print_endline @@ show_environment hd;
      print_lst tl

let rec repl env_lst buffer =
  let str = print_string ">>> "; read_line () in
  let check_end_of_input s =
    let is_end_of_input = function
      | "" -> false
      | s -> s.[String.length s - 1] == '@' in
    let del_end_of_input s = String.sub s 0 (String.length s - 1) in
    if is_end_of_input s then (
      Buffer.add_string buffer (del_end_of_input s);
      match PStatement.parse_prog (Buffer.contents buffer) with
      | Some parsed_prog -> (
        match parsed_prog with
        | Block b -> (
          match eval_block env_lst b with
          | Ok res -> 
             let last_value = (List.hd res).last_value in
             if (string_of_value last_value <> "nil") then print_endline @@ string_of_value last_value; 
             Buffer.clear buffer; repl res buffer
          | Error msg ->
              print_endline msg; Buffer.clear buffer; repl env_lst buffer )
        | _ -> () )
      | None ->
          print_endline "Error: Syntax error occured";
          Buffer.clear buffer;
          repl env_lst buffer )
    else Buffer.add_string buffer (str ^ "\n");
    repl env_lst buffer in
  check_end_of_input str

let () =
  print_endline "===== Lua REPL =====";
  print_endline "Each command should end with '@' character";
  let initial_env =
    [ { vars= Hashtbl.create 16
      ; last_value= VNull
      ; is_func= false
      ; is_loop= false
      ; jump_stmt= Default } ] in
  let buffer = Buffer.create 1024 in
  repl initial_env buffer
