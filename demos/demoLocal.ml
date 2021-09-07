open Lua_lib.Parser
open Lua_lib.Interpreter

let parse_result =
  apply PStatement.parse_all
    {|
 x = 5 
 if true then
  local x = 10
  x = 15
 end
  |}

let () =
  match parse_result with
  | None -> print_string "error"
  | Some _ -> eval parse_result
