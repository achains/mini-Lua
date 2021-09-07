open Lua_lib.Parser
open Lua_lib.Interpreter

let parse_result =
  apply PStatement.parse_all
    {|
s = 0
for i = 1, 10 do
  for j = 1, 10 do 
    s = s + 1
  end
end
|}

let () =
  match parse_result with
  | None -> print_string "error"
  | Some _ -> eval parse_result
