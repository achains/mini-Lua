open Lua_lib.Parser
open Lua_lib.Interpreter

let parse_result =
  apply PStatement.parse_all
    {|
s = 0
for i = 1, 5 do 
  if i % 2 == 0 then
    break
  end
  s = s + 1
end
|}

let () =
  match parse_result with
  | None -> print_string "error"
  | Some _ -> eval parse_result
