open Lua_lib.Parser
open Lua_lib.Interpreter

let parse_result = apply PStatement.parse_all 
{|

c = 4
function foo(x, y)
   local c = 3
end

foo()
c
|}

let () = 
   match parse_result with
   | None -> print_string "error"
   | Some _ -> eval parse_result