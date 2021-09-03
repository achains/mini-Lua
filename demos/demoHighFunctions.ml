open Lua_lib.Parser
open Lua_lib.Interpreter

let parse_result =
  apply PStatement.parse_all
    {|
function binop(x, y, op)
   return op(x, y)
end

function add(x, y)
   return x + y
end

function sub(x, y)
   return x - y
end

c = binop(sub(10, 5), 7, add)
c
|}

let () =
  match parse_result with
  | None -> print_string "error"
  | Some _ -> eval parse_result
