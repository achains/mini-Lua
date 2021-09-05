open Lua_lib.Parser
open Lua_lib.Interpreter

let parse_result =
  apply PStatement.parse_all
    {|
function factorial(n)
  if n <= 1 then
    return 1
  end
  return factorial(n - 1) * n
end

result = factorial(5)
result
|}

let () =
  match parse_result with
  | None -> print_string "error"
  | Some _ -> eval parse_result
