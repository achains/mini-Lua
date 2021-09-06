open Lua_lib.Parser
open Lua_lib.Interpreter

let parse_result =
  apply PStatement.parse_all
    {|
function fac(n)
  if n <= 1 then
    return 1
  end
  return fac(n - 1) * n
end

c = fac(5)
|}

let () =
  match parse_result with
  | None -> print_string "error"
  | Some _ -> eval parse_result
