open Lua_lib.Parser
open Lua_lib.Interpreter

let parse_result =
  PStatement.parse_prog
    {|
function foo()
   return "foo"
end

result = poo()
|}

let () = eval parse_result

(* Expected 'Attempt to call not a function value. poo is nil, so we can't call nil *)
