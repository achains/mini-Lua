open Lua_lib.Parser
open Lua_lib.Interpreter

let parse_result =
  apply PStatement.parse_all
    {|
  function true_inc(x)
    if x == 0 then
      return 1
    elseif x == 1 then 
      return 2
    elseif x == 2 then
      return 3
    else
      return -1
    end
  end
  
  result = true_inc(6)
  result
  |}

let () =
  match parse_result with
  | None -> print_string "error"
  | Some _ -> eval parse_result
