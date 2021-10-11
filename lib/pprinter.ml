(* open Ast
open Format

let string_of_arop = function
  | Sum -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | FDiv -> "/"
  | Div -> "//"
  | Mod -> "%"

let string_of_logop = function And -> "and" | Or -> "or"

let string_of_relop = function
  | Eq -> "=="
  | Neq -> "~="
  | Ge -> ">"
  | Geq -> ">="
  | Le -> "<"
  | Leq -> "<="

let string_of_unop = "not "

let pp_parens ppf fmt = fprintf fmt "(%a)" ppf

let pp_stmt fmt = function
  | IfElseBlock if_lst -> pp_print_string fmt ""
  | While (e, st) -> pp_print_string fmt ""
  | ForNumerical (n, e, st) -> pp_print_string fmt ""
  | Break -> pp_print_string fmt "break"
  | Local st -> pp_print_string fmt ""
  | Return e -> fprintf fmt "@[<v 0>return %a]@" pp_expr e 
  | VarDec  *)