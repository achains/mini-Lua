exception Unbound_Variable of string

type value =
  | VBool of bool
  | VInt of int 
  | VFloat of float
  | VString of string
  | VTable of expr list
[@@deriving show { with_path = false }]

and expr = 
  | Const of value
  | Var of string 
  | Sum of expr * expr 
  | Sub of expr * expr 
  | Mul of expr * expr
  | Div of expr * expr  (* Integer division, '//'*)
  | FDiv of expr * expr (* Float division, '/' *)
  | And of expr * expr 
  | Or of expr * expr
  | Not of expr
  | Eq of expr * expr
  | Neq of expr * expr 
  | Le of expr * expr 
  | Leq of expr * expr
  | Ge of expr * expr 
  | Geq of expr * expr 
  | TableAccess of expr * expr 
  | CallFunc of expr * expr list
  | Assign of expr * expr
  | Null
[@@deriving show { with_path = false }]

type statement =
   | If of (expr * statement) list
   | While of expr * statement 
   | ForNumerical of expr * expr list * statement (* for a(expr) = 1, 5, 2 (expr list) do <(statement)> end *)
   | Break 
   | Local of statement
   | Return of expr
   | VarDec of (string * expr) list 
   | Expression of expr
   | Block of statement list
   | FuncDec of string * (string list) * statement (* func_name * args * block *)
[@@deriving show { with_path = false }]