exception Unbound_Variable of string

type name = Name of string
[@@deriving show { with_path = false }]

type value =
  | VBool of bool
  | VInt of int 
  | VFloat of float
  | VString of string
  | VTable of expr list
  | VFunction of function_ref
[@@deriving show { with_path = false }]

and function_ref = 
  | NilFunction
  | Function of {func_name : string; args : value list}
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
   | If of expr * statement * statement option
   | While of expr * statement 
   | Break 
   | Return of expr
   | VarDec of (name * expr option) list 
   | Expression of expr
   | Block of statement
[@@deriving show { with_path = false }]