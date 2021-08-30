open Ast

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( >> ) : 'a t -> 'b t -> 'b t
end

module type MONADERROR = sig
  include MONAD

  val error : string -> 'a t
end

module Result = struct
  type 'a t = ('a, string) Result.t

  let ( >>= ) = Result.bind

  let return = Result.ok

  let error = Result.error

  let ( >> ) x f = x >>= fun _ -> f
end

module Eval (M : MONADERROR) = struct
  open M

  let ( ++ ) lhs rhs =
    match (lhs, rhs) with
    | VInt x, VInt y -> return @@ VInt (x + y)
    | VInt x, VFloat y -> return @@ VFloat (Float.of_int x +. y)
    | VFloat x, VInt y -> return @@ VFloat (x +. Float.of_int y)
    | _ -> error "Unsupported operands type for (+)"

  let ( -- ) lhs rhs =
    match (lhs, rhs) with
    | VInt x, VInt y -> return @@ VInt (x - y)
    | VInt x, VFloat y -> return @@ VFloat (Float.of_int x -. y)
    | VFloat x, VInt y -> return @@ VFloat (x -. Float.of_int y)
    | _ -> error "Unsupported operands type for (-)"

  let ( ** ) lhs rhs =
    match (lhs, rhs) with
    | VInt x, VInt y -> return @@ VInt (x * y)
    | VInt x, VFloat y -> return @@ VFloat (Float.of_int x *. y)
    | VFloat x, VInt y -> return @@ VFloat (x *. Float.of_int y)
    | _ -> error "Unsupported operands type for (*)"

  let ( // ) lhs rhs =
    match (lhs, rhs) with
    | VInt x, VInt y -> return @@ VFloat (Float.of_int x /. Float.of_int y)
    | VInt x, VFloat y -> return @@ VFloat (Float.of_int x /. y)
    | VFloat x, VInt y -> return @@ VFloat (x /. Float.of_int y)
    | _ -> error "Unsupported operands type for (/)"

  (* Integer division *)
  let ( /// ) lhs rhs =
    match (lhs, rhs) with
    | VInt x, VInt y ->
        return @@ VFloat (Float.floor (Float.of_int x /. Float.of_int y))
    | VInt x, VFloat y -> return @@ VFloat (Float.floor (Float.of_int x /. y))
    | VFloat x, VInt y -> return @@ VFloat (Float.floor (x /. Float.of_int y))
    | _ -> error "Unsupported operands type for (//)"

  (* '%' in Lua is an actual modulo, but for temporary simplicity remainder had been realised *)
  let ( %% ) lhs rhs =
    match (lhs, rhs) with
    | VInt x, VInt y -> return @@ VInt (x mod y)
    | VInt x, VFloat y ->
        let fx = Float.of_int x in
        return @@ VFloat (fx -. (y *. Float.floor (fx /. y)))
    | VFloat x, VInt y ->
        let fy = Float.of_int y in
        return @@ VFloat (fy -. (x *. Float.floor (fy /. x)))
    | _ -> error "Unsupported operands type for (%)"

  let ( <<< ) _ _ = return (VBool true)

  let ( <<<= ) _ _ = return (VBool true)

  let ( >>> ) _ _ = return (VBool true)

  let ( >>>= ) _ _ = return (VBool true)

  let ( === ) _ _ = return (VBool true)

  let ( !=== ) _ _ = return (VBool true)

  let is_true = function VBool false -> false | VNull -> false | _ -> true

  let string_of_value = function
    | VInt v -> string_of_int v
    | VFloat v -> string_of_float v
    | VString v -> v
    | VBool v -> string_of_bool v
    | _ -> failwith "Unsupported type to transform to string"

  (* ==== Enviroment ==== *)

  module Env = struct
    type name = string [@@deriving show { with_path = false }]

    type variables = (name, expr) Hashtbl.t

    type enviroment = { var_ht : variables; func_ht : variables }

    and env_lst = enviroment list

    let rec find_var varname = function
      | [] -> return @@ VNull
      | hd :: tl -> (
          match Hashtbl.find_opt hd.var_ht varname with
          | Some (Const v) -> return @@ v
          | Some _ -> error "Invalid enviroment value"
          | None -> find_var varname tl)
  end

  let rec eval_expr env = function
    | Const v -> return v
    | ArOp (op, lhs, rhs) ->
        let get_op = function
          | Sum -> ( ++ )
          | Sub -> ( -- )
          | Mul -> ( ** )
          | Div -> ( /// )
          | FDiv -> ( // )
          | Mod -> ( %% )
        in
        eval_expr env lhs >>= fun e_lhs ->
        eval_expr env rhs >>= fun e_rhs -> (get_op op) e_lhs e_rhs
    | RelOp (op, lhs, rhs) ->
        let get_op = function
          | Le -> ( <<< )
          | Leq -> ( <<<= )
          | Ge -> ( >>> )
          | Geq -> ( >>>= )
          | Eq -> ( === )
          | Neq -> ( !===)
        in
        eval_expr env lhs >>= fun e_lhs ->
        eval_expr env rhs >>= fun e_rhs -> (get_op op) e_lhs e_rhs
    | LogOp (op, lhs, rhs) ->
        let get_op = function And -> ( && ) | Or -> ( || ) in
        eval_expr env lhs >>= fun e_lhs ->
        eval_expr env rhs >>= fun e_rhs ->
        return @@ VBool ((get_op op) (is_true e_lhs) (is_true e_rhs))
    | UnOp (_, x) ->
        eval_expr env x >>= fun e_x -> return @@ VBool (not (is_true e_x))
    | Var v -> Env.find_var v env
    | TableCreate el -> table_create env el
    | TableAccess (tname, texpr) -> table_find env tname texpr
    | CallFunc (_, _) -> return @@ VInt 1
    | _ -> error "Unexpected expression"

  and table_append ht env key = function
    | [] -> return @@ VTable ht
    | hd :: tl -> (
        match hd with
        | Assign (x, y) ->
            eval_expr env x >>= fun lhs ->
            eval_expr env y >>= fun rhs ->
            Hashtbl.replace ht (string_of_value lhs) rhs;
            table_append ht env key tl
        | _ ->
            eval_expr env hd >>= fun v ->
            Hashtbl.replace ht (string_of_int key) v;
            table_append ht env (key + 1) tl)

  and table_create env elist =
    let ht = Hashtbl.create 100 in
    table_append ht env 1 elist

  and table_find env tname texpr =
    let find_opt ht key =
      match Hashtbl.find_opt ht key with
         | Some v -> return @@ v
         | None -> return @@ VNull
    in 
    Env.find_var tname env >>= function
     | VTable ht -> eval_expr env texpr >>= (function
         | VInt key -> find_opt ht (string_of_int key)
         | VString key -> find_opt ht key
         | _ -> error "Invalid key value")
     | _ -> error "Attemt to index non-table value"
end
