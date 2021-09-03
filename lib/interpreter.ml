open Ast
open Hashtbl_p

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
    | VTable _ -> "<table>"
    | VFunction _ -> "<function>"
    | VNull -> "nil"

  (* ==== Enviroment ==== *)

  type variables = (name, value) Hashtbl_p.t
  [@@deriving show {with_path= false}]

  type enviroment =
    { vars: variables
    ; last_value: value
    ; prev_env: enviroment option
    ; is_func: bool
    ; is_loop: bool }
  [@@deriving show {with_path= false}]

  let rec find_var varname = function
    | None -> return @@ VNull
    | Some env -> (
      match Hashtbl.find_opt env.vars varname with
      | Some v -> return @@ v
      | None -> find_var varname env.prev_env )

  let rec eval_expr env = function
    | Const v -> return v
    | ArOp (op, lhs, rhs) ->
        let get_op = function
          | Sum -> ( ++ )
          | Sub -> ( -- )
          | Mul -> ( ** )
          | Div -> ( /// )
          | FDiv -> ( // )
          | Mod -> ( %% ) in
        eval_expr env lhs
        >>= fun e_lhs ->
        eval_expr env rhs >>= fun e_rhs -> (get_op op) e_lhs e_rhs
    | RelOp (op, lhs, rhs) ->
        let get_op = function
          | Le -> ( <<< )
          | Leq -> ( <<<= )
          | Ge -> ( >>> )
          | Geq -> ( >>>= )
          | Eq -> ( === )
          | Neq -> ( !=== ) in
        eval_expr env lhs
        >>= fun e_lhs ->
        eval_expr env rhs >>= fun e_rhs -> (get_op op) e_lhs e_rhs
    | LogOp (op, lhs, rhs) ->
        let get_op = function And -> ( && ) | Or -> ( || ) in
        eval_expr env lhs
        >>= fun e_lhs ->
        eval_expr env rhs
        >>= fun e_rhs ->
        return @@ VBool ((get_op op) (is_true e_lhs) (is_true e_rhs))
    | UnOp (_, x) ->
        eval_expr env x >>= fun e_x -> return @@ VBool (not (is_true e_x))
    | Var v -> find_var v env
    | TableCreate el -> table_create env el
    | TableAccess (tname, texpr) -> table_find env tname texpr
    | CallFunc (n, el) -> (
        set_is_func true env
        >>= fun f_en ->
        func_call f_en n el
        >>= fun e ->
        match e with
        | Some x -> return x.last_value
        | None -> error "Out of scope" )
    | _ -> error "Unexpected expression"

  and table_append ht env key = function
    | [] -> return @@ VTable ht
    | hd :: tl -> (
      match hd with
      | Assign (x, y) ->
          eval_expr env x
          >>= fun lhs ->
          eval_expr env y
          >>= fun rhs ->
          Hashtbl.replace ht (string_of_value lhs) rhs;
          table_append ht env key tl
      | _ ->
          eval_expr env hd
          >>= fun v ->
          Hashtbl.replace ht (string_of_int key) v;
          table_append ht env (key + 1) tl )

  and table_create env elist =
    let ht = Hashtbl.create 16 in
    table_append ht env 1 elist

  and table_find env tname texpr =
    let find_opt ht key =
      match Hashtbl.find_opt ht key with
      | Some v -> return @@ v
      | None -> return @@ VNull in
    find_var tname env
    >>= function
    | VTable ht -> (
        eval_expr env texpr
        >>= function
        | VInt key -> find_opt ht (string_of_int key)
        | VString key -> find_opt ht key
        | _ -> error "Invalid key value" )
    | _ -> error "Attempt to index non-table value"

  and func_call env fname fargs =
    let create_vardec lnames lexprs =
      let rec helper l1 l2 acc =
        match (l1, l2) with
        | [], [] -> acc
        | hd1 :: tl1, hd2 :: tl2 -> (hd1, hd2) :: helper tl1 tl2 acc
        | hd1 :: tl1, [] -> (hd1, Const VNull) :: helper tl1 [] acc
        | [], _ :: _ -> acc in
      helper lnames lexprs [] in
    find_var fname env
    >>= function
    | VFunction (name_args, body) ->
        let var_args = List.map (fun x -> Var x) name_args in
        let block_with_vardec = function
          | Block b ->
              return
              @@ Block (Local (VarDec (create_vardec var_args fargs)) :: b)
          | _ -> error "Expected function body" in
        block_with_vardec body >>= fun b -> eval_stmt env b
    | _ -> error "Attempt to call not a function value"

  and eval_stmt env = function
    | Expression e ->
        eval_expr env e
        >>= fun v -> set_last_value v env >>= fun en -> return @@ en
    | VarDec el ->
        eval_vardec true env el
        >>= fun _ -> set_last_value VNull env >>= fun en -> return en
    | Local (VarDec el) ->
        eval_vardec false env el
        >>= fun _ -> set_last_value VNull env >>= fun en -> return en
    | FuncDec (n, args, b) ->
        assign n (VFunction (args, b)) true env
        >>= fun _ -> set_last_value VNull env >>= fun en -> return en
    | Local (FuncDec (n, args, b)) ->
        assign n (VFunction (args, b)) false env
        >>= fun _ -> set_last_value VNull env >>= fun en -> return en
    | Block b -> create_next_block env >>= fun e -> eval_block (Some e) b
    | Return _ -> error "Unexpected return statement"
    | Break -> error "Unexpected break statement"
    | _ -> error "Unknown statement"

  and set_last_value v = function
    | None -> error "Operation out of scope"
    | Some env -> return @@ Some {env with last_value= v}

  and set_is_func flag = function
    | None -> error "Operation out of scope"
    | Some env -> return @@ Some {env with is_func= flag}

  and set_is_loop flag = function
    | None -> error "Operation out of scope"
    | Some env -> return @@ Some {env with is_loop= flag}

  and create_next_block = function
    | None -> error "Operation out of scope"
    | Some env ->
        return @@ {env with prev_env= Some env; vars= Hashtbl.create 16}

  and eval_vardec is_global env = function
    | [] -> set_last_value VNull env
    | hd :: tl -> (
      match hd with
      | Var x, e ->
          eval_expr env e
          >>= fun v ->
          assign x v is_global env >>= fun _ -> eval_vardec is_global env tl
      | _ -> error "Wrong type to assign to" )

  and assign n v is_global env =
    let rec set_global n v env =
      match env.prev_env with
      | None -> Hashtbl.replace env.vars n v
      | Some pe -> set_global n v pe in
    match env with
    | None -> error "Operation out of scope"
    | Some e ->
        (*print_string(show_enviroment e);*)
        if is_global then return @@ set_global n v e
        else return @@ Hashtbl.replace e.vars n v

  and eval_block env = function
    | [] -> return @@ env
    | [tl] -> (
      match tl with
      | Return v ->
          eval_expr env v
          >>= fun r -> set_last_value r env >>= fun e -> return @@ e
      | _ -> eval_stmt env tl )
    | hd :: tl -> eval_stmt env hd >>= fun e -> eval_block e tl

  and eval_prog env = function
    | None -> return VNull
    | Some p -> (
        eval_stmt env p
        >>= fun en ->
        match en with
        | None -> return @@ VNull
        | Some e -> (*print_string(show_enviroment e);*) return @@ e.last_value
        )
end

open Eval (Result)

let eval parsed_prog =
  let start_env =
    { vars= Hashtbl.create 16
    ; last_value= VNull
    ; prev_env= None
    ; is_func= false
    ; is_loop= false } in
  match eval_prog (Some start_env) parsed_prog with
  | Ok res -> print_string (string_of_value res)
  | Error m -> print_endline m
