open Ast
open Hashtbl_p
open Var_zipper

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

  let transform_to_number x y err_msg =
    let string_is_number x =
      try
        float_of_string x |> ignore;
        true
      with Failure _ -> false in
    match (x, y) with
    | VNumber x, VNumber y -> return (x, y)
    | VNumber x, VString y when string_is_number y ->
        return (x, float_of_string y)
    | VString x, VNumber y when string_is_number x ->
        return (float_of_string x, y)
    | _ -> error err_msg

  (* Integer division *)
  let ( /// ) x y = Float.floor (x /. y)

  (* Remainder *)
  let ( %% ) x y =
    let int_part = x /// y in
    x -. (int_part *. y)

  let compare_values lhs rhs op =
    let get_op = function
      | Le -> return ( < )
      | Leq -> return ( <= )
      | Ge -> return ( > )
      | Geq -> return ( >= )
      | Eq -> return ( = )
      | Neq -> return ( <> ) in
    match op with
    | Le | Leq | Ge | Geq -> (
      match (lhs, rhs) with
      | VNumber x, VNumber y -> get_op op >>= fun op -> return @@ VBool (op x y)
      | VString x, VString y -> get_op op >>= fun op -> return @@ VBool (op x y)
      | _, _ ->
          error
            "Error: Comparison can be performed only between numbers or strings"
      )
    | Eq | Neq -> (
      match (lhs, rhs) with
      | VNumber x, VNumber y -> get_op op >>= fun op -> return @@ VBool (op x y)
      | VString x, VString y -> get_op op >>= fun op -> return @@ VBool (op x y)
      | VBool x, VBool y -> get_op op >>= fun op -> return @@ VBool (op x y)
      | _ -> return @@ VBool false )

  let is_true = function VBool false -> false | VNull -> false | _ -> true

  let string_of_value = function
    | VNumber v -> string_of_float v
    | VString v -> v
    | VBool v -> string_of_bool v
    | VTable _ -> "<table>"
    | VFunction _ -> "<function>"
    | VNull -> "nil"

  (* ==== Environment ==== *)

  type variables = (name, value) Hashtbl_p.t
  [@@deriving show {with_path= false}]

  type jump_statement = Default | Return | Break
  [@@deriving show {with_path= false}]

  type environment =
    { vars: variables
    ; last_value: value
    ; is_func: bool
    ; is_loop: bool
    ; jump_stmt: jump_statement
    ; last_env: environment option }
  (* last_env is needed in case we want to show variables scope after interpretation *)
  [@@deriving show {with_path= false}]

  type env_lst = environment list

  let rec find_var varname = function
    | [] -> return VNull
    | hd_env :: tl -> (
      match Hashtbl.find_opt hd_env.vars varname with
      | Some v -> return v
      | None -> find_var varname tl )

  let rec eval_expr env_lst = function
    | Const v -> return v
    | ArOp (op, lhs, rhs) -> (
        let get_op = function
          | Sum -> (( +. ), "Error: Unsupported operands type for (+)")
          | Sub -> (( -. ), "Error: Unsupported operands type for (-)")
          | Mul -> (( *. ), "Error: Unsupported operands type for (*)")
          | Div -> (( /// ), "Error: Unsupported operands type for (//)")
          | FDiv -> (( /. ), "Error: Unsupported operands type for (/)")
          | Mod -> (( %% ), "Error: Unsupported operands type for (%)") in
        eval_expr env_lst lhs
        >>= fun e_lhs ->
        eval_expr env_lst rhs
        >>= fun e_rhs ->
        match get_op op with
        | op, err_msg ->
            transform_to_number e_lhs e_rhs err_msg
            >>= fun (x, y) -> return @@ VNumber (op x y) )
    | RelOp (op, lhs, rhs) ->
        eval_expr env_lst lhs
        >>= fun e_lhs ->
        eval_expr env_lst rhs >>= fun e_rhs -> compare_values e_lhs e_rhs op
    | LogOp (op, lhs, rhs) ->
        let get_op = function And -> ( && ) | Or -> ( || ) in
        eval_expr env_lst lhs
        >>= fun e_lhs ->
        eval_expr env_lst rhs
        >>= fun e_rhs ->
        return @@ VBool ((get_op op) (is_true e_lhs) (is_true e_rhs))
    | UnOp (_, x) ->
        eval_expr env_lst x >>= fun e_x -> return @@ VBool (not (is_true e_x))
    | Var v -> find_var v env_lst
    | TableCreate el -> table_create env_lst el
    | TableAccess (tname, texpr) -> table_find env_lst tname texpr
    | CallFunc (n, el) -> (
        func_call env_lst n el
        >>= fun e ->
        get_cur_env e
        >>= fun env ->
        match env.jump_stmt with
        | Return -> return env.last_value
        | _ -> return VNull )
    | _ -> error "Error: Unexpected expression"

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
    let ht = Hashtbl.create 256 in
    table_append ht env 1 elist

  and table_find env tname texpr =
    let find_opt ht key =
      match Hashtbl.find_opt ht key with
      | Some v -> return v
      | None -> return VNull in
    find_var tname env
    >>= function
    | VTable ht -> (
        eval_expr env texpr
        >>= function
        | VNumber key -> find_opt ht (string_of_float key)
        | VString key -> find_opt ht key
        | _ -> error "Error: Invalid key value" )
    | _ -> error "Error: Attempt to index non-table value"

  and func_call env_lst fname fargs =
    find_var fname env_lst
    >>= function
    | VFunction (name_args, body) ->
        let var_args = List.map (fun x -> Var x) name_args in
        let block_with_vardec = function
          | Block b ->
              return @@ Block (Local (VarDec (var_zipper var_args fargs)) :: b)
          | _ -> error "Error: Expected function body" in
        block_with_vardec body
        >>= fun b ->
        set_hd_is_func ~is_func:true env_lst
        >>= fun env_lst -> eval_stmt env_lst b
    | _ -> error "Error: Attempt to call not a function value"

  and set_hd_is_func ?(is_func = true) = function
    | [] ->
        error
          "Error: Can't modify environment head with <is_func>. Head is absent!"
    | hd :: tl -> return @@ ({hd with is_func} :: tl)

  and set_hd_is_loop ?(is_loop = true) = function
    | [] ->
        error
          "Error: Can't modify environment head with <is_loop>. Head is absent!"
    | hd :: tl -> return @@ ({hd with is_loop} :: tl)

  and set_hd_last_value last_value = function
    | [] ->
        error
          "Error: Can't modify environment head with <last_value>. Head is \
           absent!"
    | hd :: tl -> return @@ ({hd with last_value} :: tl)

  and set_hd_vars vars = function
    | [] ->
        error
          "Error: Can't modify environment head with <vars>. Head is absent!"
    | hd :: tl -> return @@ ({hd with vars} :: tl)

  and set_hd_jump_stmt jump_stmt = function
    | [] ->
        error
          "Error: Can't modify environment head with <jump_stmt>. Head is \
           absent!"
    | hd :: tl -> return @@ ({hd with jump_stmt} :: tl)

  and modify_hd_vars value = function
    | [] ->
        error "Error: Can't modify environment head variables. Head is absent!"
    | hd :: _ -> (
      match value with name, vle -> return @@ Hashtbl.replace hd.vars name vle )

  and get_prev_env = function [] -> [] | _ :: tl -> tl

  and get_cur_env = function
    | [] -> error "Error: Current environment is absent!"
    | hd :: _ -> return hd

  and eval_stmt env_lst = function
    | Expression e ->
        eval_expr env_lst e
        >>= fun v ->
        set_hd_last_value v env_lst >>= fun env_lst -> return env_lst
    | VarDec el -> eval_vardec true env_lst el
    | Local (VarDec el) -> eval_vardec false env_lst el
    | FuncDec (n, args, b) ->
        assign n (VFunction (args, b)) true env_lst
        >>= fun _ -> set_hd_last_value VNull env_lst
    | Local (FuncDec (n, args, b)) ->
        assign n (VFunction (args, b)) false env_lst
        >>= fun _ -> set_hd_last_value VNull env_lst
    | Local _ -> error "Error: Invalid local statement"
    | IfElseBlock if_lst -> eval_if env_lst if_lst
    | ForNumerical (fvar, finit, body) ->
        set_hd_is_loop env_lst
        >>= fun env_lst -> eval_for fvar finit body env_lst
    | Block b -> create_next_env env_lst >>= fun env_lst -> eval_block env_lst b
    | Return _ -> error "Error: Unexpected return statement"
    | Break -> error "Error: Unexpected break statement"
    | While (cond, body) ->
        set_hd_is_loop env_lst >>= fun env_lst -> eval_while cond body env_lst

  and create_next_env = function
    | [] ->
        return
          [ { vars= Hashtbl.create 16
            ; last_value= VNull
            ; is_func= false
            ; is_loop= false
            ; jump_stmt= Default
            ; last_env= None } ]
    | hd_env :: tl ->
        return @@ ({hd_env with vars= Hashtbl.create 16} :: hd_env :: tl)

  and eval_vardec is_global env_lst = function
    | [] -> set_hd_last_value VNull env_lst >>= fun env_lst -> return env_lst
    | hd :: tl -> (
      match hd with
      | Var x, e ->
          eval_expr env_lst e
          >>= fun v ->
          assign x v is_global env_lst >>= fun en -> eval_vardec is_global en tl
      | TableAccess (tname, index), e -> (
          eval_expr env_lst e
          >>= fun val_to_assign ->
          eval_expr env_lst index
          >>= fun key ->
          find_var tname env_lst
          >>= function
          | VTable t ->
              Hashtbl.replace t (string_of_value key) val_to_assign;
              eval_vardec is_global env_lst tl
          | _ -> error "Attempt to index non-table value" )
      | _ -> error "Wrong type to assign to" )

  and assign n v is_global env_lst =
    let rec set_global n v = function
      | [] -> ()
      | [hd] -> Hashtbl.replace hd.vars n v
      | hd :: tl -> (
        match Hashtbl.find_opt hd.vars n with
        | None -> set_global n v tl
        | Some _ -> Hashtbl.replace hd.vars n v ) in
    if is_global then (set_global n v env_lst; return env_lst)
    else modify_hd_vars (n, v) env_lst >>= fun _ -> return env_lst

  and eval_if env_lst = function
    | [] -> return env_lst
    | hd :: tl -> (
      match hd with
      | If (cond, st) | Elif (cond, st) ->
        eval_expr env_lst cond
        >>= fun cond ->
        if is_true cond then eval_stmt env_lst st else eval_if env_lst tl
      | Else st -> eval_stmt env_lst st)

  and eval_block env_lst = function
    | [] -> return @@ get_prev_env env_lst
    | [tl] -> (
        get_cur_env env_lst
        >>= fun cur_env ->
        match tl with
        | Return v when cur_env.is_func -> eval_return env_lst v
        | Break when cur_env.is_loop -> eval_break env_lst
        | Break -> error "Error: Unexpected break statement"
        | Return _ -> error "Error: Unexpected return statement"
        | _ -> (
            eval_stmt env_lst tl
            >>= fun env_lst ->
            get_cur_env env_lst
            >>= fun cur_env ->
            match cur_env.jump_stmt with
            | Return ->
                let prev_env = get_prev_env env_lst in
                set_hd_last_value cur_env.last_value prev_env
                >>= fun prev_env -> set_hd_jump_stmt Return prev_env
            | Break -> eval_break env_lst
            | _ -> (
                let prev_env = get_prev_env env_lst in
                match prev_env with
                | [] -> return env_lst
                | _ -> return prev_env ) ) )
    | hd :: tl -> (
        eval_stmt env_lst hd
        >>= fun env_lst ->
        get_cur_env env_lst
        >>= fun cur_env ->
        match cur_env.jump_stmt with
        | Return ->
            let prev_env = get_prev_env env_lst in
            set_hd_last_value cur_env.last_value prev_env
            >>= fun prev_env -> set_hd_jump_stmt Return prev_env
        | Break -> eval_break env_lst
        | _ -> eval_block env_lst tl )

  and eval_return env_lst e =
    eval_expr env_lst e
    >>= fun v ->
    let prev_env = get_prev_env env_lst in
    set_hd_last_value v prev_env
    >>= fun prev_env -> set_hd_jump_stmt Return prev_env

  and eval_break env_lst =
    let prev_env = get_prev_env env_lst in
    set_hd_jump_stmt Break prev_env

  and eval_while cond body env_lst =
    eval_expr env_lst cond
    >>= fun predicate ->
    if is_true predicate then
      eval_stmt env_lst body
      >>= fun env_lst ->
      get_cur_env env_lst
      >>= fun cur_env ->
      match cur_env.jump_stmt with
      | Break -> set_hd_jump_stmt Default env_lst
      | Return -> set_hd_jump_stmt Return env_lst
      | _ -> eval_while cond body env_lst
    else return env_lst

  and eval_for fvar finit body env_lst =
    let check_expr_number env_lst num =
      eval_expr env_lst num
      >>= function
      | VNumber v -> return v
      | _ -> error "bad 'for' initial value (number expected)" in
    let cond_to_floats = function
      | [start; stop; step] ->
          check_expr_number env_lst start
          >>= fun start ->
          check_expr_number env_lst stop
          >>= fun stop ->
          check_expr_number env_lst step
          >>= fun step -> return [start; stop; step]
      | [start; stop] ->
          check_expr_number env_lst start
          >>= fun start ->
          check_expr_number env_lst stop >>= fun stop -> return [start; stop; 1.]
      | _ -> error "Bad 'for' constructor" in
    cond_to_floats finit >>= fun finit -> for_numerical fvar finit body env_lst

  and for_numerical fvar finit body env_lst =
    let create_local_vardec var value = Local (VarDec [(var, value)]) in
    let declare_init fvar start = function
      | Block b ->
          return
          @@ Block (create_local_vardec (Var fvar) (Const (VNumber start)) :: b)
      | _ -> error "Expected for body" in
    let rec helper start stop step body env_lst =
      if start > stop then return env_lst
      else
        declare_init fvar start body
        >>= fun body_with_init ->
        eval_stmt env_lst body_with_init
        >>= fun env_lst ->
        get_cur_env env_lst
        >>= fun cur_env ->
        match cur_env.jump_stmt with
        | Break -> set_hd_jump_stmt Default env_lst
        | Return -> set_hd_jump_stmt Return env_lst
        | _ -> helper (start +. step) stop step body env_lst in
    match finit with
    | [start; stop; step] -> helper start stop step body env_lst
    | _ -> error "Bad 'for' constructor"

  and debug_print_env_lst = function
    | [] -> print_endline "[ ]"
    | hd :: tl ->
        print_endline (show_environment hd);
        debug_print_env_lst tl

  and eval_prog = function
    | None -> error "Syntax error occured"
    | Some p -> eval_stmt [] p
end

open Eval (Result)

(* Function to run interpreter *)

let eval parsed_prog =
  match eval_prog parsed_prog with
  | Ok res -> print_endline @@ show_environment @@ List.hd res
  | Error msg -> print_endline msg
