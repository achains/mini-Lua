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
    | VInt x, VInt y -> return @@  VFloat (Float.of_int x /. Float.of_int y)
    | VInt x, VFloat y -> return @@ VFloat (Float.of_int x /. y)
    | VFloat x, VInt y -> return @@ VFloat (x /. Float.of_int y)
    | _ -> error "Unsupported operands type for (/)"

  (* Integer division *)
  let ( /// ) lhs rhs =
    match (lhs, rhs) with
    | VInt x, VInt y -> return @@ VFloat (Float.floor (Float.of_int x /. Float.of_int y))
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

  let ( <<< ) lhs rhs = return (VBool true)
  
  let ( <<<= ) lhs rhs = return (VBool true)

  let ( >>> ) lhs rhs = return (VBool true)

  let ( >>>= ) lhs rhs = return (VBool true)

  let ( === ) lhs rhs = return (VBool true)
  
  let ( !=== ) lhs rhs = return (VBool true)

  let is_true = function 
    | VBool false -> false
    | VNull -> false
    | _ -> true

  let rec eval_expr env = function
    | Const v -> return v
    | Var v -> return @@ VString v 
    | ArOp (op, lhs, rhs) -> 
      let get_op = function
        | Sum -> ( ++ )
        | Sub -> ( -- )
        | Mul -> ( ** )
        | Div -> ( /// )
        | FDiv -> ( // )
        | Mod -> ( %% ) in
      eval_expr env lhs >>= fun e_lhs ->
      eval_expr env rhs >>= fun e_rhs ->
        (get_op op) e_lhs e_rhs
       
    | RelOp (op, lhs, rhs) -> 
      let get_op = function 
        | Le -> (<<<)
        | Leq -> (<<<=)
        | Ge -> (>>>)
        | Geq -> (>>>=)
        | Eq -> (===)
        | Neq -> (!===) in
      eval_expr env lhs >>= fun e_lhs ->
      eval_expr env rhs >>= fun e_rhs ->
        (get_op op) e_lhs e_rhs 
    
    | LogOp (op, lhs, rhs) -> 
      let get_op = function
        | And -> (&&)
        | Or -> (||) in
       eval_expr env lhs >>= fun e_lhs ->
       eval_expr env rhs >>= fun e_rhs ->
        return @@ VBool ((get_op op) (is_true e_lhs) (is_true e_rhs))
      
    | _ -> return (VString "")
end
