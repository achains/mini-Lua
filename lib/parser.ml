open Ast
open Opal

let reserved =
  [
    "and";
    "break";
    "do";
    "else";
    "elseif";
    "end";
    "false";
    "for";
    "function";
    "if";
    "in";
    "local";
    "nil";
    "not";
    "or";
    "repeat";
    "return";
    "then";
    "true";
    "until";
    "while";
  ]

let apply p s = parse p (LazyStream.of_string s)

let parens = between (token "(") (token ")")

let int_parser = spaces >> many1 digit => implode % int_of_string

let float_parser =
  spaces >> many1 digit >>= fun int_part ->
  exactly '.' >> many digit >>= fun float_part ->
  return (float_of_string (implode (int_part @ ('.' :: float_part))))

module PExpression = struct
  let initial = letter <|> exactly '_'

  let subseqt = alpha_num <|> exactly '_'

  let ident =
    spaces >> initial <~> many subseqt => implode >>= function
    | s when List.mem s reserved -> mzero
    | s -> return s

  (* Atomic expressions *)
  let const_int = int_parser >>= fun n -> return (Const (VInt n))

  let const_float = float_parser >>= fun n -> return (Const (VFloat n))

  let const_number = const_float <|> const_int

  let const_bool =
    token "true"
    >> return (Const (VBool true))
    <|> (token "false" >> return (Const (VBool false)))

  let const_null = token "nil" >> return Null

  let const_var = ident => fun x -> Var x

  let const_string =
    let string_of_chars chars =
      let buf = Buffer.create 16 in
      List.iter (Buffer.add_char buf) chars;
      Buffer.contents buf
    in
    token "\"" >> many (satisfy (fun c -> c != '\"')) >>= fun list ->
    token "\"" >> return (Const (VString (string_of_chars list)))

  (* Arithmetic operators *)
  let add_op = token "+" >> return (fun x y -> Sum (x, y))

  let sub_op = token "-" >> return (fun x y -> Sub (x, y))

  let mul_op = token "*" >> return (fun x y -> Mul (x, y))

  let fdiv_op = token "/" >> return (fun x y -> FDiv (x, y))

  let div_op = token "//" >> return (fun x y -> Div (x, y))

  let mod_op = token "%" >> return (fun x y -> Mod (x, y))

  (* Relational operators *)
  let le_op = token "<" >> return (fun x y -> Le (x, y))

  let leq_op = token "<=" >> return (fun x y -> Leq (x, y))

  let ge_op = token ">" >> return (fun x y -> Ge (x, y))

  let geq_op = token ">=" >> return (fun x y -> Geq (x, y))

  let eq_op = token "==" >> return (fun x y -> Eq (x, y))

  let neq_op = token "~=" >> return (fun x y -> Neq (x, y))

  (* Logical operators *)
  let and_op = token "and" >> return (fun x y -> And (x, y))

  let or_op = token "or" >> return (fun x y -> Or (x, y))

  let not_op = token "not" >> return (fun x -> Not x)

  (* Expression parser *)
  let rec expr input = (chainl1 and_expr or_op) input

  and and_expr input = (chainl1 relational_expr and_op) input

  and relational_expr input =
    (chainl1 add_expr
       (leq_op <|> geq_op <|> le_op <|> ge_op <|> eq_op <|> neq_op))
      input

  and add_expr input = (chainl1 mul_expr (add_op <|> sub_op)) input

  and mul_expr input =
    (chainl1 unary_expr (mul_op <|> mod_op <|> div_op <|> fdiv_op)) input

  and unary_expr input =
    (token "-" >> lexeme primary
    >>= (fun x -> return (Sub (Const (VInt 0), x)))
    <|> primary)
      input

  and primary input =
    (assign <|> parens expr <|> create_table <|> call_func <|> table_access
   <|> atomic)
      input

  and atomic =
    const_var <|> const_number <|> const_string <|> const_bool <|> const_null

  and create_table input =
    ( token "{" >> sep_by1 expr (token ",") >>= fun table_elems ->
      token "}" >> return (Const (VTable table_elems)) )
      input

  and table_access input =
    ( ident >>= fun table_name ->
      token "[" >> expr >>= fun pos ->
      token "]" >> return (TableAccess (Var table_name, pos)) )
      input

  and call_func input =
    ( ident >>= fun func_name ->
      token "(" >> sep_by expr (token ",") >>= fun args ->
      token ")" >> return (CallFunc (Var func_name, args)) )
      input

  and assign input =
    let get_lhs = table_access <|> const_var in
    ( get_lhs >>= fun lhs ->
      token "=" >> expr >>= fun rhs -> return (Assign (lhs, rhs)) )
      input
end

module PStatement = struct
  open PExpression

  (* Statement parser *)
  let rec stmt input =
    choice
      [
        local_stmt;
        var_dec_stmt;
        break_stmt;
        return_stmt;
        func_stmt;
        if_stmt;
        while_stmt;
        for_num_stmt;
        expr_stmt;
        block_stmt;
      ]
      input

  and break_stmt = token "break" >> return Break

  and return_stmt =
    token "return" >> expr
    >>= (fun e -> return (Return e))
    <|> (token "return" >> return (Return Null))

  and block_stmt input =
    ( token "do" >> sep_by stmt spaces >>= fun body ->
      token "end" >> return (Block body) )
      input

  and local_stmt input =
    (token "local" >> stmt >>= function
     | FuncDec (p1, p2, p3) -> return (Local (FuncDec (p1, p2, p3)))
     | VarDec p1 -> return (Local (VarDec p1))
     | Expression (Assign (p1, p2)) ->
         return (Local (Expression (Assign (p1, p2))))
     | _ -> mzero)
      input

  and var_dec_stmt input =
    ( sep_by (table_access <|> const_var) (token ",") >>= fun vars ->
      token "=" >> sep_by expr (token ",") >>= fun values ->
      return (VarDec (var_zipper vars values)) )
      input

  and var_zipper l1 l2 =
    let rec helper l1 l2 acc =
      match (l1, l2) with
      | [], [] -> acc
      | hd1 :: tl1, hd2 :: tl2 -> (hd1, hd2) :: helper tl1 tl2 acc
      | hd1 :: tl1, [] -> (hd1, Null) :: helper tl1 [] acc
      | [], _ :: _ -> acc
    in
    helper l1 l2 []

  and expr_stmt input = (expr >>= fun e -> return (Expression e)) input

  and while_stmt input =
    ( token "while" >> expr >>= fun condition ->
      block_stmt >>= fun body -> return (While (condition, body)) )
      input

  and for_num_stmt input =
    ( token "for" >> ident >>= fun var ->
      token "=" >> sep_by1 expr (token ",") >>= function
      | conds when List.length conds < 2 || List.length conds > 3 -> mzero
      | conds ->
          block_stmt >>= fun body ->
          return (ForNumerical (Var var, conds, body)) )
      input

  (* TODO: Think how to avoid list @ concat *)
  and if_stmt input =
    let elseif_stmt input =
      ( token "elseif" >> expr >>= fun cond ->
        token "then" >> sep_by stmt spaces >>= fun elseif_body ->
        return (cond, Block elseif_body) )
        input
    in
    let else_stmt result input =
      ( token "else" >> sep_by stmt spaces >>= fun else_body ->
        return (result @ [ (Const (VBool true), Block else_body) ]) )
        input
    in
    ( token "if" >> expr >>= fun cond ->
      token "then" >> sep_by stmt spaces >>= fun if_body ->
      many elseif_stmt >>= fun elseif_stmt_list ->
      let result = (cond, Block if_body) :: elseif_stmt_list in
      else_stmt result
      >>= (fun r -> token "end" >> return (If r))
      <|> (token "end" >> return (If result)) )
      input

  and func_stmt input =
    ( token "function" >> ident >>= fun func_name ->
      token "(" >> sep_by ident (token ",") >>= fun args ->
      token ")" >> sep_by stmt spaces >>= fun body ->
      token "end" >> return (FuncDec (func_name, args, Block body)) )
      input

  (* Lua-program parser *)
  let parse_all input =
    ( sep_by stmt spaces >>= fun result ->
      print_string (show_statement (Block result));
      return (Block result) )
      input
end
