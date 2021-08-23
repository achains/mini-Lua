open Ast
open Opal

let reserved =
  ["and"; "break"; "do"; "else"; "elseif"; "end";
   "false"; "for"; "function"; "if"; "in";
   "local"; "nil"; "not"; "or"; "repeat"; "return";
   "then"; "true"; "until"; "while"];;

let apply p s = parse p (LazyStream.of_string s);;

let parens = between (token "(") (token ")")

let braces = between (token "{") (token "}")

let int_parser = spaces >> many1 digit => implode % int_of_string;;

let float_parser = 
  spaces >> many1 digit >>= fun int_part -> exactly '.' >> many digit 
  >>= fun float_part -> return (float_of_string(implode (int_part @ ('.' :: float_part))))

let%test _ = apply float_parser "1." = Some 1.
let%test _ = apply int_parser "  1" = Some 1
let%test _ = apply float_parser ".15" = None

module PExpression = struct

  let initial = letter <|> exactly '_'
  let subseqt = alpha_num <|> exactly '_'
  let ident = (spaces >> initial <~> many subseqt) => implode >>= function
    | s when List.mem s reserved -> mzero
    | s -> return s

  let%test _ = apply ident "_Test_1_" = Some ("_Test_1_")
  let%test _ = apply ident "if" = None
  let%test _ = apply ident "123Test" = None

  let const_int = int_parser >>= fun n -> return (Const (VInt n))

  let const_float = float_parser >>= fun n -> return (Const (VFloat n))

  let const_number = const_float <|> const_int

  let const_bool = (token "true" >> return (Const (VBool(true)))) <|> (token "false" >> return (Const (VBool(false))))

  let const_null = token "nil" >> return Null

  let const_var = ident => fun x -> Var(x)

  let const_string =
    let string_of_chars chars =
      let buf = Buffer.create 16 in
      List.iter (Buffer.add_char buf) chars;
      Buffer.contents buf
    in
    token "\"" >> many (satisfy (fun c -> c != '\"')) >>= fun list ->
    token "\"" >> return (Const (VString (string_of_chars list)))

  let%test _ = apply const_number "1.15" = Some (Const (VFloat 1.15))
  let%test _ = apply const_number " .15" = None 
  let%test _ = apply const_number "  3" = Some (Const (VInt 3))
  let%test _ = apply const_bool "true  " = Some (Const (VBool true))
  let%test _ = apply const_null "nil" = Some Null

  let%test _ = apply const_string "\" sample \"" = Some (Const (VString (" sample ")))
  let%test _ = apply const_string "\"\"" = Some (Const (VString ("")))

  (* Arithmetic operators *)
  let add_op = token "+" >> return (fun x y -> Sum (x, y))
  let sub_op = token "-" >> return (fun x y -> Sub (x, y))

  let mul_op = token "*" >> return (fun x y -> Mul (x, y))
  let fdiv_op = token "/" >> return (fun x y -> FDiv (x, y))
  let div_op = token "//" >> return (fun x y -> Div (x, y))

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
  let not_op = token "not" >> return (fun x -> Not (x))

  let atomic = 
    const_var <|> const_number <|> const_string 
    <|> const_bool <|> const_null

  let rec expr input = (chainl1 and_expr or_op) input 

  and and_expr input = (chainl1 test_expr and_op) input

  and test_expr input = 
      (chainl1 add_expr (leq_op <|> geq_op <|> le_op <|> ge_op <|> eq_op <|> neq_op)) input

  and add_expr input = (chainl1 mul_expr (add_op <|> sub_op)) input 

  and mul_expr input = (chainl1 unary_expr (mul_op <|> div_op <|> fdiv_op)) input 

  and unary_expr input = 
      ((token "-" >> lexeme primary >>= fun x -> return (Sub (Const (VInt 0), x))) 
      <|> (primary)) input 

  and primary input =
      (assign <|> parens expr <|> create_table <|> call_func <|> atomic) input 

  and create_table input = 
      (token "{" >> sep_by1 expr (token ",") 
      >>= fun table_elems -> 
        token "}" >> return (Const (VTable (table_elems)))) input

  and call_func input = 
      (ident >>= fun func_name -> 
        token "(" >> sep_by expr (token ",") >>= fun args -> 
        token ")" >> return (CallFunc(Var(func_name), args))) input
  
  and assign input = 
      let get_lhs = const_var in 
      (get_lhs >>= fun lhs -> 
        (* print_string(Ast.show_expr(lhs)); *)
        token "=" >> expr >>= fun rhs -> return (Assign (lhs, rhs))) input


  let%test _ = apply create_table "{a = 5}" = Some (Const (VTable ([Assign (Var "a", Const (VInt 5))])))
  let%test _ = apply expr "-5" = Some (Sub (Const (VInt 0), Const (VInt 5)))
  let%test _ = apply expr "a = 5" = Some (Assign (Var "a", Const (VInt 5)))

  let%test _ = apply expr "a = {b = 5}" = Some (Assign (Var "a", Const (VTable  ([Assign (Var "b", Const (VInt 5))]))))
  let%test _ = apply call_func "foo   (a=3, 5)" = Some (CallFunc(Var("foo"), [Assign (Var "a", Const (VInt 3)); Const (VInt 5)]));;
end 

module PStatement = struct
  open PExpression
  let end_token = token "end" <|> token "else"
  let break_stmt = token "break" >> end_token >> return Break
  
  let return_stmt = 
      (token "return" >> expr >>= fun e -> end_token >> return (Return e)) <|>
      (token "return" >> end_token >> return (Return Null))

  
  let%test _ = apply return_stmt "return a end" =  Some (Return(Var "a"))
  let%test _ = apply return_stmt "return end" = Some (Return Null)
  let%test _ = apply break_stmt "break end" = Some (Break)
  let%test _ = apply break_stmt "break a = 5 end" = None

  let rec stmt input = 
    choice 
      [
        var_dec_stmt;
        break_stmt;
        return_stmt;
        if_stmt;
        while_stmt;
        for_num_stmt; 
        expr_stmt; 
        block_stmt
      ]
      input

  and var_dec_stmt input = 
    (sep_by ident (token ",") >>= fun vars -> 
     token "=" >> sep_by expr (token ",") >>= fun values ->
     return (VarDec(var_zipper vars values))  ) input
  
  and var_zipper l1 l2 = 
    let rec helper l1 l2 acc = 
      match l1, l2 with
        | [], [] -> acc
        | hd1::tl1, hd2::tl2 -> (hd1, hd2) :: (helper tl1 tl2 acc)
        | hd1::tl1, [] -> (hd1, Null) :: (helper tl1 [] acc)
        | [], _::_ -> acc
    in 
    helper l1 l2 []
  
  and block_stmt input =
    (token "do" >> stmt >>= fun body ->
     token "end" >> return (Block(body))) input 
  
  and expr_stmt input = 
      (expr >>= fun e -> return (Expression e)) input
  
  and while_stmt input = 
    (token "while" >> expr >>= fun condition -> 
     block_stmt >>= fun body ->  return (While (condition, body))) input
  
  and for_num_stmt input = 
    (token "for" >> ident >>= fun var -> 
     token "=" >> sep_by1 expr (token ",") >>= function 
     | conds when List.length conds > 3 || List.length conds < 2  -> mzero
     | conds -> block_stmt >>= fun body -> return (ForNumerical(Var(var), conds, body))) input
  
  and if_stmt input = 
    let elseif_parser input = 
      ((token "elseif") >> expr >>= fun cond -> (token "then") >> stmt >>= fun body -> return (cond, body)) input
    in
    let else_parser input = 
      ((token "else") >> stmt >>= fun body -> return (Const(VBool true), body)) input
    in
    ((token "if") >> expr >>= fun cond -> (token "then") >> stmt >>= fun body ->
      many (elseif_parser <|> else_parser) >>= fun if_list -> token ("end") >> 
      return (If((cond, body)::if_list))) input

   let%test _ = apply var_dec_stmt "a, b = 1, 2" = Some(VarDec(["a", Const(VInt 1); "b", Const(VInt 2)]))
   let%test _ = apply var_dec_stmt "a, b = 1" = Some(VarDec(["a", Const(VInt 1); "b", Null]))
   let%test _ = apply var_dec_stmt "a = 1, 2" = Some(VarDec(["a", Const(VInt 1)]))

   let%test _ = apply block_stmt "do a = 1, 2 end" = Some(Block(VarDec(["a", Const(VInt 1)])))
   let%test _ = apply expr_stmt "a = 3" = Some(Expression(Assign(Var("a"), Const(VInt 3))))
   let%test _ = apply while_stmt "while a == true do a = false end" = 
    Some (While ((Eq ((Var "a"), (Const (VBool true))), (Block (VarDec [("a", (Const (VBool false)))])))))
    
   let%test _ = apply for_num_stmt "for i = 1,5,2 do 1 end" =
    Some (ForNumerical (Var("i"), [Const(VInt(1)); Const(VInt(5)); Const(VInt(2))], Block(Expression(Const(VInt 1)))))

   let%test _ = apply for_num_stmt "for i = 1 do 1 end" = None

   let%test _ = apply if_stmt "if true then false elseif false then false else false end" = 
    Some (If([Const(VBool(true)), Expression(Const(VBool(false))); 
              Const(VBool(false)), Expression(Const(VBool(false)));
              Const(VBool(true)), Expression(Const(VBool(false)))]))
   
  let%test _ = apply if_stmt "if true then false" = None
  let%test _ = apply if_stmt "if true then false else true end" =
    Some (If([Const(VBool true), Expression(Const(VBool false)); Const(VBool true), Expression(Const(VBool true))]))
  
  let%test _ = apply if_stmt "if a == 5 then f(x) elseif a == 6 then g(y) end" =
    Some (If([Eq(Var "a", Const(VInt 5)), Expression(CallFunc(Var "f", [Var "x"])); 
              Eq(Var "a", Const(VInt 6)), Expression(CallFunc(Var "g", [Var "y"]))]))
end