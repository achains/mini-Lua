open Ast
open Parser
open Parser.PExpression
open Parser.PStatement

(* Expression tests *)

let%test _ = apply float_parser "1." = Some 1.

let%test _ = apply int_parser "  1" = Some 1

let%test _ = apply float_parser ".15" = None

let%test _ = apply ident "_Test_1_" = Some "_Test_1_"

let%test _ = apply ident "if" = None

let%test _ = apply ident "123Test" = None

let%test _ = apply const_number "1.15" = Some (Const (VFloat 1.15))

let%test _ = apply const_number " .15" = None

let%test _ = apply const_number "  3" = Some (Const (VInt 3))

let%test _ = apply const_bool "true  " = Some (Const (VBool true))

let%test _ = apply const_null "nil" = Some Null

let%test _ =
  apply const_string "\" sample \"" = Some (Const (VString " sample "))

let%test _ = apply const_string "\"\"" = Some (Const (VString ""))

let%test _ =
  apply create_table "{a = 5}"
  = Some (Const (VTable [ Assign (Var "a", Const (VInt 5)) ]))

let%test _ =
  apply table_access "data[3]" = Some (TableAccess (Var "data", Const (VInt 3)))

let%test _ = apply expr "-5" = Some (Sub (Const (VInt 0), Const (VInt 5)))

let%test _ = apply expr "a = 5" = Some (Assign (Var "a", Const (VInt 5)))

let%test _ =
  apply expr "a = {b = 5}"
  = Some (Assign (Var "a", Const (VTable [ Assign (Var "b", Const (VInt 5)) ])))

let%test _ =
  apply call_func "foo   (a=3, 5)"
  = Some
      (CallFunc (Var "foo", [ Assign (Var "a", Const (VInt 3)); Const (VInt 5) ]))

(* Statement tests *)

let%test _ = apply return_stmt "return a" = Some (Return (Var "a"))

let%test _ = apply return_stmt "return" = Some (Return Null)

let%test _ = apply break_stmt "break" = Some Break

let%test _ =
  apply var_dec_stmt "a, b = 1, 2"
  = Some (VarDec [ ("a", Const (VInt 1)); ("b", Const (VInt 2)) ])

let%test _ =
  apply var_dec_stmt "a, b = 1"
  = Some (VarDec [ ("a", Const (VInt 1)); ("b", Null) ])

let%test _ =
  apply var_dec_stmt "a = 1, 2" = Some (VarDec [ ("a", Const (VInt 1)) ])

let%test _ =
  apply block_stmt "do a = 1, 2 end"
  = Some (Block [ VarDec [ ("a", Const (VInt 1)) ] ])

let%test _ =
  apply expr_stmt "a = 3" = Some (Expression (Assign (Var "a", Const (VInt 3))))

let%test _ =
  apply while_stmt "while a == true do a = false end"
  = Some
      (While
         ( Eq (Var "a", Const (VBool true)),
           Block [ VarDec [ ("a", Const (VBool false)) ] ] ))

let%test _ =
  apply for_num_stmt "for i = 1,5,2 do 1 2 end"
  = Some
      (ForNumerical
         ( Var "i",
           [ Const (VInt 1); Const (VInt 5); Const (VInt 2) ],
           Block [ Expression (Const (VInt 1)); Expression (Const (VInt 2)) ] ))

let%test _ = apply for_num_stmt "for i = 1 do 1 end" = None

let%test _ =
  apply if_stmt "if true then false elseif false then true false else false end"
  = Some
      (If
         [
           (Const (VBool true), Block [ Expression (Const (VBool false)) ]);
           ( Const (VBool false),
             Block
               [
                 Expression (Const (VBool true));
                 Expression (Const (VBool false));
               ] );
           (Const (VBool true), Block [ Expression (Const (VBool false)) ]);
         ])

let%test _ = apply if_stmt "if true then false" = None

let%test _ =
  apply if_stmt "if true then false elseif false then false end"
  = Some
      (If
         [
           (Const (VBool true), Block [ Expression (Const (VBool false)) ]);
           (Const (VBool false), Block [ Expression (Const (VBool false)) ]);
         ])

let%test _ =
  apply if_stmt "if true then false else false elseif true then false end"
  = None

let%test _ =
  apply func_stmt "function a(x, y) return x + y end"
  = Some
      (FuncDec ("a", [ "x"; "y" ], Block [ Return (Sum (Var "x", Var "y")) ]))

(* Parse all *)

let%test _ =
  apply parse_all
    "\n\
     function fact (n)\n\
    \    if n == 0 then\n\
    \        return 1\n\
    \    else \n\
    \        return n * fact (n - 1)\n\
    \    end\n\
     end\n\n\n\
     data = {1, 2, 3, 4, 5, 6, 7}\n\
     s = 0\n\
     s = s + fact (data[i])\n\
     for i = 1,7 do\n\
    \    s = s + fact(data[i])\n\
     end\n"
  = Some
      (Block
         [
           FuncDec
             ( "fact",
               [ "n" ],
               Block
                 [
                   If
                     [
                       ( Eq (Var "n", Const (VInt 0)),
                         Block [ Return (Const (VInt 1)) ] );
                       ( Const (VBool true),
                         Block
                           [
                             Return
                               (Mul
                                  ( Var "n",
                                    CallFunc
                                      ( Var "fact",
                                        [ Sub (Var "n", Const (VInt 1)) ] ) ));
                           ] );
                     ];
                 ] );
           VarDec
             [
               ( "data",
                 Const
                   (VTable
                      [
                        Const (VInt 1);
                        Const (VInt 2);
                        Const (VInt 3);
                        Const (VInt 4);
                        Const (VInt 5);
                        Const (VInt 6);
                        Const (VInt 7);
                      ]) );
             ];
           VarDec [ ("s", Const (VInt 0)) ];
           VarDec
             [
               ( "s",
                 Sum
                   ( Var "s",
                     CallFunc (Var "fact", [ TableAccess (Var "data", Var "i") ])
                   ) );
             ];
           ForNumerical
             ( Var "i",
               [ Const (VInt 1); Const (VInt 7) ],
               Block
                 [
                   VarDec
                     [
                       ( "s",
                         Sum
                           ( Var "s",
                             CallFunc
                               ( Var "fact",
                                 [ TableAccess (Var "data", Var "i") ] ) ) );
                     ];
                 ] );
         ])
