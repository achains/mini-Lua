open Ast
open Parser
(* open Parser.PExpression *)
open Parser.PStatement

(* Expression tests *)

(* Statement tests *)

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
