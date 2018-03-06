open GT
open List
open Language

(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)
 let eval config prg =
   let eval_step (stack, ((state, input, output) as config)) insn =
     match insn with
       | BINOP op ->
         let rhs :: lhs :: stack' = stack in
         let lhs = Language.Expr.Const lhs in
         let rhs = Language.Expr.Const rhs in
         let expr = Language.Expr.Binop (op, lhs, rhs) in
         let result = Language.Expr.eval state expr in
         (result :: stack', config)
       | CONST c -> (c :: stack, config)
       | READ -> (hd input :: stack, (state, tl input, output))
       | WRITE -> (tl stack, (state, input, output @ [hd stack]))
       | LD v -> (state v :: stack, config)
       | ST v -> (tl stack, (Language.Expr.update v (hd stack) state, input, output))
   in fold_left eval_step config prg

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i = let (_, (_, _, o)) = eval ([], (Language.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)
 let rec compile stmt =
   let rec compile_expr expr = match expr with
     | Language.Expr.Const c -> [CONST c]
     | Language.Expr.Var v -> [LD v]
     | Language.Expr.Binop (op, lhs, rhs) ->
       compile_expr lhs @ compile_expr rhs @ [BINOP op]
   in match stmt with
     | Language.Stmt.Read v -> [READ; ST v]
     | Language.Stmt.Write expr -> compile_expr expr @ [WRITE]
     | Language.Stmt.Assign (v, expr) -> compile_expr expr @ [ST v]
     | Language.Stmt.Seq (prg1, prg2) -> compile prg1 @ compile prg2
