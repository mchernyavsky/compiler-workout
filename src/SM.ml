open GT
open List

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
type config = int list * Syntax.Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)
let eval config prg =
  let eval_step (stack, ((state, input, output) as config)) insn =
    match insn with
      | BINOP op ->
        let rhs :: lhs :: stack' = stack in
        let lhs = Syntax.Expr.Const lhs in
        let rhs = Syntax.Expr.Const rhs in
        let expr = Syntax.Expr.Binop (op, lhs, rhs) in
        let result = Syntax.Expr.eval state expr in
        (result :: stack', config)
      | CONST c -> (c :: stack, config)
      | READ -> (hd input :: stack, (state, tl input, output))
      | WRITE -> (tl stack, (state, input, output @ [hd stack]))
      | LD v -> (state v :: stack, config)
      | ST v -> (tl stack, (Syntax.Expr.update v (hd stack) state, input, output))
      | _ -> failwith "Unknown instruction"
  in fold_left eval_step config prg

(* Stack machine compiler

     val compile : Syntax.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)

let rec compile stmt =
  let rec compile_expr expr = match expr with
    | Syntax.Expr.Const c -> [CONST c]
    | Syntax.Expr.Var v -> [LD v]
    | Syntax.Expr.Binop (op, lhs, rhs) ->
      compile_expr lhs @ compile_expr rhs @ [BINOP op]
  in match stmt with
    | Syntax.Stmt.Read v -> [READ; ST v]
    | Syntax.Stmt.Write expr -> compile_expr expr @ [WRITE]
    | Syntax.Stmt.Assign (v, expr) -> compile_expr expr @ [ST v]
    | Syntax.Stmt.Seq (prg1, prg2) -> compile prg1 @ compile prg2
