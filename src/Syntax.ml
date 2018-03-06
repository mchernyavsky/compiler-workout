(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT
open List

(* Simple expressions: syntax and semantics *)
module Expr =
  struct

    (* The type for expressions. Note, in regular OCaml there is no "@type..."
       notation, it came from GT.
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)

    (* State: a partial map from variables to integer values. *)
    type state = string -> int

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    (* Expression evaluator

          val eval : state -> t -> int

       Takes a state and an expression, and returns the value of the expression in
       the given state.
    *)
    let rec eval state expr = match expr with
      | Const c -> c
      | Var v -> state v
      | Binop (op, expr1, expr2) ->
        let int2bool i = i <> 0 in
        let bool2int b = if b then 1 else 0 in
        let lhs = eval state expr1 in
        let rhs = eval state expr2 in
        match op with
          | "+" -> lhs + rhs
          | "-" -> lhs - rhs
          | "*" -> lhs * rhs
          | "/" -> lhs / rhs
          | "%" -> lhs mod rhs
          | "<" -> bool2int (lhs < rhs)
          | "<=" -> bool2int (lhs <= rhs)
          | ">" -> bool2int (lhs > rhs)
          | ">=" -> bool2int (lhs >= rhs)
          | "==" -> bool2int (lhs = rhs)
          | "!=" -> bool2int (lhs <> rhs)
          | "&&" -> bool2int ((int2bool lhs) && (int2bool rhs))
          | "!!" -> bool2int ((int2bool lhs) || (int2bool rhs))
          | _ -> failwith (Printf.sprintf "Undefined operator %s" op)

  end

(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list

    (* Statement evaluator

          val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval ((state, input, output) as config) stmt = match stmt with
      | Read v -> (Expr.update v (hd input) state, tl input, output)
      | Write expr -> (state, input, Expr.eval state expr :: output)
      | Assign (v, expr) -> (Expr.update v (Expr.eval state expr) state, input, output)
      | Seq (stmt1, stmt2) -> eval (eval config stmt1) stmt2

  end
