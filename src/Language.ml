(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT
open List

(* Opening a library for combinator-based syntax analysis *)
open Ostap
open Ostap.Combinators

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

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string

    *)

    let make_binop ops = List.map (fun op -> ostap ($(op)), fun x y -> Binop(op, x, y)) ops

    ostap (
      parse: expr;
      expr:
         !(Util.expr
             (fun x -> x)
             [|
               `Lefta, make_binop ["!!"];
               `Lefta, make_binop ["&&"];
               `Nona,  make_binop ["<="; "<"; ">="; ">"; "=="; "!="];
               `Lefta, make_binop ["+"; "-"];
               `Lefta, make_binop ["*"; "/"; "%"];
             |]
             primary
          );
      primary: v:IDENT {Var v} | c:DECIMAL {Const c} | -"(" expr -")"
    )

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
      | Write expr -> (state, input, output @ [Expr.eval state expr])
      | Assign (v, expr) -> (Expr.update v (Expr.eval state expr) state, input, output)
      | Seq (stmt1, stmt2) -> eval (eval config stmt1) stmt2

    (* Statement parser *)
    ostap (
      parse: seq;
      stmt: read | write | assign;
      read: "read" -"(" v:IDENT -")" {Read v};
      write: "write" -"(" expr:!(Expr.parse) -")" {Write expr};
      assign: v:IDENT -":=" expr:!(Expr.parse) {Assign (v, expr)};
      seq: <s::ss> : !(Util.listBy)[ostap (";")][stmt] {
          List.fold_left (fun x y -> Seq (x, y)) s ss
      }
    )

  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval p i =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o
