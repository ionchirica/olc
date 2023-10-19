module Lambda = struct
  type var = string
               [@@deriving show]

  type btype = int
                 [@@deriving show]

  type arithFn =
    | Add
[@@deriving show]

  type cmprFn =
    | Less
    | Equal
    | Greater
[@@deriving show]

  type bfn =
    | Arithmetic of arithFn * expr * expr  (* Arithmetic * OPa * OPb *)
    | Comparison of cmprFn * expr * expr   (* Comparison * LHS * RHS *)

  and expr =
    | Var of var
    | Abs of var * expr          (* Abstraction: λvar. M *)
    | App of expr * expr         (* Application: M N - apply M to N *)
    | Lit of btype               (* Literals *)
    | Builtin of bfn             (* Functions *)
    | Cond of expr * expr * expr (* Conditionals: Pred - True - False *)
                              [@@deriving show]
end

let () =

  let incFn = Lambda.Abs( "x", Builtin( Arithmetic( Add, Var("x"), Lit 1 ) )) in

  let incrApp( n: int ) = Lambda.App( incFn, Lit(n)) in

  print_endline (Lambda.show_expr (incrApp(22)));
