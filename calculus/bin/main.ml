
exception Invalid_argument of string

module Lambda = struct
  type var = string

  type btype = int

  type arithFn =
    | Add
    | Sub
    | Mul
    | Div

  module ArithmeticFn = struct
    let apply (fn: arithFn) (a: int) (b: int): int  =
      match fn with
      | Add -> a + b
      | Sub -> a - b
      | Mul -> a * b
      | Div -> a / b
  end

  type cmprFn =
    | Less
    | Equal
    | Greater

  module ComparisonFn = struct
    let apply (fn: cmprFn) (a: int) (b: int): int  =
      let asInt x = if x then 1 else 0 in

      match fn with
      | Less -> a < b |> asInt
      | Equal -> a = b |> asInt
      | Greater -> a > b |> asInt
  end

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
  module Expr = struct
    let asInt = function
      | Lit( x ) -> x
      | _ -> raise( Invalid_argument "Not a number" )

    let asAbs = function
      | Abs( var, expr ) -> var, expr
      | _ -> raise ( Invalid_argument "Not an abstraction" )

    let rec substitute (replaceWhat: var) (replaceFor: expr) (original: expr) =
      let substituteFn = substitute replaceWhat replaceFor in
      match original with
      | Lit _ -> original
      | Builtin ( Arithmetic( fn, opA, opB ) ) ->
         Builtin ( Arithmetic (fn, substituteFn opA, substituteFn opB) )
      | Builtin ( Comparison( fn, opA, opB ) ) ->
         Builtin ( Comparison (fn, substituteFn opA, substituteFn opB) )
      | Cond ( pred, t, f ) -> Cond( substituteFn pred, substituteFn t, substituteFn f )
      | App ( expr, args ) -> App( substituteFn expr, substituteFn args )
      | Var boundName -> if boundName = replaceWhat then replaceFor else original
      | Abs ( boundName, body ) ->
         if boundName = replaceWhat then original
         else Abs ( boundName, substituteFn body )
  end

  let rec eval ( expression: expr ) : expr =
    match expression with
    | Lit( _ ) -> expression
    | Builtin ( Arithmetic( fn, opA, opB ) ) ->
       let valA = eval opA |> Expr.asInt in
       let valB = eval opB |> Expr.asInt in
       Lit( ArithmeticFn.apply fn valA valB  )
    | Builtin ( Comparison( fn, lhs, rhs ) ) ->
       let lhs = eval lhs |> Expr.asInt in
       let rhs = eval rhs |> Expr.asInt in
       Lit( ComparisonFn.apply fn lhs rhs )
    | Cond ( pred, trueBranch, falseBranch ) ->
       let valPred = eval pred |> Expr.asInt in
       if valPred <> 0 then eval trueBranch else eval falseBranch
    | Abs _ ->
       (* option 1: reduce the body, return simplified *)
       (* option 2: return as is *)
       expression
    | App (expr, args) ->
       let lambdaVar, lambdaBody = eval expr |> Expr.asAbs in
       (* evaluate the arguments and not delay its evaluation until needed *)
       let valArg = eval args in
       (* \x. x + 1 *)
       (* lambdaVar = x, lambdaExpr = x + 1 *)
       (* perform substitution *)
       Expr.substitute lambdaVar valArg lambdaBody |> eval
    | Var _ -> expression (* free occurence with no value *)

end


let subApply (frst: int) (snd: int): int =
  let subFn = Lambda.Abs( "x", Lambda.Abs( "y", Builtin( Arithmetic( Sub, Var "x", Var "y") ) ) ) in
  let subApp = Lambda.App( Lambda.App( subFn, Lit frst), Lit snd ) in
  Lambda.eval subApp |> Lambda.Expr.asInt

let lazyFixpoint =
  (* y = \f. (\x. f (x x)) (\x. f (x x)) *)
  let innerAbs = Lambda.Abs( "x", App( Var "f", App( Var "x", Var("x") ) ) ) in
  Lambda.Abs( "f", App( innerAbs, innerAbs ) )

let eagerFixpoint =
  (* y = \f. (\x. f( \v. x x v )) (\x. f(x x v)) *)
  let indirection = Lambda.Abs( "v", App( App( Var "x", Var "x" ), Var "v" ) ) in
  let innerAbs = Lambda.Abs( "x", App( Var "f", indirection ) ) in
  Lambda.Abs( "f", App( innerAbs, innerAbs ) )


let fibStep =
  (* \f. x. if n < 2 then 1 else f (x - 1) + f ( x- 2)*)
  let xMinus n = Lambda.Builtin( Arithmetic( Sub, Var "x", Lit n ) ) in
  let falseBranch = Lambda.Builtin( Arithmetic( Add, App( Var "f", xMinus 1), App( Var "f", xMinus 2) )) in

  Lambda.Abs( "f", Lambda.Abs( "x", Cond( Lambda.Builtin( Comparison (Less, Var "x", Lit 2) ), Lit 1, falseBranch ) ) )

let fib (n: int) =
  let fn = Lambda.App( eagerFixpoint, fibStep ) in
  Lambda.eval (Lambda.App( fn, Lit n )) |> Lambda.Expr.asInt


let () =

  let incFn = Lambda.Abs( "x", Builtin( Arithmetic( Add, Var("x"), Lit 1 ) )) in
  let incrApp( n: int ) = Lambda.App( incFn, Lit(n)) in

  Printf.printf "%d\n" (Lambda.eval(incrApp(22)) |> Lambda.Expr.asInt);
  Printf.printf "%d\n" (subApply 60 20);
  Printf.printf "%d\n" (fib 30 );
