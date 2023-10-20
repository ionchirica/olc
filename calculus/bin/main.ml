open Map

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

  module VarMap = Map.Make(struct
                      type t = var
                      let compare = String.compare
                    end)

  type value =
    | Int of int
    | Closure of closure

  and closure =
    { environment: value VarMap.t; var: var; body: expr }

  module Value = struct
    let asInt = function
      | Int( x ) -> x
      | _ -> raise ( Invalid_argument "Not an integer" )

    let asClosure = function
      | Closure ( c ) -> c
      | _ -> raise ( Invalid_argument "Not a closure" )
  end


  let rec eval (environment: value VarMap.t) ( expression: expr ) : value =
    match expression with
    | Lit( i ) -> Int i
    | Builtin ( Arithmetic( fn, opA, opB ) ) ->
       let valA = eval environment opA |> Value.asInt in
       let valB = eval environment opB |> Value.asInt in
       Int( ArithmeticFn.apply fn valA valB )
    | Builtin ( Comparison( fn, lhs, rhs ) ) ->
       let lhs = eval environment lhs |> Value.asInt in
       let rhs = eval environment rhs |> Value.asInt in
       Int( ComparisonFn.apply fn lhs rhs )
    | Cond ( pred, trueBranch, falseBranch ) ->
       let valPred = eval environment pred |> Value.asInt in
       if valPred <> 0 then eval environment trueBranch else eval environment falseBranch
    | Abs (var, body) ->
       Closure { environment = environment; var = var; body = body }
    | App (expr, args) ->
       let {environment = closureEnv; var = closureVar; body = closureBody } = eval environment expr |> Value.asClosure in
       (* evaluate the arguments and not delay its evaluation until needed *)
       let valArg = eval environment args in
       (* \x. x + 1 *)
       (* lambdaVar = x, lambdaExpr = x + 1 *)
       (* perform substitution *)

       let newEnv = VarMap.add closureVar valArg closureEnv in
       eval newEnv closureBody

      (* raises Invalid Argument if Option is None *)
    | Var name -> VarMap.find_opt name environment |> Option.get

end


let subApply (frst: int) (snd: int): int =

  let subFn = Lambda.Abs( "x", Lambda.Abs( "y", Builtin( Arithmetic( Sub, Var "x", Var "y") ) ) ) in
  let subApp = Lambda.App( Lambda.App( subFn, Lit frst), Lit snd ) in
  Lambda.eval (Lambda.VarMap.empty) subApp |> Lambda.Value.asInt

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
  (* \f. x. if n < 2 then 1 else f (x - 1) + f ( x - 2)*)
  let xMinus n = Lambda.Builtin( Arithmetic( Sub, Var "x", Lit n ) ) in
  let falseBranch = Lambda.Builtin( Arithmetic( Add, App( Var "f", xMinus 1), App( Var "f", xMinus 2) )) in

  Lambda.Abs( "f", Lambda.Abs( "x", Cond( Lambda.Builtin( Comparison (Less, Var "x", Lit 2) ), Lit 1, falseBranch ) ) )

let fib (n: int) =
  let fn = Lambda.App( eagerFixpoint, fibStep ) in
  Lambda.eval Lambda.VarMap.empty (Lambda.App( fn, Lit n )) |> Lambda.Value.asInt


let () =

  let incFn = Lambda.Abs( "x", Builtin( Arithmetic( Add, Var("x"), Lit 1 ) )) in
  let incrApp( n: int ) = Lambda.App( incFn, Lit(n)) in

  Printf.printf "%d\n" (Lambda.eval Lambda.VarMap.empty (incrApp(22)) |> Lambda.Value.asInt);
  Printf.printf "%d\n" (subApply 60 20);
  Printf.printf "%d\n" (fib 30 );
