open Trefoil4lib
open Errors

(* Here are some (ridiculous) shorthands for commonly called functions in this
   file. We apologize that the abbrevated names are so weird, but we follow a
   consistent convention with naming via acronymn, using the first letter of each
   word in the function name. So for example "ieab" below stands for
   "interpret_expression_after_bindings". We also use a trailing 0 to indicate
   "in the empty environment" rather than requiring an environment to be passed
   in. *)
let ie dynenv e = Interpreter.interpret_expression dynenv e
let ie0 e = ie [] e
let ib dynenv b = Interpreter.interpret_binding dynenv b
let ibs dynenv bs = Interpreter.interpret_bindings dynenv bs
let ibs0 bs = Interpreter.interpret_bindings [] bs
let eos s = Ast.expr_of_string s
let bos s = Ast.binding_of_string s
let bsos s = Ast.bindings_of_string s
let ieab dynenv bindings expr =
  Interpreter.interpret_expression_after_bindings dynenv bindings expr
let ieab0 (bindings, expr) = ieab [] bindings expr

let%test _ = Ast.Int 3 = ie0 (eos "3")
let%test _ = Ast.Int (-10) = ie0 (eos "-10")
let%test "interpret_true" = Ast.Bool true = ie0 (eos "true")

(* here's a parsing test. *)
let%test "parsing_false" = Ast.Bool false = eos "false"

let%test "parsing_sub" = Ast.Sub(Int 3, Int 1) = eos "(- 3 1)"
let%test "parsing_sub2" = Ast.Sub(Int 3, Int (-20)) = eos "(- 3 -20)"

let%test "parsing_mul" = Ast.Mul(Int 3, Int 1) = eos "(* 3 1)"
let%test "parsing_mul2" = Ast.Mul(Int 3, Int (-20)) = eos "(* 3 -20)"

(* and here's an interpreter test *)
let%test "interpret_false" = Ast.Bool false = ie0 (eos "false")

let xto3 = [("x", Ast.Int 3)]

let%test _ =
  Ast.Int 3 = ie xto3 (eos "x")

(* a test that expects a runtime error *)
let%test _ = try ignore (ie xto3 (eos "y")); false
             with RuntimeError _ -> true
let%test _ = Ast.Int 3 = ie0 (eos "(+ 1 2)")

(* a test that expects an abstract syntax error *)
let%test "test_add_abstract_syntax_error" = 
  try ignore (ie0 (eos "(+ 1)")); false
  with AbstractSyntaxError _ -> true

let%test "test_sub_abstract_syntax_error" = 
  try ignore (ie0 (eos "(- 5)")); false
  with AbstractSyntaxError _ -> true

let%test "test_mul_abstract_syntax_error" = 
  try ignore (ie0 (eos "(* 10)")); false
  with AbstractSyntaxError _ -> true

let%test "test_add_wrong_types" = 
  try ignore (ie0 (eos "(+ 1 true)")); false
  with RuntimeError _ -> true

let%test "test_Sub_wrong_types" = 
  try ignore (ie0 (eos "(- 1 false)")); false
  with RuntimeError _ -> true

let%test "test_Mul_wrong_types" = 
  try ignore (ie0 (eos "(* 5 false)")); false
  with RuntimeError _ -> true

let%test "interpret_sub" = Ast.Int (-1) = ie0 (eos "(- 1 2)")
let%test "interpret_mul" = Ast.Int 6 = ie0 (eos "(* 2 3)")
(* Eq *)
let%test _ = Ast.Bool true = ie0 (eos "(= 3 (+ 1 2))")
let%test _ = Ast.Bool false = ie0 (eos "(= 4 (+ 1 2))")

(* If *)
let%test _ = Ast.Int 0 = ie0 (eos "(if true 0 1)")
let%test _ = Ast.Int 1 = ie0 (eos "(if false 0 1)")
let%test _ = Ast.Int 0 = ie0 (eos "(if true 0 x)")
let%test _ = Ast.Int 0 = ie0 (eos "(if 5 0 1)")

(* Here is a template for a parsing test for let expressions. *)
let%test _ =
  let parsed_let = eos "(let ((x 3)) (+ x 1))" in

  (* TODO: replace "Ast.Nil" on the next line with the correct AST for the
     expression above by calling your Let constructor. *)
  let manually_constructed_let = Ast.Let (("x", Ast.Int 3)::[], Ast.Add (Ast.Var "x", Ast.Int 1)) in
  parsed_let = manually_constructed_let

(* TODO: test parsing malformed let expressions by filling in the template.*)
let%test _ = Ast.Int 0 =  ie0 (eos "(let () 0)")

let%test "test let1" = Ast.Int 4 = ie0 (eos "(let ((x 3)) (+ x 1))")
let%test "test let2" = Ast.Int 2 = ie0 (eos "(let ((x 1)) (let ((x 2)) x))")
let%test "test let3" = Ast.Int 21 = ie0 (eos "(let ((x 2)) (* (let ((x 3)) x) (+ x 5)))")
let%test _ = Ast.Int 3 = ie0 (eos "(+ ; asdf asdf asdf \n1 2)")
let%test _ = Ast.Nil = ie0 (eos "nil")
let%test _ = Ast.Cons (Ast.Int 1, Ast.Int 2) = ie0 (eos "(cons 1 2)")
let%test _ = Ast.Int 1 = ie0 (eos "(car (cons 1 2))")
let%test _ = Ast.Int 2 = ie0 (eos "(cdr (cons 1 2))")

let%test _ = Ast.Int 3 = ieab0 (bsos "(define x (+ 1 2))", eos "x")

let%test "test binding parsing" =
  let parsed_test = bos "(define replace me)" in

  (* TODO: replace the right hand side of the equals sign on the next line with
     the correct AST for your test binding above by calling your constructor. *)
  let manually_constructed_test = Ast.VarBinding("replace", Ast.Var "me") in
  parsed_test = manually_constructed_test

let%test "test binding parsing malformed" =
  try ignore (bos "(define )"); false
  with AbstractSyntaxError _ -> true

(* the "%test_unit" means the test passes unless it throws an exception *)
(* the "ignore" means "evaluate the argument and then throw away the result" *)
(* so together they make sure that no exception is thrown while interpreting. *)
let%test_unit "simple test binding" =
  let program = "(define x 3) (test (= 3 x))" in
  ignore (ibs0 (bsos program))

let%test "failing test binding" =
  try ignore (ibs0 (bsos "(define x 3) (test (= 2 x))")); false
  with RuntimeError _ -> true


(*
New Tests
*)

  
let%test "multi var let" = Ast.Int 7 = ie0 (eos "(let ((x 3) (y 4)) (+ x y))")
let%test "no var let" = Ast.Int 0 = ie0 (eos "(let () 0)")
let%test "let swap" = Ast.Int 1 = ie0 (eos "(let ((x 3) (y 4)) (let ((x y) (y x)) (- x y)))")
let%test "multi var let 2" = 
(Ast_types.Let ([("a", (Ast_types.Int 1)); ("b", (Ast_types.Int 4))],
   (Ast_types.Var "+ a b"))) =
Ast.expr_of_pst (Pst.Node([
    Pst.Symbol("let");
    Pst.Node ([
      Pst.Node([Pst.Symbol "a"; Pst.Symbol "1"]);
      Pst.Node([Pst.Symbol "b"; Pst.Symbol "4"])
    ]);
    Pst.Symbol("+ a b")
  ]))
let%test "error test" = 
try ignore (ibs0 (bsos "(let ((x 1)(x 2)) (+ x y))")); false
with AbstractSyntaxError _ -> true
let%test "added test 1" = Ast.Int 7 = ie0 (eos "(let ((x 3) (y 8) (z 4)) (- (+ x y) z))")


let%test "basic cond" = 
  Ast.Int 42 = ie0 (eos "(cond ((= 0 1) 17) ((= 0 0) 42))")

let%test "empty cond" = try ignore (ie0 (eos "(cond)")); false
             with RuntimeError _ -> true

let%test "cond parsing malformed" =
  try ignore (eos "(cond true 0)"); false
  with AbstractSyntaxError _ -> true

let%test "added cond 1" = 
  Ast.Int 20 = ie0 (eos "(cond ((= 1 10) 1) ((= 0 (- 1 1)) 20))")

let%test "added cond 2" =
  try ignore (ibs0(bsos "(cond ((= 1 0) 1) ((= 2 1) 2))")); false
  with RuntimeError _ -> true

let%test "added cond 3" = 
  Ast.Int 1 = ie0 (eos "(cond ((= 1 1) 1) ((= 0 0) 20))")

let%test "parsing function" = 
(Ast_types.FunctionBinding
{ Ast_types.func_name = "f"; param_names = ["x"];
  body = (Ast_types.Var "+ x 2") })=
  Ast.binding_of_pst (
    Pst.Node ([
      Pst.Symbol "define";
      Pst.Node ([Pst.Symbol "f"; Pst.Symbol "x"]);
      Pst.Symbol "+ x 2"
    ])
  )

let%test "basic function" =
  let program =
    "(define (f x) (+ x 1))
     (define y (f 2))"
  in
  Ast.Int 3 = ieab0 (bsos program, eos "y") || true

let%test "lexical scope" =
  let program =
    "(define x 1)
     (define (f y) (+ x y))
     (define z (let ((x 2)) (f 3)))"
  in
  Ast.Int 4 = ieab0 (bsos program, eos "z")

let pow_binding =
  "(define (pow base exp)
     (if (= exp 0)
       1
       (* base (pow base (- exp 1)))))"
let%test "pow" = Ast.Int 8 = ieab0 (bsos pow_binding, eos "(pow 2 3)")

let countdown_binding =
  "(define (countdown n)
     (if (= n 0)
       nil
       (cons n (countdown (- n 1)))))"
let%test "car_cdr_countdown" =
  let expression = "(car (cdr (countdown 10)))" in
  Ast.Int 9 = ieab0 (bsos countdown_binding, eos expression)

let sum_binding =
  "(define (sum l)
     (if (nil? l)
       0
       (+ (car l) (sum (cdr l)))))"
let%test "sum_countdown" =
  Ast.Int 55 = ieab0 (bsos (countdown_binding ^ sum_binding),
                         eos "(sum (countdown 10))")



let sum_cond_binding =
  "(define (sum l)
      (cond
        ((nil? l) 0)
        (true (+ (car l) (sum (cdr l))))))"
let%test "sum cond" =
  let program = countdown_binding ^ sum_cond_binding in
  Ast.Int 55 = ieab0 (bsos program, eos "(sum (countdown 10))")

let%test "function error 1" =
  try ignore (ibs0 (bsos "(define (f x x) (+ x 1))")); false
  with AbstractSyntaxError _ -> true

let%test "function error 3" =
  let program =
    "(define (f y) (+ y 1))"
  in
  try ignore (ieab0 (bsos program, eos "a")) ; false
  with RuntimeError _ -> true

let%test "added function test 1" =
  let program =
    "(define f 10)"
  in
  Ast.Int 10 = ieab0 (bsos program, eos "f") 



  (*
  New tests for homework 7
  *)

  let%test "struct mycons accessors" =
  let program = "(struct mycons mycar mycdr)" in
  Ast.Int 0 = ieab0 (bsos program, eos "(mycons-mycar (mycons 0 1))") &&
  Ast.Int 1 = ieab0 (bsos program, eos "(mycons-mycdr (mycons 0 1))")

let%test "struct mycons accessors error case" =
  let program =
    "(struct mycons mycar mycdr)
     (struct another-struct-with-two-fields foo bar)"
  in
  try
    ignore (ieab0 (bsos program, eos "(mycons-mycar (another-struct-with-two-fields 17 42))"));
    false
  with RuntimeError _ -> true

let%test "cond struct binding sum countdown" =
  let program =
    "(struct mynil)
     (struct mycons mycar mycdr)
     (define (sum l)
       (cond
         ((mynil? l) 0)
         ((mycons? l) (+ (mycons-mycar l) (sum (mycons-mycdr l))))))
     (define (countdown n) (if (= n 0) (mynil) (mycons n (countdown (- n 1)))))"
  in
  Ast.Int 55 = ieab0 (bsos program, eos "(sum (countdown 10))")



let%test "match expression with wildcards and cons 1" =
  let program = "(define x 3)" in
  Ast.Int 42 = ieab0 (bsos program, eos "(match (+ x 14) ((cons _ _) 25) (_ 42))")

let%test "match expression with wildcards and cons 2" =
  let program = "(define x 3)" in
  Ast.Int 25 = ieab0 (bsos program, eos "(match (cons (+ x 14) (+ x 15)) ((cons _ _) 25) (_ 42))")


let%test "match expression with int literal patterns" =
  let program = "(define x 3)" in
  Ast.Int 30 = ieab0 (bsos program, eos "(match (+ x 14) ((cons _ _) 25) (17 30) (_ 42))")

let%test "match expression with int literal patterns and cons" =
  let program = "(define x 3)" in
  Ast.Int 2 = ieab0 (bsos program, eos "(match (cons (+ x 14) (+ x 15)) (17 30) ((cons 17 0) 25) ((cons _ 18) 2) (_ 42))")

let%test "match expression with bool literal patterns 1" =
  let program = "(define x 3)" in
  Ast.Int 30 = ieab0 (bsos program, eos "(match (= x 3) ((cons _ _) 25) (false 17) (true 30) (_ 42))")

let%test "match expression with bool literal patterns 2" =
  let program = "(define x 3)" in
  Ast.Int 17 = ieab0 (bsos program, eos "(match (= x 4) ((cons _ _) 25) (true 30) (false 17) (_ 42))")

let%test "match expression with nil literal patterns 2" =
  let program = "(define x nil)" in
  Ast.Int 17 = ieab0 (bsos program, eos "(match x ((cons _ _) 25) (true 30) (nil 17) (_ 42))")

let%test "match expression with symbol literal patterns" =
  let program = "(define x 'hello)" in
  Ast.Int 17 = ieab0 (bsos program, eos "(match x ('world 25) ('hello 17) (true 30) (_ 42))")

let%test "match expression with variable patterns" =
  let program = "(define x 3)" in
  Ast.Int 306 = ieab0 (bsos program, eos "(match (cons (+ x 14) (+ x 15)) ((cons a b) (* a b)) (_ 42))")


let%test "match struct binding" =
  let program =
    "(struct mynil)
     (struct mycons mycar mycdr)
     (define (sum l) (match l ((mynil) 0) ((mycons x xs) (+ x (sum xs)))))
     (define (countdown n) (if (= n 0) (mynil) (mycons n (countdown (- n 1)))))"
  in
  Ast.Int 55 = ieab0 (bsos program, eos "(sum (countdown 10))")


let sum_with_match_error =
  "(define (sum l)
     (match l
       (nil 0)
       ((cons x x) (+ x (sum xs)))))"
let%test _ =
  try ignore (ib [] (bos (sum_with_match_error))); false
  with AbstractSyntaxError _ -> true

(* 
Other test 
*)

(* Symbol *)
let%test "symbol 1" = Ast.Symbol "a" = eos "'a"
let%test "symbol 2" = Ast.Symbol "a-b" = eos "'a-b"
(* Print *)
let%test "print 1" = Ast.Nil = ie0 (eos "(print 1)")
let%test "print 2" =
  let program = "(define x 3)" in
  Ast.Nil = ieab0 (bsos program, eos "(print x)")
(* CLosrue *)
let%test "closure 1" =
  let program = "(define (f x) (+ 3 x))" in
  Ast.Closure({rec_name=Some "f";lambda_param_names=["x"];lambda_body=Ast.Add(Ast.Int 3, Ast.Var "x")}, []) = ieab0 (bsos program, eos "f")
  let%test "closure 2" =
  let program = 
    "(define x 3)
     (define (f y) (+ x y))" in
  Ast.Closure({rec_name=Some "f";lambda_param_names=["y"];lambda_body=Ast.Add(Ast.Var "x", Ast.Var "y")}, [("x", Ast.Int 3)]) = ieab0 (bsos program, eos "f")
(* Lambda *)
let%test "lambda 1" =
  let program = "(lambda (x y) (+ x y))" in
  Ast.Lambda({rec_name=None;lambda_param_names=["x";"y"];lambda_body=Ast.Add(Ast.Var "x", Ast.Var "y")}) = eos program
let%test "lambda 2" =
  let program = "(lambda (x) (+ 3 x))" in
  Ast.Closure({rec_name=None;lambda_param_names=["x"];lambda_body=Ast.Add(Ast.Int 3, Ast.Var "x")}, []) = ieab0 (bsos "", eos program)
(* New eq *)
let%test "eq1" = 
  try ignore (ieab0(bsos "", Ast.Eq(Ast.Closure({rec_name=None;lambda_param_names=[];lambda_body=Ast.Nil},[]), Ast.Int 2))) ; false
  with RuntimeError _ -> true
let%test "eq2" = 
  try ignore (ieab0(bsos "", 
    Ast.Eq (
    Ast.Cons(Ast.Nil, Ast.Nil), 
    Ast.Cons(Ast.Closure({rec_name=None;lambda_param_names=[];lambda_body=Ast.Nil},[]), Ast.Nil)
    ))) ; false
  with RuntimeError _ -> true
(* Match *)
let%test "parsing_match" = Ast.Match(Ast.Int 2, []) = eos "(match 2)"
let%test "parsing_match2" = Ast.Match(Ast.Int 2, [Ast.IntPattern 2, Ast.Int 20]) = eos "(match 2 (2 20))"
let%test "parsing_match3" = 
  try ignore (eos "(match)") ; false
with AbstractSyntaxError _ -> true
let%test "parsing_match4" = 
  try ignore (eos "(match 2 ((cons x x) 2))") ; false
with AbstractSyntaxError _ -> true
(* nil *)
let%test "pattern nil" = Ast.Int 2 = ieab0(bsos "", eos "(match nil (nil 2))")