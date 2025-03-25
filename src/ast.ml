include Ast_types
open Errors

let rec pattern_of_pst p =
  match p with
  | Pst.Symbol sym -> begin
      match int_of_string_opt sym with
      | Some n -> IntPattern n
      | None ->
         match sym with
         | "_" -> WildcardPattern
         | "true" -> BoolPattern true
         | "false" -> BoolPattern false
         | "nil" -> NilPattern
         | _ ->
            if String.get sym 0 = '\'' (* if the string starts with an apostrophe *)
            then let sym_without_apostrophe = String.sub sym 1 (String.length sym - 1)
                 in SymbolPattern sym_without_apostrophe
            else VarPattern sym
    end
  | Pst.Node [] -> raise (AbstractSyntaxError "Expected pattern but got '()'")
  | Pst.Node (head :: args) ->
     match head, args with
     | Pst.Symbol "cons", [p1; p2] -> ConsPattern (pattern_of_pst p1, pattern_of_pst p2)
     | Pst.Symbol s, ps -> StructPattern (s, List.map (pattern_of_pst) ps)
     | _ -> raise (AbstractSyntaxError ("Expected pattern, but got " ^ Pst.string_of_pst p))

let pattern_of_string s =
  s
  |> Pstparser.pst_of_string
  |> pattern_of_pst

(* last stage of parser: converts pst to expr *)
let rec expr_of_pst p =
  match p with
  | Pst.Symbol sym -> begin
     try
       Int (int_of_string sym)
     with
       Failure _ ->
       match sym with
       | "true" -> Bool true
       | "false" -> Bool false
       
       | "nil" -> Nil

       | _ -> begin
          if String.get sym 0 = '\''
            then Symbol (String.sub sym 1 (String.length sym -1))
            else Var sym
        end
    end
  | Pst.Node [] -> raise (AbstractSyntaxError "Expected expression but got '()'")
  | Pst.Node (head :: args) ->
     match head, args with
     | Pst.Symbol "+", [left; right] -> Add (expr_of_pst left, expr_of_pst right)
     | Pst.Symbol "+", _ -> raise (AbstractSyntaxError ("operator + expects 2 args but got " ^ Pst.string_of_pst p))

     | Pst.Symbol "-", [left; right] -> Sub (expr_of_pst left, expr_of_pst right)
     | Pst.Symbol "-", _ -> raise (AbstractSyntaxError ("operator + expects 2 args but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "*", [left; right] -> Mul (expr_of_pst left, expr_of_pst right)
     | Pst.Symbol "*", _ -> raise (AbstractSyntaxError ("operator + expects 2 args but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "=", [left; right] -> Eq (expr_of_pst left, expr_of_pst right)
     | Pst.Symbol "=", _ -> raise (AbstractSyntaxError ("operator = expects 2 args but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "let", [Pst.Node node; arg] -> begin
        let rec letdefHelper node nameDefined= 
          match node with
          | [] -> []
          | Pst.Node (Pst.Symbol n::e::[])::tl -> 
            if (List.exists (fun x -> x=n) nameDefined) then
              raise (AbstractSyntaxError ("Variable name "^n^" appears more than once"))
            else
              (n, expr_of_pst e)::letdefHelper tl (n::nameDefined)
          | _ -> raise (AbstractSyntaxError ("first argument of Let expects list of string*expr but got "^Pst.string_of_pst p))
          in
        Let (letdefHelper node [], expr_of_pst arg)
      end
     | Pst.Symbol "let", _ -> raise (AbstractSyntaxError ("operator Let expects 2 args but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "cons", [left; right] -> Cons (expr_of_pst left, expr_of_pst right)
     | Pst.Symbol "cons", _ -> raise (AbstractSyntaxError ("operator cons expects 2 args but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "nil?", [arg] -> IsNil (expr_of_pst arg)
     | Pst.Symbol "nil?", _ -> raise (AbstractSyntaxError ("operator nil? expects 1 arg but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "cons?", [arg] -> IsCons (expr_of_pst arg)
     | Pst.Symbol "cons?", _ -> raise (AbstractSyntaxError ("operator cons? expects 1 arg but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "car", [arg] -> Car (expr_of_pst arg)
     | Pst.Symbol "car", _ -> raise (AbstractSyntaxError ("operator car expects 1 arg but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "cdr", [arg] -> Cdr (expr_of_pst arg)
     | Pst.Symbol "cdr", _ -> raise (AbstractSyntaxError ("operator cdr expects 1 arg but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "cond", arg -> begin
        let rec getArgs arg = 
          match arg with
          | [] -> []
          | Pst.Node [e1;e2]::tl -> (expr_of_pst e1, expr_of_pst e2)::getArgs tl 
          | _ -> raise (AbstractSyntaxError ("Operator cond expects pairs of expressions but got "^Pst.string_of_pst p))
        in
        Cond (getArgs arg)
      end

     | Pst.Symbol "if", [branch; thn; els] -> If (expr_of_pst branch, expr_of_pst thn, expr_of_pst els)
     | Pst.Symbol "if", _ -> raise (AbstractSyntaxError ("'if' special form expects 3 args but got " ^ Pst.string_of_pst p))
     
     | Pst.Symbol "print", [arg] -> Print (expr_of_pst arg)
     | Pst.Symbol "print", _ -> raise (AbstractSyntaxError ("'print' expression expects 1 arg but got " ^ Pst.string_of_pst p))
     
     | Pst.Symbol "lambda", [Node args; exp] -> begin
      let rec getArgs arg addedArgs = 
        match arg with 
        | [] -> addedArgs
        | Pst.Symbol a::arg -> if List.exists (fun x-> x=a) addedArgs then raise (AbstractSyntaxError ("args of function cannot have repeated names "^Pst.string_of_pst p))
                  else getArgs arg (addedArgs@[a])
        | _ ->raise (AbstractSyntaxError ("args of function cannot have repeated names "^Pst.string_of_pst p))
      in
      Lambda ({rec_name=None; lambda_param_names= (getArgs args []); lambda_body= expr_of_pst exp})
     end
     | Pst.Symbol "lambda", _ -> raise (AbstractSyntaxError ("'lambda' expression expects 2 arg but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "match", expr::clauses -> begin
        let rec vars_of_pattern p usedVar = 
          match p with 
          | VarPattern x -> if List.mem x usedVar then
              raise (AbstractSyntaxError ("Duplicate variable " ^ x)) else x :: usedVar
          | ConsPattern (a, b) -> let usedVar = vars_of_pattern a usedVar in
              vars_of_pattern b usedVar
          | StructPattern (_, pl) -> List.fold_left (fun v c -> vars_of_pattern c v) usedVar pl
          | _ -> usedVar
        in
        let rec getClauses cla = 
          match cla with
          | [] -> []
          | Pst.Node [p;e]::tl -> 
            let _ = vars_of_pattern (pattern_of_pst(p)) [] in
            (pattern_of_pst p, expr_of_pst e)::getClauses tl 
          | _ -> raise (AbstractSyntaxError ("Operator cond expects pairs of expressions but got "^Pst.string_of_pst p))
        in
        Match (expr_of_pst expr, getClauses clauses)
      end
     | Pst.Symbol "match", _ -> raise (AbstractSyntaxError ("'match' expression expects at least 1 arg but got " ^ Pst.string_of_pst p))
     | f, args -> Call (expr_of_pst f, List.map (expr_of_pst) args )
     

let expr_of_string s =
  s
  |> Pstparser.pst_of_string
  |> expr_of_pst


let binding_of_pst p =
  match p with
  | Pst.Symbol _ -> TopLevelExpr (expr_of_pst p)
  | Pst.Node [] -> raise (AbstractSyntaxError "Expected binding but got '()'")
  | Pst.Node (head :: args) ->
     match head, args with
     | Pst.Symbol "define", [Pst.Symbol lhs_var; rhs] -> VarBinding (lhs_var, expr_of_pst rhs)
     | Pst.Symbol "define", [Pst.Node (Pst.Symbol n::args); body] -> begin
        let rec getArgs arg addedArgs = 
          match arg with 
          | [] -> addedArgs
          | Pst.Symbol a::arg -> if List.exists (fun x-> x=a) addedArgs then raise (AbstractSyntaxError ("args of function cannot have repeated names "^Pst.string_of_pst p))
                    else getArgs arg (addedArgs@[a])
          | _ ->raise (AbstractSyntaxError ("args of function cannot have repeated names "^Pst.string_of_pst p))
        in
        FunctionBinding {func_name=n; param_names=(getArgs args []); body=expr_of_pst body}
      end
     | Pst.Symbol "define", _ -> raise (AbstractSyntaxError("This definition is malformed " ^ Pst.string_of_pst p))
     | Pst.Symbol "struct", Pst.Symbol sname::fnames -> begin
        let rec getfnames fn addedfn = 
          match fn with
          | [] -> addedfn
          | Pst.Symbol f::fn -> if List.exists (fun x-> x=f) addedfn then raise (AbstractSyntaxError ("args of function cannot have repeated names "^Pst.string_of_pst p))
            else getfnames fn (addedfn@[f])
          | _ ->raise (AbstractSyntaxError ("error parsing struct fields "^Pst.string_of_pst p))
        in
        StructBinding { struct_name = sname; field_names= (getfnames fnames []) }
     end
     | Pst.Symbol "struct", _ -> raise (AbstractSyntaxError("This struct is malformed " ^ Pst.string_of_pst p))
     | Pst.Symbol "test", [rhs] -> TestBinding (expr_of_pst rhs)
     | Pst.Symbol "test", _ -> raise (AbstractSyntaxError("This test is malformed " ^ Pst.string_of_pst p))

     | Pst.Node _, _ -> raise (AbstractSyntaxError("Expected binding to start with a symbol but got " ^ Pst.string_of_pst p))
     | _ -> TopLevelExpr (expr_of_pst p)

let binding_of_string s =
  s
  |> Pstparser.pst_of_string
  |> binding_of_pst

let bindings_of_string s =
  let p = Pstparser.pstparser_of_string s in
  let rec parse_binding_list () =
    match Pstparser.parse_pst p with
    | None -> []
    | Some pst ->
       binding_of_pst pst :: parse_binding_list ()
  in
  parse_binding_list ()
