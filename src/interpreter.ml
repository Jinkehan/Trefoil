open Ast
open Errors

let string_of_dynenv_entry (x, v) = x ^ " -> " ^ string_of_expr v

let rec lookup dynenv name =
  match dynenv with
  | [] -> None
  | (x, value) :: dynenv ->
     if x = name
     then Some value
     else lookup dynenv name


let rec interpret_pattern pattern value =
  match pattern, value with
  | WildcardPattern, _ -> Some []
  | ConsPattern (p1, p2), Cons (v1, v2) -> begin
     match interpret_pattern p1 v1, interpret_pattern p2 v2 with
     | Some l1, Some l2 -> Some (l1 @ l2)
     | _ -> None
    end
  | IntPattern n, Int v -> if (n=v) then Some [] else None
  | BoolPattern b, Bool v -> if (b=v) then Some [] else None
  | NilPattern, Nil -> Some []
  | SymbolPattern s, Symbol v -> if (s=v) then Some [] else None
  | VarPattern x, v -> Some [(x, v)]
  | StructPattern (s, ps), StructConstructor(s', vs) -> begin
    if (s <> s') then None else
    if (List.length ps <> List.length vs) then None else
    let rec structPatternHelper ps vs b = 
      match ps, vs with
      | [], [] -> b
      | pi::ps, vi::vs -> begin
        match interpret_pattern pi vi with
        | Some l -> structPatternHelper ps vs (Some (l@Option.value ~default:[] b))
        | _ -> None
      end
      | _ -> None
    in
    structPatternHelper ps vs (Some [])
  end
  | _ -> None



let rec interpret_expression dynenv e =
  match e with
  | Int _ -> e
  | Var x -> begin
      match lookup dynenv x with
      | None -> raise (RuntimeError ("Unbound var " ^ x))
      | Some value -> value
    end
  | Add (e1, e2) -> begin
      match interpret_expression dynenv e1, interpret_expression dynenv e2 with
      | Int n1, Int n2 -> Int (n1 + n2)
      | Int _, v2 -> raise (RuntimeError ("Add applied to non-integer " ^ string_of_expr v2))
      | v1, _ -> raise (RuntimeError ("Add applied to non-integer " ^ string_of_expr v1))
    end
  | Bool _ -> e
  | Nil -> Nil
  | Sub (e1, e2) -> begin
      match interpret_expression dynenv e1, interpret_expression dynenv e2 with
      | Int n1, Int n2 -> Int (n1 - n2)
      | Int _, v2 -> raise (RuntimeError ("Add applied to non-integer " ^ string_of_expr v2))
      | v1, _ -> raise (RuntimeError ("Add applied to non-integer " ^ string_of_expr v1))
    end
  | Mul (e1, e2) -> begin
      match interpret_expression dynenv e1, interpret_expression dynenv e2 with
      | Int n1, Int n2 -> Int (n1 * n2)
      | Int _, v2 -> raise (RuntimeError ("Add applied to non-integer " ^ string_of_expr v2))
      | v1, _ -> raise (RuntimeError ("Add applied to non-integer " ^ string_of_expr v1))
    end
  | Eq (e1, e2) -> begin
    let rec eqHelper e1 e2 = 
      match e1, e2 with
      | Int v1, Int v2 -> Bool (v1 = v2)
      | Bool true, Bool true -> Bool true
      | Bool false, Bool false -> Bool true
      | Nil, Nil -> Bool true
      | Symbol v1, Symbol v2 -> Bool (v1 = v2)
      | Cons(v11, v12), Cons(v21, v22)  -> eqHelper (eqHelper v11 v21) (eqHelper v12 v22)
      | StructConstructor(s1, vs1), StructConstructor(s2, vs2) -> begin
          if ((s1=s2) && List.length(vs1)=List.length(vs2)) then
            let rec vshelper vs1 vs2 = 
              match vs1, vs2 with
              | vs1::t1, vs2::t2 -> (
                match eqHelper vs1 vs2 with
                | Bool true -> vshelper t1 t2
                | _ -> Bool false
              )
              | _ -> Bool true
            in
            vshelper vs1 vs2
          else Bool false
        end
      | Closure _, _ -> raise (RuntimeError ("Cannot compare closures" ^ string_of_expr e1))
      | _, Closure _ -> raise (RuntimeError ("Cannot compare closures" ^ string_of_expr e2))
      | _ -> Bool false
      in
      eqHelper (interpret_expression dynenv e1) (interpret_expression dynenv e2)
    end
  | If (e1, e2, e3) -> begin
      match interpret_expression dynenv e1 with
      | Bool false -> interpret_expression dynenv e3
      | _ -> interpret_expression dynenv e2
    end
  | Let (e1, e2) -> begin
      let rec getExtended defs env = 
        match defs with
        | [] -> env
        | (xi, ei)::tl -> getExtended tl ((xi, (interpret_expression dynenv ei))::env) in
      let extendedEnv = getExtended e1 dynenv in
      interpret_expression extendedEnv e2
      
    end
  | Cons (e1, e2) -> begin
      Cons (interpret_expression dynenv e1, interpret_expression dynenv e2)
    end
  | IsNil (e1) -> begin
      if ((interpret_expression dynenv e1) = Nil) then Bool (true)
      else Bool (false)
    end
  | IsCons (e1) -> begin
      match interpret_expression dynenv e1 with
      | Cons (_, _) -> Bool (true)
      | _ -> Bool (false)
    end
  | Car (e1) -> begin
      match interpret_expression dynenv e1 with
      | Cons (a, _) -> a
      | _ -> raise (RuntimeError ("Car applied to non-cons " ^ string_of_expr e1))
    end
  | Cdr (e1) -> begin
      match interpret_expression dynenv e1 with
      | Cons (_, b) -> b
      | _ -> raise (RuntimeError ("Car applied to non-cons " ^ string_of_expr e1))
    end
  | Cond (eList) -> begin
      let rec evalClauses l = 
        match l with
        | [] -> raise (RuntimeError ("Cond has to have at least one case passing "^string_of_expr e))
        | (pi, bi)::tl ->
          match interpret_expression dynenv pi with
          | Ast.Bool false -> evalClauses tl
          | _ -> interpret_expression dynenv bi
      in
      evalClauses eList
    end
  | Call (f, args) -> begin
      match interpret_expression dynenv f with 
      | Closure (fa, defenv) -> begin
        if (List.compare_lengths args fa.lambda_param_names <> 0) then raise (RuntimeError ("Incorrect argument for f "^string_of_expr e))
        else (
          let vals = List.map (fun x -> (interpret_expression dynenv x)) args in
          let extendedEnv = (List.combine fa.lambda_param_names vals)@defenv in
          match fa.rec_name with
          | Some f -> interpret_expression ((f, Closure(fa, extendedEnv))::extendedEnv) fa.lambda_body
          | _ -> interpret_expression extendedEnv fa.lambda_body
        )
      end
      | _ -> raise (RuntimeError ("Unbound function "^string_of_expr f))
    end
  | Symbol _ -> e
  | Print (e) -> begin
    let v = interpret_expression dynenv e in
    print_endline ("Result: " ^ string_of_expr v);
    Nil
  end
  | Closure _ -> e
  | Lambda (fa) -> Closure ({ rec_name=None; lambda_param_names=fa.lambda_param_names; lambda_body=fa.lambda_body }, dynenv)
  | StructConstructor (s, es) -> StructConstructor(s, List.map (fun x -> interpret_expression dynenv x) es)
  | StructPredicate (s, e) -> begin
    match interpret_expression dynenv e with
    | StructConstructor(s', _) -> if (s=s') then Bool true else Bool false
    | _ -> Bool false
  end
  | StructAccess (s, i, e) -> begin
      match interpret_expression dynenv e with
      | StructConstructor(s', vs) -> if (s=s' && i<List.length(vs)) then List.nth vs i else raise (RuntimeError ("Error accessing struct element "^string_of_expr e)) 
      | a -> raise (RuntimeError ("Error accessing struct element "^string_of_expr (a ))) 
    end
  | Match (e, clauses) -> begin
    let v = interpret_expression dynenv e in
    let rec matchHelper v clauses = 
      match clauses with
      | [] -> raise (RuntimeError ("Melformed matching"))
      | (pi, bi)::tl -> 
        match interpret_pattern pi v with
        | Some blist -> interpret_expression (blist@dynenv) bi
        | _ -> matchHelper v tl
    in
    matchHelper v clauses
  end

let interpret_binding dynenv b =
  match b with
  | VarBinding (x, e) -> begin
      let value = interpret_expression dynenv e in
      Printf.printf "%s = %s\n%!" x (string_of_expr value);
       (x, value) :: dynenv
    end
  | TopLevelExpr e -> begin
      let v = interpret_expression dynenv e in
      print_endline (string_of_expr v);
      dynenv
    end
  | TestBinding e -> begin
      match interpret_expression dynenv e with
      | Bool true -> dynenv
      | _ -> raise (RuntimeError ("failing test binding" ^ string_of_expr e))
    end
  | FunctionBinding fb -> begin
      (fb.func_name, Closure ({rec_name= Some fb.func_name; lambda_param_names=fb.param_names; lambda_body=fb.body}, dynenv)) :: dynenv
    end
  | StructBinding sb -> begin
      let rec vnameHelper i = 
        if (i>0) then (vnameHelper (i-1))@["x"^(string_of_int i)] else []
      in
      let s = (sb.struct_name, Closure ({rec_name=Some sb.struct_name; lambda_param_names=vnameHelper (List.length sb.field_names); lambda_body=StructConstructor(sb.struct_name, List.map (fun x -> Ast.Var x) (vnameHelper (List.length sb.field_names)))}, dynenv)) in
      let sq = (sb.struct_name^"?", Closure ({rec_name=Some (sb.struct_name^"?"); lambda_param_names=["x"]; lambda_body=StructPredicate(sb.struct_name, Ast.Var "x")}, dynenv)) in
      let rec fsHelper fs i = 
        match fs with
        | [] -> []
        | f::fs -> (sb.struct_name^"-"^f, Closure ({rec_name=Some (sb.struct_name^"-"^f); lambda_param_names=["x"]; lambda_body=StructAccess(sb.struct_name, i, Ast.Var "x")}, s::sq::dynenv))::(fsHelper fs (i+1))
      in
      s::sq::(fsHelper sb.field_names 0)@dynenv
    end


(* the semantics of a whole program (sequence of bindings) *)
let interpret_bindings dynenv bs =
  List.fold_left interpret_binding dynenv bs

(* starting from dynenv, first interpret the list of bindings in order. then, in
   the resulting dynamic environment, interpret the expression and return its
   value *)
let interpret_expression_after_bindings dynenv bindings expr =
  interpret_expression (interpret_bindings dynenv bindings) expr

