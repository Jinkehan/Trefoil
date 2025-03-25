type pattern =
  | WildcardPattern
  | VarPattern of string
  | IntPattern of int
  | BoolPattern of bool
  | NilPattern
  | SymbolPattern of string
  | ConsPattern of pattern * pattern
  | StructPattern of string * pattern list
[@@deriving show]
let string_of_pattern = show_pattern


type expr =
  | Int of int
  | Bool of bool
  | Var of string
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Eq of expr * expr
  | If of expr * expr * expr
  | Let of (string * expr) list * expr
  | Nil
  | Cons of expr * expr
  | IsNil of expr
  | IsCons of expr
  | Car of expr
  | Cdr of expr
  | Symbol of string
  | Cond of (expr * expr) list 

  | Call of expr * expr list
  | Closure of func_args * dynamic_env
  | Lambda of func_args

  | StructConstructor of string * expr list
  | StructAccess of string * int * expr
  | StructPredicate of string * expr

  | Match of expr * (pattern * expr) list

  | Print of expr
[@@deriving show]
and func_args = { rec_name: string option; lambda_param_names: string list; lambda_body: expr }
[@@deriving show]
and function_binding = { func_name: string; param_names: string list; body: expr }
[@@deriving show]
and struct_binding = { struct_name: string; field_names: string list }
[@@deriving show]
and binding =
  | VarBinding of string * expr
  | TopLevelExpr of expr
  | TestBinding of expr
  | FunctionBinding of function_binding
  | StructBinding of struct_binding
[@@deriving show]
and dynamic_env = (string * expr) list 
[@@deriving show]

let string_of_expr = show_expr
let string_of_binding = show_binding
let string_of_dynamic_env = show_dynamic_env
