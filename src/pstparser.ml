open Errors

(** A "source" of characters. Since the built-in in_channel is
    inherently mutable, it makes sense for all sources to be treated
    mutably. *)
type source = File of in_channel | String of {string: string; index: int ref}

(** Read the next character from the given source and return it. None indicates end of file. *)
let source_advance s =
  match s with
  | File ic -> begin
     try
       Some (input_char ic)
     with
       End_of_file -> None
     end
  | String ({string; index}) ->
    if !index >= String.length string
    then None
    else begin
        let i = !index in
        index := i + 1;
        Some (String.get string i)
      end

type peek_reader = {
    source: source;
    next: char option ref;
    line_num: int ref;  (* line_num/column_num *of* next char *)
    column_num: int ref
  }

let peek_reader_of_source s =
  let c = source_advance s in
  {source = s; next = ref c; line_num = ref 1; column_num = ref 0}

let peek_reader_advance pr =
  if !(pr.next) = None then failwith "peek_reader_advance: advancing past EOF";
  let next = source_advance pr.source in
  let (line, col) =
    if !(pr.next) = Some '\n'
    then (!(pr.line_num)+1, 0)
    else (!(pr.line_num), !(pr.column_num)+1)
  in
  (* Printf.printf "char debug('%s')\n%!" (match next with None -> "" | Some c -> String.make 1 c);  *)
  pr.next := next;
  pr.line_num := line;
  pr.column_num := col;
  next

type raw_token = SymbolToken of string | OpenParen | CloseParen | EOFToken
[@@deriving show]
let string_of_raw_token = show_raw_token

type token = {
    raw: raw_token;
    line_num: int;
    column_num: int;
  }

let string_of_token t =
  Printf.sprintf "%s:%d:%d" (string_of_raw_token t.raw) t.line_num t.column_num

type tokenizer = {
    reader: peek_reader;
  }

let tokenizer_of_peek_reader reader = {reader}

let tokenizer_of_source s =
  s
  |> peek_reader_of_source
  |> tokenizer_of_peek_reader

let rec tokenizer_advance t =
  let rec skip_to_line t =
    match peek_reader_advance t.reader with
    | None | Some '\n' -> ()
    | _ -> skip_to_line t
  in
  let rec consume_symbol_into t buf =
    match !(t.reader.next) with
    | None | Some (';' | ')' | '(' | ' ' | '\n' | '\t' | '\r')-> Buffer.contents buf
    | Some c ->
       ignore (peek_reader_advance t.reader);
       Buffer.add_char buf c;
       consume_symbol_into t buf
  in
  let consume_symbol t = consume_symbol_into t (Buffer.create 16) in
  let c = !(t.reader.next) in
  let line_num = !(t.reader.line_num) in
  let column_num = !(t.reader.column_num) in
  match c with
  | None ->
     { raw = EOFToken;
       line_num;
       column_num;
     }
  | Some (' ' | '\n' | '\t' | '\r') -> ignore (peek_reader_advance t.reader); tokenizer_advance t
  | Some (('(' | ')') as c) ->
     ignore (peek_reader_advance t.reader);
     { raw = if c = '(' then OpenParen else CloseParen;
       line_num;
       column_num;
     }
  | Some ';' -> skip_to_line t; tokenizer_advance t
  | Some _ ->
     let sym = consume_symbol t in
     { raw = SymbolToken sym;
       line_num;
       column_num;
     }

let parse_pst t =
  let rec parse_pst_on_stack t stack =
    let tkn = tokenizer_advance t in
    (* Printf.printf "tokenizer_advance debug %s\n%!" (string_of_token tkn); *)
    match tkn.raw with
    | OpenParen -> parse_pst_on_stack t ([] :: stack)
    | CloseParen -> begin
        match stack with
        | xs1 :: xs2 :: xss -> parse_pst_on_stack t ((Pst.Node (List.rev xs1) :: xs2) :: xss)
        | [xs1] -> Some (Pst.Node (List.rev xs1))
        | [] -> raise (ParenthesizedSymbolError (Printf.sprintf "%d:%d: Unexpected close parenthesis" tkn.line_num tkn.column_num))
      end
    | EOFToken -> begin
        match stack with
        | [] -> None
        | _ -> raise (ParenthesizedSymbolError (Printf.sprintf "%d:%d: Unexpected EOF (missing close paren?)" tkn.line_num tkn.column_num))
      end
    | SymbolToken sym -> begin
        match stack with
        | xs :: xss -> parse_pst_on_stack t ((Pst.Symbol sym :: xs) :: xss)
        | [] -> Some (Pst.Symbol sym)
      end
  in
  let r = parse_pst_on_stack t [] in
  (* Printf.printf "parse_pst debug %s\n%!" (match r with None -> "None" | Some x -> Pst.string_of_pst x); *)
  r

type pstparser = tokenizer
let pstparser_of_tokenizer (t: tokenizer): pstparser = t
let pstparser_of_peek_reader = tokenizer_of_peek_reader
let pstparser_of_source s =
  s
  |> tokenizer_of_source
  |> pstparser_of_tokenizer

let pstparser_of_channel (c: in_channel): pstparser =
  File c
  |> peek_reader_of_source
  |> pstparser_of_peek_reader

let pstparser_of_string (s: string): pstparser =
  String {string=s; index=ref 0}
  |> peek_reader_of_source
  |> pstparser_of_peek_reader

let ensure_eof (p: pstparser): unit =
  let tkn = tokenizer_advance p in
  match tkn.raw with
  | EOFToken -> ()
  | _ -> raise (ParenthesizedSymbolError ("PST ended before end of file. Trailing token: " ^ string_of_token tkn))

let pst_of_string (s: string): Pst.pst =
  let parser = pstparser_of_string s in
  match parse_pst parser with
  | None -> failwith ("pst_of_string: unexpected end of file in string: '" ^ s ^ "'")
  | Some p ->
     ensure_eof parser;
     p

