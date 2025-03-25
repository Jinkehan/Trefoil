open Trefoil4lib
open Errors

let main () =
  (* Our main read-eval-print loop.

     Reads bindings from t one at a time and evaluates them on the current
     dynamic environment. *)
  let rec loop dynenv errored_so_far pstparser =
    try
      match Pstparser.parse_pst pstparser with
      | None -> (dynenv, errored_so_far)
      | Some pst ->
         (* uncomment next line to print each PST for debugging *)
         (* print_endline (Pst.string_of_pst pst); *)
         let b = Ast.binding_of_pst pst in
         (* uncomment next line to print each AST for debugging *)
         (* print_endline (Ast.string_of_binding b); *)
         let dynenv = Interpreter.interpret_binding dynenv b in
         loop dynenv errored_so_far pstparser
    with
      (ParenthesizedSymbolError msg |
       AbstractSyntaxError msg |
       RuntimeError msg) -> begin
        Printf.printf "error:%d:%d: %s\n%!" !(pstparser.reader.line_num) !(pstparser.reader.column_num) msg;
        loop dynenv true pstparser
      end
    | InternalError msg as e ->
        Printf.printf "error:%d:%d: Impossible! %sPlease contact interpreter implementor!! \n%!"
          !(pstparser.reader.line_num) !(pstparser.reader.column_num) msg;
        raise e
  in
  (* beginning of code which handles command line arguments. *)
  (* you don't need to understand how this works. *)
  let arg: string option ref = ref None in
  let set_arg s =
    match !arg with
    | None -> arg := Some s
    | Some _ -> raise (Arg.Bad "got multiple command line arguments, but expected at most one (the filename)")
  in
  let usage =
    "USAGE\n\n" ^

    "    trefoil [FILENAME]\n\n" ^

    "With no argument, read bindings from standard input.\n" ^
    "With an argument, read bindings from the given filename.\n\n" ^

    "OPTIONS"
  in
  Arg.parse [] set_arg usage;
  print_endline "welcome to trefoil v4!";
  let source = match !arg with
    | None -> stdin
    | Some s -> open_in s
  in
  (* end of code that handles command line arguments *)

  (* now call the main read-eval-print loop *)
  let pstparser = Pstparser.pstparser_of_source (Pstparser.File source) in
  let final_env, errored = loop [] false pstparser in
  if errored then exit 1;
  print_endline "final environment:";
  List.iter (fun entry -> 
      print_string "  "; 
      print_endline (Interpreter.string_of_dynenv_entry entry)) 
    final_env

let () = main ()
