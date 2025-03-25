type pst =
  | Symbol of string
  | Node of pst list
[@@deriving show]

(* write our own string function with nicer output *)
let string_of_pst pst =
  let rec loop buf pst =
    match pst with
    | Symbol sym -> Buffer.add_string buf sym
    | Node [] -> Buffer.add_string buf "()"
    | Node (x :: xs) ->
       Buffer.add_char buf '(';
       loop buf x;
       let rec list_loop xs =
         match xs with
         | [] -> ()
         | x :: xs -> Buffer.add_char buf ' '; loop buf x; list_loop xs
       in
       list_loop xs;
       Buffer.add_char buf ')'
  in
  let buf = Buffer.create 16 in
  loop buf pst;
  Buffer.contents buf
