(* The first three exceptions indicate something that is the user's fault *)
exception ParenthesizedSymbolError of string
exception AbstractSyntaxError of string
exception RuntimeError of string


(* This exception indicates something that is our fault. *)
exception InternalError of string
