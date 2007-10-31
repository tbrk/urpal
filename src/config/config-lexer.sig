(* $Id$ *)

signature CONFIG_LEXER = sig

  datatype lexresult = Id of Atom.atom
                     | Int of int
                     | Color of string
                     | Real of real
                     | OpenSection
                     | CloseSection
                     | Assign
                     | StringStart
                     | StringLine of string
                     | StringEnd
                     | Eof

  val makeLexer : (int -> string) -> unit -> lexresult
  val linenum   : unit -> int

end

