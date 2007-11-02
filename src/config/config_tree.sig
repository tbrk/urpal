(* $Id$
 *
 * 20071018 T. Bourke
 * Original code.
 *)

signature CONFIG_TREE = sig

  type symbol = Atom.atom

  exception ParseError of int

  datatype entry = Entry   of symbol * entry_type
                 | Section of symbol * entry list

       and entry_type = Id     of symbol
                      | Int    of int
                      | Color  of string
                      | Real   of real
                      | String of string

  type t = entry list

  val entryToString : entry_type -> string

  val getEntry  : t * symbol list -> entry_type option
  val getSymbol : t * symbol list -> symbol option (* String -> symbol *)
  val getString : t * symbol list -> string option (* Id -> string,
                                                      Int -> string,
                                                      Color -> string  *)
  val getInt    : t * symbol list -> int option
  val getReal   : t * symbol list -> real option   (* Int -> Real      *)
  val getColor  : t * symbol list -> string option
  val getBool   : t * symbol list * bool -> bool    (* give default value *)

  val parse : (string, 'strm) StringCvt.reader -> 'strm -> t
  val output : TextIO.outstream * t -> unit

end
    
