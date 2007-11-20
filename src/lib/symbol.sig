(* $Id$
 *
 * Shortcuts over Atom and AtomSet.
 *    infix <+ <- ++ <\ \ =:=  (*`*); open Symbol
 *)

signature SYMBOL = sig

  type symbol = Atom.atom
  type symbolset = AtomSet.set

  val emptyset   : symbolset

  val <+         : symbolset * symbol -> symbolset    (* add    *)
  val <-         : symbol * symbolset -> bool         (* member *)
  val ++         : symbolset * symbolset -> symbolset (* union  *)
  val <\         : symbolset * symbol -> symbolset    (* difference *)
  val \          : symbolset * symbolset -> symbolset (* difference *)
  val =:=        : symbol * symbol -> bool            (* equality *)
  val `          : string -> symbol                   (* atom *)

  val getNewName : symbol * symbolset -> symbol

end

