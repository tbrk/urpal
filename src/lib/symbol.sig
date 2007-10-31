(* $Id$
 *
 * Shortcuts over Atom and AtomSet.
 *    infix <+ <- ++ </ =:= ; open Symbol
 *)

signature SYMBOL = sig

  type symbol = Atom.atom
  type symbolset = AtomSet.set

  val emptyset   : symbolset

  val <+         : symbolset * symbol -> symbolset
  val <-         : symbol * symbolset -> bool
  val ++         : symbolset * symbolset -> symbolset
  val </         : symbolset * symbol -> symbolset
  val =:=        : symbol * symbol -> bool

  val getNewName : symbol * symbolset -> symbol

end

