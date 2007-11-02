(* $Id$

   20070822 T. Bourke
   Attributes with minimal type structure.
 *)

structure TextAttribute =
struct
  datatype t = Att of string * string option

  fun name (Att (n, _)) = n

  fun hasValue (Att (_, NONE)) = false
    | hasValue _               = true

  fun value (Att (_, SOME v)) = v
    | value (Att (n, NONE))   = raise Fail ("attribute "^n^" has no value")

end

