(* $Id$ *)

structure ActionMap : sig
  include ORD_MAP

  val actionInsert : ((Atom.atom * 'a) * 'a map) -> 'a map
end
=
let structure E = Expression
in struct
  open ActionRedBlackMap

  fun actionInsert ((chandir, v), m) = let
      fun getTail (l, r) = (Atom.atom (Substring.string l), Substring.first r)

      val s = Substring.full (Atom.toString chandir)
      val l = Substring.size s
      val (n, d) = if l = 0 then (chandir, NONE)
                   else getTail (Substring.splitAt (s, l-1))
    in
      case d of
        SOME #"?" => insert' (((n, E.Input),  v), m)
      | SOME #"!" => insert' (((n, E.Output), v), m)
      | _ => let val m' = insert' (((n, E.Input), v), m)
             in insert' (((n, E.Output), v), m') end
    end

end end

