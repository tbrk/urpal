(* $Id$ *)

structure Symbol : SYMBOL = struct
  infix <+ <- ++ <\ \ =:=

  type symbol = Atom.atom
  type symbolset = AtomSet.set

  fun set <+ item = AtomSet.add (set, item)
  fun set <\ item = AtomSet.difference (set, AtomSet.singleton item)
  fun setA \ setB = AtomSet.difference (setA, setB)
  fun i <- set    = AtomSet.member (set, i)
  val op++        = AtomSet.union
  val emptyset    = AtomSet.empty
  val `           = Atom.atom

  fun s1 =:= s2   = Atom.compare (s1, s2) = EQUAL

  local structure SS = Substring in
  fun getNewName (base, names) = 
    (*{{{1*)
    if not (AtomSet.member (names, base)) then base
    else let
           fun getPrefix s = SS.string (SS.dropr Char.isDigit (SS.full s))

           fun getSuffix s = let
               val suf = SS.taker Char.isDigit (SS.full s)
             in
               if SS.isEmpty suf then 0
               else valOf (Int.fromString (SS.string suf))
             end
          
           val prefix = getPrefix (Atom.toString base)

           fun maxSuffix (a, m) = let val s = Atom.toString a in
               if String.isPrefix prefix s
               then Int.max (m, getSuffix s) else m
             end

           val suffix = (AtomSet.foldl maxSuffix 0 names) + 1
         in
            Atom.atom (prefix ^ Int.toString suffix) 
         end (*}}}1*)
  end (*local*)

end

