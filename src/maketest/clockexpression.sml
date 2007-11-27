(* $Id$ *)

structure ClockExpression :> CLOCK_EXPRESSION =
let structure E = Expression
          and Env = Environment
          and ECVT = ExpressionCvt
in struct

  exception NonClockTerm

  type symbol    = Atom.atom
   and symbolset = AtomSet.set
   and var       = Expression.var

  datatype clockrel = Lt | Leq | Eq | Geq | Gt

  datatype clockval = Simple of int
                    | Complex of E.expr

  datatype clockterm = NonClock of E.expr
                     | CRel of E.var * clockrel * clockval
                     | CDiff of E.var * var * clockrel * clockval

  datatype t = Term of clockterm
             | And of t * t
             | Or  of t * t

  (* shortcuts over Atom and AtomSet *)
  infix <+ <- ++ <\ \ =:= ; open Symbol

  local (*{{{1*)
    (* TODO: fix smlnj-lib and replace with:
             fun pad n = Iterate.iterate (fn s=> "    " ^ s) n ""*)
    fun pad n = let fun iter (s, 0) = s
                      | iter (s, n) = iter (s ^ "    ", n - 1)
                in iter ("", n) end

    fun clkRelToStr Lt  = " < " | clkRelToStr Leq = " <= "
      | clkRelToStr Eq  = " = " | clkRelToStr Geq = " >= "
      | clkRelToStr Gt  = " > "

    fun clkValToStr (Simple i)  = Int.toString i 
      | clkValToStr (Complex e) = "#" ^ ECVT.Expr.toString e ^ "#"

    fun clkTermToStr (NonClock e)             = "#" ^ ECVT.Expr.toString e ^ "#"
      | clkTermToStr (CRel (s, rel, v))       = ECVT.Var.toString s ^
                                                clkRelToStr rel ^
                                                clkValToStr v
      | clkTermToStr (CDiff (s1, s2, rel, v)) = ECVT.Var.toString s1 ^
                                                " - " ^
                                                ECVT.Var.toString s2 ^
                                                clkRelToStr rel ^
                                                clkValToStr v
  in (*}}}1*)
  fun clkToStr (n, Term t)       = pad n ^ clkTermToStr t
    | clkToStr (n, And (c1, c2)) = clkEToStr (n, "&&", c1, c2)
    | clkToStr (n, Or (c1, c2))  = clkEToStr (n, "||", c1, c2)

  and clkEToStr (n, s, c1, c2) = clkToStr (n + 1, c1)
                               ^ "\n" ^ pad n ^ s ^ "\n"
                               ^ clkToStr (n + 1, c2)
  fun toString t = clkToStr (0, t)
  end (* local *)

  local (*{{{1*)
    fun negRel Lt  = Geq
      | negRel Leq = Gt
      | negRel Geq = Lt
      | negRel Gt  = Leq
      | negRel Eq  = raise Fail "ClockExpression.negRel: Cannot negate Eq"

    fun negT (NonClock e) = Term (NonClock (E.negate e))

      | negT (CRel (c, Eq, e))       = Or (Term (CRel (c, Lt, e)),
                                           Term (CRel (c, Gt, e)))
      | negT (CDiff (c1, c2, Eq, e)) = Or (Term (CDiff (c1, c2, Lt, e)),
                                           Term (CDiff (c1, c2, Gt, e)))

      | negT (CRel (c, rel, e))       = Term (CRel (c, negRel rel, e))
      | negT (CDiff (c1, c2, rel, e)) = Term (CDiff (c1, c2, negRel rel, e))
  in (*}}}1*)
    fun negate (Term t)       = negT t
      | negate (And (e1, e2)) = Or  (negate e1, negate e2)
      | negate (Or  (e1, e2)) = And (negate e1, negate e2)
  end

  fun getValFree (Simple _)  = emptyset
    | getValFree (Complex e) = E.getFreeNames e

  val getVarFree = E.getFreeNames o E.VarExpr

  fun getTermFree (NonClock e)            = E.getFreeNames e
    | getTermFree (CRel (s, _, cv))       = getVarFree s ++ getValFree cv
    | getTermFree (CDiff (s1, s2, _, cv)) = getVarFree s1 ++ getVarFree s2
                                                          ++ getValFree cv

  fun getFree (And (c1, c2))                 = getFree c1 ++ getFree c2
    | getFree (Or (c1, c2))                  = getFree c1 ++ getFree c2
    | getFree (Term t)                       = getTermFree t
    
  local (*{{{1*)
    fun isClk (env, v) = case Env.findVarExprType env v
                         of SOME (E.CLOCK) => true
                          | NONE           => raise NonClockTerm
                          | _              => false

    fun isClkVar (env, E.VarExpr v) = isClk (env, v)
      | isClkVar _                  = false

    fun containsClocks(env, expr) =
        not (List.null (E.filter (fn e=>isClkVar (env, e)) expr))

    fun notClk (env, e) = if containsClocks (env, e)
                          then raise NonClockTerm
                          else Term (NonClock e)

    fun toRel E.LtOp = Lt  | toRel E.LeOp = Leq
      | toRel E.EqOp = Eq  | toRel E.NeOp = raise NonClockTerm
      | toRel E.GeOp = Geq | toRel E.GtOp = Gt

    fun flipRel Lt  = Gt | flipRel Leq = Geq
      | flipRel Eq  = Eq | flipRel Geq = Leq
      | flipRel Gt  = Lt

    val toFlipRel = flipRel o toRel

    fun toVal (E.IntCExpr i) = Simple i
      | toVal e              = Complex e

    fun toVar (E.VarExpr v) = v
      | toVar _             = raise Fail "toVar: bad call"

  in (*}}}1*)

  (* raises NonClockTerm *)
  fun fromExpr (usednames, env, expr) = let

    val used = ref usednames
    val forallbindings = ref ([] : (symbol * E.ty) list)

    fun addForAllBinding (s, ty) = let
        val newName = getNewName (s, !used)
      in
        used := (!used <+ newName);
        forallbindings := (newName, ty) :: !forallbindings;
        newName
      end

    fun conv (env, e as E.BinBoolExpr {left, bop, right, ...}) = let
      (*{{{1*)
          val (l, r) = (conv (env, left), conv (env, right))
        in case (l, r, bop)
            of (Term (NonClock le), Term (NonClock re), _)=> Term (NonClock e)
             | (Term (NonClock le), _, E.ImplyOp)         => Or (negate l, r)
             | (_, _, E.ImplyOp)                          => raise NonClockTerm
             | (_, _, E.OrOp)                             => Or (l, r)
             | (_, _, E.AndOp)                            => And (l, r)
        end

      (* possibly: c1 - c2 {<,<=,==,>=,>} e *)
      | conv (env, e as E.RelExpr {left=E.BinIntExpr
                              {left=lclk, bop=E.MinusOp, right=rclk, ...},
                            rel, right, ...}) =
          if isClkVar (env, lclk) andalso isClkVar (env, rclk)
          then Term (CDiff (toVar lclk, toVar rclk, toRel rel, toVal right))
          else notClk (env, e)

      | conv (env, e as E.RelExpr {right=E.BinIntExpr
                              {left=lclk, bop=E.MinusOp, right=rclk, ...},
                            rel, left, ...})  = 
          if isClkVar (env, lclk) andalso isClkVar (env, rclk)
          then Term (CDiff (toVar lclk, toVar rclk, toFlipRel rel, toVal left))
          else notClk (env, e)

      (* possibly: c {<,<=,==,>=,>} e *)
      | conv (env, e as E.RelExpr {left, rel, right, ...}) = let in
            case (isClkVar (env, left), isClkVar (env, right))
            of (true, true)   => raise NonClockTerm
             | (false, false) => Term (NonClock e)
             | (true, false)  => Term (CRel (toVar left,toRel rel,toVal right))
             | (false, true)  => Term (CRel (toVar right, toFlipRel rel,
                                             toVal left))
            end

      | conv (env, e as E.VarExpr v)    = if isClk (env, v) then raise NonClockTerm
                                                            else Term (NonClock e)

      (* Guard must be side-effect free *)
      | conv (env, E.UnaryModExpr _)    = raise NonClockTerm
      | conv (env, E.Deadlock _)        = raise NonClockTerm

      | conv (env, e as E.IntCExpr _)           = Term (NonClock e)
      | conv (env, e as E.BoolCExpr _)          = Term (NonClock e)
      | conv (env, e as E.AssignExpr _)         = notClk (env, e)
      | conv (env, e as E.NegExpr _)            = notClk (env, e)
      | conv (env, e as E.NotExpr _)            = notClk (env, e)
      | conv (env, e as E.BinIntExpr _)         = notClk (env, e)
      | conv (env, e as E.CallExpr {args, ...}) = notClk (env, e)

      (* tested in Uppaal 4.0.2: (i==1)?(x>2):(x>3)  //int i, clock x;
         gives: incompatible arguments to inline if *)
      | conv (env, e as E.CondExpr _)           = notClk (env, e)
      | conv (env, e as E.ForAllExpr {id,ty,expr,...}) = let
            val env' = Env.addId Env.BoundScope ((id, ty), env)
          in
            if containsClocks (env', expr)
            then let (* lift forall to top-level, i.e. make prenex
                        the usual proviso on names must be respected *)
                   val id' = addForAllBinding (id, ty)
                 in
                   conv (Env.addId Env.BoundScope ((id', ty), env),
                         E.renameVar ({old=id, new=id'}, expr))
                 end
            else Term (NonClock e)
          end
      | conv (env, e as E.ExistsExpr _)         = notClk (env, e)
    (*}}}1*)
  in (conv (env, expr), !forallbindings, !used) end
  end (* local *)

  local
    (*{{{1*)
    fun fromCRel Lt  = E.LtOp  | fromCRel Leq = E.LeOp
      | fromCRel Geq = E.GeOp  | fromCRel Gt  = E.GtOp
      | fromCRel Eq  = E.EqOp

    fun fromCVal (Simple i) = E.IntCExpr i
      | fromCVal (Complex e) = e

    fun fromClkT (NonClock e) = e
      | fromClkT (CRel (v, rel, cv)) = E.RelExpr {left=E.VarExpr v,
                                                  rel=fromCRel rel,
                                                  right=fromCVal cv,
                                                  pos=E.nopos}
      | fromClkT (CDiff (v1, v2, rel, cv)) = let
            val d = E.BinIntExpr {left=E.VarExpr v1,
                                  bop=E.MinusOp,
                                  right=E.VarExpr v2,
                                  pos=E.nopos}
          in
            E.RelExpr {left=d, rel=fromCRel rel,
                       right=fromCVal cv, pos=E.nopos}
          end
    (*}}}1*)
  in
  fun toExpr (t, fas) = let
      fun toE (Term ct)        = fromClkT ct
        | toE (And (ce1, ce2)) = E.BinBoolExpr {left=toE ce1, bop=E.AndOp,
                                                right=toE ce2, pos=E.nopos}
        | toE (Or (ce1, ce2))  = E.BinBoolExpr {left=toE ce1, bop=E.OrOp,
                                                right=toE ce2, pos=E.nopos}
      fun wrapForall ((s, ty), e)=E.ForAllExpr {id=s,ty=ty,expr=e,pos=E.nopos}

    in foldl wrapForall (toE t) fas end
  end (* local *)
      
  fun rename (r, clockexpr) = let
      fun rCVar v = let val E.VarExpr v' = E.renameVar (r, E.VarExpr v)
                    in v' end

      fun rCVal (s as Simple _) = s
        | rCVal (Complex e)     = Complex (E.renameVar (r, e))

      fun rCTerm (NonClock e)              = NonClock (E.renameVar (r, e))
        | rCTerm (CRel (s, rel, cv))       = CRel (rCVar s, rel, rCVal cv)
        | rCTerm (CDiff (s1, s2, rel, cv)) = CDiff (rCVar s1, rCVar s2,
                                                    rel, rCVal cv)

      fun ren (Term t)       = Term (rCTerm t)
        | ren (And (c1, c2)) = And  (ren c1, ren c2)
        | ren (Or  (c1, c2)) = Or   (ren c1, ren c2)

    in ren clockexpr end

  (* see tech. report: canswap predicate *)
  fun conflictExists (sE, sA, psi) = let
      exception ConflictingQuantifiers

      fun termToPairs ct = let
          val f = getTermFree ct
          val e = AtomSet.intersection (f, sE)
          val a = AtomSet.intersection (f, sA)
        in
          if not (AtomSet.isEmpty e) andalso not (AtomSet.isEmpty a)
          then raise ConflictingQuantifiers else (e, a)
        end

      fun pairUnion ((e1,a1), (e2,a2)) = (e1 ++ e2, a1 ++ a2)
      val emptyPair = (emptyset, emptyset)

      fun findCommon ((e,a), (se,sa)) = let
          val ei = if AtomSet.isEmpty a then se ++ e else se
          val ai = if AtomSet.isEmpty e then sa ++ a else sa
        in (ei, ai) end

      val termPairs = map (fn cl=>map termToPairs cl) psi
        (* [ [(E,A), ..., (E,A)], ..., [(E,A), ..., (E,A)] ] *)
      val clausePairs = map (fn cl=>foldl pairUnion emptyPair cl) termPairs
        (* [ (E,A), ..., (E,A) ] *)

      val (sEf, sAf) = foldl findCommon emptyPair clausePairs

      fun conflict ([], _) = false
        | conflict ((e, a)::xs, (confE, confA)) =
            if (AtomSet.isSubset (e, sEf) andalso AtomSet.isEmpty (a))
               orelse (AtomSet.isSubset (a, sAf) andalso AtomSet.isEmpty (e))
            then conflict (xs, (confE, confA))
            else if not (AtomSet.isEmpty (AtomSet.intersection (e, confE)))
                 orelse not (AtomSet.isEmpty (AtomSet.intersection (a, confA)))
                 then true else conflict (xs, (confE ++ e, confA ++ a))

    in conflict (clausePairs, (sEf, sAf)) end
       handle ConflictingQuantifiers => true

  local (*{{{1*)
    datatype termchoice =
            KeepLeft   (* M |- (l && r)  <=>  M |- l *)
          | KeepRight  (* M |- (l && r)  <=>  M |- r *)
          | MergeEqual (* M |- (le >= c && re <= c)  <=>  M |- (le = c) *)
          | Contra     (* M |- (l && r)  <==>  M |- false *)
          | KeepBoth   (* none of the above *)

    (* Guards are integer-valued, but the clocks themselves are real-valued. *)
    fun chooseFromSimpleRel ((Lt, i), r) = (case r
                      of (Lt , j) => if i <= j then KeepLeft   else KeepRight
                       | (Leq, j) => if i <= j then KeepLeft   else KeepRight
                       | (Eq , j) => if i > j  then KeepRight  else Contra
                       | (Geq, j) => if i > j  then KeepBoth   else Contra
                       | (Gt , j) => if i > j  then KeepBoth   else Contra
                      )
      | chooseFromSimpleRel ((Leq, i), r) = (case r                 (*{{{2*)
                      of (Lt , j) => if i < j  then KeepLeft   else KeepRight
                       | (Leq, j) => if i < j  then KeepLeft   else KeepRight
                       | (Eq , j) => if i >= j then KeepRight  else Contra
                       | (Geq, j) => if i = j  then MergeEqual else
                                     if i > j  then KeepBoth   else Contra
                       | (Gt , j) => if i > j  then KeepBoth   else Contra
                      )
      | chooseFromSimpleRel ((Eq , i), r) = (case r
                      of (Lt , j) => if i < j  then KeepLeft   else Contra
                       | (Leq, j) => if i <= j then KeepLeft   else Contra
                       | (Eq , j) => if i = j  then KeepLeft   else Contra
                       | (Geq, j) => if i >= j then KeepLeft   else Contra
                       | (Gt , j) => if i > j  then KeepLeft   else Contra
                      )
      | chooseFromSimpleRel ((Geq, i), r) = (case r
                      of (Lt , j) => if i < j  then KeepBoth   else Contra
                       | (Leq, j) => if i = j  then MergeEqual else
                                     if i < j  then KeepBoth   else Contra
                       | (Eq , j) => if i <= j then KeepRight  else Contra
                       | (Geq, j) => if i > j  then KeepLeft   else KeepRight
                       | (Gt , j) => if i > j  then KeepLeft   else KeepRight
                      )
      | chooseFromSimpleRel ((Gt , i), r) = (case r
                      of (Lt , j) => if i < j  then KeepBoth   else Contra 
                       | (Leq, j) => if i < j  then KeepBoth   else Contra
                       | (Eq , j) => if i < j  then KeepRight  else Contra
                       | (Geq, j) => if i >= j then KeepLeft   else KeepRight
                       | (Gt , j) => if i >= j then KeepLeft   else KeepRight
        (*}}}2*)      )

    (* Handle ((el1 ~1 er1) && (el2 ~2 er2)) with el1=er1, er1=er2: *)
    fun chooseFromComplexRel (Lt , r) = (case r of Lt  => KeepLeft
                                                 | Leq => KeepLeft
                                                 | _   => Contra)
      | chooseFromComplexRel (Leq, r) = (case r of Geq => MergeEqual (*{{{2*)
                                                 | Gt  => Contra
                                                 | _   => KeepRight)
      | chooseFromComplexRel (Eq , r) = (case r of Lt  => Contra
                                                 | Gt  => Contra
                                                 | _   => KeepLeft)
      | chooseFromComplexRel (Geq, r) = (case r of Leq => MergeEqual
                                                 | Lt  => Contra
                                                 | _   => KeepRight)
      | chooseFromComplexRel (Gt , r) = (case r of Gt  => KeepLeft
                                                 | Geq => KeepLeft
      (*}}}2*)                                   | _   => Contra)

    fun chooseTerm (CRel (c1, rel1, Simple i1),
                    CRel (c2, rel2, Simple i2))
                   = if E.varequal (c1, c2)
                     then chooseFromSimpleRel ((rel1, i1), (rel2, i2))
                     else KeepBoth
        (*{{{2*)
      | chooseTerm (CRel (c1, rel1, Complex e1),
                    CRel (c2, rel2, Complex e2))
                   = if E.varequal (c1, c2) andalso E.equal (e1, e2)
                     then chooseFromComplexRel (rel1, rel2)
                     else KeepBoth
                                                  
      | chooseTerm (CDiff (cl1, cr1, rel1, Simple i1),
                    CDiff (cl2, cr2, rel2, Simple i2))
                   = if E.varequal (cl1, cl2) andalso E.varequal (cr1, cr2)
                     then chooseFromSimpleRel ((rel1, i1), (rel2, i2))
                     else KeepBoth

      | chooseTerm (CDiff (cl1, cr1, rel1, Complex e1),
                    CDiff (cl2, cr2, rel2, Complex e2))
                   = if E.varequal (cl1, cl2) andalso E.varequal (cr1, cr2)
                        andalso E.equal (e1, e2)
                     then chooseFromComplexRel (rel1, rel2) else KeepBoth

      | chooseTerm (NonClock(E.BoolCExpr b),_)= if b then KeepRight else Contra
      | chooseTerm (_,NonClock(E.BoolCExpr b)) = if b then KeepLeft else Contra

      | chooseTerm (NonClock e1, NonClock e2)
                   = if E.isNegation (e1, e2) then Contra else KeepBoth

      | chooseTerm _ = KeepBoth
      (*}}}2*)

    exception Contradiction

    fun termAndConj (t, cs) = let
        fun mergeEqual (CRel (c, _, v))       = CRel (c, Eq, v)
          | mergeEqual (CDiff (c1, c2, _, v)) = CDiff (c1, c2, Eq, v)

        fun tandc (rs, t, [])    = t::rs
          | tandc (rs, t, x::xs) = case chooseTerm (t, x)
                of KeepLeft   => tandc (rs, t, xs)
                 | KeepRight  => List.revAppend (rs, x::xs)
                 | MergeEqual => List.revAppend (rs, mergeEqual t::xs)
                 | Contra     => raise Contradiction
                 | KeepBoth   => tandc (x::rs, t, xs)
      in tandc ([], t, cs) end

    fun conjAndConj and1 and2 = SOME (foldl termAndConj and2 and1)
                                  handle Contradiction => NONE

    fun andDNFLists (or1, or2) = let
        fun throughSecond andx = let
            val r = List.mapPartial (conjAndConj andx) or2
          in if null r then NONE else SOME r end
      in
        List.concat (List.mapPartial throughSecond or1)
      end
    (*}}}1*)
  in
    fun toDNF (Term ct)      = [[ct]]
      | toDNF (Or (e1, e2))  = toDNF e1 @ toDNF e2
      | toDNF (And (e1, e2)) = andDNFLists (toDNF e1, toDNF e2)
  end (* local *)

  fun fromDNF xxs = let
      fun combineAnd (t, e) = And (e, Term t)
      fun makeAnd []        = NONE
        | makeAnd (x::xs)   = SOME (foldl combineAnd (Term x) xs)

      fun combineOr (e1, e2) = Or (e2, e1)
      fun makeOr []      = Term (NonClock (E.falseExpr))
        | makeOr (x::xs) = foldl combineOr x xs

      fun trueExpr (Term (NonClock t)) = E.equal (t, E.trueExpr)
        | trueExpr _                   = false

      val andclauses = List.mapPartial makeAnd xxs

    in if List.exists trueExpr andclauses
       then Term (NonClock (E.trueExpr))
       else makeOr andclauses
    end

  fun andexpr (e1, e2) = fromDNF (toDNF (And (e1, e2)))
    (* exploit the simplification built into toDNF *)

end
end

