(* $Id$
 *
 * 20070614 T. Bourke 
 *
 * Based on Uppaal Timed Automata Parser Library documentation
 * http://www.cs.auc.dk/~behrmann/utap/syntax.html 20070424
 *
 * Adapted from `Modern compiler implementation in ML', Appel 1998.
 *
 *)

(* TODO:
 *  * Conditionals on LHS of assignments
 *)

structure Expression : EXPRESSION =
struct
  
  (* shortcuts over Atom and AtomSet *)
  infix <+ <- ++ <\ \ =:= ; open Symbol

  type symbol = Atom.atom

  type unique = int
  val nextUnique = ref 0
  fun uniqueTag () = !nextUnique before (nextUnique := !nextUnique + 1)

  (* Const takes preference over Meta: since neither need be stored in
     the state vector *)
  datatype tyqual = NoQual | Meta | Const
  datatype direction = Output | Input

  val defaultBounds = (~32768, 32767)

  datatype unaryModOp = PreIncOp | PostIncOp | PreDecOp | PostDecOp

  datatype binIntOp   = PlusOp | MinusOp | TimesOp | DivideOp | ModOp
                      | BAndOp | BOrOp | BXorOp | ShlOp | ShrOp
                      | MinOp | MaxOp

  datatype rel        = LtOp | LeOp | EqOp | NeOp | GeOp | GtOp

  datatype binBoolOp  = AndOp | OrOp | ImplyOp

  datatype assignOp   = AssignOp | PlusEqOp | MinusEqOp | TimesEqOp
                      | DivideEqOp | ModEqOp | BOrEqOp | BAndEqOp
                      | BXorEqOp | ShlEqOp | ShrEqOp

  datatype var        = SimpleVar    of symbol
                      | ReturnVar of {func: symbol, args: expr list}
                      | RecordVar    of var * symbol
                      | SubscriptVar of var * expr

  and      ty         = VOID
  (*{{{1*)            | INT of (expr * expr) option * tyqual
                      | BOOL of tyqual
                      | CLOCK
                      | CHANNEL of {urgent: bool, broadcast: bool}
                      | SCALAR of expr * tyqual * unique
                      | RECORD of (symbol * ty) list * tyqual * unique
                      | ARRAY of ty * unresolvedty
  (*}}}1*)            | NAME of symbol * tyqual * ty option

  and      expr       = VarExpr      of var
  (*{{{1*)            | IntCExpr     of int
                      | BoolCExpr    of bool
                      | CallExpr     of {func: symbol,
                                         args: expr list}
                      | NegExpr      of expr
                      | NotExpr      of expr
                      | UnaryModExpr of {uop: unaryModOp,
                                         expr: expr}
                      | BinIntExpr   of {left: expr,
                                         bop: binIntOp,
                                         right: expr}
                      | BinBoolExpr  of {left: expr,
                                         bop: binBoolOp,
                                         right: expr}
                      | RelExpr      of {left: expr,
                                         rel: rel,
                                         right: expr}
                      | AssignExpr   of {var: expr,   (* Can be ?: *)
                                         aop: assignOp,
                                         expr: expr}
                      | CondExpr     of {test: expr,
                                         trueexpr: expr,
                                         falseexpr: expr}
                      | ForAllExpr   of {id: symbol,
                                         ty: ty,
                                         expr: expr}
                      | ExistsExpr   of {id: symbol,
                                         ty: ty,
                                         expr: expr}
  (*}}}1*)            | Deadlock

  and      boundid    = BoundId of symbol * ty

  and    unresolvedty = Unresolved of symbol
                      | Type of ty

  fun varName (SimpleVar nm)          = Atom.toString nm
    | varName (ReturnVar {func, ...}) = Atom.toString func ^ "(...)"
    | varName (RecordVar (v, field))  = varName v ^ "." ^ Atom.toString field
    | varName (SubscriptVar (v, _))   = varName v ^ "[]"

  fun getFreeNames e = let
      (*{{{1*)
      val add = AtomSet.add
      val mem = AtomSet.member
      val without = AtomSet.delete

      fun lv (s, SimpleVar v)            = add (s, v)
        | lv (s, ReturnVar {args, ...}) = lelist (s, args)
        | lv (s, RecordVar (v, _))      = lv (s, v)
        | lv (s, SubscriptVar (v, sub)) = le (lv (s, v), sub)

      and le (s, VarExpr v)                     = lv (s, v)
        | le (s, IntCExpr _)                    = s
        | le (s, BoolCExpr _)                   = s
        | le (s, CallExpr {args, ...})          = lelist (s, args)
        | le (s, NegExpr expr)                  = le (s, expr)
        | le (s, NotExpr expr)                  = le (s, expr)
        | le (s, UnaryModExpr {expr, ...})      = le (s, expr)
        | le (s, BinIntExpr {left, right, ...}) = lele (s, left, right)
        | le (s, BinBoolExpr {left, right, ...})= lele (s, left, right)
        | le (s, RelExpr {left, right, ...})    = lele (s, left, right)
        | le (s, AssignExpr {var, expr, ...})   = lele (s, var, expr)
        | le (s, CondExpr {test, trueexpr,
                  falseexpr, ...}) = le (lele (s, trueexpr, falseexpr), test)
        | le (s, ForAllExpr {id, expr, ...})    = let val s' = le (s, expr)
                                in (* remove bound variables from set *)
                                  if mem (s, id) orelse not (mem (s', id))
                                  then s else without (s', id)
                                end
        | le (s, ExistsExpr {id, expr, ...})    = let val s' = le (s, expr)
                                in (* remove bound variables from set *)
                                  if mem (s, id) orelse not (mem (s', id))
                                  then s else without (s', id)
                                end
        | le (s, Deadlock)                    = s

      and lele (s, a, b) = le (le (s, a), b)
      and lelist (s, l) = List.foldl (fn (e, s)=> le (s, e)) s l

    in le (AtomSet.empty, e) end (*}}}1*)

  val getBoundNames = let
      fun add (BoundId (n, _), s) = s <+ n
    in foldl add emptyset end

  fun renameVars' (renmap : symbol AtomMap.map, oldnames, newnames) = let
      (*{{{1*)
      val bothnames = oldnames ++ newnames

      fun re (VarExpr v)                  = VarExpr (rv v)
        | re (e as IntCExpr _)            = e
        | re (e as BoolCExpr _)           = e
        | re (CallExpr {func, args}) = CallExpr
                                    {func=func, args=map re args}

        | re (NegExpr expr)          = NegExpr (re expr)
        | re (NotExpr expr)          = NotExpr (re expr)

        | re (UnaryModExpr {uop, expr}) = UnaryModExpr
                                    {uop=uop, expr=re expr}

        | re (BinIntExpr {left, bop, right}) = BinIntExpr
                          {left=re left, bop=bop, right=re right}

        | re (BinBoolExpr {left, bop, right}) = BinBoolExpr
                          {left=re left, bop=bop, right=re right}

        | re (RelExpr {left, rel, right}) = RelExpr
                          {left=re left, rel=rel, right=re right}

        | re (AssignExpr {var, aop, expr}) = AssignExpr
                          {var=re var, aop=aop, expr=re expr}

        | re (CondExpr {test, trueexpr, falseexpr}) = CondExpr
                          {test=re test, 
                           trueexpr=re trueexpr,
                           falseexpr=re falseexpr}

        | re (e as ForAllExpr {id, ty, expr}) =
            if id <- oldnames
            then (* bound, so don't rename within scope *)
                 ForAllExpr {id=id, ty=ty,
                             expr=renameVars' (
                               #1 (AtomMap.remove (renmap, id)),
                               AtomSet.delete (oldnames, id),
                               newnames) expr}

            else if id <- newnames
                 then let (* avoid variable capture *)
                        val fresh = getNewName (id, getFreeNames expr
                                                    ++ bothnames)
                      in
                        ForAllExpr {id=fresh, ty=ty,
                                    expr=renameVars' (
                                      AtomMap.insert (renmap, id, fresh),
                                      oldnames <+ id,
                                      newnames <+ fresh) expr}
                      end
            else ForAllExpr {id=id, ty=ty, expr=re expr}

        | re (e as ExistsExpr {id, ty, expr}) =
            if id <- oldnames
            then (* bound, so don't rename within scope *)
                 ExistsExpr {id=id, ty=ty,
                             expr=renameVars' (
                               #1 (AtomMap.remove (renmap, id)),
                               AtomSet.delete (oldnames, id),
                               newnames) expr}

            else if id <- newnames
                 then let (* avoid variable capture *)
                        val fresh = getNewName (id, getFreeNames expr
                                                    ++ bothnames)
                      in
                        ExistsExpr {id=fresh, ty=ty,
                                    expr=renameVars' (
                                      AtomMap.insert (renmap, id, fresh),
                                      oldnames <+ id,
                                      newnames <+ fresh) expr}
                      end
            else ExistsExpr {id=id, ty=ty, expr=re expr}

        | re (e as Deadlock)  = e

      and rv (v as SimpleVar s)       = (case AtomMap.find (renmap, s) of
                                           NONE    => v
                                         | SOME s' => SimpleVar s')
        | rv (ReturnVar {func, args}) = ReturnVar {func=func,
                                              args=map re args}
        | rv (RecordVar (v, field))   = RecordVar (rv v, field)
        | rv (SubscriptVar (v, sub))  = SubscriptVar (rv v, re sub)

    in re end
    (*}}}1*)

  fun renameVars map = let
      fun f (old, new, (oldnames, newnames)) = (oldnames <+ old, newnames <+ new)
      val (oldnames, newnames) = AtomMap.foldli f (emptyset, emptyset) map
    in renameVars' (map, oldnames, newnames) end

  fun renameVar ({old, new}, expr) =
          renameVars' (AtomMap.insert (AtomMap.empty, old, new),
                       AtomSet.singleton old, AtomSet.singleton new) expr

  fun stripArray ty = let
      fun strip (ARRAY (ty, Unresolved tynm), subs) = strip (ty, NAME (tynm,
                                                            NoQual,NONE)::subs)
        | strip (ARRAY (ty, Type tye), subs)        = strip (ty, tye::subs)
        | strip (ty, subs)                          = (ty, subs)
    in strip (ty, []) end

  fun inc (IntCExpr c) = IntCExpr (c+1)
    | inc (BinIntExpr {left, bop=MinusOp, right=IntCExpr 1}) = left
    | inc (BinIntExpr {left, bop=MinusOp, right=IntCExpr c}) =
           BinIntExpr {left=left, bop=MinusOp, right=IntCExpr (c - 1)}
    | inc (BinIntExpr {left, bop=PlusOp, right=IntCExpr c}) =
           BinIntExpr {left=left, bop=PlusOp, right=IntCExpr (c + 1)}
    | inc v = BinIntExpr {left=v, bop=PlusOp, right=IntCExpr 1}

  fun dec (IntCExpr c) = IntCExpr (c-1)
    | dec (BinIntExpr {left, bop=PlusOp, right=IntCExpr 1}) = left
    | dec (BinIntExpr {left, bop=PlusOp, right=IntCExpr c}) =
           BinIntExpr {left=left, bop=PlusOp, right=IntCExpr (c - 1)}
    | dec (BinIntExpr {left, bop=MinusOp, right=IntCExpr c}) =
           BinIntExpr {left=left, bop=MinusOp, right=IntCExpr (c - 1)}
    | dec v = BinIntExpr {left=v, bop=MinusOp, right=IntCExpr 1}

  val trueExpr  = BoolCExpr true
  val falseExpr = BoolCExpr false

  fun otherDirection Input  = Output
    | otherDirection Output = Input

  fun andexpr (BoolCExpr true,          e2)                      = e2
    | andexpr (e1 as (BoolCExpr false), e2)                      = e1
    | andexpr (e1,                      BoolCExpr true)          = e1
    | andexpr (e1,                      e2 as (BoolCExpr false)) = e2
    | andexpr (e1, e2) = BinBoolExpr {left=e1, bop=AndOp, right=e2}

  fun orexpr (BoolCExpr false,         e2)                      = e2
    | orexpr (e1 as (BoolCExpr true),  e2)                      = e1
    | orexpr (e1,                      BoolCExpr false)         = e1
    | orexpr (e1,                      e2 as (BoolCExpr true))  = e2
    | orexpr (e1, e2) = BinBoolExpr {left=e1, bop=OrOp, right=e2}


  fun checkVar (p, VarExpr (SimpleVar nm))        = p nm
    | checkVar (p, VarExpr (SubscriptVar (v, _))) = checkVar (p, VarExpr v)
    | checkVar (_, _) = false

  fun mulExpr (IntCExpr l, IntCExpr r) = IntCExpr (l * r)
    | mulExpr (IntCExpr 0, _) = IntCExpr 0
    | mulExpr (_, IntCExpr 0) = IntCExpr 0
    | mulExpr (IntCExpr 1, e) = e
    | mulExpr (e, IntCExpr 1) = e
    | mulExpr (l, r) = BinIntExpr {left=l, bop=TimesOp, right=r}

  (* Given a relation expression with a clock variable on one side, we
   * multiply the other side, as clocks can only be reset or compared in
   * limited ways (conditional expressions cannot be used, for instance).
   * Care must be taken though, to recurse through quantifiers and boolean
   * connectives. *)
  fun mulClocks (m, isClk) e = let
      fun mul (NotExpr expr) = NotExpr (mul expr)
        | mul (BinBoolExpr {left, bop, right}) =
               BinBoolExpr {left=mul left, bop=bop, right=mul right}

        | mul (RelExpr {left=l, rel=c, right=r}) =
            if checkVar (isClk, l)      then RelExpr {left=l, rel=c,
                                                      right=mulExpr (r, m)}
            else if checkVar (isClk, r) then RelExpr {right=r, rel=c,
                                                      left=mulExpr (l, m)}
            else RelExpr {left=mul l, rel=c, right=mul r}
               
        | mul (ForAllExpr {id, ty, expr}) = let
              fun isClk' s = not (s =:= id) andalso (isClk s)
            in
              ForAllExpr {id=id, ty=ty,
                          expr=mulClocks (m, isClk') expr}
            end
        | mul (ExistsExpr {id, ty, expr}) = let
              fun isClk' s = not (s =:= id) andalso (isClk s)
            in
              ExistsExpr {id=id, ty=ty,
                          expr=mulClocks (m, isClk') expr}
            end

        | mul e = e

    in mul e end

  fun conflictExists (cs1, cs2, e) = let
      infix <^>;
      fun set1 <^> set2 = not (AtomSet.isEmpty
                                  (AtomSet.intersection (set1, set2)))

      fun hideThenCheck (s, e) = conflictExists (cs1 <\ s, cs2 <\ s, e)

      fun conflict (NotExpr expr)                   = conflict expr
        | conflict (BinBoolExpr {left, right, ...}) = conflict left
                                                      orelse conflict right
        
        | conflict (ForAllExpr {id, expr, ...})     = hideThenCheck (id, expr)
        | conflict (ExistsExpr {id, expr, ...})     = hideThenCheck (id, expr)

        | conflict (IntCExpr _)                     = false
        | conflict (BoolCExpr _)                    = false
        | conflict Deadlock                         = false

        | conflict e = let val s = getFreeNames e
                       in s <^> cs1 andalso s <^> cs2 end

    in conflict e end

  fun equal (VarExpr v1, VarExpr v2)     = varequal(v1, v2)
    | equal (IntCExpr i1, IntCExpr i2)   = i1=i2                  (*{{{1*)
    | equal (BoolCExpr b1, BoolCExpr b2) = b1=b2

    | equal (CallExpr {func=f1, args=a1, ...},
             CallExpr {func=f2, args=a2, ...})
            = f1 =:= f2 andalso lequal (a1,a2)

    | equal (NegExpr e1, NegExpr e2) = equal (e1, e2)
    | equal (NotExpr e1, NotExpr e2) = equal (e1, e2)

    | equal (UnaryModExpr {uop=op1, expr=e1, ...},
             UnaryModExpr {uop=op2, expr=e2, ...})
            = op1=op2 andalso equal (e1, e2)

    | equal (BinIntExpr {left=l1, bop=op1, right=r1, ...},
             BinIntExpr {left=l2, bop=op2, right=r2, ...})
            = op1=op2 andalso equal (l1, l2) andalso equal (r1, r2)

    | equal (BinBoolExpr {left=l1, bop=op1, right=r1, ...},
             BinBoolExpr {left=l2, bop=op2, right=r2, ...})
            = op1=op2 andalso equal (l1, l2) andalso equal (r1, r2)

    | equal (RelExpr {left=l1, rel=rel1, right=r1, ...},
             RelExpr {left=l2, rel=rel2, right=r2, ...})
            = rel1=rel2 andalso equal (l1, l2) andalso equal (r1, r2)

    | equal (AssignExpr {var=v1, aop=op1, expr=e1, ...},
             AssignExpr {var=v2, aop=op2, expr=e2, ...})
            = op1=op2 andalso equal (v1, v2) andalso equal (e1, e2)

    | equal (CondExpr {test=b1, trueexpr=t1, falseexpr=f1, ...},
             CondExpr {test=b2, trueexpr=t2, falseexpr=f2, ...})
            = equal (b1, b2) andalso equal (t1, t2) andalso equal (f1, f2)

    | equal (ForAllExpr {id=id1, ty=t1, expr=e1, ...},
             ForAllExpr {id=id2, ty=t2, expr=e2, ...})
            = id1 =:= id2 andalso tyequal (t1, t2) andalso equal (e1, e2)

    | equal (ExistsExpr {id=id1, ty=t1, expr=e1, ...},
             ExistsExpr {id=id2, ty=t2, expr=e2, ...})
            = id1 =:= id2 andalso tyequal (t1, t2) andalso equal (e1, e2)

    | equal (Deadlock, Deadlock) = true
    | equal (_, _) = false

  and lequal ([], [])          = true
    | lequal (x1::xs1,x2::xs2) = equal (x1, x2) andalso lequal (xs1,xs2)
    | lequal (_, _)            = false

  and varequal (SimpleVar s1, SimpleVar s2) = s1 =:= s2

    | varequal (ReturnVar {func=f1, args=a1, ...},
                ReturnVar {func=f2, args=a2, ...})
                = f1 =:= f2 andalso lequal (a1, a2)

    | varequal (RecordVar (v1, s1), RecordVar (v2, s2))
                = s1 =:= s2 andalso varequal (v1, v2)

    | varequal (SubscriptVar (v2, e1), SubscriptVar (v1, e2))
                = varequal (v1, v2) andalso equal (e1, e2)

    | varequal (_, _) = false

  and tyequal (INT (NONE, tq1)         , INT (NONE, tq2)) = tq1=tq2
    | tyequal (INT (SOME (l1, u1), tq1),
              INT (SOME (l2, u2), tq2))
             = tq1=tq2 andalso equal (l1, l2) andalso equal (u1, u2)
    | tyequal (INT _, INT _) = false

    | tyequal (BOOL tq1, BOOL tq2) = tq1=tq2
    | tyequal (CLOCK, CLOCK)       = true
    | tyequal (CHANNEL {urgent=u1, broadcast=b1},
              CHANNEL {urgent=u2, broadcast=b2}) = u1=u2 andalso b1=b2

    | tyequal (SCALAR (e1, tq1, unique1),
              SCALAR (e2, tq2, unique2))
             = unique1=unique2 andalso tq1=tq2 andalso equal (e1, e2)

    | tyequal (RECORD (fields1, tq1, unique1),
              RECORD (fields2, tq2, unique2))
             = unique1=unique2 andalso tq1=tq2
                               andalso fequal (fields1, fields2)

    | tyequal (ARRAY (ty1, se1), ARRAY (ty2, se2))
             = tyequal (ty1, ty2)
               andalso (case (se1, se2)
                       of (Unresolved s1, Unresolved s2) =>
                                            Atom.compare (s1, s2) = EQUAL
                       |  (Type t1, Type t2) => tyequal (t1, t2)
                       |  _ => false)

    | tyequal (NAME (s1, tq1, SOME tyex1), NAME (s2, tq2, SOME tyex2))
             = tyequal (tyex1, tyex2)

    | tyequal (NAME (s1, tq1, _), NAME (s2, tq2, _))
             = tq1=tq2 andalso s1 =:= s2

    | tyequal (VOID, VOID) = true
    | tyequal (_, _) = false
  
  and fequal ([], []) = true
    | fequal ((s1, ty1)::xs1, (s2, ty2)::xs2)
         = s1 =:= s2 andalso tyequal (ty1, ty2) andalso fequal (xs1, xs2)
    | fequal (_, _)   = false
  (*}}}1*)

  local
    fun isInverseRel (LtOp, GeOp) = true | isInverseRel (LeOp, GtOp) = true
      | isInverseRel (EqOp, NeOp) = true | isInverseRel (NeOp, EqOp) = true
      | isInverseRel (GeOp, LtOp) = true | isInverseRel (GtOp, LeOp) = true
      | isInverseRel (_   ,    _) = false

    fun inverseRel LtOp = GeOp | inverseRel LeOp = GtOp
      | inverseRel EqOp = NeOp | inverseRel NeOp = EqOp
      | inverseRel GeOp = LtOp | inverseRel GtOp = LeOp
  in
  fun isNegation (e1, NotExpr e2)             = equal (e1, e2)
    | isNegation (NotExpr e1, e2)             = equal (e1, e2)
    | isNegation (BoolCExpr b1, BoolCExpr b2) = b1 <> b2
    | isNegation (RelExpr {left=l1, rel=rel1, right=r1},
                  RelExpr {left=l2, rel=rel2, right=r2})
                 = isInverseRel (rel1, rel2) 
                     andalso equal (l1, l2) andalso equal (r1, r2)
    | isNegation (_, _) = false

  fun negate (NotExpr e)           = e
    | negate (BoolCExpr b)         = BoolCExpr (not b)
    | negate (RelExpr {left, rel, right})
          = RelExpr {left=left, rel=inverseRel rel, right=right}
    | negate (BinBoolExpr {left, bop=AndOp, right})
          = BinBoolExpr {left=negate left, bop=OrOp, right=negate right}
    | negate (BinBoolExpr {left, bop=OrOp, right})
          = BinBoolExpr {left=negate left, bop=AndOp, right=negate right}
    | negate (BinBoolExpr {left, bop=ImplyOp, right})
          = BinBoolExpr {left=left, bop=AndOp, right=negate right}
    | negate (ForAllExpr {id, ty, expr})
          = ExistsExpr {id=id, ty=ty, expr=negate expr}
    | negate (ExistsExpr {id, ty, expr})
          = ForAllExpr {id=id, ty=ty, expr=negate expr}
            
    | negate e                       = NotExpr e

  end (* local *)

  local
    exception CannotBind
  in
  fun shrinkScope ((id, ty, forallbinding), p) expr = let
      fun bind e = if p e
                   then (if forallbinding
                         then ForAllExpr {id=id,ty=ty, expr=e}
                         else ExistsExpr {id=id,ty=ty, expr=e})
                   else raise CannotBind

      (* invariant: given f e, id <- (getFreeNames e) *)
      fun f (e as NotExpr _)     = bind e
            (* we could flip the quanitifier, negate the inner expression, and
             * recurse, but Expression.negate may simply introduce another
             * NotExpr wrapper thus giving non-termination. *)

        | f (e as BinBoolExpr {left=l, bop, right=r}) =
            if not (id <- getFreeNames l)
            then BinBoolExpr {left=l, bop=bop, right=f r}
            else if not (id <- getFreeNames r)
            then BinBoolExpr {left=f l, bop=bop, right=r}
            else bind e
        
        | f (e as ForAllExpr {id=bid, ty=bty, expr=be}) =
            if forallbinding
            then ForAllExpr {id=bid, ty=bty, expr=f be}
                 (* we can swap forall bindings, invariant => not (id=bid) *)
            else bind e
        
        | f (e as ExistsExpr {id=bid, ty=bty, expr=be}) =
            if forallbinding then bind e
            else ExistsExpr {id=bid, ty=bty, expr=f be}
                 (* we can swap exists bindings, invariant => not (id=bid) *)
      
        (* terms: *)
        | f (e as VarExpr _)      = bind e
        | f (e as CallExpr _)     = bind e
        | f (e as NegExpr _)      = bind e
        | f (e as UnaryModExpr _) = bind e
        | f (e as BinIntExpr _)   = bind e
        | f (e as RelExpr _)      = bind e
        | f (e as AssignExpr _)   = bind e
        | f (e as CondExpr _)     = bind e

        | f (IntCExpr _)  = raise Fail "shrinkScope: invariant false."
        | f (BoolCExpr _) = raise Fail "shrinkScope: invariant false."
        | f Deadlock      = raise Fail "shrinkScope: invariant false."
    in
      if id <- getFreeNames expr
      then SOME (f expr) handle CannotBind => NONE
      else SOME expr      (* throw the unreferenced binding away *)
    end
    end (* local *)

  fun ensureNoBindingConflict (rb, re) (b, e) = let
      val rbn = getBoundNames rb
      val bn  = getBoundNames b

      fun checkName (bid as BoundId (n, ty), (bs, e, used)) =
          if n <- rbn
          then let val n' = getNewName (n, used)
               in (BoundId (n', ty)::bs,
                   renameVar ({old=n, new=n'}, e),
                   used <+ n')
               end
          else (bid::bs, e, used)

      val (b', e', _) = foldl checkName ([], e, rbn ++ bn) (rev b)
    in (b', e') end

end

