(* $Id$
 *
 * 20070731 T. Bourke
 *   Original code. For descriptions of Pretty Printing in SML refer:
 *      * Paulson, 8.10
 *      * http://caml.inria.fr/pub/old_caml_site/FAQ/format-eng.html
 *
 *)

functor ExpressionPPFn (structure PPD : PP_DESC
                      structure E : EXPRESSION)
  :> EXPRESSION_PP where type stream    = PPD.PPS.stream
                     and type pp_desc   = PPD.pp_desc
                     and type expr      = E.expr
                     and type ty        = E.ty
                     and type var       = E.var
                     and type boundid   = E.boundid
                     and type direction = E.direction
  =
struct (*{{{1*)

  type pp_desc = PPD.pp_desc
  type stream  = PPD.PPS.stream

  type symbol = Atom.atom

  type expr      = E.expr
   and ty        = E.ty
   and var       = E.var
   and boundid   = E.boundid
   and direction = E.direction

  (* precedence details (e.g. precExpr, LeftAssoc, etc.) {{{1*)

  datatype associativity = NoAssoc | LeftAssoc | RightAssoc

  val precHighest = 20
  val precLowest  = ~1
  val precBinding = 1

  fun precIntOp E.TimesOp = 17 | precIntOp E.DivideOp = 17
    | precIntOp E.ModOp   = 17
    | precIntOp E.PlusOp  = 16 | precIntOp E.MinusOp  = 16
    | precIntOp E.ShlOp   = 15 | precIntOp E.ShrOp    = 15
    | precIntOp E.MinOp   = 14 | precIntOp E.MaxOp    = 14
    | precIntOp E.BAndOp  = 11
    | precIntOp E.BXorOp  = 10
    | precIntOp E.BOrOp   = 9

  fun precRel E.EqOp = 12 | precRel E.NeOp = 12
    | precRel _      = 13

  fun precBoolOp E.AndOp   = 8
    | precBoolOp E.OrOp    = 7
    | precBoolOp E.ImplyOp = 2

  fun precExpr (E.VarExpr _)               = (NoAssoc, precHighest)
    | precExpr (E.IntCExpr _)              = (NoAssoc, precHighest)
    | precExpr (E.BoolCExpr _)             = (NoAssoc, precHighest)
    | precExpr (E.CallExpr _)              = (NoAssoc, precHighest)
    | precExpr (E.Deadlock _)              = (NoAssoc, precHighest)
    | precExpr (E.NegExpr _)               = (NoAssoc, 18)
    | precExpr (E.NotExpr _)               = (NoAssoc, 18)
    | precExpr (E.UnaryModExpr {uop, ...}) = (RightAssoc, 18)
    | precExpr (E.BinIntExpr {bop, ...})   = (LeftAssoc, precIntOp bop)
    | precExpr (E.BinBoolExpr {bop, ...})  = (LeftAssoc, precBoolOp bop)
    | precExpr (E.RelExpr {rel, ...})      = (LeftAssoc, precRel rel)
    | precExpr (E.AssignExpr {aop, ...})   = (RightAssoc, 5)
    | precExpr (E.CondExpr _)              = (RightAssoc, 6)
    | precExpr (E.ForAllExpr _)            = (RightAssoc, precBinding)
    | precExpr (E.ExistsExpr _)            = (RightAssoc, precBinding)
  (*}}}1*)

  (* PPD.string constants (e.g. cTrue and noIndent etc.) {{{1*)
  val cTrue      = PPD.string "true"
  val cFalse     = PPD.string "false"
  val cDeadlock  = PPD.string "deadlock"
  val cForAll    = PPD.string "forall"
  val cExists    = PPD.string "exists"
  val cMeta      = PPD.string "meta"
  val cConst     = PPD.string "const"
  val cOParen    = PPD.string "("
  val cCParen    = PPD.string ")"
  val cOSquare   = PPD.string "["
  val cCSquare   = PPD.string "]"
  val cOBrace    = PPD.string "{"
  val cCBrace    = PPD.string "}"
  val cQuestion  = PPD.string "?"
  val cExclam    = PPD.string "!"
  val cColon     = PPD.string ":"
  val cSemicolon = PPD.string ";"
  val cDot       = PPD.string "."
  val cComma     = PPD.string ","
  val cAmpersand = PPD.string "&"

  val cInt       = PPD.string "int"
  val cBool      = PPD.string "bool"
  val cChan      = PPD.string "chan"
  val cScalar    = PPD.string "scalar"
  val cClock     = PPD.string "clock"
  val cVoid      = PPD.string "void"
  val cStruct    = PPD.string "struct"

  val cUrgent    = PPD.string "urgent"
  val cBroadcast = PPD.string "broadcast"

  val opInc      = PPD.string "++"
  val opDec      = PPD.string "--"

  val opPlus     = PPD.string "+"
  val opMinus    = PPD.string "-"
  val opTimes    = PPD.string "*"
  val opDivide   = PPD.string "/"
  val opMod      = PPD.string "%"
  val opBAnd     = PPD.string "&"
  val opBOr      = PPD.string "|"
  val opBXor     = PPD.string "^"
  val opShl      = PPD.string "<<"
  val opShr      = PPD.string ">>"
  val opMin      = PPD.string "<?"
  val opMax      = PPD.string ">?"

  val relLt      = PPD.string "<"
  val relLe      = PPD.string "<="
  val relEq      = PPD.string "=="
  val relNe      = PPD.string "!="
  val relGe      = PPD.string ">="
  val relGt      = PPD.string ">"

  val opAnd      = PPD.string "&&"
  val opOr       = PPD.string "||"
  val opImply    = PPD.string "imply"

  val opAssign   = PPD.string "="
  val opPlusEq   = PPD.string "+="
  val opMinusEq  = PPD.string "-="
  val opTimesEq  = PPD.string "*="
  val opDivideEq = PPD.string "/="
  val opModEq    = PPD.string "%="
  val opBOrEq    = PPD.string "|="
  val opBAndEq   = PPD.string "&="
  val opBXorEq   = PPD.string "^="
  val opShlEq    = PPD.string "<<="
  val opShrEq    = PPD.string ">>="

  val noIndent = PPD.PPS.Rel 0
  val normIndent = PPD.PPS.Rel 1
  val medIndent = PPD.PPS.Rel 4

  val space   = PPD.space 1
  val nbspace = PPD.nbSpace 1
  val hovBox  = PPD.hovBox
  (*}}}1*)

 (* desc functions (e.g. descUnaryOp, etc.) {{{1*)
  fun descUnaryOp (E.PreIncOp, fe)  = [opInc, fe]
    | descUnaryOp (E.PostIncOp, fe) = [fe, opInc]
    | descUnaryOp (E.PreDecOp, fe)  = [opDec, fe]
    | descUnaryOp (E.PostDecOp, fe) = [fe, opDec]

  fun descIntOp E.PlusOp   = opPlus  | descIntOp E.MinusOp  = opMinus
    | descIntOp E.TimesOp  = opTimes | descIntOp E.DivideOp = opDivide
    | descIntOp E.ModOp    = opMod   | descIntOp E.BAndOp   = opBAnd
    | descIntOp E.BOrOp    = opBOr   | descIntOp E.BXorOp   = opBXor
    | descIntOp E.ShlOp    = opShl   | descIntOp E.ShrOp    = opShr
    | descIntOp E.MinOp    = opMin   | descIntOp E.MaxOp    = opMax

  fun descRel E.LtOp = relLt | descRel E.LeOp = relLe
    | descRel E.EqOp = relEq | descRel E.NeOp = relNe
    | descRel E.GeOp = relGe | descRel E.GtOp = relGt

  fun descBoolOp E.AndOp   = opAnd   | descBoolOp E.OrOp    = opOr
    | descBoolOp E.ImplyOp = opImply

  fun descAssign E.AssignOp   = opAssign   | descAssign E.PlusEqOp  = opPlusEq
    | descAssign E.MinusEqOp  = opMinusEq  | descAssign E.TimesEqOp = opTimesEq
    | descAssign E.DivideEqOp = opDivideEq | descAssign E.ModEqOp   = opModEq
    | descAssign E.BOrEqOp    = opBOrEq    | descAssign E.BAndEqOp  = opBAndEq
    | descAssign E.BXorEqOp   = opBXorEq   | descAssign E.ShlEqOp   = opShlEq
    | descAssign E.ShrEqOp    = opShrEq

  fun descTyQual (E.NoQual, fty) = hovBox (noIndent, [fty])
    | descTyQual (E.Meta,   fty) = hovBox (normIndent, [cMeta, space, fty])
    | descTyQual (E.Const,  fty) = hovBox (normIndent, [cConst, space, fty])

  val descSymbol = PPD.string o Atom.toString
  (*}}}1*)

  fun fromExpr e                = bracket (false, precLowest, e)
  and fromExpr' (assoc, pri, e) = let in case e                 (*{{{1*)
    of (E.VarExpr v)       => fromVar v
     | (E.IntCExpr i)      => PPD.string (Int.toString i)
     | (E.BoolCExpr true)  => cTrue
     | (E.BoolCExpr false) => cFalse
     | (E.Deadlock _)      => cDeadlock

     | (E.NegExpr {expr, ...}) => hovBox (normIndent, [PPD.string "-",
                                            bracket (false, pri, expr)])

     | (E.NotExpr {expr, ...}) => hovBox (normIndent, [PPD.string "!",
                                            bracket (false, pri, expr)])

     | (E.UnaryModExpr {uop, expr, ...})       => hovBox (normIndent,
                              descUnaryOp (uop, bracket (false, pri, expr)))
                              
     | (E.BinIntExpr {left, bop, right, ...})  => hovBox (noIndent, [
                                                   bracket (false, pri, left),
                                                   nbspace, descIntOp bop,
                                                   space,
                                                   bracket (true, pri, right)])

     | (E.BinBoolExpr {left, bop, right, ...}) => hovBox (noIndent, [
                                                   bracket (false, pri, left),
                                                   nbspace, descBoolOp bop,
                                                   space,
                                                   bracket (true, pri, right)])

     | (E.RelExpr {left, rel, right, ...})     => hovBox (normIndent, [
                                                   bracket (false, pri, left),
                                                   descRel rel,
                                                   bracket (true, pri, right)])

     | (E.AssignExpr {var, aop, expr, ...})    => hovBox (normIndent, [
                                                   bracket (false, pri, var),
                                                   space, descAssign aop,
                                                   space,
                                                   bracket (true, pri, expr)])

     | (E.ForAllExpr {id, ty, expr, ...}) => makeBound (cForAll, id, ty, expr)
     | (E.ExistsExpr {id, ty, expr, ...}) => makeBound (cExists, id, ty, expr)
     
     | (E.CallExpr {func, args, ...})     => makeFuncCall (func, args)

     | (E.CondExpr {test, trueexpr, falseexpr, ...}) => let
              val brs = PPD.hvBox (noIndent, [cQuestion, nbspace,
                                              bracket (false, pri, trueexpr),
                                              space, cColon, nbspace,
                                              bracket (false, pri, falseexpr)])
            in
              PPD.hvBox (noIndent, [bracket (true, pri, test), nbspace, brs])
            end
    end (*}}}1 fromExpr' *)

  and fromType (E.INT (NONE, tyq))     = descTyQual (tyq, cInt) (*{{{1*)
    | fromType (E.INT (SOME (l, u), tyq)) = let
          val t = hovBox (noIndent, [cInt, cOSquare, fromExpr l, cComma,
                                         PPD.cut, fromExpr u,
                                         PPD.cut, cCSquare])
        in descTyQual (tyq, t) end

    | fromType (E.BOOL tyq)            = descTyQual (tyq, cBool)

    | fromType (E.CHANNEL {urgent, broadcast}) = hovBox (noIndent,
          List.concat [if urgent    then [cUrgent,    nbspace] else [],
                       if broadcast then [cBroadcast, nbspace] else [],
                       [cChan] ])

    | fromType (E.SCALAR (e, tyq, _))  = let
            val t = hovBox (noIndent, [cScalar, cOSquare, fromExpr e,
                                           PPD.cut, cCSquare])
          in descTyQual (tyq, t) end

    | fromType (E.RECORD (fs, tyq, _)) = let
            val t = PPD.hvBox (noIndent, cStruct::nbspace::cOBrace
                                           ::PPD.break {nsp=1, offset=2}
                                           ::makeFieldList fs @ [cCBrace])
          in descTyQual (tyq, t) end

    | fromType (E.ARRAY (ty, bound))   = let
            val t = fromType ty
            val b = case bound of E.Type b       => fromType b
                                | E.Unresolved s => descSymbol s
          in
            hovBox (noIndent, [t, cOSquare, b, PPD.cut, cCSquare])
          end

    | fromType (E.NAME (s, tyq, _))    = descTyQual (tyq, descSymbol s)
    | fromType E.CLOCK                 = cClock
    | fromType E.VOID                  = cVoid
    (*}}}1*)

  and fromArraySubType (E.INT (SOME (E.IntCExpr 0, u), E.NoQual)) =
                                      hovBox (noIndent, [fromExpr (E.inc u)])
    | fromArraySubType arg = fromType arg

  and fromTypeWithId' (E.ARRAY (ty, bound), id, isref) = let    (*{{{1*)
      val b = case bound of E.Type b       => fromArraySubType b
                          | E.Unresolved s => descSymbol s

    in hovBox (noIndent, [fromTypeWithId' (ty, id, isref),
                          cOSquare, b, PPD.cut, cCSquare]) end

    | fromTypeWithId' (ty, s, false) =
        hovBox (noIndent, [fromType ty, nbspace, descSymbol s])
    | fromTypeWithId' (ty, s, true) =
        hovBox (noIndent, [fromType ty, cAmpersand, nbspace, descSymbol s])
    (*}}}1*)

  and bracket (must, contextpri, e) = let                       (*{{{1*)
      val (assoc, localpri) = precExpr e
      val relpri = Int.compare (localpri, contextpri)
    in
      if relpri = LESS orelse (relpri = EQUAL andalso must)
      then hovBox (normIndent, [cOParen,
                                    fromExpr' (assoc, localpri, e),
                                    cCParen])
      else fromExpr' (assoc, localpri, e)
    end (*}}}1*)

  and makeFieldList fields = let                                (*{{{1*)
      fun makeField (s, ty) = PPD.hBox [fromTypeWithId' (ty, s, false),
                                        cSemicolon]
      fun doit []      = []
        | doit [f]     = [makeField f, PPD.break {nsp=1, offset=0}]
        | doit (f::fs) = makeField f :: PPD.break {nsp=1, offset=2} :: doit fs
    in
      doit fields
    end (*}}}1*)

  and makeBound (tag, id, ty, expr) = hovBox (normIndent, [ (*{{{1*)
                        tag, PPD.break {nsp=1, offset=4},
                        cOParen, descSymbol id, space,
                                 cColon, space, fromType ty,
                        cCParen, PPD.break {nsp=1, offset=4},
                        bracket (false, precBinding, expr)])
       (*}}}1*)

  and makeFuncCall (name, args) = let                           (*{{{1*)
      fun doArgs []      = []
        | doArgs [e]     = [fromExpr e]
        | doArgs (e::es) = hovBox (noIndent, [fromExpr e, cComma])
                             :: space :: doArgs es
      val inner = hovBox (noIndent, (cOParen::doArgs args)
                                    @ [PPD.cut, cCParen])
    in
      hovBox (normIndent, [descSymbol name, inner])
    end (*}}}1*)

  and fromVar v = let                                           (*{{{1*)
      fun fvar (E.SimpleVar (s, _))            = [descSymbol s]
        | fvar (E.ReturnVar {func, args, ...}) = [makeFuncCall (func, args)]
        | fvar (E.RecordVar (v, s, _))         = descSymbol s :: cDot
                                                  :: fvar v
        | fvar (E.SubscriptVar (var, expr, _)) = let
              val t = hovBox (noIndent, [cOSquare, fromExpr expr,
                                               PPD.cut, cCSquare])
            in t :: fvar var end
    in
      hovBox (noIndent, rev (fvar v))
    end (*}}}1*)

  fun fromSelects syncs = let (*{{{1*)
      fun makeBound (E.BoundId (id, ty, _)) = hovBox (normIndent,
                [descSymbol id, space, cColon, space, fromType ty])

      fun doList []      = []
        | doList [s]     = [makeBound s]
        | doList (s::ss) = hovBox (noIndent, [makeBound s, cComma])
                            :: space :: doList ss
    in hovBox (noIndent, doList syncs) end (*}}}1*)

  fun fromSync (id, dir, es) = let (*{{{1*)
      fun makeSubs []      = (case dir of E.Input  => [cQuestion]
                                        | E.Output => [cExclam])
        | makeSubs (e::es) = hovBox (noIndent, [cOSquare, fromExpr e,
                                                PPD.cut, cCSquare])
                             :: makeSubs es
    in hovBox (normIndent, descSymbol id ::makeSubs es) end (*}}}1*)

  fun fromExprList es = let (*{{{1*)
      fun makeList []      = []
        | makeList [e]     = [fromExpr e]
        | makeList (e::es) = fromExpr e :: cComma :: space :: makeList es
    in hovBox (normIndent, makeList es) end (*}}}1*)

  fun fromTypeWithId (ty, id)    = fromTypeWithId' (ty, id, false)
  fun fromRefTypeWithId (ty, id) = fromTypeWithId' (ty, id, true)

  fun print strm desc = PPD.description (strm, desc)

end (*}}}1*)

(*
local
  structure STREAM = PPStreamFn (structure Token = StringToken
                                 structure Device = SimpleTextIODev)
  structure TextPPD = PPDescFn (STREAM)
in
structure ExpressionPP = ExpressionPP (structure PPD = TextPPD
                                       structure E = Expression)
end
*)

