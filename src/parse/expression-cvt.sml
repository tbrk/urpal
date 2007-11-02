(* $Id$
 *
 * 20070808 T. Bourke
 *  Original code.
 *
 *)

(* TODO:
    * rename? now that it also includes some conversion functions for
      declarations and environments.
 *)

structure ExpressionCvt :> EXPRESSION_CVT
=
struct
  structure E = Expression
  structure D = Declaration

  structure StringPPS = PPStreamFn (structure Token = StringToken
                                          and Device = PPDevString)
  structure StringPPD = PPDescFn (StringPPS)
  structure StringEPP = ExpressionPPFn (structure PPD=StringPPD and E=E)

  structure OutPPS = PPStreamFn (structure Token  = StringToken
                                       and Device = SimpleTextIODev)
  structure OutPPD = PPDescFn (OutPPS)
  structure OutEPP = ExpressionPPFn (structure PPD=OutPPD and E=E)
  structure OutDPP = DeclarationPPFn (structure PPD=OutPPD
                                      and D=D and EPP=OutEPP)

  type expr = E.expr
   and ty   = E.ty
   and var  = E.var
   and decl = D.decl

  type outstream = TextIO.StreamIO.outstream

  val width = ref 76

  fun ppdToString pp_desc = let
      val dev = PPDevString.openDev {dst="", wid=(!width)}
      val strm = StringPPD.PPS.openStream dev
    in
      StringEPP.print strm pp_desc;
      StringPPD.PPS.closeStream strm;
      PPDevString.toString dev
    end

  fun ppdToOutput (os, pp_desc) = let
      val dev = SimpleTextIODev.openDev {dst=TextIO.mkOutstream os,
                                         wid=Settings.maxLabelWidth()}
      val strm = OutPPD.PPS.openStream dev
    in
      OutEPP.print strm pp_desc;
      OutPPD.PPS.closeStream strm
    end

  structure Expr = struct
    fun toString e = ppdToString (StringEPP.fromExpr e)
    (*val fromString = UppaalParse.parseExpression "ExpressionCvt"*)
    fun toStream (os, e) = ppdToOutput (os, OutEPP.fromExpr e) 
  end

  structure Ty = struct
    fun toString t = ppdToString (StringEPP.fromType t)
    fun toStream (os, t) = ppdToOutput (os, OutEPP.fromType t) 
  end

  structure Var = struct
    fun toString v = ppdToString (StringEPP.fromVar v)
    fun toStream (os, v) = ppdToOutput (os, OutEPP.fromVar v) 
  end

  structure Decl = struct
    fun toStream (os, d) = ppdToOutput (os, OutDPP.fromDecl d)
  end

  fun selectToStream (os, sels) = ppdToOutput (os, OutEPP.fromSelects sels)
  fun selectToString sels = ppdToString (StringEPP.fromSelects sels)
  fun syncToStream (os, sync) = ppdToOutput (os, OutEPP.fromSync sync)
  fun syncToString sync = ppdToString (StringEPP.fromSync sync)
  fun exprlistToStream (os, exprs) = ppdToOutput (os,OutEPP.fromExprList exprs)
  fun exprlistToString exprs = ppdToString (StringEPP.fromExprList exprs)
  fun paramlistToStream (os, params) = ppdToOutput(os,OutDPP.fromParams params)

end

