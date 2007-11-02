(* $Id$
 *
 * 20070818 T. Bourke
 *   Original code. For descriptions of Pretty Printing in SML refer:
 *      * Paulson, 8.10
 *      * http://caml.inria.fr/pub/old_caml_site/FAQ/format-eng.html
 *
 *)

functor EnvironmentPPFn (
  structure PPD : PP_DESC
  structure Env : ENVIRONMENT
  structure EPP : EXPRESSION_PP
  structure DPP : DECLARATION_PP
  sharing type EPP.ty = Env.Expression.ty
  sharing type PPD.pp_desc = EPP.pp_desc
  sharing type PPD.pp_desc = DPP.pp_desc
  sharing type Env.Declaration.initialiser = DPP.initialiser
  sharing type Env.Declaration.stmt = DPP.stmt

) :> ENVIRONMENT_PP where type stream    = PPD.PPS.stream
                      and type pp_desc   = PPD.pp_desc
                      and type env       = Env.env
                      and type scopetag  = Env.scopetag
  =
struct

  type pp_desc = PPD.pp_desc
  type stream  = PPD.PPS.stream

  type symbol = Atom.atom

  type env      = Env.env
   and scopetag = Env.scopetag

  (* PPD.string constants (e.g. cTrue and noIndent etc.) {{{1*)
  val cTypedef   = PPD.string "typedef"
  val cEquals    = PPD.string "="
  val cSemicolon = PPD.string ";"
  val cComma     = PPD.string ","
  val cOParen    = PPD.string "("
  val cCParen    = PPD.string ")"

  val noIndent   = PPD.PPS.Rel 0
  val normIndent = PPD.PPS.Rel 1

  val space    = PPD.space 1
  val nbspace  = PPD.nbSpace 1
  val hovBox   = PPD.hovBox
  (*}}}1*)

  val descSymbol = PPD.string o Atom.toString

  fun fromType (id, ty) = hovBox (normIndent,
        [cTypedef, space, EPP.fromTypeWithId (ty, id), cSemicolon])

  fun fromValue (id, Env.VarEntry {ty, init, ref=r, ...}) = hovBox (normIndent,
        (if r
         then EPP.fromRefTypeWithId (ty, id)
         else EPP.fromTypeWithId    (ty, id))
        ::
        (case init
         of NONE   => [cSemicolon]
          | SOME i => [space, cEquals, space,
                       DPP.fromInitialiser i, cSemicolon]))

    | fromValue (id, Env.FunEntry {formals, result, body, ...}) = let
          fun formal {ty, id, ref=true}  = EPP.fromRefTypeWithId (ty, id)
            | formal {ty, id, ref=false} = EPP.fromTypeWithId (ty, id)

          fun doFormals []      = []
            | doFormals [f]     = [formal f]
            | doFormals (f::fs) = formal f :: cComma :: space :: doFormals fs
        in
          PPD.hBox [EPP.fromType result, space, descSymbol id, space,
                    cOParen, hovBox (noIndent, doFormals formals),
                    cCParen, PPD.newline, DPP.fromStmt body]
        end

  val addSeps = foldr (fn(i, l)=> i :: PPD.cut :: l) []

  fun fromEnv env = let
      fun fType (id, s, ty) = SOME (fromType (id, ty))
      val types = PPD.vBox (noIndent, addSeps (Env.mapTypes fType env))
      val values = PPD.vBox (noIndent, addSeps
                                  (Env.mapValues (SOME o fromValue) env))
    in PPD.vBox (noIndent, [types, PPD.cut, PPD.cut, values]) end

  fun fromEnv' (env, scope) = let
      fun fType (id, s, ty) = if s = scope
                              then SOME (fromType (id, ty)) else NONE

      fun fValue (id, v as Env.VarEntry {scope=s, ...}) =
              if s = scope then SOME (fromValue (id, v)) else NONE
        | fValue (id, v as Env.FunEntry {scope=s, ...}) =
              if s = scope then SOME (fromValue (id, v)) else NONE

      val types = PPD.vBox (noIndent, addSeps (Env.mapTypes fType env))
      val values = PPD.vBox (noIndent, addSeps (Env.mapValues fValue env))
    in PPD.vBox (noIndent, [types, PPD.cut, PPD.cut, values]) end

  fun print strm desc = PPD.description (strm, desc)

end

