(* $Id$
 *
 * 20070817 T. Bourke
 *   Original code. For descriptions of Pretty Printing in SML refer:
 *      * Paulson, 8.10
 *      * http://caml.inria.fr/pub/old_caml_site/FAQ/format-eng.html
 *
 *)

functor DeclarationPPFn (structure PPD : PP_DESC
                         structure EPP : EXPRESSION_PP
                         structure D   : DECLARATION
                         sharing type EPP.ty = D.ty
                         sharing type PPD.pp_desc = EPP.pp_desc
                         sharing type D.expr = EPP.expr)
  :> DECLARATION_PP where type stream      = PPD.PPS.stream
                      and type pp_desc     = PPD.pp_desc
                      and type decl        = D.decl
                      and type stmt        = D.stmt
                      and type param       = D.param
                      and type initialiser = D.initialiser
  =
struct

  type pp_desc = EPP.pp_desc
  type stream  = PPD.PPS.stream

  type symbol = Atom.atom

  type decl        = D.decl
   and stmt        = D.stmt
   and param       = D.param
   and initialiser = D.initialiser

  (* PPD.string constants (e.g. cFor and noIndent etc.) {{{1*)
  val cRef       = PPD.string "&" 
  val cEquals    = PPD.string "="
  val cOParen    = PPD.string "("
  val cCParen    = PPD.string ")"
  val cOBrace    = PPD.string "{"
  val cCBrace    = PPD.string "}"
  val cOSquare   = PPD.string "["
  val cCSquare   = PPD.string "]"
  val cColon     = PPD.string ":"
  val cSemicolon = PPD.string ";"
  val cComma     = PPD.string ","

  val cFor       = PPD.string "for"
  val cWhile     = PPD.string "while"
  val cDo        = PPD.string "do"
  val cIf        = PPD.string "if"
  val cElse      = PPD.string "else"
  val cReturn    = PPD.string "return"
  val cTypedef   = PPD.string "typedef"

  val noIndent   = PPD.PPS.Rel 0
  val normIndent = PPD.PPS.Rel 1

  val space    = PPD.space 1
  val nbspace  = PPD.nbSpace 1
  val newline  = PPD.newline
  val tabbedline = PPD.newline
  val hovBox   = PPD.hovBox
  val hvBox    = PPD.hvBox
  val hBox     = PPD.hBox
  val vBox     = PPD.vBox
  (*}}}1*)

  fun mapSep f = let
      fun addSep []      = []
        | addSep [x]     = [f x]
        | addSep (x::xs) = f x :: newline :: addSep xs
    in addSep end

  fun mapSep' f = let
      fun addSep []      = []
        | addSep [x]     = [f x, newline]
        | addSep (x::xs) = f x :: newline :: addSep xs
    in addSep end

  val descSymbol = PPD.string o Atom.toString

  fun fromDecl (D.TyDecl {id, ty, ...}) = hovBox (normIndent,       (*{{{1*)
              [cTypedef, space, EPP.fromTypeWithId (ty, id), cSemicolon])

    | fromDecl (D.VarDecl {id, ty, initial=NONE, ...}) = hovBox (normIndent,
              [EPP.fromTypeWithId (ty, id), cSemicolon])
    | fromDecl (D.VarDecl {id, ty, initial=SOME v, ...}) = hovBox (normIndent,
              [EPP.fromTypeWithId (ty, id), space, cEquals,
               space, fromInitialiser v, cSemicolon])

    | fromDecl (D.FunDecl {id, rty, params, body, ...}) =
          vBox (noIndent, [EPP.fromType rty, space, descSymbol id, cOParen,
                           fromParams params, cCParen, space, fromStmt body])

      (* D.ChanPriDecl ((chanexpr list) list) * pos) *)
    | fromDecl (D.ChanPriDecl (ces, _)) = hovBox (noIndent,
              [PPD.string "//ChanPriDecl unimplemented", newline])
      (*}}}1*)

  and fromParam (D.ValParam {id, ty}) = EPP.fromTypeWithId (ty, id) (*{{{1*)
    | fromParam (D.RefParam {id, ty}) = EPP.fromRefTypeWithId (ty, id)(*}}}1*)

  and fromParams params = let                                       (*{{{1*)
      fun doParams []      = []
        | doParams [p]     = [fromParam p]
        | doParams (p::ps) = hovBox (noIndent, [fromParam p, cComma])
                               :: space :: doParams ps
    in hovBox (noIndent, doParams params) end (*}}}1*)

  and fromStmt (D.BlockStmt {decls, body}) = hBox [cOBrace,         (*{{{1*)
              newline, PPD.nbSpace 4,
              vBox (noIndent, mapSep' fromDecl decls @ mapSep  fromStmt body),
              newline, cCBrace]

    | fromStmt (D.ExprStmt (e, _)) = hovBox (normIndent,
                                             [EPP.fromExpr e, cSemicolon])

    | fromStmt (D.ForStmt {init, cond, step, body, ...}) = let
          val args = hvBox (noIndent, [EPP.fromExprList init, cSemicolon,space,
                                       EPP.fromExprList cond, cSemicolon,space,
                                       EPP.fromExprList step])
        in vBox (noIndent, [cFor, nbspace, cOParen, args, cCParen,
                            nbspace, fromStmt body])
        end

    | fromStmt (D.IterateStmt {id, ty, body, ...}) = vBox (noIndent,
                  [cFor, nbspace, cOParen, descSymbol id, nbspace, cColon,
                   nbspace, EPP.fromType ty, cCParen, nbspace, fromStmt body])

    | fromStmt (D.WhileStmt {cond, body, ...}) = vBox (noIndent,
            [cWhile, nbspace, cOParen, EPP.fromExprList cond, cCParen,
             nbspace, fromStmt body])

    | fromStmt (D.DoWhileStmt {cond, body, ...}) = vBox (noIndent,
            [cDo, nbspace, fromStmt body, nbspace, cWhile, cOParen,
             EPP.fromExprList cond, cCParen, cSemicolon])

    | fromStmt (D.IfStmt {cond, thenb, elseb, ...}) = let
          val firstline = hBox [cIf, nbspace, cOParen, EPP.fromExprList cond,
                                cCParen, nbspace]
          val elselist = case elseb
                         of D.NothingStmt => []
                          | _ => [nbspace, cElse, nbspace,
                                  fromStmt elseb, newline]
        in vBox (noIndent, firstline :: fromStmt thenb :: elselist) end

    | fromStmt (D.Return (SOME e, _)) = hovBox (normIndent,
              [cReturn, nbspace, cOParen, EPP.fromExpr e, cCParen, cSemicolon])

    | fromStmt (D.Return (NONE, _)) = hovBox (normIndent, [cReturn,cSemicolon])

    | fromStmt (D.NothingStmt) = nbspace
      (*}}}1*)

  and fromInitialiser (D.SimpleInit e)    = EPP.fromExpr e          (*{{{1*)
    | fromInitialiser (D.ArrayInit inits) = let
          fun fromInitList []      = [cCBrace]
            | fromInitList [x]     = [fromInitialiser x, cCBrace]
            | fromInitList (x::xs) = fromInitialiser x :: cComma
                                       :: space :: fromInitList xs
        in hovBox (normIndent, cOBrace :: fromInitList inits) end
    (*}}}1*)

  fun print strm desc = PPD.description (strm, desc)

end

