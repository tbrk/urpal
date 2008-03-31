(* $Id$ *)

(* TODO:
 *   -Many of these functions should be shifted into ParsedNta.
 *)

structure UppaalParse : UPPAAL_PARSE
  =
struct

  structure Environment = Environment
  structure Expression  = Expression
  structure Declaration = Declaration
  structure ExpressionCvt = ExpressionCvt

  structure T = TextNta and P = ParsedNta
  structure Env = Environment
        and O = Option

  type symbol = Atom.atom

  structure LrVals = UppaalLrFun(structure Token=LrParser.Token
                                 structure FilePos=FilePos
                                 structure Expression=Expression
                                 structure Declaration=Declaration
                                 structure Result=Result)

  structure Lex = UppaalLexFun(structure Tokens=LrVals.Tokens
                               structure FilePos=FilePos)

  structure Parser = JoinWithArg (structure Lex=Lex
                                  structure ParserData=LrVals.ParserData
                                  structure LrParser=LrParser)

  val errorLookahead = 15

  fun parseString (token, name, s) =
    let
      local val done = ref false
      in fun read _ = if !done then "" else s before (done := true) end

      val prError = FilePos.error (Settings.progName ^ ":" ^ name)

      val posState = FilePos.newstate ()
      val stream = Parser.Stream.cons (token (FilePos.zero, FilePos.zero),
                                       Parser.makeLexer read posState)
      val (res, stream') = Parser.parse (errorLookahead, stream, prError, ())

    in res end

  fun parseExpression name s =
    (case parseString (LrVals.Tokens.PARSEEXPR, name, s)
     of Result.Expr e => SOME e
      | _             => NONE)
    handle Parser.ParseError => NONE

  fun parseExpressionList name s =
    (case parseString (LrVals.Tokens.PARSEEXPRLIST, name, s)
     of Result.ExprList es => SOME es
      | _                  => NONE)
    handle Parser.ParseError => NONE

  fun parseDeclarations name s = let
      val _ = Util.debugIndent (Settings.Detailed, fn ()=>["=declarations="])
      val result = (case parseString (LrVals.Tokens.PARSEDECL, name, s)
                    of Result.Decls dl => SOME dl
                     | _               => NONE)
                   handle Parser.ParseError => NONE
    in
      Util.debugVeryDetailed (fn ()=>
        [Option.getOpt (Option.map (fn ds=>
                                    Int.toString (length ds)^" from:\n") result,
                        "!!!NONE!!!"), s]);
      Util.debugOutdent (Settings.Detailed, fn ()=>["=declarations-end="]);
      result
    end

  fun parseParameters name s =
    (case parseString (LrVals.Tokens.PARSEPARAMS, name, s)
     of Result.Params ps => SOME ps
      | _                => NONE)
    handle Parser.ParseError => NONE
  
  fun parseSelect name s =
    (case parseString (LrVals.Tokens.PARSESELECT, name, s)
     of Result.Select dl => SOME dl
      | _                => NONE)
    handle Parser.ParseError => NONE

  fun parseSync name s =
    (case parseString (LrVals.Tokens.PARSESYNC, name, s)
     of Result.Sync dl => SOME dl
      | _              => NONE)
    handle Parser.ParseError => NONE

  fun parseLocId (T.LocId i) = P.LocId i
  fun parseTransId (T.TransId i) = P.TransId i

  fun defaultTrue (_, NONE)   = Expression.BoolCExpr true
    | defaultTrue (f, SOME s) = f s

  fun defaultEmpty (_, NONE)   = []
    | defaultEmpty (f, SOME s) = f s

  fun locName (NONE, T.LocId i) = "id" ^ Int.toString i
    | locName (SOME n, _)       = n

  fun locName' (NONE, T.LocId i) = "id" ^ Int.toString i
    | locName' (SOME n, T.LocId i) = n ^ " (id" ^ Int.toString i ^ ")"

  fun parseLocation (env, templatename) (T.Location {id, position,
                     color, name=tname as (n, _), invariant=(invo, invPos),
                     comments, urgent, committed}) =
    let
      val _ = Util.debugIndent (Settings.Detailed, fn ()=>["=location ",
                                                           locName' (n,id),"="])
      val name = String.concat [templatename, "-invariant(",
                                locName (n, id), ")"]
      val inv = defaultTrue (valOf o (parseExpression name), invo)
    in
      P.Location {id=parseLocId id,
                  position=position, color=color, name=tname,
                  invariant=(inv, invPos),
                  comments=comments, urgent=urgent, committed=committed}
      before (Util.debugOutdent (Settings.Detailed, fn ()=>["=location-end="]))
    end

  fun parseTransition (env, templatename, getLocName)
        (T.Transition {id, source, target,
                       select=(selo, selPos), guard=(guardo, guardPos),
                       sync=(synco, syncPos), update=(updateo, updatePos),
                       comments, position, color, nails}) =
    let
      val _ = Util.debugIndent (Settings.Detailed, fn ()=>["=transition ",
                         getLocName source, "->", getLocName target, " ="])
      val name = String.concat [templatename, ":", getLocName source,
                                "->", getLocName target, " "]

      (* TODO: need the types in selo' be expanded? *)
      val sel    = defaultEmpty (valOf o (parseSelect (name ^ "select")), selo)
      val guard  = defaultTrue (valOf o (parseExpression (name ^ "guard")),
                                guardo)

      val synco' = O.mapPartial (parseSync (name ^ "sync")) synco

      val update = defaultEmpty (valOf o (parseExpressionList
                                            (name ^ "update")), updateo)
    in
      P.Transition {id=O.map parseTransId id,
                    source=parseLocId source,
                    target=parseLocId target,
                    select=(sel,    selPos),
                    guard= (guard,  guardPos),
                    sync=  (synco', syncPos),
                    update=(update, updatePos),
                    comments=comments, position=position, color=color,
                    nails=nails}
      before (Util.debugOutdent (Settings.Detailed,fn()=>["=transition-end="]))
    end

  fun addDecls (env, name, _, NONE) = env
    | addDecls (env, name, scope, SOME d) =
          Env.addDeclarations (env, scope, valOf (parseDeclarations name d))

  fun parseTemplate (env, name) (T.Template {name=tname as (n, _),
                                             parameter=(paramo, paramPos),
                                             declaration, initial, locations,
                                             transitions, ...}) =
    let
      val _ = Util.debugIndent (Settings.Detailed, fn ()=>["=template ",n,"="])
      val name = String.concat [name, ":", n]
      val params = case paramo
               of NONE    => []
                | SOME ps => valOf (parseParameters (name ^ ":parameters") ps)
      val paramenv = Env.addParameters (env, Env.ParameterScope, params)
      val locals = addDecls (paramenv, name ^ ":declarations",
                             Env.TemplateScope, declaration)

      val locs = map (parseLocation (locals, name)) locations

      fun getLocName (id as T.LocId i) = let
          fun matchingLoc (P.Location {id=P.LocId j, ...}) = (i = j)
          fun getLocName (P.Location {name=(n, _), ...}) = n
        in
          locName (Option.composePartial
                      (getLocName, List.find matchingLoc) locs, id)
        end
    in
      P.Template {name=tname,
                  parameter=(params, paramPos),
                  declaration=locals,
                  initial=O.map parseLocId initial,
                  locations=locs,
                  transitions=map (parseTransition (locals, name, getLocName))
                                  transitions}
      before (Util.debugOutdent (Settings.Detailed, fn ()=>["=template-end="]))
    end 

  fun parse (T.Nta {imports, declaration, templates, instantiation, system},
             filename) =
    let
      val _ = Util.debugIndent (Settings.Detailed, fn ()=>["=parsing ",
                                                           filename,"="])
      val globals = addDecls (Env.base_env, filename ^ ":project declarations",
                              Env.GlobalScope, declaration)
    in
      SOME (P.Nta {imports=imports,
                   declaration=globals,
                   templates=map (parseTemplate (globals, filename)) templates,
                   instantiation=instantiation,
                   system=system})
      before
      (Util.debugOutdent (Settings.Detailed, fn ()=>["=parsing-end="]))
    end
    handle Option => NONE before
                     Util.debugOutdent (Settings.Detailed,
                                        fn()=>["=parsing-end-withfailure="])

    fun removeUnusedSelectIds (P.Transition {id, source, target,
             select=(sel,selPos), guard=(g,gP), sync=(syn,synP),
             update=(upd,updP), comments, position, color, nails}) =
      let
        fun listFree (e, s) = AtomSet.union (Expression.getFreeNames e, s)
        fun isUsed s (Expression.BoundId (nm, _, _)) = AtomSet.member (s, nm)

        val free = foldl listFree AtomSet.empty
                        ((g::upd) @ (case syn of NONE => []
                                               | SOME (_, _, subs) => subs))
      in
        P.Transition {id=id, source=source, target=target,
                      select=(List.filter (isUsed free) sel, selPos),
                      guard=(g,gP), sync=(syn,synP), update=(upd,updP),
                      comments=comments, position=position,
                      color=color, nails=nails}
      end
end

