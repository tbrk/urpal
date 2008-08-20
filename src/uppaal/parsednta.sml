(* $Id$
 *
 * Copyright (c) 2008 Timothy Bourke (University of NSW and NICTA)
 * All rights reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the "BSD License" which is distributed with the
 * software in the file LICENSE.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the BSD
 * License for more details.
 *)

local
structure ParsedOutput : NTA_TYPES_OUTPUT
=
let structure E      = Expression
          and Env    = Environment
          and D      = Declaration
          and ECVT   = ExpressionCvt
          and EnvCVT = EnvironmentCvt
       type symbol   = Atom.atom
in
struct
  type invariant     = E.expr
   and select        = E.boundid list
   and guard         = E.expr
   and sync          = (symbol * E.direction * E.expr list) option
   and update        = E.expr list
   and parameter     = D.param list
   and declaration   = Env.env
   and imports       = string option
   and instantiation = string option
   and system        = string

  val noInvariant = E.trueExpr
  and noSelect    = []
  and noGuard     = E.trueExpr
  and noSync      = NONE
  and noUpdate    = []

  val noParameter     = []
  val noDeclaration   = Env.base_env
  val noImports       = NONE
  val noInstantiation = NONE
  val noSystem        = ""

  type outstream = TextIO.StreamIO.outstream
  fun pr strm str = TextIO.StreamIO.output (strm, str)

  fun hasExpression e = not (E.equal (e, E.BoolCExpr true))

  val hasInvariant     = hasExpression
  fun hasSelect s      = (not o List.null) s
  val hasGuard         = hasExpression
  val hasSync          = Option.isSome
  fun hasUpdate s      = (not o List.null) s
  fun hasParameter s   = (not o List.null) s
  val hasDeclaration   = (not o Env.isEmpty)
  val hasImports       = Option.isSome
  val hasInstantiation = Option.isSome

  val outputInvariant                  = ECVT.Expr.toStream
  val outputSelect                     = ECVT.selectToStream
  val outputGuard                      = ECVT.Expr.toStream
  fun outputSync (os, SOME sync)       = ECVT.syncToStream (os, sync)
    | outputSync _                     = ()

  val outputUpdate                     = ECVT.exprlistToStream
  val outputParameter                  = ECVT.paramlistToStream
  val outputDeclaration                = EnvCVT.toStream
  val outputTemplateDeclaration        = EnvCVT.templateToStream

  fun outputImports (os, NONE)         = ()
    | outputImports (os, SOME s)       = pr os s

  fun outputInstantiation (os, NONE)   = ()
    | outputInstantiation (os, SOME s) = pr os s

  fun outputSystem (os, s) = pr os s
end
end

structure ParsedNtaOutput = NtaOutputFn (structure T = ParsedOutput
                                         structure S = TextIO.StreamIO)

structure E = Expression
in
structure ParsedNta : PARSED_NTA = struct
  open ParsedNtaOutput

  (* shortcuts over Atom and AtomSet *)
  infix <+ <- ++ <\ \ =:= ; open Symbol

  fun freeTransitionNames (Transition {select=(sel, _), guard=(g, _),
                                      sync=(sync, _), update=(upd, _), ...}) =
    let
      fun syncSubs NONE = [] | syncSubs (SOME (_, _, subs)) = subs
      val exprListNames = foldl (fn (e, s)=> s ++ (E.getFreeNames e)) emptyset

      val boundnms = foldl (fn (E.BoundId (nm, _), s)=> s <+ nm) emptyset sel
      val updatenms = exprListNames upd
      val syncnms = exprListNames (syncSubs sync)
    in (updatenms ++ syncnms ++ E.getFreeNames g) \ boundnms end

end 
end (* local *)

