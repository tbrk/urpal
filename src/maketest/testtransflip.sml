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
structure TestTransFlip :
sig
  val runTest : {input: string, output: TextIO.outstream} -> OS.Process.status
end
=
struct

  local structure TIO  = TextIO
              and SIO  = TextIO.StreamIO
              and S    = String
              and SS   = Substring
              and UP   = UppaalParse
              and Env  = Environment
              and TF   = TransitionFlipper
              and E    = UppaalParse.Expression
              and ECVT = ExpressionCvt
  in

  val testName = "test"

  fun readSegments filename = let
      fun readSegment strm = let
          fun count (n, strm) = case SIO.inputLine strm
                  of NONE => if n=0 then NONE else SOME n
                   | SOME ("--\n", _) => SOME n
                   | SOME (s, strm')  => count (n + S.size s, strm')
        in
          Option.map (fn n=> SIO.inputN (strm, n)) (count (0, strm))
        end

      fun skipDelim strm = case SIO.inputLine strm
                           of SOME ("--\n", strm') => strm'
                            | _ => strm

      fun go strm = let in
          case readSegment strm
          of NONE => (SIO.closeIn strm; [])
           | SOME (s, strm') => s :: go (skipDelim strm')
        end
    in
      go (TIO.getInstream (TIO.openIn filename))
    end

  fun parseTrans env tstr = let

      fun notNewline c = (c <> #"\n")
      fun expandTypes (E.BoundId (s,ty)) = E.BoundId(s,Env.expandTyIds (env,ty))

      val (selstr, rest)     = SS.splitl notNewline (SS.full tstr)
      val (actstr, guardstr) = SS.splitl notNewline (SS.triml 1 rest)

      (*val _ = TextIO.print "Parsing transition:select:"*)
      val select     = if SS.isEmpty selstr then []
                       else valOf (UP.parseSelect testName (SS.string selstr))
      (*val _ = TextIO.print "sync:"*)
      val (channame, subscripts) =
                       case UP.parseSync testName (SS.string actstr) of
                          NONE             => (Atom.atom "c", [])
                        | SOME (nm, _, sl) => (nm, sl)
      (*val _ = TextIO.print "guard:"*)
      val guard      = valOf (UP.parseExpression testName (SS.string guardstr))
      (*val _ = TextIO.print "\n"*)

    in
      SOME (channame, {selectids=map expandTypes select,
                       actionsubs=subscripts,
                       guard=guard})
    end
    handle Option => NONE

  fun readTest filename = let
      val (comments::decls::trans) = case readSegments filename
          of all as (_::_::_) => all
           | _ => Util.abort [filename, " does not contain enough sections."]
      val env = case UP.parseDeclarations testName decls of
                   NONE       => Util.abort ["bad declarations: ", filename]
                 | SOME decls => Env.addDeclarations (Env.base_env,
                                                      Env.GlobalScope, decls)
      val trans = List.mapPartial (parseTrans env) trans

    in (env, comments, decls, trans) end
    handle Bind => Util.abort ["bad test file: ", filename]

  fun checkName ((nm, tr), NONE)          = SOME (nm, [tr])
    | checkName ((nm, tr), SOME (n, trs)) =
        if Atom.compare (n, nm) = EQUAL
        then SOME (nm, tr::trs)
        else Util.abort ["Channel names are inconsistent: ",
                         Atom.toString n, " <> ", Atom.toString nm]

  fun runTest {input, output=ostrm} = let
      val (env, comment, decls, nmtrans) = readTest input
      fun print s = TextIO.output (ostrm, s)

      val (transtysop, trans) = case foldl checkName NONE nmtrans
           of NONE             => (TF.chanToSubRanges (env, Atom.atom "c"), [])
              (* default to a channel named "c" if no transitions are given *)
            | SOME (nm, trans) => (TF.chanToSubRanges (env, nm), rev trans)
      val transtys = case transtysop
                    of NONE     => []
                     | SOME tys => tys
      val _ = print comment
      val _ = print "--------------------\n"
      val _ = print decls
      val _ = print "--------------------\nBEFORE:\n"
      val _ = app (print o TF.toString) trans
      val _ = print "\n--------------------\nAFTER:\n"
      val trans' = TF.negateTransitions env (transtys, trans, E.trueExpr)
      val _ = app (print o TF.toString) trans'
      val _ = print "\n--------------------\n"
    in OS.Process.success end
    handle Env.DuplicateDefinition s => 
                (Util.abort ["duplicate definition: ", s, "\n"])
         | TF.FlipFailed reason => (Util.abort ["flip failed: ", reason, "\n"])
         | e => (Util.abort ["exception: ", General.exnMessage e, "\n"])
  end (* local *)
end

