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

structure EnvironmentCvt :> ENVIRONMENT_CVT
=
struct
  structure Env = Environment

  structure OutPPS = PPStreamFn (structure Token  = StringToken
                                       and Device = SimpleTextIODev)
  structure OutPPD = PPDescFn (OutPPS)
  structure EPP = ExpressionPPFn (structure PPD=OutPPD and E=Expression)
  structure DPP = DeclarationPPFn (structure PPD=OutPPD and D=Declaration
                                   and EPP=EPP)
  structure OutEnvPP = EnvironmentPPFn (structure PPD=OutPPD and Env=Env
                                        and EPP=EPP and DPP=DPP)

  type env = Env.env
  type outstream = TextIO.StreamIO.outstream

  fun ppdToOutput (os, pp_desc) = let
      val dev = SimpleTextIODev.openDev {dst=TextIO.mkOutstream os,
                                         wid=Settings.maxDeclarationWidth()}
      val strm = OutPPD.PPS.openStream dev
    in
      OutEnvPP.print strm pp_desc;
      OutPPD.PPS.closeStream strm
    end

    fun toStream (os, env) = ppdToOutput (os, OutEnvPP.fromEnv env)
    fun templateToStream (os, env) =
            ppdToOutput (os, OutEnvPP.fromEnv' (env, Env.TemplateScope))

end

