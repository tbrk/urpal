(* $Id$
 *
 * 20070820 T. Bourke
 *  Original code.
 *
 *)

structure EnvironmentCvt :> ENVIRONMENT_CVT
=
struct
  structure Env = Environment

  structure OutPPS = PPStreamFn (structure Token  = StringToken
                                       and Device = SimpleTextIODev)
  structure OutPPD = PPDescFn (OutPPS)
  structure EPP = ExpressionPP (structure PPD=OutPPD and E=Expression)
  structure DPP = DeclarationPP (structure PPD=OutPPD and D=Declaration
                                 and EPP=EPP)
  structure OutEnvPP = EnvironmentPP (structure PPD=OutPPD and Env=Env
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

