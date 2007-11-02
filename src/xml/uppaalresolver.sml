(* $Id$ *)

structure UppaalResolver : Resolve =
struct
  exception DtdNotSupported of string
  val publicID="-//Uppaal Team//DTD Flat System 1.1//EN"

  local
  fun trySystem (pubstr, NONE)            = raise DtdNotSupported pubstr
    | trySystem (_, SOME (base, file, _)) = Uri.uriJoin (base, file)
  in

  fun resolveExtId (Base.EXTID(SOME (str, _), sys)) =
      if str=publicID then case (Settings.dtdPath ()) of
                             NONE   => trySystem (str, sys)
                           | SOME s => Uri.String2Uri s
      else trySystem (str, sys)

    | resolveExtId (Base.EXTID(NONE, sys)) = trySystem ("-none given-", sys)

  end (* local *)
      
end

