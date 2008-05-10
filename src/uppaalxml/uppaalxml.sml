(* $Id$ *)

structure UppaalXML : UPPAAL_XML
=
let structure Parser = Parse (structure Dtd = UppaalDtd
                              structure Hooks = UppaalHooks
                              structure Resolve = UppaalResolver
                              structure ParserOptions = ParserOptions ())
          and T = TextNta
in
struct

  fun parse uri = let
      val dtd = UppaalDtd.initDtdTables ()
    in
      SOME (Parser.parseDocument (SOME uri) (SOME dtd) UppaalHooks.initData)
    end
    handle UppaalHooks.ParseError s => NONE before (Util.warn [s])
end
end

