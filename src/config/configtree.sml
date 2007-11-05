(* $Id$ *)

structure ConfigTree :> CONFIG_TREE = ConfigTree (struct
    datatype lexresult = datatype ConfigLex.UserDeclarations.lexresult
    val makeLexer = ConfigLex.makeLexer
    val linenum = (fn ()=> !ConfigLex.UserDeclarations.linenum)
  end
)

