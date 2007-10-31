(* $Id$ *)

structure ConfigTree = ConfigTree (struct
    datatype lexresult = datatype ConfigLex.UserDeclarations.lexresult
    val makeLexer = ConfigLex.makeLexer
    val linenum = (fn ()=> !ConfigLex.UserDeclarations.linenum)
  end
) :> CONFIG_TREE

