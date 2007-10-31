signature CmdLang_TOKENS =
sig
type ('a,'b) token
type svalue
val EOF:  'a * 'a -> (svalue,'a) token
val UNKNOWN:  'a * 'a -> (svalue,'a) token
val SEMICOLON:  'a * 'a -> (svalue,'a) token
val LARROW:  'a * 'a -> (svalue,'a) token
val RARROW:  'a * 'a -> (svalue,'a) token
val DBLARROW:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val ASSIGN:  'a * 'a -> (svalue,'a) token
val UNION:  'a * 'a -> (svalue,'a) token
val BACKSLASH:  'a * 'a -> (svalue,'a) token
val SLASH:  'a * 'a -> (svalue,'a) token
val EXCLAM:  'a * 'a -> (svalue,'a) token
val QUESTION:  'a * 'a -> (svalue,'a) token
val RBRACE:  'a * 'a -> (svalue,'a) token
val LBRACE:  'a * 'a -> (svalue,'a) token
val RBARBRACE:  'a * 'a -> (svalue,'a) token
val LBARBRACE:  'a * 'a -> (svalue,'a) token
val RSQPAR:  'a * 'a -> (svalue,'a) token
val LSQPAR:  'a * 'a -> (svalue,'a) token
val RPAR:  'a * 'a -> (svalue,'a) token
val LPAR:  'a * 'a -> (svalue,'a) token
val SETINITIAL:  'a * 'a -> (svalue,'a) token
val CONFLATE:  'a * 'a -> (svalue,'a) token
val HELP:  'a * 'a -> (svalue,'a) token
val PARAMETERS:  'a * 'a -> (svalue,'a) token
val SPLIT:  'a * 'a -> (svalue,'a) token
val CHANNELS:  'a * 'a -> (svalue,'a) token
val TABULATE:  'a * 'a -> (svalue,'a) token
val ACCEPTALL:  'a * 'a -> (svalue,'a) token
val NAMES:  'a * 'a -> (svalue,'a) token
val SCALE:  'a * 'a -> (svalue,'a) token
val RENAMETRANS:  'a * 'a -> (svalue,'a) token
val RENAMELOCS:  'a * 'a -> (svalue,'a) token
val MAKETEST:  'a * 'a -> (svalue,'a) token
val DROP:  'a * 'a -> (svalue,'a) token
val WRITEGRAPHICS:  'a * 'a -> (svalue,'a) token
val SHOW:  'a * 'a -> (svalue,'a) token
val LIST:  'a * 'a -> (svalue,'a) token
val QUIT:  'a * 'a -> (svalue,'a) token
val STRING: (string) *  'a * 'a -> (svalue,'a) token
val REAL: (real) *  'a * 'a -> (svalue,'a) token
val INTEGER: (int) *  'a * 'a -> (svalue,'a) token
val ID: (Atom.atom) *  'a * 'a -> (svalue,'a) token
end
signature CmdLang_LRVALS=
sig
structure Tokens : CmdLang_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
