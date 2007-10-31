signature Uppaal_TOKENS =
sig
type ('a,'b) token
type svalue
val SELECT:  'a * 'a -> (svalue,'a) token
val PROGRESS:  'a * 'a -> (svalue,'a) token
val PRIORITY:  'a * 'a -> (svalue,'a) token
val META:  'a * 'a -> (svalue,'a) token
val AFTER_UPDATE:  'a * 'a -> (svalue,'a) token
val BEFORE_UPDATE:  'a * 'a -> (svalue,'a) token
val RATE:  'a * 'a -> (svalue,'a) token
val DEADLOCK:  'a * 'a -> (svalue,'a) token
val TRANS:  'a * 'a -> (svalue,'a) token
val SYSTEM:  'a * 'a -> (svalue,'a) token
val ASSIGN:  'a * 'a -> (svalue,'a) token
val SYNC:  'a * 'a -> (svalue,'a) token
val GUARD:  'a * 'a -> (svalue,'a) token
val STATE:  'a * 'a -> (svalue,'a) token
val PROCESS:  'a * 'a -> (svalue,'a) token
val INIT:  'a * 'a -> (svalue,'a) token
val COMMIT:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
val UNKNOWN:  'a * 'a -> (svalue,'a) token
val DEFAULT:  'a * 'a -> (svalue,'a) token
val CASE:  'a * 'a -> (svalue,'a) token
val RETURN:  'a * 'a -> (svalue,'a) token
val SWITCH:  'a * 'a -> (svalue,'a) token
val CONTINUE:  'a * 'a -> (svalue,'a) token
val BREAK:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val DO:  'a * 'a -> (svalue,'a) token
val WHILE:  'a * 'a -> (svalue,'a) token
val FOR:  'a * 'a -> (svalue,'a) token
val CONST:  'a * 'a -> (svalue,'a) token
val BROADCAST:  'a * 'a -> (svalue,'a) token
val URGENT:  'a * 'a -> (svalue,'a) token
val STRUCT:  'a * 'a -> (svalue,'a) token
val SHRIGHTASSIGN:  'a * 'a -> (svalue,'a) token
val SHLEFTASSIGN:  'a * 'a -> (svalue,'a) token
val BITXORASSIGN:  'a * 'a -> (svalue,'a) token
val BITANDASSIGN:  'a * 'a -> (svalue,'a) token
val BITORASSIGN:  'a * 'a -> (svalue,'a) token
val MODASSIGN:  'a * 'a -> (svalue,'a) token
val DIVIDEASSIGN:  'a * 'a -> (svalue,'a) token
val TIMESASSIGN:  'a * 'a -> (svalue,'a) token
val MINUSASSIGN:  'a * 'a -> (svalue,'a) token
val PLUSASSIGN:  'a * 'a -> (svalue,'a) token
val ASSIGNMENT:  'a * 'a -> (svalue,'a) token
val NOT:  'a * 'a -> (svalue,'a) token
val EXISTS:  'a * 'a -> (svalue,'a) token
val FORALL:  'a * 'a -> (svalue,'a) token
val IMPLY:  'a * 'a -> (svalue,'a) token
val OR:  'a * 'a -> (svalue,'a) token
val AND:  'a * 'a -> (svalue,'a) token
val DBLPIPE:  'a * 'a -> (svalue,'a) token
val DBLAMPERSAND:  'a * 'a -> (svalue,'a) token
val MAX:  'a * 'a -> (svalue,'a) token
val MIN:  'a * 'a -> (svalue,'a) token
val GT:  'a * 'a -> (svalue,'a) token
val GE:  'a * 'a -> (svalue,'a) token
val NEQ:  'a * 'a -> (svalue,'a) token
val EQ:  'a * 'a -> (svalue,'a) token
val LE:  'a * 'a -> (svalue,'a) token
val LT:  'a * 'a -> (svalue,'a) token
val SHRIGHT:  'a * 'a -> (svalue,'a) token
val SHLEFT:  'a * 'a -> (svalue,'a) token
val BITXOR:  'a * 'a -> (svalue,'a) token
val BITOR:  'a * 'a -> (svalue,'a) token
val AMPERSAND:  'a * 'a -> (svalue,'a) token
val MOD:  'a * 'a -> (svalue,'a) token
val DIVIDE:  'a * 'a -> (svalue,'a) token
val TIMES:  'a * 'a -> (svalue,'a) token
val PLUS:  'a * 'a -> (svalue,'a) token
val MINUS:  'a * 'a -> (svalue,'a) token
val MINUSMINUS:  'a * 'a -> (svalue,'a) token
val PLUSPLUS:  'a * 'a -> (svalue,'a) token
val UMINUS:  'a * 'a -> (svalue,'a) token
val EXCLAM:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val DOT:  'a * 'a -> (svalue,'a) token
val SEMICOLON:  'a * 'a -> (svalue,'a) token
val COLON:  'a * 'a -> (svalue,'a) token
val QUESTION:  'a * 'a -> (svalue,'a) token
val RBRACE:  'a * 'a -> (svalue,'a) token
val LBRACE:  'a * 'a -> (svalue,'a) token
val RSQPAR:  'a * 'a -> (svalue,'a) token
val LSQPAR:  'a * 'a -> (svalue,'a) token
val RPAR:  'a * 'a -> (svalue,'a) token
val LPAR:  'a * 'a -> (svalue,'a) token
val FALSE:  'a * 'a -> (svalue,'a) token
val TRUE:  'a * 'a -> (svalue,'a) token
val CHAN:  'a * 'a -> (svalue,'a) token
val CLOCK:  'a * 'a -> (svalue,'a) token
val TYPEDEF:  'a * 'a -> (svalue,'a) token
val SCALAR:  'a * 'a -> (svalue,'a) token
val BOOL:  'a * 'a -> (svalue,'a) token
val INT:  'a * 'a -> (svalue,'a) token
val VOID:  'a * 'a -> (svalue,'a) token
val INTEGER: (int) *  'a * 'a -> (svalue,'a) token
val ID: (Atom.atom) *  'a * 'a -> (svalue,'a) token
val PARSEPARAMS:  'a * 'a -> (svalue,'a) token
val PARSEEXPRLIST:  'a * 'a -> (svalue,'a) token
val PARSESYNC:  'a * 'a -> (svalue,'a) token
val PARSESELECT:  'a * 'a -> (svalue,'a) token
val PARSEDECL:  'a * 'a -> (svalue,'a) token
val PARSEEXPR:  'a * 'a -> (svalue,'a) token
end
signature Uppaal_LRVALS=
sig
structure Tokens : Uppaal_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
