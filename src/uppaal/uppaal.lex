(* $Id$

   ML-Lex lexer for Uppaal 4.x

   20070612 T.Bourke
     Derived from the ML-Yacc Pascal example, and the
     Uppaal Timed Automata Parser Library documentation
     (http://www.cs.auc.dk/~behrmann/utap/syntax.html).

 *)

(* Type abbreviations to match signature for ML-Yacc *)
structure Tokens = Tokens
type pos = FilePos.pos
type svalue = Tokens.svalue
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token
type arg = FilePos.state
 
(* Auxilliaries *)
fun addPos  (token, st, pos) = let val p=FilePos.currpos (st, pos)
                               in token (p,p) end
fun addPos2 (token, st, pos) = token (FilePos.currpos (st, pos),
                                      FilePos.currpos (st, pos + 1))
fun addPos3 (token, st, pos) = token (FilePos.currpos (st, pos),
                                      FilePos.currpos (st, pos + 2))

fun eof (yyarg) = Tokens.EOF (FilePos.zero, FilePos.zero)

(* Handle keywords *)
structure Keyword =
  KeywordFn (type token=(svalue, pos) token
             type pos=pos
             val ident=Tokens.ID
             val keywords= [
                ("after_update",Tokens.AFTER_UPDATE),
                ("and",         Tokens.AND),
                ("assign",      Tokens.ASSIGN),
                ("before_update",Tokens.BEFORE_UPDATE),
                ("bool",        Tokens.BOOL),
                ("break",       Tokens.BREAK),
                ("broadcast",   Tokens.BROADCAST),
                ("case",        Tokens.CASE),
                ("chan",        Tokens.CHAN),
                ("clock",       Tokens.CLOCK),
                ("commit",      Tokens.COMMIT),
                ("const",       Tokens.CONST),
                ("continue",    Tokens.CONTINUE),
                ("deadlock",    Tokens.DEADLOCK),
                ("default",     Tokens.DEFAULT),
                ("do",          Tokens.DO),
                ("else",        Tokens.ELSE),
                ("exists",      Tokens.EXISTS),
                ("false",       Tokens.FALSE),
                ("for",         Tokens.FOR),
                ("forall",      Tokens.FORALL),
                ("guard",       Tokens.GUARD),
                ("if",          Tokens.IF),
                ("imply",       Tokens.IMPLY),
                ("init",        Tokens.INIT),
                ("int",         Tokens.INT),
                ("meta",        Tokens.META),
                ("not",         Tokens.NOT),
                ("or",          Tokens.OR),
                ("priority",    Tokens.PRIORITY),
                ("process",     Tokens.PROCESS),
                ("progress",    Tokens.PROGRESS),
                ("rate",        Tokens.RATE),
                ("return",      Tokens.RETURN),
                ("scalar",      Tokens.SCALAR),
                ("select",      Tokens.SELECT),
                ("state",       Tokens.STATE),
                ("struct",      Tokens.STRUCT),
                ("switch",      Tokens.SWITCH),
                ("sync",        Tokens.SYNC),
                ("system",      Tokens.SYSTEM),
                ("trans",       Tokens.TRANS),
                ("true",        Tokens.TRUE),
                ("typedef",     Tokens.TYPEDEF),
                ("urgent",      Tokens.URGENT),
                ("void",        Tokens.VOID),
                ("while",       Tokens.WHILE)
             ])

(* NB: FXP gives tabs as "\t" and newlines as "\n". *)

%%
%header (functor UppaalLexFun(structure Tokens : Uppaal_TOKENS
                              structure FilePos : FILE_POS));
%arg (posstate);
%s COMMENT MCOMMENT;
alpha=[A-Za-z];
digit=[0-9];
underscore=[_];
ws=([\ \t] | "\\t");
newline=(\n | "\\n");
%%
<INITIAL>{ws}+     => (continue());
<INITIAL>{newline} => (FilePos.nextline (yyarg, yypos + 1); continue());

<INITIAL>{alpha}({alpha}|{digit}|{underscore})*
                   => (Keyword.keyword (yytext,
                                        FilePos.currpos (yyarg, yypos),
                         FilePos.currpos (yyarg,
                                           yypos + String.size yytext - 1)));

<INITIAL>{digit}+  => (Tokens.INTEGER (valOf (Int.fromString yytext),
                         FilePos.currpos (yyarg, yypos),
                         FilePos.currpos (yyarg,
                                           yypos + String.size yytext - 1)));

<INITIAL>"//"      => (YYBEGIN COMMENT; continue());
<INITIAL>"/*"      => (YYBEGIN MCOMMENT; continue());

<INITIAL>"("       => (addPos (Tokens.LPAR          ,yyarg,yypos));
<INITIAL>")"       => (addPos (Tokens.RPAR          ,yyarg,yypos));
<INITIAL>"["       => (addPos (Tokens.LSQPAR        ,yyarg,yypos));
<INITIAL>"]"       => (addPos (Tokens.RSQPAR        ,yyarg,yypos));
<INITIAL>"{"       => (addPos (Tokens.LBRACE        ,yyarg,yypos));
<INITIAL>"}"       => (addPos (Tokens.RBRACE        ,yyarg,yypos));
<INITIAL>"?"       => (addPos (Tokens.QUESTION      ,yyarg,yypos));
<INITIAL>":"       => (addPos (Tokens.COLON         ,yyarg,yypos));
<INITIAL>";"       => (addPos (Tokens.SEMICOLON     ,yyarg,yypos));
<INITIAL>"."       => (addPos (Tokens.DOT           ,yyarg,yypos));
<INITIAL>","       => (addPos (Tokens.COMMA         ,yyarg,yypos));

<INITIAL>"++"      => (addPos2 (Tokens.PLUSPLUS     ,yyarg,yypos));
<INITIAL>"--"      => (addPos2 (Tokens.MINUSMINUS   ,yyarg,yypos));

<INITIAL>"+"       => (addPos  (Tokens.PLUS         ,yyarg,yypos));
<INITIAL>"-"       => (addPos  (Tokens.MINUS        ,yyarg,yypos));
<INITIAL>"!"       => (addPos  (Tokens.EXCLAM       ,yyarg,yypos));
<INITIAL>"*"       => (addPos  (Tokens.TIMES        ,yyarg,yypos));
<INITIAL>"/"       => (addPos  (Tokens.DIVIDE       ,yyarg,yypos));
<INITIAL>"%"       => (addPos  (Tokens.MOD          ,yyarg,yypos));
<INITIAL>"&"       => (addPos  (Tokens.AMPERSAND    ,yyarg,yypos));
<INITIAL>"|"       => (addPos  (Tokens.BITOR        ,yyarg,yypos));
<INITIAL>"^"       => (addPos  (Tokens.BITXOR       ,yyarg,yypos));
<INITIAL>"<<"      => (addPos2 (Tokens.SHLEFT       ,yyarg,yypos));
<INITIAL>">>"      => (addPos2 (Tokens.SHRIGHT      ,yyarg,yypos));

<INITIAL>"<"       => (addPos  (Tokens.LT           ,yyarg,yypos));
<INITIAL>"<="      => (addPos2 (Tokens.LE           ,yyarg,yypos));
<INITIAL>"=="      => (addPos2 (Tokens.EQ           ,yyarg,yypos));
<INITIAL>"!="      => (addPos2 (Tokens.NEQ          ,yyarg,yypos));
<INITIAL>">="      => (addPos2 (Tokens.GE           ,yyarg,yypos));
<INITIAL>">"       => (addPos  (Tokens.GT           ,yyarg,yypos));
<INITIAL>"<?"      => (addPos2 (Tokens.MIN          ,yyarg,yypos));
<INITIAL>">?"      => (addPos2 (Tokens.MAX          ,yyarg,yypos));

<INITIAL>"&&"      => (addPos2 (Tokens.DBLAMPERSAND ,yyarg,yypos));
<INITIAL>"||"      => (addPos2 (Tokens.DBLPIPE      ,yyarg,yypos));

<INITIAL>"="       => (addPos  (Tokens.ASSIGNMENT   ,yyarg,yypos));
<INITIAL>":="      => (addPos2 (Tokens.ASSIGNMENT   ,yyarg,yypos));
<INITIAL>"+="      => (addPos2 (Tokens.PLUSASSIGN   ,yyarg,yypos));
<INITIAL>"-="      => (addPos2 (Tokens.MINUSASSIGN  ,yyarg,yypos));
<INITIAL>"*="      => (addPos2 (Tokens.TIMESASSIGN  ,yyarg,yypos));
<INITIAL>"/="      => (addPos2 (Tokens.DIVIDEASSIGN ,yyarg,yypos));
<INITIAL>"%="      => (addPos2 (Tokens.MODASSIGN    ,yyarg,yypos));
<INITIAL>"|="      => (addPos2 (Tokens.BITORASSIGN  ,yyarg,yypos));
<INITIAL>"&="      => (addPos2 (Tokens.BITANDASSIGN ,yyarg,yypos));
<INITIAL>"^="      => (addPos2 (Tokens.BITXORASSIGN ,yyarg,yypos));
<INITIAL>"<<="     => (addPos3 (Tokens.SHLEFTASSIGN ,yyarg,yypos));
<INITIAL>">>="     => (addPos3 (Tokens.SHRIGHTASSIGN,yyarg,yypos));

<INITIAL>.         => (addPos  (Tokens.UNKNOWN      ,yyarg,yypos));

<COMMENT>{newline}  => (FilePos.nextline (yyarg, yypos + 1);
                        YYBEGIN INITIAL; continue());
<COMMENT>[^\n\\]+   => (continue());
<COMMENT>[\\]       => (continue());

<MCOMMENT>{newline} => (FilePos.nextline (yyarg, yypos + 1); continue());
<MCOMMENT>"*/"      => (YYBEGIN INITIAL; continue());
<MCOMMENT>"*"       => (continue());
<MCOMMENT>[\\]      => (continue());
<MCOMMENT>[^*\n\\]+ => (continue());

