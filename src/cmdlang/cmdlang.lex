(* $Id$ *)

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

fun stripParen s = let
    fun df #"(" = true | df c = Char.isSpace c
  in Substring.string (Substring.dropr df (Substring.full s)) end

fun eof (yyarg) = Tokens.EOF (FilePos.zero, FilePos.zero)

(* Handle keywords *)
structure Keyword =
  KeywordFn (type token=(svalue, pos) token
             type pos=pos
             val ident=Tokens.ID
             val keywords= [
                ("acceptall", Tokens.ACCEPTALL),
                ("channels",  Tokens.CHANNELS),
                ("conflate",  Tokens.CONFLATE),
                ("drop",      Tokens.DROP),
                ("help",      Tokens.HELP),
                ("list",      Tokens.LIST),
                ("maketest",  Tokens.MAKETEST),
                ("names",     Tokens.NAMES),
                ("parameters",Tokens.PARAMETERS),
                ("renamelocs",Tokens.RENAMELOCS),
                ("renametrans",Tokens.RENAMETRANS),
                ("scale",     Tokens.SCALE),
                ("setinitial",Tokens.SETINITIAL),
                ("show",      Tokens.SHOW),
                ("split",     Tokens.SPLIT),
                ("tabulate",  Tokens.TABULATE),
                ("quit",      Tokens.QUIT),
                ("writegraphics",Tokens.WRITEGRAPHICS)
             ])

%%
%header (functor CmdLangLexFn(structure Tokens  : CmdLang_TOKENS
                              structure FilePos : FILE_POS));
%arg (posstate);
%s COMMENT MCOMMENT;
alpha=[A-Za-z];
digit=[0-9];
underscore=[_];
ws=[\ \t];
newline=[\n];
strchar=([\\]["] | [^"\n]);
%%
<INITIAL>{ws}+     => (continue());
<INITIAL>{newline} => (FilePos.nextline (yyarg, yypos + 1); continue());

<INITIAL>("list"|"show"|"writegraphics"|"quit"|"drop"|"help")
                   => (Keyword.keyword (yytext,
                                        FilePos.currpos (yyarg, yypos),
                         FilePos.currpos (yyarg,
                                           yypos + String.size yytext - 1)));
<INITIAL>{alpha}+{ws}*\(
                   => (Keyword.keyword (stripParen(yytext),
                                        FilePos.currpos (yyarg, yypos),
                         FilePos.currpos (yyarg,
                                           yypos + String.size yytext - 1)));

<INITIAL>{alpha}({alpha}|{digit}|{underscore})*
                   => (Tokens.ID (Atom.atom yytext,
                         FilePos.currpos (yyarg, yypos),
                         FilePos.currpos (yyarg,
                                           yypos + String.size yytext - 1)));

<INITIAL>-?{digit}+  => (Tokens.INTEGER (valOf (Int.fromString yytext),
                           FilePos.currpos (yyarg, yypos),
                           FilePos.currpos (yyarg,
                                            yypos + String.size yytext - 1)));
<INITIAL>-?{digit}*\.{digit}+ =>
                        (Tokens.REAL (valOf (Real.fromString yytext),
                           FilePos.currpos (yyarg, yypos),
                           FilePos.currpos (yyarg,
                                            yypos + String.size yytext - 1)));

<INITIAL>"\""{strchar}*"\"" =>
                        (let val len = String.size yytext
                         in Tokens.STRING (substring (yytext, 1, len-2),
                              FilePos.currpos (yyarg, yypos),
                              FilePos.currpos (yyarg, yypos + len - 1))
                         end);

<INITIAL>"//"      => (YYBEGIN COMMENT; continue());
<INITIAL>"/*"      => (FilePos.incCommentDepth yyarg;
                       YYBEGIN MCOMMENT; continue());

<INITIAL>"("       => (addPos (Tokens.LPAR       ,yyarg,yypos));
<INITIAL>")"       => (addPos (Tokens.RPAR       ,yyarg,yypos));
<INITIAL>"["       => (addPos (Tokens.LSQPAR     ,yyarg,yypos));
<INITIAL>"]"       => (addPos (Tokens.RSQPAR     ,yyarg,yypos));
<INITIAL>"{|"      => (addPos (Tokens.LBARBRACE  ,yyarg,yypos));
<INITIAL>"|}"      => (addPos (Tokens.RBARBRACE  ,yyarg,yypos));
<INITIAL>"{"       => (addPos (Tokens.LBRACE     ,yyarg,yypos));
<INITIAL>"}"       => (addPos (Tokens.RBRACE     ,yyarg,yypos));
<INITIAL>","       => (addPos (Tokens.COMMA      ,yyarg,yypos));
<INITIAL>"?"       => (addPos (Tokens.QUESTION   ,yyarg,yypos));
<INITIAL>"!"       => (addPos (Tokens.EXCLAM     ,yyarg,yypos));
<INITIAL>"/"       => (addPos (Tokens.SLASH      ,yyarg,yypos));
<INITIAL>"\\"      => (addPos (Tokens.BACKSLASH  ,yyarg,yypos));
<INITIAL>"++"      => (addPos (Tokens.UNION      ,yyarg,yypos));
<INITIAL>"="       => (addPos (Tokens.ASSIGN     ,yyarg,yypos));
<INITIAL>";"       => (addPos (Tokens.SEMICOLON  ,yyarg,yypos));
<INITIAL>"<->"     => (addPos2(Tokens.DBLARROW   ,yyarg,yypos));
<INITIAL>"->"      => (addPos2(Tokens.RARROW     ,yyarg,yypos));
<INITIAL>"<-"      => (addPos2(Tokens.LARROW     ,yyarg,yypos));

<INITIAL>.         => (addPos (Tokens.UNKNOWN    ,yyarg,yypos));

<COMMENT>{newline}  => (FilePos.nextline (yyarg, yypos + 1);
                        YYBEGIN INITIAL; continue());
<COMMENT>[^\n]+     => (continue());

<MCOMMENT>{newline} => (FilePos.nextline (yyarg, yypos + 1); continue());
<MCOMMENT>"/*"      => (FilePos.incCommentDepth yyarg; continue());
<MCOMMENT>"*/"      => (if FilePos.decCommentDepth yyarg
                        then YYBEGIN INITIAL else ();
                        continue());
<MCOMMENT>"*"       => (continue());
<MCOMMENT>[/]       => (continue());
<MCOMMENT>[^*\n/]+  => (continue());

