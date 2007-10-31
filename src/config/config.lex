(* $Id$

   ML-Lex lexer for hierarchical config files

   20071018 T. Bourke
     Original code, based on calculator example from ML-Lex manual.

 *)

datatype lexresult = Id of Atom.atom
                   | Int of int
                   | Color of string
                   | Real of real
                   | OpenSection
                   | CloseSection
                   | Assign
                   | StringStart
                   | StringLine of string
                   | StringEnd
                   | Eof

val linenum = ref 1
val commentDepth = ref 0

(* Auxilliaries *)
fun eof () = (linenum := 1; commentDepth := 0; Eof)

%%
%structure ConfigLex

%s STRING COMMENT MCOMMENT;
alpha=[A-Za-z];
digit=[0-9];
underscore=[_];
color=#[0-9A-Fa-f]{6};
ws=([\ \t] | "\\t");
newline=(\n | "\\n");
stringchar=([\\]["] | [^"\n]);
%%
<INITIAL>{ws}+      => (lex());
<INITIAL>{newline}  => (linenum := !linenum + 1; lex());
 
<INITIAL>{alpha}({alpha}|{digit}|{underscore})*
                    => (Id (Atom.atom yytext));

<INITIAL>-?{digit}+           => (Int (valOf (Int.fromString yytext)));
<INITIAL>-?{digit}*\.{digit}+ => (Real (valOf (Real.fromString yytext)));

<INITIAL>{color}    => (Color yytext);

<INITIAL>"\""       => (YYBEGIN STRING; StringStart);
<INITIAL>"//"       => (YYBEGIN COMMENT; lex());
<INITIAL>"/*"       => (commentDepth := !commentDepth + 1;
                        YYBEGIN MCOMMENT; lex());

<INITIAL>"{"        => (OpenSection);
<INITIAL>"("        => (OpenSection);
<INITIAL>"}"        => (CloseSection);
<INITIAL>")"        => (CloseSection);
<INITIAL>"="        => (Assign);
<INITIAL>[,;]       => (lex());
<INITIAL>.          => (lex());

<STRING>{stringchar}*[\n] => (linenum := !linenum + 1; StringLine yytext);

<STRING>{stringchar}* => (StringLine yytext);

<STRING>["]         => (YYBEGIN INITIAL; StringEnd);

<COMMENT>{newline}  => (linenum := !linenum + 1; YYBEGIN INITIAL; lex());
<COMMENT>[^\n\\]+   => (lex());
<COMMENT>[\\]       => (lex());

<MCOMMENT>{newline} => (linenum := !linenum + 1; lex());
<MCOMMENT>"/*"      => (commentDepth := !commentDepth + 1; lex());
<MCOMMENT>"*/"      => (commentDepth := Int.max (0, !commentDepth - 1);
                        if !commentDepth = 0
                        then YYBEGIN INITIAL else YYBEGIN MCOMMENT; lex());
<MCOMMENT>[*/]       => (lex());
<MCOMMENT>[^*\n\\/]+ => (lex());

