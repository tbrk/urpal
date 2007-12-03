(* $Id: cmdlang.lex 326 2007-10-31 05:39:12Z tbourke $ *)

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

fun eof (yyarg) = Tokens.EOF (FilePos.zero, FilePos.zero)

(* Handle keywords *)
structure Keyword =
  KeywordFn (type token=(svalue, pos) token
             type pos=pos
             val ident=Tokens.DIRECT
             val keywords= [
                ("A",    Tokens.A),
                ("C",    Tokens.C),
                ("AB",   Tokens.AB),
                ("DPTR", Tokens.DPTR),
                ("PC",   Tokens.PC),
                ("ADD",  Tokens.ADD),
                ("ADDC", Tokens.ADDC),
                ("SUBB", Tokens.SUBB),
                ("INC",  Tokens.INC),
                ("DEC",  Tokens.DEC),
                ("MUL",  Tokens.MUL),
                ("DIV",  Tokens.DIV),
                ("DA",   Tokens.DA),
                ("ANL",  Tokens.ANL),
                ("ORL",  Tokens.ORL),
                ("XRL",  Tokens.XRL),
                ("CLR",  Tokens.CLR),
                ("CPL",  Tokens.CPL),
                ("RL",   Tokens.RL),
                ("RLC",  Tokens.RLC),
                ("RR",   Tokens.RR),
                ("RRC",  Tokens.RRC),
                ("SWAP", Tokens.SWAP),
                ("MOV",  Tokens.MOV),
                ("MOVC", Tokens.MOVC),
                ("MOVX", Tokens.MOVX),
                ("PUSH", Tokens.PUSH),
                ("POP",  Tokens.POP),
                ("XCH",  Tokens.XCH),
                ("XCHD", Tokens.XCHD),
                ("SETB", Tokens.SETB),
                ("ACALL",Tokens.ACALL),
                ("LCALL",Tokens.LCALL),
                ("RET",  Tokens.RET),
                ("RETI", Tokens.RETI),
                ("AJMP", Tokens.AJMP),
                ("LJMP", Tokens.LJMP),
                ("SJMP", Tokens.SJMP),
                ("JMP",  Tokens.JMP),
                ("JZ",   Tokens.JZ),
                ("JNZ",  Tokens.JNZ),
                ("JC",   Tokens.JC),
                ("JNC",  Tokens.JNC),
                ("JB",   Tokens.JB),
                ("JNB",  Tokens.JNB),
                ("JBC",  Tokens.JBC),
                ("CJNE", Tokens.CJNE),
                ("DJNZ", Tokens.DJNZ),
                ("NOP",  Tokens.NOP)
             ])

%%
%header (functor MCS51LexFn(structure Tokens  : MCS51_TOKENS
                            structure FilePos : FILE_POS));
%arg (posstate);
alpha=[A-Za-z];
digit=[0-9];
underscore=[_];
ws=[\ \t];
newline=[\n];
comment=[;][^\n]*[\n];
%%
<INITIAL>{comment} => (FilePos.nextline (yyarg, yypos + size yytext);
                       addPos (Tokens.EOL, yyarg, yypos + size yytext));
<INITIAL>{ws}+     => (continue());
<INITIAL>{newline} => (FilePos.nextline (yyarg, yypos + 1);
                       addPos (Tokens.EOL, yyarg, yypos));

<INITIAL>"@R0"     => (Tokens.INDIRECT (0, FilePos.currpos (yyarg, yypos),
                                           FilePos.currpos (yyarg, yypos+3)));
<INITIAL>"@R1"     => (Tokens.INDIRECT (1, FilePos.currpos (yyarg, yypos),
                                           FilePos.currpos (yyarg, yypos+3)));

<INITIAL>"R0"      => (Tokens.REGISTER (0, FilePos.currpos (yyarg, yypos),
                                           FilePos.currpos (yyarg, yypos+2)));
<INITIAL>"R1"      => (Tokens.REGISTER (1, FilePos.currpos (yyarg, yypos),
                                           FilePos.currpos (yyarg, yypos+2)));
<INITIAL>"R2"      => (Tokens.REGISTER (2, FilePos.currpos (yyarg, yypos),
                                           FilePos.currpos (yyarg, yypos+2)));
<INITIAL>"R3"      => (Tokens.REGISTER (3, FilePos.currpos (yyarg, yypos),
                                           FilePos.currpos (yyarg, yypos+2)));
<INITIAL>"R4"      => (Tokens.REGISTER (4, FilePos.currpos (yyarg, yypos),
                                           FilePos.currpos (yyarg, yypos+2)));
<INITIAL>"R5"      => (Tokens.REGISTER (5, FilePos.currpos (yyarg, yypos),
                                           FilePos.currpos (yyarg, yypos+2)));
<INITIAL>"R6"      => (Tokens.REGISTER (6, FilePos.currpos (yyarg, yypos),
                                           FilePos.currpos (yyarg, yypos+2)));
<INITIAL>"R7"      => (Tokens.REGISTER (7, FilePos.currpos (yyarg, yypos),
                                           FilePos.currpos (yyarg, yypos+2)));

<INITIAL>"#"({alpha}|{digit}|{underscore})+
                   => (Tokens.IMMEDIATE (String.extract (yytext, 1, NONE),
                         FilePos.currpos (yyarg, yypos),
                         FilePos.currpos (yyarg, yypos + size yytext - 1)));

<INITIAL>({alpha}|{digit}|[*]|{underscore})+
                   => (Keyword.keyword (yytext, FilePos.currpos (yyarg,yypos),
                            FilePos.currpos (yyarg, yypos + size yytext - 1)));

<INITIAL>","       => (addPos (Tokens.COMMA      ,yyarg,yypos));
<INITIAL>"+"       => (addPos (Tokens.PLUS       ,yyarg,yypos));
<INITIAL>"@"       => (addPos (Tokens.AT         ,yyarg,yypos));
<INITIAL>":"       => (addPos (Tokens.COLON      ,yyarg,yypos));
<INITIAL>"/"       => (addPos (Tokens.SLASH      ,yyarg,yypos));

<INITIAL>.         => (addPos (Tokens.UNKNOWN    ,yyarg,yypos));

