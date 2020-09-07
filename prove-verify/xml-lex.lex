(*
 Copyright (c) <2008,2009> <Deepak Garg dg@cs.cmu.edu>
 
 Permission is hereby granted, free of charge, to any person obtaining a copy
 of this software and associated documentation files (the "Software"), to deal
 in the Software without restriction, including without limitation the rights
 to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 copies of the Software, and to permit persons to whom the Software is
 furnished to do so, subject to the following conditions:
 
 The above copyright notice and this permission notice shall be included in
 all copies or substantial portions of the Software.
 
 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 THE SOFTWARE.
*)

structure Tokens = Tokens
  
  fun inc(n) = (n := !n+1)
  
  
  val linenum = ref(1)
  val charpos = ref(1)
  fun getcurrpos() = (!linenum, !charpos)

  fun inccp(x) = (charpos:= !charpos + String.size(x))

  val commentdepth = ref 0

  fun initlex() = (linenum:=1; charpos:=1; commentdepth:=0)
  
  fun toMonth (1) = Date.Jan
  | toMonth (2) = Date.Feb
  | toMonth (3) = Date.Mar
  | toMonth (4) = Date.Apr
  | toMonth (5) = Date.May
  | toMonth (6) = Date.Jun
  | toMonth (7) = Date.Jul
  | toMonth (8) = Date.Aug
  | toMonth (9) = Date.Sep
  | toMonth (10) = Date.Oct
  | toMonth (11) = Date.Nov
  | toMonth (12) = Date.Dec
  | toMonth n = toMonth (n mod 12 + 1)
  
  fun string2date s = 
  let val sl = String.tokens (fn (#":") => true | _ => false) s
      val ints = map (Option.valOf o Int.fromString) sl
      val ints2 = if (length(ints) = 6) then ints else ints @ [0,0,0]
      val arr = Array.fromList ints2
  in
	 Date.toTime (Date.date ({year = Array.sub(arr,0), month = toMonth(Array.sub(arr,1)),
				  day = Array.sub(arr,2), hour = Array.sub(arr, 3), 
				  minute = Array.sub(arr,4), second = Array.sub(arr, 5),
				  offset = NONE}))
  end
     
  
type pos = int * int

type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue, pos) token

val eof = (fn () => Tokens.EOFMARK(getcurrpos(), getcurrpos()))

  fun extractstring s = String.substring (s, 1, (String.size s) - 2)

%% 

%header (functor XMLLexFun(structure Tokens: XML_TOKENS));
%full

ws=[\ \t];
endline=[\n];

digit=[0-9];
lletter=[a-z];
uletter=[A-Z];
letter={uletter}|{lletter};
integer=("-")?{digit}+;

date=({digit}+":"{digit}+":"{digit}+)(":"{digit}+":"{digit}+":"{digit}+)? ;

validstrvalue=[^\n\r\"];
str="\""{validstrvalue}*"\"" ;

base64chars={uletter}|{lletter}|{digit}|"+"|"/";

base64=({base64chars})*;
base64end="=""="?;


header_begin_pubkey="-----BEGIN PUBLIC KEY-----";
header_end_pubkey="-----END PUBLIC KEY-----";

eoflimiter=[\255];
start_cert=[\254];
start_keymap=[\253];
start_date=[\252];

%s COMMENT;

%%

<INITIAL> {endline} => (inc(linenum); charpos:=1;lex());
<INITIAL> {ws}+ => (inccp(yytext); lex());
{eoflimiter} => (eof());

<INITIAL> {start_cert} => (inccp(yytext); Tokens.START_CERT((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> {start_keymap} => (inccp(yytext); Tokens.START_KEYMAP((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> {start_date} => (inccp(yytext); Tokens.START_DATE((!linenum, !charpos - String.size(yytext)), getcurrpos()));

<INITIAL> "<comment>" => (inccp(yytext); commentdepth:=1; YYBEGIN(COMMENT); lex());
<COMMENT> "<comment>" => (inccp(yytext); commentdepth:= (!commentdepth) + 1; lex());
<COMMENT> "</comment>" => (inccp(yytext); commentdepth:= (!commentdepth) -1; 
			   if ((!commentdepth) = 0) then YYBEGIN(INITIAL) else YYBEGIN(COMMENT); 
			   lex()); 
<COMMENT> {endline} => (inc(linenum); charpos:=1; lex());
<COMMENT> . => (inccp(yytext); lex());

<INITIAL> "<signature>" => (inccp(yytext); Tokens.SIGN_OPEN((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "</signature>" => (inccp(yytext); Tokens.SIGN_CLOSE((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "<element>" => (inccp(yytext); Tokens.ELEM_OPEN((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "</element>" => (inccp(yytext); Tokens.ELEM_CLOSE((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "<pubkey>" => (inccp(yytext); Tokens.PUBKEY_OPEN((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "</pubkey>" => (inccp(yytext); Tokens.PUBKEY_CLOSE((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "<data>" => (inccp(yytext); Tokens.DATA_OPEN((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "</data>" => (inccp(yytext); Tokens.DATA_CLOSE((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "<type>" => (inccp(yytext); Tokens.TYPE_OPEN((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "</type>" => (inccp(yytext); Tokens.TYPE_CLOSE((!linenum, !charpos - String.size(yytext)), getcurrpos()));

<INITIAL> "<from>" => (inccp(yytext); Tokens.FROM_OPEN((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "</from>" => (inccp(yytext); Tokens.FROM_CLOSE((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "<to>" => (inccp(yytext); Tokens.TO_OPEN((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "</to>" => (inccp(yytext); Tokens.TO_CLOSE((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "<subject>" => (inccp(yytext); Tokens.SUBJECT_OPEN((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "</subject>" => (inccp(yytext); Tokens.SUBJECT_CLOSE((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "<keymap>" => (inccp(yytext); Tokens.KEYMAP_OPEN((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "</keymap>" => (inccp(yytext); Tokens.KEYMAP_CLOSE((!linenum, !charpos - String.size(yytext)), getcurrpos()));


<INITIAL> {header_begin_pubkey} => (inccp(yytext); Tokens.HEADER_BEGIN_PUBKEY(yytext, (!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> {header_end_pubkey} => (inccp(yytext); Tokens.HEADER_END_PUBKEY(yytext, (!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> {str} => (inccp(yytext); Tokens.STR_VAL(extractstring yytext, (!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> {base64} => (inccp(yytext); Tokens.BASE64_COMPONENT(yytext, (!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> {base64end} => (inccp(yytext); Tokens.BASE64_END(yytext, (!linenum, !charpos - String.size(yytext)), getcurrpos()));

<INITIAL> {date} => (inccp(yytext); Tokens.PRIM_DATE(string2date(yytext),(!linenum, !charpos - String.size(yytext)), getcurrpos()));


<INITIAL> . => (inccp(yytext);print("Unexpected character: "^yytext^" in line "^Int.toString(!linenum)^" at position "^Int.toString(!charpos - String.size(yytext))^". Ignoring it...\n"); lex());


