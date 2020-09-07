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

  fun initlex() = (linenum:=1; charpos:=1)
  
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

%header (functor BLLexFun(structure Tokens: BL_TOKENS));
%full

ws=[\ \t];
endline=[\n];
validstrvalue=[^\"\n];

stringlimit="\"";
digit=[0-9];
lletter=[a-z];
uletter=[A-Z];
letter={uletter}|{lletter};
special="_"|"'"|"@"|"-"|"/";
genchar={lletter}|{uletter}|{digit}|{special};

opencomment="%";
closecomment="\n"|"\r";

iden={letter}({letter}|{special}|{digit})*;
integer=("-")?{digit}+;

date=({digit}+":"{digit}+":"{digit}+)(":"{digit}+":"{digit}+":"{digit}+)? ;

str="\""{validstrvalue}*"\"";

hex={lletter}|{digit}|{uletter};
hex2={hex}{hex};
hex4={hex2}{hex2};
hex8={hex4}{hex4};
hex16={hex8}{hex8};
hex32={hex16}{hex16};
hex40={hex32}{hex8};
mac="#"({ws}|"\n"|"\r")*{hex40};

eoflimiter=[\255];

start_prop=[\254];
start_declarations=[\253];
start_proofterm=[\252];
start_proof=[\251];
start_program=[\250];
start_constraint=[\249];
start_term=[\248];
start_qprocap=[\247];
start_substitution=[\246];

%s COMMENT;

%%

<INITIAL> {endline} => (inc(linenum); charpos:=1;lex());
<INITIAL> {ws}+ => (inccp(yytext); lex());
{eoflimiter} => (eof());
<INITIAL> {start_prop} => (inccp(yytext); Tokens.START_PROP((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> {start_declarations} => (inccp(yytext); Tokens.START_DECLARATIONS((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> {start_proof} => (inccp(yytext); Tokens.START_PROOF((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> {start_proofterm} => (inccp(yytext); Tokens.START_PROOFTERM((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> {start_program} => (inccp(yytext); Tokens.START_PROGRAM((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> {start_term} => (inccp(yytext); Tokens.START_TERM((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> {start_constraint} => (inccp(yytext); Tokens.START_CONSTRAINT((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> {start_qprocap} => (inccp(yytext); Tokens.START_QPROCAP((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> {start_substitution} => (inccp(yytext); Tokens.START_SUBSTITUTION((!linenum, !charpos - String.size(yytext)), getcurrpos()));

<INITIAL> {opencomment} => (inccp(yytext); YYBEGIN(COMMENT); lex());
<COMMENT> {closecomment} => (inc(linenum); charpos:=1 ; inccp(yytext); YYBEGIN(INITIAL); lex());
<COMMENT> . => (inccp(yytext); lex());

<INITIAL> "sort" => (inccp(yytext); Tokens.SORT_LIT((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "." => (inccp(yytext); Tokens.DOT((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "term" => (inccp(yytext); Tokens.TERM_LIT((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "->" => (inccp(yytext); Tokens.RARROW((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> ":" => (inccp(yytext); Tokens.COLON((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "constraint" => (inccp(yytext); Tokens.CONSTRAINT_LIT((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "pred" => (inccp(yytext); Tokens.PRED_LIT((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "state" => (inccp(yytext); Tokens.STATE_LIT((!linenum, !charpos - String.size(yytext)), getcurrpos()));

<INITIAL> "any" => (inccp(yytext); Tokens.ANY((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "principal" => (inccp(yytext); Tokens.PRINCIPAL((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "time" => (inccp(yytext); Tokens.TIME((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "exp" => (inccp(yytext); Tokens.EXP((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "file" => (inccp(yytext); Tokens.FILE((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "perm" => (inccp(yytext); Tokens.PERM((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "str" => (inccp(yytext); Tokens.STR((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "int" => (inccp(yytext); Tokens.INT((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "date" => (inccp(yytext); Tokens.DATE((!linenum, !charpos - String.size(yytext)), getcurrpos()));


<INITIAL> "loca" => (inccp(yytext); Tokens.LOCA((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "ctime" => (inccp(yytext); Tokens.CTIME((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "ninfty" => (inccp(yytext); Tokens.NINFTY((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "pinfty" => (inccp(yytext); Tokens.PINFTY((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "prim_str2file" => (inccp(yytext); Tokens.PRIM_STR2FILE((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "prim_date2time" => (inccp(yytext); Tokens.PRIM_DATE2TIME((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "prim_int2time" => (inccp(yytext); Tokens.PRIM_INT2TIME((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "prim_int2principal" => (inccp(yytext); Tokens.PRIM_INT2PRINCIPAL((!linenum, !charpos - String.size(yytext)), getcurrpos()));

<INITIAL> "read" => (inccp(yytext); Tokens.READ((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "write" => (inccp(yytext); Tokens.WRITE((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "execute" => (inccp(yytext); Tokens.EXECUTE((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "identity" => (inccp(yytext); Tokens.IDENTITY((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "govern" => (inccp(yytext); Tokens.GOVERN((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "time2exp" => (inccp(yytext); Tokens.TIME2EXP((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "exp_add" => (inccp(yytext); Tokens.EXP_ADD((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "exp_subtract" => (inccp(yytext); Tokens.EXP_SUBTRACT((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "exp_min" => (inccp(yytext); Tokens.EXP_MIN((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "exp_max" => (inccp(yytext); Tokens.EXP_MAX((!linenum, !charpos - String.size(yytext)), getcurrpos()));

<INITIAL> "leq" => (inccp(yytext); Tokens.LEQ((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "stronger" => (inccp(yytext); Tokens.STRONGER((!linenum, !charpos - String.size(yytext)), getcurrpos()));

<INITIAL> "owner" => (inccp(yytext); Tokens.OWNER((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "has_xattr" => (inccp(yytext); Tokens.HAS_XATTR((!linenum, !charpos - String.size(yytext)), getcurrpos()));


<INITIAL> "atom" => (inccp(yytext); Tokens.ATOM((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "is" => (inccp(yytext); Tokens.IS((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "conj" => (inccp(yytext); Tokens.CONJ((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "disj" => (inccp(yytext); Tokens.DISJ((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "imp" => (inccp(yytext); Tokens.IMP((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "top" => (inccp(yytext); Tokens.TOP((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "bot" => (inccp(yytext); Tokens.BOT((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "at" => (inccp(yytext); Tokens.AT((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "says" => (inccp(yytext); Tokens.SAYS((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "forall" => (inccp(yytext); Tokens.FORALL((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "exists" => (inccp(yytext); Tokens.EXISTS((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "cinj" => (inccp(yytext); Tokens.CINJ((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "sinj" => (inccp(yytext); Tokens.SINJ((!linenum, !charpos - String.size(yytext)), getcurrpos()));

<INITIAL> "hyp_true" => (inccp(yytext); Tokens.HYP_TRUE((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "hyp_claims" => (inccp(yytext); Tokens.HYP_CLAIMS((!linenum, !charpos - String.size(yytext)), getcurrpos()));

<INITIAL> "pf_conjI" => (inccp(yytext); Tokens.PF_CONJI((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "pf_conjE1" => (inccp(yytext); Tokens.PF_CONJE1((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "pf_conjE2" => (inccp(yytext); Tokens.PF_CONJE2((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "pf_disjI1" => (inccp(yytext); Tokens.PF_DISJI1((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "pf_disjI2" => (inccp(yytext); Tokens.PF_DISJI2((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "pf_disjE" => (inccp(yytext); Tokens.PF_DISJE((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "pf_impI" => (inccp(yytext); Tokens.PF_IMPI((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "pf_impE" => (inccp(yytext); Tokens.PF_IMPE((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "pf_topI" => (inccp(yytext); Tokens.PF_TOPI((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "pf_botE" => (inccp(yytext); Tokens.PF_BOTE((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "pf_atI" => (inccp(yytext); Tokens.PF_ATI((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "pf_atE" => (inccp(yytext); Tokens.PF_ATE((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "pf_saysI" => (inccp(yytext); Tokens.PF_SAYSI((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "pf_saysE" => (inccp(yytext); Tokens.PF_SAYSE((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "pf_forallI" => (inccp(yytext); Tokens.PF_FORALLI((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "pf_forallE" => (inccp(yytext); Tokens.PF_FORALLE((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "pf_existsI" => (inccp(yytext); Tokens.PF_EXISTSI((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "pf_existsE" => (inccp(yytext); Tokens.PF_EXISTSE((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "pf_cinjI" => (inccp(yytext); Tokens.PF_CINJI((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "pf_cinjE" => (inccp(yytext); Tokens.PF_CINJE((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "pf_sinjI" => (inccp(yytext); Tokens.PF_SINJI((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "pf_sinjE" => (inccp(yytext); Tokens.PF_SINJE((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "pf_synth2check" => (inccp(yytext); Tokens.PF_SYNTH2CHECK((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "pf_hyp" => (inccp(yytext); Tokens.PF_HYP((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "pf_is" => (inccp(yytext); Tokens.PF_IS((!linenum, !charpos - String.size(yytext)), getcurrpos()));


<INITIAL> "(" => (inccp(yytext); Tokens.LP((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> ")" => (inccp(yytext); Tokens.RP((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "[" => (inccp(yytext); Tokens.LSQ((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "]" => (inccp(yytext); Tokens.RSQ((!linenum, !charpos - String.size(yytext)), getcurrpos()));

<INITIAL> "parameters" => (inccp(yytext); Tokens.PARAMETERS_LIT((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "procap" => (inccp(yytext); Tokens.PROCAP_LIT((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "qhcl_base" => (inccp(yytext); Tokens.QHCL_BASE((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "qhcl_all" => (inccp(yytext); Tokens.QHCL_ALL((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "hypconstraint_" => (inccp(yytext); Tokens.HYPCONSTRAINT_((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "hypconstraint_cons" => (inccp(yytext); Tokens.HYPCONSTRAINT_CONS((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "hypconstraint_nil" => (inccp(yytext); Tokens.HYPCONSTRAINT_NIL((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "constraint_cons" => (inccp(yytext); Tokens.CONSTRAINT_CONS((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "constraint_nil" => (inccp(yytext); Tokens.CONSTRAINT_NIL((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "state_cons" => (inccp(yytext); Tokens.STATE_CONS((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> "state_nil" => (inccp(yytext); Tokens.STATE_NIL((!linenum, !charpos - String.size(yytext)), getcurrpos()));
<INITIAL> {mac} => (inccp(yytext); Tokens.MAC(yytext,(!linenum, !charpos - String.size(yytext)), getcurrpos()));

<INITIAL> {iden} => (inccp(yytext); Tokens.ID(yytext,(!linenum, !charpos - String.size(yytext)), getcurrpos()));

<INITIAL> {str} => (inccp(yytext); Tokens.PRIM_STR(extractstring yytext, (!linenum, !charpos - String.size(yytext)), getcurrpos()));

<INITIAL> {integer} => (inccp(yytext); Tokens.PRIM_INT(Option.valOf(Int.fromString(yytext)),(!linenum, !charpos - String.size(yytext)), getcurrpos()));

<INITIAL> {date} => (inccp(yytext); Tokens.PRIM_DATE(string2date(yytext),(!linenum, !charpos - String.size(yytext)), getcurrpos()));

<INITIAL> . => (inccp(yytext);print("Unexpected character: "^yytext^" in line "^Int.toString(!linenum)^" at position "^Int.toString(!charpos - String.size(yytext))^". Ignoring it...\n"); lex());


