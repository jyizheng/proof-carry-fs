/*

The MIT License

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

*/



%include {
#include "pcfs-parse-ds.hpp"
#include <assert.h>
}

%name PcfsCParse
%token_prefix PCFS_TOKEN_C_

%extra_argument { ParserOutput * output}   // Return top level tree to this argument
%start_symbol start

%token_type                        {PrimitiveValue *}
%token_destructor                  {freeDataPrimitiveValue($$); delete($$);}


// start has no type and no destructor -- data is saved into an externally passed structure
// The three terminals BEGIN_DECLARATION, BEGIN_PROCAP and BEGIN_TERM are not generated by the
// lexer. These are to be passed in by the routine driving the lexer and scanner to push the 
// parser into one of the three modes. The driving routine must also set the value of 
// output -> type to parser_error, before passing it in.
start ::= BEGIN_DECLARATION declarationList.     
	{
		output -> type = ParserOutput :: parser_declaration; 
	}   
start ::= BEGIN_PROCAP PROCAP procap(P).
	{
	  output -> type = ParserOutput :: parser_procap;
	  output -> data.procap = P;
	  
	}
start ::= BEGIN_TERM term(T).
	{
		output -> type = ParserOutput :: parser_term;
		output -> data.term = T;
	}

// Lists of declarations and declarations don't return anything
// They just add declarations to the index
declarationList ::= .
declarationList ::= declaration declarationList.

declaration ::= ID(I) COLON SORT DOT.
	{
		Declaration * d = new DeclarationSort;
		if (output -> data.index -> add(I -> data.str, d) != 1) {
			delete d;
		}
		delete [] (I -> data.str);
		delete I;
	}
declaration ::= ID(I) COLON declarationTerm(DT) DOT.
	{
		if (output -> data.index -> add(I -> data.str, DT) != 1) {
			delete DT;
		}
		delete [] (I -> data.str);
		delete I;
	}
declaration ::= ID(I) COLON declarationConstraint(DC) DOT.
	{
		if (output -> data.index -> add(I -> data.str, DC) != 1) {
			delete DC;
		}
		delete [] (I -> data.str);
		delete I;
	}
declaration ::= ID(I) COLON declarationPredicate(DP) DOT.
	{
		if (output -> data.index -> add(I -> data.str, DP) != 1) {
			delete DP;
		}
		delete [] (I -> data.str);
		delete I;
	}
declaration ::= ID(I) COLON declarationState(DS) DOT.
	{
		if (output -> data.index -> add(I -> data.str, DS) != 1)  {
			delete DS;
		}
		delete [] (I -> data.str);
		delete I;
	}

%type declarationTerm              {DeclarationTerm *}
%destructor declarationTerm        {delete ($$);}
declarationTerm(DT) ::= TERM sortid(S).
	{
		DT = new DeclarationTerm(new Queue<Sort *>, S);
	}
declarationTerm(DT1) ::= TERM sortid(S) RARROW declarationTerm(DT2).
	{
		DT1 = DT2;
		DT1 -> sortsOfArgs -> pushHead(S); 
	}
declarationTerm(DT1) ::= LPAREN declarationTerm(DT2) RPAREN.
	{
		DT1 = DT2;
	}

%type declarationConstraint        {DeclarationConstraint *}
%destructor declarationConstraint  {delete ($$);}
declarationConstraint(DT) ::= CONSTRAINT.
	{
		DT = new DeclarationConstraint (new Queue<Sort *>);
	}
declarationConstraint(DT1) ::= TERM sortid(S) RARROW declarationConstraint(DT2).
	{
		DT1 = DT2;
		DT1 -> sortsOfArgs -> pushHead(S);
	}
declarationConstraint(DT1) ::= LPAREN declarationConstraint(DT2) RPAREN.
	{
		DT1 = DT2;
	}

%type declarationPredicate        {DeclarationPredicate *}
%destructor declarationPredicate  {delete ($$);}
declarationPredicate(DT) ::= PRED.
	{
		DT = new DeclarationPredicate(new Queue<Sort *>);
	}
declarationPredicate(DT1) ::= TERM sortid(S) RARROW declarationPredicate(DT2).
	{
		DT1 = DT2;
		DT1 -> sortsOfArgs -> pushHead(S);
	}
declarationPredicate(DT1) ::= LPAREN declarationPredicate(DT2) RPAREN.
	{
		DT1 = DT2;
	}

%type declarationState            {DeclarationState *}
%destructor declarationState      {delete ($$);}
declarationState(DT) ::= STATE.
	{
		DT = new DeclarationState(new Queue<Sort *>);
	}
declarationState(DT1) ::= TERM sortid(S) RARROW declarationState(DT2).
	{
		DT1 = DT2;
		DT1 -> sortsOfArgs -> pushHead(S);
	}
declarationState(DT1) ::= LPAREN declarationState(DT2) RPAREN.
	{
		DT1 = DT2;
	}


// Sorts

%type sortid                      {Sort *}
%destructor sortid                {delete ($$);}
sortid(S) ::= SORT_ANY.
	{
		S = new SortAny;
	}
sortid(S) ::= SORT_PRINCIPAL.
	{
		S = new SortPrincipal;
	}
sortid(S) ::= SORT_TIME.
	{
		S = new SortTime;
	}
sortid(S) ::= SORT_EXP.
	{
		S = new SortExp;
	}
sortid(S) ::= SORT_FILE.
	{
		S = new SortFile;
	}
sortid(S) ::= SORT_PERM.
	{
		S = new SortPerm;
	}
sortid(S) ::= SORT_STR.
	{
		S = new SortStr;
	}
sortid(S) ::= SORT_INT.
	{
		S = new SortInt;
	}
sortid(S) ::= SORT_DATE.
	{
		S = new SortDate;
	}
sortid(S) ::= ID(I).
	{
		S = new SortOther (I -> data.str);
		delete I;
	}

// ProCaps
	
%type procap                      {ProCap *}
%destructor procap                {delete ($$);}
procap(P) ::= arg(K) arg(F) arg(PR) qhcl(Q) stateList(S) DOT MAC(M).
	{
		P = new ProCap(K, F, PR, Q, S, M -> data.str);
		delete M;
	}

%type qhcl                        {Qhcl *}
%destructor qhcl                  {delete ($$);}
qhcl(Q) ::= QHCL_BASE hypconstraintList(HL).
	{
		Q = new Qhcl(new Queue <IdSortPair *>, HL);	
	}
qhcl(Q1) ::= QHCL_ALL LPAREN LSQ ID(I) COLON TERM sortid(S) RSQ qhcl(Q2) RPAREN.
	{
		Q1 = Q2;
		Q1 -> boundVars -> push (new IdSortPair(I -> data.str, S));
		delete I;
	}
qhcl(Q1) ::= LPAREN qhcl(Q2) RPAREN.
	{
		Q1 = Q2;
	}

%type hypconstraintList           {Queue <HypConstraint *> *}
%destructor hypconstraintList     {freeQueueWithData($$);}
hypconstraintList(HL) ::= HYPCONSTRAINT_NIL.
	{
		HL = new Queue<HypConstraint *>;
	}
hypconstraintList(HL1) ::= HYPCONSTRAINT_CONS hypconstraint(H) hypconstraintList(HL2).
	{
		HL1 = HL2;
		HL1 -> pushHead (H);
	}
hypconstraintList(HL1) ::= LPAREN hypconstraintList(HL2) RPAREN.
	{
		HL1 = HL2;
	}

%type hypconstraint               {HypConstraint *}
%destructor hypconstraint         {delete ($$);}

hypconstraint(H) ::= HYPCONSTRAINT_ constraintList(CL) constraint(C).
	{
		H = new HypConstraint(CL, C);
	}
hypconstraint(H1) ::= LPAREN hypconstraint(H2) RPAREN.
	{
		H1 = H2;
	}

%type constraintList              {Queue <Constraint *> *}
%destructor constraintList        {freeQueueWithData($$);}
constraintList(CL) ::= CONSTRAINT_NIL.
	{
		CL = new Queue<Constraint *>;
	}
constraintList(CL1) ::= CONSTRAINT_CONS constraint(C) constraintList(CL2).
	{
		CL1 = CL2;
		CL1 -> pushHead(C);
	}
constraintList(CL1) ::= LPAREN constraintList(CL2) RPAREN.
	{
		CL1 = CL2;
	}

%type constraint                  {Constraint *}
%destructor constraint            {delete ($$);}
constraint(C) ::= STRONGER arg(T1) arg(T2).
	{
		C = new ConstraintStronger(T1, T2);
	}
constraint(C) ::= LEQ arg(T1) arg(T2).
	{
		C = new ConstraintLeq(T1, T2);
	}
constraint(C) ::= ID(I) argList(AL).
	{
		C = new ConstraintOther(I -> data.str, AL);
		delete I;
	}
constraint(C) ::= LPAREN constraint(C1) RPAREN.
	{
		C = C1;
	}

// Terms are subdivided into various types, depending
// on where they occur. This is necessary to prevent
// right associativity. (a b c) should parse as ((a b) c),
// not (a (b c)).

%type term	 	{Term *}
%destructor term	{delete ($$);}
term(T) ::= termOther(TO).  // Term with unknown constructor 
	{
		T = TO;
	}
term(T) ::= termNull(TN). // Term with nullary constructor like loca
	{
		T = TN;
	}
term(T) ::= termMult(TM). // Term with known n-ary constructor like int2principal
	{
		T = TM;
	}
term(T) ::= LPAREN term(T1) RPAREN. 
	{
		T = T1;
	}

// Term whose constructor is not pre-defined.
%type termOther		{TermOther *}
%destructor termOther	{delete($$);}
termOther(T) ::= ID(I).
	{
		T = new TermOther(I -> data.str, new Queue<Term *>);
		delete I;
	}		

termOther(T) ::= termOther(TH) arg(A).
	{
		T = TH;
		T -> args -> push(A);
	}

// A term argument.
%type arg		{Term *}
%destructor arg		{delete ($$);}
arg(A) ::= termNull(T).
	{
		A = T;
	}
arg(A) ::= ID(I).
	{
		A = new TermOther(I -> data.str, new Queue<Term *>);
		delete I;
	}
arg(A) ::= LPAREN term(T) RPAREN.
	{
		A = T;
	}

// Terms with fixed nullary constructors

%type termNull		{Term *}
%destructor termNull	{delete ($$);}
termNull(T) ::= LOCA.
	{
		T = new TermLoca;
	}
termNull(T) ::= CTIME.
	{
		T = new TermCtime;
	}
termNull (T) ::= NINFTY.
        {
	        T = new TermNinfty;
        }
termNull (T) ::= PINFTY.
        {
	        T = new TermPinfty;
        }
termNull(T) ::= STRING(S).
	{
		T = new TermPrim_str(S -> data.str);
		delete S;
	}
termNull(T) ::= INTEGER(I).
	{
		T = new TermPrim_int(I -> data.i);
		delete I;
	}
termNull(T) ::= DATE(D).
	{
		T = new TermPrim_date(D -> data.t);
		delete D;
	}

termNull(T) ::= READ.
	{
		T = new TermRead;
	}
termNull(T) ::= WRITE.
	{
		T = new TermWrite;
	}
termNull (T) ::= EXECUTE.
        {
	        T = new TermExecute;
        }
termNull(T) ::= IDENTITY.
	{
		T = new TermIdentity;
	}
termNull(T) ::= GOVERN.
	{
		T = new TermGovern;
	}


// Term with fixed non-nullary constructors
%type termMult		{Term *}
%destructor termMult	{delete ($$);}
termMult(T) ::= PRIM_STR2FILE arg(A).
	{
		T = new TermPrim_str2file(A);
	}
termMult(T) ::= PRIM_DATE2TIME arg(A).
	{
		T = new TermPrim_date2time(A);
	}	
termMult(T) ::= PRIM_INT2PRINCIPAL arg(A).
	{
		T = new TermPrim_int2principal(A);
	}
termMult(T) ::= PRIM_INT2TIME arg(A).
	{
		T = new TermPrim_int2time(A);
	}
termMult(T) ::= TIME2EXP arg(A).
	{
		T = new TermTime2exp(A);
	}
termMult(T) ::= EXP_ADD arg(A1) arg(A2).
	{
		T = new TermExp_add(A1,A2);
	}
termMult(T) ::= EXP_SUBTRACT arg(A1) arg(A2).
	{
		T = new TermExp_subtract(A1, A2);
	}
termMult(T) ::= EXP_MAX arg(A1) arg(A2).
	{
		T = new TermExp_max(A1, A2);
	}
termMult(T) ::= EXP_MIN arg(A1) arg(A2).
	{
		T = new TermExp_min(A1, A2);
	}

%type stateList                   {Queue <State *> *}
%destructor stateList             {freeQueueWithData($$);}
stateList(SL) ::= STATE_NIL.
	{
		SL = new Queue <State *>;
	}
stateList(SL1) ::= STATE_CONS state(S) stateList(SL2).
	{
		SL1 = SL2;
		SL1 -> pushHead(S);
	}
stateList(SL1) ::= LPAREN stateList(SL2) RPAREN.
	{
		SL1 = SL2;
	}

%type state                       {State *}
%destructor state                 {delete ($$);}
state(S) ::= OWNER arg(T1) arg(T2).
	{	
		S = new StateOwner(T1, T2);
	}
state(S) ::= HAS_XATTR arg(T1) arg(T2) arg(T3).
	{
		S = new StateHas_xattr(T1, T2, T3);
	}
state(S1) ::= LPAREN state(S2) RPAREN.
	{
		S1 = S2;
	}
state(S) ::= ID(I) argList(AL).
	{
		S = new StateOther(I -> data.str, AL);
		delete I;
	}

%type argList			{Queue<Term *> *}
%destructor argList		{freeQueueWithData($$);}
argList(AL) ::= .
	{
		AL = new Queue<Term *>;
	}
argList(AL) ::= argList(AL1) arg(A).
	{
		AL = AL1;
		AL -> push(A);
	}

// ERR is a symbol that is never returned
// This rule, in effect, causes strict checking
// In case of error, the stack is popped to the end
// Without this rule, the parser will try to discard symbols in case of error, thus allowing parsing with extra tokens.
error ::= ERR.
