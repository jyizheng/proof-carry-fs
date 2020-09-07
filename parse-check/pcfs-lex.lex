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


%{
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "pcfs-parse-ds.hpp"
#include "pcfs-gram.h"

  // Convert date of the form yyyy:mm:dd to integers
  static void readDateYYYYMMDD(const char * date, int * yy, int * mm, int * dd)
  {
    *yy = 0;
    while (*date != ':') {
      * yy = *yy * 10 + (*date - '0');
      date ++;
    }

    date++; // skip ':'

    *mm = 0;
    while (*date != ':') {
      * mm = *mm * 10 + (*date -'0');
      date ++;
    }

    date++;

    *dd = 0;
    while (*date != '\0') {
      *dd = *dd * 10 + (*date - '0');
      date ++;
    }
  }

  // Convert date of the form yyyy:mm:dd:hh:mm:ss to integers
  static void readDateYYYYMMDDHHMMSS(const char * date, int * yy, int * mm, int * dd, int * h, int * m, int * s)
  {
    *yy = 0;
    while (*date != ':') {
      * yy = *yy * 10 + (*date - '0');
      date ++;
    }

    date++; // skip ':'

    *mm = 0;
    while (*date != ':') {
      * mm = *mm * 10 + (*date -'0');
      date ++;
    }

    date++;

    *dd = 0;
    while (*date != ':') {
      *dd = *dd * 10 + (*date - '0');
      date ++;
    }

    date ++;

    *h = 0;
    while (*date != ':') {
      *h = *h * 10 + (*date - '0');
      date ++;
    }

    date ++;

    *m = 0;
    while (*date != ':') {
      * m = *m * 10 + (*date - '0');
      date ++;
    }
 
    date ++;

    *s = 0;
    while (*date != '\0') {
      * s = *s * 10 + (*date - '0');
      date ++;
    }
  }

  // Convert a string representing a number to the number
  static int asciiToInt(const char * c) {
    const char * d = c;
    if (*c == '-') d++;

    int v = 0;
    while (*d != '\0') {
      v = v * 10 + (*d - '0');
      d ++;
    }

    return ((*c == '-') ? (-v) : v);
  }

%}

%option reentrant
%option extra-type="PrimitiveValue *"
%option noyywrap
%option warn nodefault
%option outfile="pcfs-lex.cpp" header-file="pcfs-lex.hpp"

%option prefix="PcfsC"
%x COMMENT1 COMMENT2 COMMENT3 COMMENT4 COMMENT5 STRING

ID		[a-zA-Z][a-zA-Z0-9@/_\-\']*
NUMBER		-?[0-9]+
WS		[\n\t \r]
DATE		[0-9]{1,4}:[0-9]{1,2}:[0-9]{1,2}
DATETIME	{DATE}:[0-9]{1,2}:[0-9]{1,2}:[0-9]{1,2}
STRINGCHAR	[^\"\n]
MAC             #({WS}*)([a-fA-Z0-9]{40})

%%

{WS}+			;

procap			return PCFS_TOKEN_C_PROCAP;

:			return PCFS_TOKEN_C_COLON;

sort			return PCFS_TOKEN_C_SORT;

\.			return PCFS_TOKEN_C_DOT;

term			return PCFS_TOKEN_C_TERM;

->			return PCFS_TOKEN_C_RARROW;

"("			return PCFS_TOKEN_C_LPAREN;

")"			return PCFS_TOKEN_C_RPAREN;

constraint		return PCFS_TOKEN_C_CONSTRAINT;

pred			return PCFS_TOKEN_C_PRED;

state			return PCFS_TOKEN_C_STATE;

any			return PCFS_TOKEN_C_SORT_ANY;

principal		return PCFS_TOKEN_C_SORT_PRINCIPAL;

time			return PCFS_TOKEN_C_SORT_TIME;

exp			return PCFS_TOKEN_C_SORT_EXP;

file			return PCFS_TOKEN_C_SORT_FILE;

perm			return PCFS_TOKEN_C_SORT_PERM;

str			return PCFS_TOKEN_C_SORT_STR;

int			return PCFS_TOKEN_C_SORT_INT;

date			return PCFS_TOKEN_C_SORT_DATE;

qhcl_base		return PCFS_TOKEN_C_QHCL_BASE;
	
qhcl_all 		return PCFS_TOKEN_C_QHCL_ALL;

"["			return PCFS_TOKEN_C_LSQ;

"]"			return PCFS_TOKEN_C_RSQ;

hypconstraint_nil	return PCFS_TOKEN_C_HYPCONSTRAINT_NIL;

hypconstraint_cons	return PCFS_TOKEN_C_HYPCONSTRAINT_CONS;

hypconstraint_		return PCFS_TOKEN_C_HYPCONSTRAINT_;

constraint_nil		return PCFS_TOKEN_C_CONSTRAINT_NIL;

constraint_cons		return PCFS_TOKEN_C_CONSTRAINT_CONS;

stronger		return PCFS_TOKEN_C_STRONGER;

leq			return PCFS_TOKEN_C_LEQ;

loca			return PCFS_TOKEN_C_LOCA;

ctime			return PCFS_TOKEN_C_CTIME;

ninfty                  return PCFS_TOKEN_C_NINFTY;

pinfty                  return PCFS_TOKEN_C_PINFTY;

prim_str2file		return PCFS_TOKEN_C_PRIM_STR2FILE;

prim_date2time		return PCFS_TOKEN_C_PRIM_DATE2TIME;

prim_int2time		return PCFS_TOKEN_C_PRIM_INT2TIME;

prim_int2principal	return PCFS_TOKEN_C_PRIM_INT2PRINCIPAL;

read			return PCFS_TOKEN_C_READ;

write			return PCFS_TOKEN_C_WRITE;

execute			return PCFS_TOKEN_C_EXECUTE;

identity		return PCFS_TOKEN_C_IDENTITY;

govern                  return PCFS_TOKEN_C_GOVERN;

time2exp		return PCFS_TOKEN_C_TIME2EXP;

exp_add			return PCFS_TOKEN_C_EXP_ADD;

exp_subtract		return PCFS_TOKEN_C_EXP_SUBTRACT;

exp_max			return PCFS_TOKEN_C_EXP_MAX;

exp_min			return PCFS_TOKEN_C_EXP_MIN;

\"			BEGIN(STRING);
<STRING>{STRINGCHAR}*\"	{
				int n = strlen(yytext);
				char * p = new char[n+1];
				strcpy(p,yytext);
				p[n-1]='\0';
				yyextra->data.str = p;
				yyextra->type = PrimitiveValue::string;
				BEGIN(0);
				return PCFS_TOKEN_C_STRING;		
			}

<STRING>.		BEGIN(0); return 0;
<STRING>\n		BEGIN(0); return 0;

{DATE}		{
			int yy, mm, dd;	
			//sscanf(yytext, "%d:%d:%d", &yy, &mm, &dd);
			readDateYYYYMMDD(yytext, &yy, &mm, &dd);
			NativeTime * nt = new NativeTime (yy,mm,dd);
			yyextra->data.t = nt;	
			yyextra->type = PrimitiveValue::date;
			return PCFS_TOKEN_C_DATE;
		}

{DATETIME}	{
			int yy, mm, dd, h, m ,s;
			//sscanf(yytext, "%d:%d:%d:%d:%d:%d", &yy, &mm, &dd, &h, &m, &s);
			readDateYYYYMMDDHHMMSS(yytext, &yy, &mm, &dd, &h, &m, &s);
			NativeTime * nt = new NativeTime (yy, mm, dd, h, m, s);
			yyextra->data.t = nt;
			yyextra->type = PrimitiveValue::date;
			return PCFS_TOKEN_C_DATE;
		}


{NUMBER}	{
                        int i = asciiToInt(yytext);
                        yyextra->data.i = i;
			yyextra->type = PrimitiveValue::integer;
			return PCFS_TOKEN_C_INTEGER;
		}

state_nil		return PCFS_TOKEN_C_STATE_NIL;

state_cons		return PCFS_TOKEN_C_STATE_CONS;

owner			return PCFS_TOKEN_C_OWNER;

has_xattr		return PCFS_TOKEN_C_HAS_XATTR;

{ID}		{
			char * p = new char[strlen(yytext)+1];
			strcpy(p, yytext);
			yyextra->data.str = p;
			yyextra->type = PrimitiveValue::string;
			return PCFS_TOKEN_C_ID;
		}

{MAC}		{
			char * p = new char[strlen(yytext) + 1];
			char * c = yytext;
			c++; // Ignore leading #
			while (*c == ' ' || *c == '\t' || *c == '\n' || *c == '\r') 
			  c++; // Ignore whitespace
			strcpy(p, c); 
			yyextra->data.str = p;
			yyextra->type = PrimitiveValue::string;
			return PCFS_TOKEN_C_MAC;
		}

"/*"			BEGIN(COMMENT1);
<COMMENT1><<EOF>>	return 0;
<COMMENT1>"\0"		return 0;
<COMMENT1>.		;
<COMMENT1>\n		;
<COMMENT1>"*/"		BEGIN(0);

"%{"			BEGIN(COMMENT2);
<COMMENT2><<EOF>>	BEGIN(0);return 0;
<COMMENT2>"\0"		return 0;
<COMMENT2>.		;
<COMMENT2>\n		;
<COMMENT2>"%}"		BEGIN(0);

"//"			BEGIN(COMMENT3);
<COMMENT3><<EOF>>	return 0;
<COMMENT3>"\0"		return 0;
<COMMENT3>.*		;
<COMMENT3>\n		BEGIN(0);

"%"			BEGIN(COMMENT4);
<COMMENT4><<EOF>>	return 0;
<COMMENT4>"\0"		return 0;
<COMMENT4>.*		;
<COMMENT4>\n		BEGIN(0);

"(*"			BEGIN(COMMENT5);
<COMMENT5><<EOF>>	return 0;
<COMMENT5>"\0"		return 0;
<COMMENT5>.		;
<COMMENT5>\n		;
<COMMENT5>"*)"		BEGIN(0);

.		return 0;
<<EOF>>		return 0;

%%

