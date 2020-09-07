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

#ifndef __PCFS_SYN_H
#define __PCFS_SYN_H

#include <time.h>
#include "pcfs-declarations.hpp"
#include "pcfs-time.hpp"
#include "../crypto-common.hpp"


/* Data structures for representing PCFS syntax, as relevant to ProCaps.
   Included are: terms, constraints, state formulas.

   See pcfs-declarations.hpp for more details.
*/

//----------------------------------------------------------------------

// Pairs of ids and sorts. Used to represent local contexts.

struct IdSortPair
{
  char * id; 
  Sort * sort;

  IdSortPair(char * i, Sort * s) 
  {
    id = i;
    sort = s;
  }

  ~IdSortPair()
  {
    delete sort;
    delete [] id;
  }

  ostream & print (ostream & out) {
    out << "[" << id << ":term ";
    sort -> print(out);
    out << "]";
    return out;
  }

  charstream & print (charstream & out) {
    out << "[" << id << ":term ";
    sort -> print(out);
    out << "]";
    return out;
  }

  // Looks up a local context for "id" and returns the sort obtained,
  // if it exists, or NULL otherwise. The sort returned is held in
  // another data structure and must not be freed by the caller. If
  // there are multiple occurences of id in the context, the first one
  // is matched. The local context may be NULL, in which case it is
  // assumed to be empty, and NULL is returned.
  
  static Sort * lookupLocal ( char * id, Queue<IdSortPair *> * local )
  {
    if (local == NULL) return NULL;

    QueueNode <IdSortPair *> * qn = local -> getHead() -> next;
    while (qn != NULL) {
      if (!strcmp (qn -> d -> id, id)) return qn -> d -> sort;
      qn = qn -> next;
    }
    return NULL;
  }


};


//--------------------------------------------------------------------------

enum TermConstructor
  {
    constr_loca,
    constr_ctime,
    constr_pinfty,
    constr_ninfty,
    constr_prim_str2file,
    constr_prim_date2time,
    constr_prim_int2principal,
    constr_prim_int2time,
    constr_read,
    constr_write,
    constr_execute,
    constr_identity,
    constr_govern,
    constr_time2exp,
    constr_exp_add,
    constr_exp_subtract,
    constr_exp_max,
    constr_exp_min,

    // These three constructors have no concrete representation
    constr_prim_str,         // primitive string
    constr_prim_int,         // primitive integer
    constr_prim_date,        // primitive date (yyyy:mm:dd or yyyy:mm:dd:hh:mm:ss)

    constr_other             // Some other constructor, including parameters
  };


//----------------------------------------------------------------------------------


/* Data Structures for representing PCFS first-order terms. Terms are
   made from constructors. Constructors are of two types.

   1. Pre-defined. 

   2. User-defined. These are represented as strings (constructors)
   applied to terms recursively.
   
   Pre-defined term constructors:

   loca: term sort.                  %% local authority
   ctime: term time.                 %% current time
   ninfty: term time.                %% time - infinity
   pinfty: term time.                %% time + infinity
   prim_str2file: prim_string -> term file.
   prim_date2time: prim_date -> term time. 
   prim_int2principal: prim_int -> term principal.  %% convert a uid to a principal.
   prim_int2time: prim_int -> term time.
   read: term perm.
   write: term perm.
   execute: term perm.
   identity: term perm.
   govern: term perm.

   time2exp: term time -> term exp.
   exp_add: term exp -> term exp -> term exp.
   exp_subtract: term exp -> term exp -> term exp.
   exp_max: term exp -> term exp -> term exp.
   exp_min: term exp -> term exp -> term exp.

   The following constructors have no concrete representation, but are
   present in the abstract syntax. They are automatically added by the
   parser. They inject values of primitive types to terms.

   prim_str: string -> prim_str.
   prim_int: int -> prim_str.
   prim_date: date -> prim_str.

   string constants are enclosed between " ". Every character except "
   and \n is valid. Note that it is not possible to escape characters
   using \ because the lexer does not understand this. 

   int constants are defined by the regular expression -?[0-9]+. 

   date constants must have one of the forms yyyy:mm:dd or
   yyyy:mm:dd:h:m:s. There is no check on the values in any of the
   fields, except that they must be only digits. It is perfectly okay
   to have fields shorter than the required length, but fields cannot
   be omitted altogether.
   
*/

class Term
{
protected:
  Term (TermConstructor c) { 
    constr = c; 
  }

public:
  TermConstructor constr;

  virtual ~Term() { }

  // Print only the constructor name
  virtual ostream & printConstructor(ostream & out) = 0;
  virtual charstream & printConstructor(charstream & out) = 0;

  // print the whole term ... overload as needed
  virtual ostream & print(ostream & out) {
    return printConstructor(out);
  }

  virtual charstream & print(charstream & out) {
    return printConstructor(out);
  }

  // Checks that the term has a given sort in the given signature (di)
  // and a given local context (local). di cannot be NULL, but local
  // may be.
  virtual bool check(Sort * sort, DeclarationIndex * di, Queue<IdSortPair *> * local) = 0;



  // Check that 'this' is equal to t syntactically. Override this
  // method for every non-nullary term.
  virtual bool equal (Term * t) 
  {
    return (constr == t -> constr);
  }



  // Check that two lists of terms are identical
  static bool equalList (Queue<Term *> * tl1, Queue<Term *> * tl2)
  {
    if (tl1 -> size() != tl2 -> size()) return false;

    QueueNode <Term *> * qn1 = tl1 -> getHead() -> next;
    QueueNode <Term *> * qn2 = tl2 -> getHead() -> next;

    while (qn1 != NULL) { // qn2 != NULL is implicit because the two lists have same length
      
      bool b = qn1 -> d -> equal (qn2 -> d);

      if (!b) return false;

      qn1 = qn1 -> next;
      qn2 = qn2 -> next;
    }

    return true;
  }



  // Checks a list of terms against a list of sorts
  static bool checkList (Queue <Term *> * tl, Queue<Sort *> * sl, 
			 DeclarationIndex * di, Queue <IdSortPair*> * local)
  {
    if (tl -> size() != sl -> size()) {
      return false;
    }

    QueueNode <Sort *> * args_sorts = sl -> getHead() -> next;
    QueueNode <Term *> * args_terms = tl -> getHead() -> next;

    while (args_sorts != NULL) {

      bool b = args_terms -> d -> check(args_sorts -> d, di, local);

      if (!b) {
	return false;
      }

      args_terms = args_terms -> next;
      args_sorts = args_sorts -> next;
    }
    
    return true;
  }

};


class TermUnary: public Term
{
protected:
  TermUnary(TermConstructor c, Term * a1): Term(c) {
    arg1 = a1;
  }

public:
  Term * arg1;

  virtual ~TermUnary() {
    delete arg1;
  }

  virtual ostream & print(ostream & out) {
    out << "(";
    printConstructor(out);
    out << " ";
    arg1 -> print(out);
    out << ")";
    return out;
  }

  virtual charstream & print(charstream & out) {
    out << "(";
    printConstructor(out);
    out << " ";
    arg1 -> print(out);
    out << ")";
    return out;
  }

  virtual bool equal(Term * t) 
  {
    return ((constr == t -> constr) &&
	    (arg1 -> equal(((TermUnary *) t) -> arg1)));
  }

};

class TermBinary: public Term
{
protected:
  TermBinary(TermConstructor c, Term * a1, Term * a2): Term(c) {
    arg1 = a1;
    arg2 = a2;
  }

public:
  Term * arg1, *arg2;

  virtual ~TermBinary() {
    delete arg1;
    delete arg2;
  }

  virtual ostream & print(ostream & out) {
    out << "(";
    printConstructor(out);
    out << " ";
    arg1 -> print(out);
    out << " ";
    arg2 -> print(out);
    out << ")";
    return out;
  }

  virtual charstream & print(charstream & out) {
    out << "(";
    printConstructor(out);
    out << " ";
    arg1 -> print(out);
    out << " ";
    arg2 -> print(out);
    out << ")";
    return out;
  }

  virtual bool equal(Term * t) 
  {
    return ((constr == t -> constr) &&
	    (arg1 -> equal(((TermBinary *) t) -> arg1)) && 
	    (arg2 -> equal(((TermBinary *) t) -> arg2)));
  }

};

class TermNary: public Term
{
protected:
  TermNary(TermConstructor c, Queue <Term *> * as): Term (c) {
    args = as;
  }

public:
  Queue<Term *> * args;

  virtual ~TermNary() {
    freeQueueWithData(args);
  }

  virtual ostream & print(ostream & out) {
    if (args -> size() == 0) 
      printConstructor(out);
    else {
      out << "(";
      printConstructor(out);
      printQueue(out, args, " ", "");
      out << ")";
    }
    return out;
  }

  virtual charstream & print(charstream & out) {
    if (args -> size() == 0) 
      printConstructor(out);
    else {
      out << "(";
      printConstructor(out);
      printQueue(out, args, " ", "");
      out << ")";
    }
    return out;
  }

};

//-------------------------------------------

class TermLoca: public Term 
{
public:
  TermLoca() : Term(constr_loca) { }

  ostream & printConstructor(ostream & out) {
    out << "loca";
    return out;
  }

  charstream & printConstructor(charstream & out) {
    out << "loca";
    return out;
  }

  bool check(Sort * sort, DeclarationIndex * di, Queue<IdSortPair *> * local) {
    if (sort -> sort == sortname_any || 
	sort -> sort == sortname_principal)
      return true;
    else return false;
  }
};


//-------------------------------------------

class TermCtime: public Term 
{
public:
  TermCtime() : Term(constr_ctime) { }

  ostream & printConstructor(ostream & out) {
    out << "ctime";
    return out;
  }

  charstream & printConstructor(charstream & out) {
    out << "ctime";
    return out;
  }

  bool check(Sort * sort, DeclarationIndex * di, Queue<IdSortPair *> * local) {
    if (sort -> sort == sortname_any || 
	sort -> sort == sortname_time)
      return true;
    else return false;
  }
};


//-------------------------------------------

class TermNinfty: public Term 
{
public:
  TermNinfty() : Term(constr_ninfty) { }

  ostream & printConstructor(ostream & out) {
    out << "ninfty";
    return out;
  }

  charstream & printConstructor(charstream & out) {
    out << "ninfty";
    return out;
  }

  bool check(Sort * sort, DeclarationIndex * di, Queue<IdSortPair *> * local) {
    if (sort -> sort == sortname_any || 
	sort -> sort == sortname_time)
      return true;
    else return false;
  }
};


//-------------------------------------------

class TermPinfty: public Term 
{
public:
  TermPinfty() : Term(constr_pinfty) { }

  ostream & printConstructor(ostream & out) {
    out << "pinfty";
    return out;
  }

  charstream & printConstructor(charstream & out) {
    out << "pinfty";
    return out;
  }

  bool check(Sort * sort, DeclarationIndex * di, Queue<IdSortPair *> * local) {
    if (sort -> sort == sortname_any || 
	sort -> sort == sortname_time)
      return true;
    else return false;
  }
};


//-------------------------------------------------

class TermPrim_str2file : public TermUnary
{
public:
  TermPrim_str2file(Term * arg): TermUnary(constr_prim_str2file, arg) 
  { }

  ostream & printConstructor(ostream & out) {
    out << "prim_str2file";
    return out;
  }

  charstream & printConstructor(charstream & out) {
    out << "prim_str2file";
    return out;
  }

  bool check(Sort * sort, DeclarationIndex * di, Queue<IdSortPair *> * local) {
    if ((arg1 -> check(&sort_global_str, di, local)) &&
	((sort -> equal (&sort_global_any)) || 
	 (sort -> equal (&sort_global_file))))
      return true;
    else return false;
  }

};

//-------------------------------------------

class TermPrim_date2time : public TermUnary
{
public:
  TermPrim_date2time(Term * arg): TermUnary(constr_prim_date2time, arg)
  { }

  ostream & printConstructor(ostream & out) {
    out << "prim_date2time";
    return out;
  }

  charstream & printConstructor(charstream & out) {
    out << "prim_date2time";
    return out;
  }

  bool check(Sort * sort, DeclarationIndex * di, Queue<IdSortPair *> * local) {
    if ((arg1 -> check(&sort_global_date, di, local)) &&
	((sort -> equal (&sort_global_any)) || 
	 (sort -> equal (&sort_global_time))))
      return true;
    else return false;
  }

};

//-------------------------------------------

class TermPrim_int2principal: public TermUnary
{
public:
  TermPrim_int2principal (Term * arg): TermUnary(constr_prim_int2principal, arg) 
  { }

  ostream & printConstructor(ostream & out) {
    out << "prim_int2principal";
    return out;
  }

  charstream & printConstructor(charstream & out) {
    out << "prim_int2principal";
    return out;
  }

  bool check(Sort * sort, DeclarationIndex * di, Queue<IdSortPair *> * local) {
    if ((arg1 -> check(&sort_global_int, di, local)) &&
	((sort -> equal (&sort_global_any)) || 
	 (sort -> equal (&sort_global_principal))))
      return true;
    else return false;
  }

};

//-------------------------------------------

class TermPrim_int2time: public TermUnary
{
public:
  TermPrim_int2time (Term * arg): TermUnary(constr_prim_int2time, arg) 
  { }

  ostream & printConstructor(ostream & out) {
    out << "prim_int2time";
    return out;
  }

  charstream & printConstructor(charstream & out) {
    out << "prim_int2time";
    return out;
  }

  bool check(Sort * sort, DeclarationIndex * di, Queue<IdSortPair *> * local) {
    if ((arg1 -> check(&sort_global_int, di, local)) &&
	((sort -> equal (&sort_global_any)) || 
	 (sort -> equal (&sort_global_time))))
      return true;
    else return false;
  }
};

//--------------------------------------------

class TermRead: public Term
{
public:
  TermRead () : Term (constr_read) { }

  ostream & printConstructor(ostream & out) {
    out << "read";
    return out;
  }

  charstream & printConstructor(charstream & out) {
    out << "read";
    return out;
  }

  bool check(Sort * sort, DeclarationIndex * di, Queue<IdSortPair *> * local) {
    if ((sort -> equal (&sort_global_any)) || 
	(sort -> equal (&sort_global_perm)))
      return true;
    else return false;
  }

};

//--------------------------------------------

class TermWrite: public Term
{
public:
  TermWrite () : Term (constr_write) { }

  ostream & printConstructor(ostream & out) {
    out << "write";
    return out;
  }

  charstream & printConstructor(charstream & out) {
    out << "write";
    return out;
  }

  bool check(Sort * sort, DeclarationIndex * di, Queue<IdSortPair *> * local) {
    if ((sort -> equal (&sort_global_any)) || 
	(sort -> equal (&sort_global_perm)))
      return true;
    else return false;
  }

};

//--------------------------------------------

class TermExecute: public Term
{
public:
  TermExecute () : Term (constr_execute) { }

  ostream & printConstructor(ostream & out) {
    out << "execute";
    return out;
  }

  charstream & printConstructor(charstream & out) {
    out << "execute";
    return out;
  }

  bool check(Sort * sort, DeclarationIndex * di, Queue<IdSortPair *> * local) {
    if ((sort -> equal (&sort_global_any)) || 
	(sort -> equal (&sort_global_perm)))
      return true;
    else return false;
  }

};


//--------------------------------------------

class TermIdentity: public Term
{
public:
  TermIdentity () : Term (constr_identity) { }

  ostream & printConstructor(ostream & out) {
    out << "identity";
    return out;
  }

  charstream & printConstructor(charstream & out) {
    out << "identity";
    return out;
  }

  bool check(Sort * sort, DeclarationIndex * di, Queue<IdSortPair *> * local) {
    if ((sort -> equal (&sort_global_any)) || 
	(sort -> equal (&sort_global_perm)))
      return true;
    else return false;
  }

};


//--------------------------------------------

class TermGovern: public Term
{
public:
  TermGovern () : Term (constr_govern) { }

  ostream & printConstructor(ostream & out) {
    out << "govern";
    return out;
  }

  charstream & printConstructor(charstream & out) {
    out << "govern";
    return out;
  }

  bool check(Sort * sort, DeclarationIndex * di, Queue<IdSortPair *> * local) {
    if ((sort -> equal (&sort_global_any)) || 
	(sort -> equal (&sort_global_perm)))
      return true;
    else return false;
  }

};

//--------------------------------------------

class TermTime2exp : public TermUnary
{
public:
  TermTime2exp(Term * arg1) : TermUnary (constr_time2exp,arg1) {}

  ostream & printConstructor(ostream & out) {
    out << "time2exp";
    return out;
  }

  charstream & printConstructor(charstream & out) {
    out << "time2exp";
    return out;
  }

  bool check(Sort * sort, DeclarationIndex * di, Queue<IdSortPair *> * local) {
    if ((arg1 -> check(&sort_global_time, di, local)) &&
	((sort -> equal (&sort_global_any)) || 
	 (sort -> equal (&sort_global_exp))))
      return true;
    else return false;
  }
};

//--------------------------------------------

class TermExp_add : public TermBinary
{
public:
  TermExp_add (Term * arg1, Term * arg2) : TermBinary(constr_exp_add,arg1,arg2) {}

  ostream & printConstructor(ostream & out) {
    out << "exp_add";
    return out;
  }

  charstream & printConstructor(charstream & out) {
    out << "exp_add";
    return out;
  }

  bool check(Sort * sort, DeclarationIndex * di, Queue<IdSortPair *> * local) {
    if ((arg1 -> check(&sort_global_exp, di, local)) &&
	(arg2 -> check(&sort_global_exp, di, local)) &&
	((sort -> equal (&sort_global_any)) || 
	 (sort -> equal (&sort_global_exp))))
      return true;
    else return false;
  }

};

//---------------------------------------------

class TermExp_subtract : public TermBinary
{
public:
  TermExp_subtract (Term * arg1, Term * arg2): TermBinary (constr_exp_subtract,arg1,arg2) {}

  ostream & printConstructor(ostream & out) {
    out << "exp_subtract";
    return out;
  }

  charstream & printConstructor(charstream & out) {
    out << "exp_subtract";
    return out;
  }

  bool check(Sort * sort, DeclarationIndex * di, Queue<IdSortPair *> * local) {
    if ((arg1 -> check(&sort_global_exp, di, local)) &&
	(arg2 -> check(&sort_global_exp, di, local)) &&
	((sort -> equal (&sort_global_any)) || 
	 (sort -> equal (&sort_global_exp))))
      return true;
    else return false;
  }
};


//----------------------------------------------

class TermExp_max : public TermBinary
{
public:
  TermExp_max(Term * arg1, Term * arg2): TermBinary (constr_exp_max,arg1,arg2) {}

  ostream & printConstructor(ostream & out) {
    out << "exp_max";
    return out;
  }

  charstream & printConstructor(charstream & out) {
    out << "exp_max";
    return out;
  }

  bool check(Sort * sort, DeclarationIndex * di, Queue<IdSortPair *> * local) {
    if ((arg1 -> check(&sort_global_exp, di, local)) &&
	(arg2 -> check(&sort_global_exp, di, local)) &&
	((sort -> equal (&sort_global_any)) || 
	 (sort -> equal (&sort_global_exp))))
      return true;
    else return false;
  }
};

//-----------------------------------------------

class TermExp_min : public TermBinary
{
public:
  TermExp_min(Term * arg1, Term * arg2): TermBinary (constr_exp_min,arg1,arg2) { }

  ostream & printConstructor(ostream & out) {
    out << "exp_min";
    return out;
  }

  charstream & printConstructor(charstream & out) {
    out << "exp_min";
    return out;
  }

  bool check(Sort * sort, DeclarationIndex * di, Queue<IdSortPair *> * local) {
    if ((arg1 -> check(&sort_global_exp, di, local)) &&
	(arg2 -> check(&sort_global_exp, di, local)) &&
	((sort -> equal (&sort_global_any)) || 
	 (sort -> equal (&sort_global_exp))))
      return true;
    else return false;
  }
};


//-----------------------------------------------

class TermPrim_int : public Term
{
public:
  TermPrim_int(int v): Term(constr_prim_int) 
  { 
    i = v;
  }

  int i;

  // This function should never get called but is needed
  ostream & printConstructor(ostream & out) {
    out << "#prim_int#";
    return out;
  }

  charstream & printConstructor(charstream & out) {
    out << "#prim_int#";
    return out;
  }

  ostream & print(ostream & out) {
    out << i;
    return out;
  }

  charstream & print(charstream & out) {
    out << i;
    return out;
  }

  bool check(Sort * sort, DeclarationIndex * di, Queue<IdSortPair *> * local) {
    if ((sort -> equal (&sort_global_any)) || 
	(sort -> equal (&sort_global_int)))
      return true;
    else return false;
  }


  bool equal(Term * t) {
    if (t -> constr != constr_prim_int) 
      return false;
    
    TermPrim_int * ti = (TermPrim_int *) t;
    
    return (i == ti -> i);
  }
};

//-----------------------------------------------

class TermPrim_str : public Term
{
public:
  TermPrim_str(char * s): Term(constr_prim_str) 
  { 
    str = s;
  }

  char * str;

  ~TermPrim_str () {
    delete [] str;
  }

  // This function should never be called but is needed
  ostream & printConstructor(ostream & out) {
    out << "#prim_str#";
    return out;
  }

  charstream & printConstructor(charstream & out) {
    out << "#prim_str#";
    return out;
  }

  ostream & print(ostream & out) {
    out << "\"" << str << "\"";
    return out;
  }

  charstream & print(charstream & out) {
    out << "\"" << str << "\"";
    return out;
  }

  bool check(Sort * sort, DeclarationIndex * di, Queue<IdSortPair *> * local) {
    if ((sort -> equal (&sort_global_any)) || 
	(sort -> equal (&sort_global_str)))
      return true;
    else return false;
  }

  bool equal (Term * t) {
    if (t -> constr != constr_prim_str)
      return false;
    
    TermPrim_str * ts = (TermPrim_str *) t;

    return (!strcmp (str, ts -> str));
  } 

};

//-----------------------------------------------

class TermPrim_date : public Term
{
public:
  TermPrim_date(NativeTime * dd): Term(constr_prim_date) 
  { 
    d = dd;
  }

  NativeTime * d;

  ~TermPrim_date () {
    delete d;
  }

  // This function should never be called but is needed 
  ostream & printConstructor(ostream & out) {
    out << "#prim_date#";
    return out;
  }

  charstream & printConstructor(charstream & out) {
    out << "#prim_date#";
    return out;
  }

  ostream & print(ostream & out) {
    d -> print(out);
    return out;
  }

  charstream & print(charstream & out) {
    d -> print(out);
    return out;
  }

  bool check(Sort * sort, DeclarationIndex * di, Queue<IdSortPair *> * local) {
    if ((sort -> equal (&sort_global_any)) || 
	(sort -> equal (&sort_global_date)))
      return true;
    else return false;
  }


  bool equal(Term * t) {
    if (t -> constr != constr_prim_date) 
      return false;

    TermPrim_date * td = (TermPrim_date *) t;

    return (d -> getSinceEpoch() == td -> d -> getSinceEpoch());
  }

};


//--------------------------------------------------

class TermOther: public TermNary
{
public:
  TermOther(char * c_name, Queue<Term * > * as) : TermNary(constr_other, as) {
    name = c_name;
  }

  char * name;

  ~TermOther() {
    delete [] name;
  }

  ostream & printConstructor(ostream & out) {
    out << name ;
    return out;
  }

  charstream & printConstructor(charstream & out) {
    out << name ;
    return out;
  }

  bool check(Sort * sort, DeclarationIndex * di, Queue<IdSortPair *> * local)
  {
    Sort * ls = IdSortPair :: lookupLocal(name, local);

    if (ls != NULL) {
      // We found it in the local list!  So args must be empty and
      // expected sort must be 'any' or ls
      return ((args -> size() == 0) && 
	      ((sort -> equal(&sort_global_any)) ||
	       (sort -> equal(ls))));
    }

    // We did not find the constructor in the local list, so lookup the signature

    Declaration *d = di -> find(name);
    if (d == NULL || d -> getType() != decl_term) return false;

    DeclarationTerm * dt = (DeclarationTerm *) d;

    if (checkList (args, dt -> sortsOfArgs, di, local) == false) return false;

    Sort * term_sort = dt -> sortOfResult;

    if (sort -> equal (&sort_global_any) ||
	sort -> equal (term_sort))
      return true;

    return false;
  }


  virtual bool equal(Term * t) 
  {
    if (t -> constr != constr_other) return false;

    TermOther * t1 = (TermOther *) t;

    if (strcmp (name, t1 -> name)) return false;

    return (equalList(args, t1 -> args));
  }

};


//----------------------------------------------------------------------------------

/* Data structures for representing constraints. Constraints are made
   of constraint constructors applied to term lists. Constraint
   constructors are of two types.

   1. Pre-defined:
   stronger: term principal -> term principal -> constraint.
   leq: term time -> term time -> constraint.
   
   2. User defined (represented as a string).
 
*/

enum ConstraintConstructor {
    constraint_stronger,
    constraint_leq,
    
    constraint_other
  };

class Constraint
{
protected:
  Constraint (ConstraintConstructor c) {
    constr = c;
  }
  
public:
  ConstraintConstructor constr;

  virtual ~Constraint () { }

  // Print the constructor
  virtual ostream & printConstructor(ostream & out) = 0;
  virtual charstream & printConstructor(charstream & out) = 0;

  // This will usually be overloaded
  virtual ostream & print (ostream & out) {
    return printConstructor(out);
  }

  virtual charstream & print (charstream & out) {
    return printConstructor(out);
  }

  // Check for well-formedness in signature di
  virtual bool check(DeclarationIndex * di, Queue<IdSortPair *> * local) = 0;

  // Check that two constraints are syntactically equal
  virtual bool equal(Constraint * c) = 0;

};

//-------------------------------------------

class ConstraintBinary : public Constraint
{

protected:
  ConstraintBinary(ConstraintConstructor c, Term * a1, Term * a2): Constraint(c)
  {
    arg1 = a1;
    arg2 = a2;
  }

public:
  Term * arg1, * arg2;

  virtual ~ConstraintBinary() 
  {
    delete arg1;
    delete arg2;
  }

  ostream & print(ostream & out) {
    out << "(";
    printConstructor(out);
    out << " ";
    arg1 -> print(out);
    out << " ";
    arg2 -> print(out);
    out << ")";
    return out;
  }


  charstream & print(charstream & out) {
    out << "(";
    printConstructor(out);
    out << " ";
    arg1 -> print(out);
    out << " ";
    arg2 -> print(out);
    out << ")";
    return out;
  }

};


class ConstraintNary : public Constraint
{
protected:
  ConstraintNary(ConstraintConstructor c, Queue<Term *> * as) : Constraint(c)
  {
    args = as;
  }

public:
  Queue <Term *> * args;

  virtual ~ConstraintNary()
  {
    freeQueueWithData(args);
  }

  ostream & print(ostream & out) {
    if (args -> size() == 0) 
      printConstructor(out);
    else {
      out << "(";
      printConstructor(out);
      printQueue(out, args, " ", "");
      out << ")";
    }
    return out;
  }

  charstream & print(charstream & out) {
    if (args -> size() == 0) 
      printConstructor(out);
    else {
      out << "(";
      printConstructor(out);
      printQueue(out, args, " ", "");
      out << ")";
    }
    return out;
  }

};

//----------------------------------------------

class ConstraintStronger : public ConstraintBinary
{
public:
  
  ConstraintStronger(Term * a1, Term * a2)
    : ConstraintBinary(constraint_stronger, a1, a2)
  { }

  ostream & printConstructor(ostream & out) {
    out << "stronger";
    return out;
  }

  charstream & printConstructor(charstream & out) {
    out << "stronger";
    return out;
  }

  bool check(DeclarationIndex * di, Queue<IdSortPair *> * local) 
  {
    if (arg1 -> check(&sort_global_principal, di, local) &&
	arg2 -> check(&sort_global_principal, di, local))
      return true;
    else return false;
  }

  bool equal (Constraint * c) {
    if (c -> constr != constraint_stronger) 
      return false;

    ConstraintStronger * cs = (ConstraintStronger *) c;

    return (arg1 -> equal (cs -> arg1) &&
	    arg2 -> equal (cs -> arg2));
  }

};

//----------------------------------------------

class ConstraintLeq : public ConstraintBinary
{
public:
  
  ConstraintLeq(Term * a1, Term * a2)
    : ConstraintBinary(constraint_leq, a1, a2)
  { }


  ostream & printConstructor(ostream & out) {
    out << "leq";
    return out;
  }

  charstream & printConstructor(charstream & out) {
    out << "leq";
    return out;
  }

  bool check(DeclarationIndex * di, Queue<IdSortPair *> * local) 
  {
    if (arg1 -> check(&sort_global_time, di, local) &&
	arg2 -> check(&sort_global_time, di, local))
      return true;
    else return false;
  }

  bool equal (Constraint * c) {
    if (c -> constr != constraint_leq) 
      return false;

    ConstraintLeq * cl = (ConstraintLeq *) c;

    return (arg1 -> equal (cl -> arg1) &&
	    arg2 -> equal (cl -> arg2));
  }

};

//-----------------------------------------------

class ConstraintOther: public ConstraintNary
{
public:
  char * name; // Name of constraint constructor

  ConstraintOther(char * c_name, Queue<Term *> * as)
    : ConstraintNary(constraint_other, as)
  {
    name = c_name;
  }
  
  ~ConstraintOther()
  {
    delete [] name;
  }

  ostream & printConstructor(ostream & out) {
    out << name;
    return out;
  }

  charstream & printConstructor(charstream & out) {
    out << name;
    return out;
  }

  bool check(DeclarationIndex * di, Queue<IdSortPair *> * local) 
  {
    Declaration * d = di -> find(name);
    if (d == NULL || d -> getType() != decl_constraint) return false;

    DeclarationConstraint * dc = (DeclarationConstraint *) d;

    return (Term :: checkList (args, dc -> sortsOfArgs, di, local));
  }

  bool equal (Constraint * c) {
    if (c -> constr != constraint_other) 
      return false;

    ConstraintOther * co = (ConstraintOther *) c;

    if (strcmp (name, co -> name)) return false;

    return Term :: equalList (args, co -> args);
  }

};

//----------------------------------------------------------------------------------

/* Data structures for representing state formulas. State formulas are
   made of state constructors applied to term lists. State
   constructors are of two types.

   1. Pre-defined:

   owner: term file -> term principal -> state.

   has_xattr: term file -> term prim_str -> term any -> state.
   %% has_xattr F A V -- file F has the extended attribute A = V
   
   2. User defined (represented as a string).

*/

enum StateConstructor {
  state_owner,
  state_has_xattr,
  state_other
};

class State
{
protected:
  State (StateConstructor c) {constr = c;}

public:
  StateConstructor constr;

  virtual ~State() { }

  virtual ostream & printConstructor(ostream & out) = 0;
  virtual charstream & printConstructor(charstream & out) = 0;

  virtual ostream & print(ostream & out) {
    return printConstructor(out);
  }

  virtual charstream & print(charstream & out) {
    return printConstructor(out);
  }

  // Check for well-formedness in signature di and local context di
  virtual bool check (DeclarationIndex * di, Queue<IdSortPair *> * local) = 0;

};

//-------------------------------------------

class StateBinary : public State
{
protected:
  StateBinary(StateConstructor c, Term * a1, Term * a2)
    : State(c) 
  {
    arg1 = a1;
    arg2 = a2;
  }
public:
  Term * arg1;
  Term * arg2;

  ~StateBinary() {
    delete arg1;
    delete arg2;
  }

  ostream & print(ostream & out) {
    out << "(";
    printConstructor(out);
    out << " ";
    arg1 -> print(out);
    out << " ";
    arg2 -> print(out);
    out << ")";
    return out;
  }

  charstream & print(charstream & out) {
    out << "(";
    printConstructor(out);
    out << " ";
    arg1 -> print(out);
    out << " ";
    arg2 -> print(out);
    out << ")";
    return out;
  }

};

class StateTernary : public State
{
protected:
  StateTernary(StateConstructor c, Term * a1, Term * a2,  Term * a3)
    : State(c) 
  {
    arg1 = a1;
    arg2 = a2;
    arg3 = a3;
  }
public:
  Term * arg1;
  Term * arg2;
  Term * arg3;

  ~StateTernary() {
    delete arg1;
    delete arg2;
    delete arg3;
  }

  ostream & print(ostream & out) {
    out << "(";
    printConstructor(out);
    out << " ";
    arg1 -> print(out);
    out << " ";
    arg2 -> print(out);
    out << " ";
    arg3 -> print(out);
    out << ")";
    return out;
  }

  charstream & print(charstream & out) {
    out << "(";
    printConstructor(out);
    out << " ";
    arg1 -> print(out);
    out << " ";
    arg2 -> print(out);
    out << " ";
    arg3 -> print(out);
    out << ")";
    return out;
  }

};

class StateNary : public State
{
protected:
  StateNary(StateConstructor c, Queue<Term *> * as)
    : State(c)
  {
    args = as;
  }

public:
  Queue<Term *> * args;

  virtual ~StateNary()
  {
    freeQueueWithData(args);
  }

  ostream & print(ostream & out) {
    if (args -> size() == 0)
      printConstructor(out);
    else {
      out << "(";
      printConstructor(out);
      printQueue(out, args, " ", "");
      out << ")";
    }
    return out;
  }

  charstream & print(charstream & out) {
    if (args -> size() == 0)
      printConstructor(out);
    else {
      out << "(";
      printConstructor(out);
      printQueue(out, args, " ", "");
      out << ")";
    }
    return out;
  }

};

//--------------------------------------------

class StateOwner : public StateBinary
{
public:
  StateOwner(Term * file, Term * principal)
    : StateBinary(state_owner, file, principal)
  { }

  ostream & printConstructor(ostream & out) {
    out << "owner";
    return out;
  }

  charstream & printConstructor(charstream & out) {
    out << "owner";
    return out;
  }

  bool check(DeclarationIndex * di, Queue<IdSortPair *> * local) 
  {
    if (arg1 -> check(&sort_global_file, di, local) &&
	arg2 -> check(&sort_global_principal, di, local))
      return true;
    else return false;
  }

};

class StateHas_xattr : public StateTernary
{
public:
  StateHas_xattr(Term * a1, Term * a2, Term * a3)
    : StateTernary(state_has_xattr, a1, a2, a3)
  { }

  ostream & printConstructor(ostream & out) {
    out << "has_xattr";
    return out;
  }
  
  charstream & printConstructor(charstream & out) {
    out << "has_xattr";
    return out;
  }
  
  bool check(DeclarationIndex * di, Queue<IdSortPair *> * local)
  {
    if (arg1 -> check(&sort_global_file, di, local) &&
	arg2 -> check(&sort_global_str, di, local) &&
	arg3 -> check(&sort_global_any, di, local))
      return true;
    else return false;
  }
};

class StateOther : public StateNary
{
public:
  StateOther(char * p, Queue<Term * > * as) 
    : StateNary (state_other, as)
  { 
    pred = p;
  }

  char *pred;

  ~StateOther() {
    delete [] pred;
  }

  ostream & printConstructor(ostream & out) {
    out << pred;
    return out;
  }

  charstream & printConstructor(charstream & out) {
    out << pred;
    return out;
  }

  bool check(DeclarationIndex * di, Queue<IdSortPair *> * local) 
  {
    Declaration * d = di -> find(pred);

    if (d == NULL || d -> getType() != decl_state)
      return false;

    DeclarationState * ds = (DeclarationState *) d;

    return (Term :: checkList (args, ds -> sortsOfArgs, di, local));
  }
};


//-------------------------------------------------------

class HypConstraint
{
public:
  Constraint * goal;
  Queue <Constraint *> * hyps;

  HypConstraint(Queue<Constraint * > * h, Constraint * g)
  {
    hyps = h;
    goal = g;
  }

  ~HypConstraint() {
    freeQueueWithData(hyps);
    delete goal;
  }

  ostream & print(ostream & out) {
    out << "(hypconstraint_ ";
    
    QueueNode<Constraint *> * qn = hyps -> getHead() -> next;
    while (qn != NULL) {
      out << "(constraint_cons ";
      qn -> d -> print(out);
      out << " ";
      qn = qn -> next;
    }

    out << "constraint_nil";

    int i;
    for (i = 0; i < hyps -> size(); i++) 
      out << ")";
    
    out << " ";

    goal -> print(out);

    out << ")";
    return out;
  }

  charstream & print(charstream & out) {
    out << "(hypconstraint_ ";
    
    QueueNode<Constraint *> * qn = hyps -> getHead() -> next;
    while (qn != NULL) {
      out << "(constraint_cons ";
      qn -> d -> print(out);
      out << " ";
      qn = qn -> next;
    }

    out << "constraint_nil";

    int i;
    for (i = 0; i < hyps -> size(); i++) 
      out << ")";
    
    out << " ";

    goal -> print(out);

    out << ")";
    return out;
  }

  virtual bool check(DeclarationIndex * di, Queue<IdSortPair *> * local) 
  {
    if (! (goal -> check(di, local))) return false;

    QueueNode <Constraint *> * qn = hyps -> getHead() -> next;
    while (qn != NULL) {
      if (! (qn -> d -> check(di, local))) return false;
      qn = qn -> next;
    }
    
    return true;
  }

};

//---------------------------------------------------------

class Qhcl
{
public:
  Queue <IdSortPair *> * boundVars;
  // Variables bound in the qhcl. Bound variables are stored inside
  // out: the variables bound deepest (closest to the body) are
  // towards the head of the queue.

  Queue <HypConstraint *> * hcs;

  Qhcl(Queue<IdSortPair *> * bvs, Queue<HypConstraint *> * h)
  {
    boundVars = bvs;
    hcs = h;
  }

  ~Qhcl()
  {
    freeQueueWithData(boundVars);
    freeQueueWithData(hcs);
  }

  ostream & print(ostream & out)
  {
    out << "(";
    QueueNode <IdSortPair *> * qnb = boundVars -> getHead() -> next;

    while (qnb != NULL) {
      out << "qhcl_all (";
      qnb -> d -> print(out);
      out << " ";
      qnb = qnb -> next;
    }
    
    out << "qhcl_base ";

    QueueNode <HypConstraint * > * qnh = hcs -> getHead() -> next;

    while (qnh != NULL) {
      out << "(hypconstraint_cons ";
      qnh -> d -> print(out);
      out << " ";
      qnh = qnh -> next;
    }

    out << "hypconstraint_nil";
    
    int i;
    for (i = 0; i < boundVars -> size(); i++)
      out << ")";
    for (i = 0; i < hcs -> size(); i++)
      out << ")";
    
    out << ")";
    return out;
  }

  charstream & print(charstream & out)
  {
    out << "(";
    QueueNode <IdSortPair *> * qnb = boundVars -> getHead() -> next;

    while (qnb != NULL) {
      out << "qhcl_all (";
      qnb -> d -> print(out);
      out << " ";
      qnb = qnb -> next;
    }
    
    out << "qhcl_base ";

    QueueNode <HypConstraint * > * qnh = hcs -> getHead() -> next;

    while (qnh != NULL) {
      out << "(hypconstraint_cons ";
      qnh -> d -> print(out);
      out << " ";
      qnh = qnh -> next;
    }

    out << "hypconstraint_nil";
    
    int i;
    for (i = 0; i < boundVars -> size(); i++)
      out << ")";
    for (i = 0; i < hcs -> size(); i++)
      out << ")";
    
    out << ")";
    return out;
  }

  virtual bool check(DeclarationIndex * id) 
  {
    QueueNode <HypConstraint *> * qn = hcs -> getHead() -> next;
    while (qn != NULL) {
      HypConstraint * hyp = qn -> d;
      if (! hyp -> check(id, boundVars)) return false;
      
      qn = qn -> next;
    }
    return true;
  }

};


//---------------------------------------------------------

class ProCap
{

protected:

public:
  Term * k;  // Principal allowed
  Term * f;       // File allowed
  Term * p;       // Permission allowed
  Qhcl * q;          // Constraints
  Queue <State *> * s; // State constraints;
  char * mac;

  ProCap (Term * prin, Term * file, Term * perm, Qhcl * qhcl, Queue <State *> * st, char * m) 
  {
    k = prin;
    f = file;
    p = perm;
    q = qhcl;
    s = st;
    mac = m;
  }

  ~ProCap()
  {
    delete k;
    delete f;
    delete p;
    delete q;
    freeQueueWithData(s);
    delete [] mac;
  }

  // Print the procap. If suppressMAC is true, the MAC and its
  // preceding # character are not printed.

  ostream & printSpecial (ostream & out, bool suppressMAC) {
    out << "procap\n";
    (k -> print(out)) << "   %% Principal authorized\n";
    (f -> print(out)) << "   %% File authorized\n";
    (p -> print(out)) << "   %% Permission authorized\n";

    out << "\n%% Constraints\n";
    q -> print(out);
    
    out << "\n";

    out << "\n%% State predicates\n";
    
    QueueNode <State *> * ns = s -> getHead() -> next;

    while (ns != NULL) {
      out << "(state_cons ";
      ns -> d -> print(out);
      out << " ";
      ns = ns -> next;
      
    }

    out << "state_nil";
    
    int i;
    for (i = 0; i < s -> size() ; i++) {
      out << ")";
    }
    
    out << ".\n";
    
    if (!suppressMAC) {
      out << "\n%% MAC\n#\n";
      out << mac << "\n";
    }

    return out;
  }

  charstream & printSpecial (charstream & out, bool suppressMAC) {
    out << "procap\n";
    (k -> print(out)) << "   %% Principal authorized\n";
    (f -> print(out)) << "   %% File authorized\n";
    (p -> print(out)) << "   %% Permission authorized\n";

    out << "\n%% Constraints\n";
    q -> print(out);
    
    out << "\n";

    out << "\n%% State predicates\n";
    
    QueueNode <State *> * ns = s -> getHead() -> next;

    while (ns != NULL) {
      out << "(state_cons ";
      ns -> d -> print(out);
      out << " ";
      ns = ns -> next;
      
    }

    out << "state_nil";
    
    int i;
    for (i = 0; i < s -> size() ; i++) {
      out << ")";
    }
    
    out << ".\n";
    
    if (!suppressMAC) {
      out << "\n%% MAC\n#\n";
      out << mac << "\n";
    }

    return out;
  }

  ostream & print (ostream & out) {
    return printSpecial (out, false);
  }

  charstream & print (charstream & out) {
    return printSpecial (out, false);
  }

  bool check(DeclarationIndex * id) 
  {
    if (!k -> check (&sort_global_principal, id, NULL))
      return false;


    if (!f -> check (&sort_global_file, id, NULL))
      return false;

    if (!p -> check (&sort_global_perm, id, NULL))
      return false;

    if (!q -> check (id))
      return false;

    QueueNode <State *> * qn = s -> getHead() -> next;
    while (qn != NULL) {
      if (!qn -> d -> check(id, NULL))
	return false;

      qn = qn -> next;
    }
    
    return true;
    
  }

};

#endif


