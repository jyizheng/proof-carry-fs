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

#include <sys/time.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <iostream>
#include <string>
#include "../paths-common.hpp"
#include "checkProCapConditions.hpp"
#include "xattr-terms.hpp"

using namespace std;

/* This file contains code that checks the hypothetical constraints
   and state conditions on a ProCap. We start with the hypothetical
   constraints. These constraints have the form Q |= C, where Q is a
   list of constraints, and C is a single constraint. Hypothetical
   constraints may contain parameters, but no existential
   variables. Depending on the form of C, there are three classes of
   hypothetical constraints:

   1. C = leq T1 T2.
   2. C = stronger K1 K2.
   3. C = user-defined-constructor ...
   
   There are special procedures for solving (1) and (2) that we
   describe below. Hypothetical constraints of form (3) are checked
   simply by lookup -- Q |= C holds iff C \in Q.

   ------------

   Procedure to check (1) Q |= leq T1 T2

   This procedure is based on treating every constraint of the form
   leq T T' \in Q as an edge from T to T'. First we define a function
   directLeqTime(T1, T2) which returns true iff T1 is directly less
   than T2, according to the following definition.

   directLeqTime(ninfty, _).

   directLeqTime(_, pinfty).

   directLeqTime(prim_date2time D1, prim_date2time D2) iff D1 before
   D2 in the usual order on dates.

   directLeqTime(T, T).

   Note that prim_int2time is not recognized as a primitive
   constructor here since it is meant to represent relative time only
   (for example, when performing arithmetic with time
   expressions). Next, we perform a transitive closure on the time
   points mentioned in Q. This is done by using a Bellman-Ford
   iteration. We construct an intermediate set R of "reachable"
   points, and initialize R to {T1}. Then we perform a fixed point
   iteration using the "edges" Q.

   procedure checkHypConstraintTime(Q, leq T1 T2)

   if (directLeqTime(T1, T2)) return true

   R = {T1}

   while (R has not reached a fixed point) 
     foreach (leq Tl Tu) in Q
       foreach T in R
         if (directLeqTime(T, Tl) and (Tu not in R))
            {
	       if (directLeqTime(Tu, T2)) return true
	       
	       R = R + {Tu}
	    }


   return false

   --------------

   Procedure to check (2) Q |= stronger K1 K2

   This procedure is very similar to the above procedure for Q |= leq
   T1 T2 except that the function directStongerPrincipal is trivial:

   directStrongerPrincipal(Loca,K).
   directStrongerPrincipal(K, K).

   Besides this, the fixed point iteration is the same.

   --------------

*/

// Implementation of the directLeqTime function described
// above. Returns 1 if the relation holds, falsee otherwise. One
// addition we make is the treatment of ctime. Whenever ctime is
// encountered, it is automatically replaced by a new Time object
// which is passed as the third argument. It is expected that this
// third argument actually contains the current time, i.e., it was
// constructed as: new TermPrim_date2time (new TermPrim_date (new
// NativeTime ((long) time(NULL)))) where time() is defined in <time.h>.

static inline bool directLeqTime(Term * t1, Term * t2, TermPrim_date2time * vctime) 
{
  // If t1 == ninfty or t2 == pinfty, return true
  if (t1 -> constr == constr_ninfty) return true;
  if (t2 -> constr == constr_pinfty) return true;

  // If either one of t1 or t2 is ctime, set it to vctime
  if (t1 -> constr == constr_ctime) t1 = vctime;
  if (t2 -> constr == constr_ctime) t2 = vctime;

  
  // If t1 and t2 each have the form prim_date2time (prim_date (...)),
  // then compare the dates
  if ((t1 -> constr == constr_prim_date2time) && (t2 -> constr == constr_prim_date2time)) {
    TermPrim_date2time * t11 = (TermPrim_date2time *) t1;
    TermPrim_date2time * t21 = (TermPrim_date2time *) t2;

    if ((t11 -> arg1 -> constr == constr_prim_date) && (t21 -> arg1 -> constr == constr_prim_date)) {
      TermPrim_date * t12 = (TermPrim_date *) (t11 -> arg1);
      TermPrim_date * t22 = (TermPrim_date *) (t21 -> arg1);

      long time1 = t12 -> d -> getSinceEpoch();
      long time2 = t22 -> d -> getSinceEpoch();

      return (time1 <= time2);
    }
  }
  
  // Else return true iff t1 and t2 are identical.
  return (t1 -> equal(t2));
}

// A function to check that a given term is in a Queue of terms.
static bool termInQueue (Term * t, Queue<Term * > * tq)
{
  QueueNode <Term *> * qn = tq -> getHead() -> next;
  while (qn != NULL) {
    if (t -> equal(qn -> d)) return true;

    qn = qn -> next;
  }

  return false;
}

// Function checkHypConstraintTime() described above. It checks that Q
// |= leq T1 T2. Like directLeqTime, this function also takes an extra
// argument that is used to substitute for ctime. Do not pass in NULL
// for Q, but an empty list may be passed.
static bool checkHypConstraintTime(Queue <Constraint *> * Q, Term * T1, Term * T2, TermPrim_date2time * vctime)
{
  if (directLeqTime(T1, T2, vctime)) return true;

  Queue<Term *> * R = new Queue<Term *>;
  
  R -> push(T1);  // R = {T1}

  int reachedFP = false;  // We have not reached a fixed point yet.

  while (!reachedFP) {

    reachedFP = true;  
    // Assume for next iteration that fixed point has been
    // reached. This assumption may be falsified if something in R
    // changes in this iteration.

    // Iterate over Q.
    QueueNode <Constraint *> * Qnode = Q -> getHead() -> next;
    while (Qnode != NULL) {

      Constraint * c = Qnode -> d;
      
      // Check if current node of Q is of the form leq Tl Tu
      if (c -> constr == constraint_leq) {
	
	//Yes it is! So extract Tl and Tu
	Term * Tl = ((ConstraintLeq *) c) -> arg1;
	Term * Tu = ((ConstraintLeq *) c) -> arg2;

	// Now iterate over R
	bool insertedTu = false;  // This will be set to true as soon as we find a match in R

	QueueNode <Term *> * Rnode = R -> getHead() -> next;

	while (Rnode != NULL && !insertedTu) {
	  
	  Term * T = Rnode -> d;

	  if (directLeqTime(T, Tl, vctime) &&
	      !termInQueue(Tu, R)) {

	    if (directLeqTime(Tu, T2, vctime)) {
	      delete R;
	      return true;
	    }

	    R -> push (Tu); 
	    // Note that although we have an explicit pointer Rnode to
	    // a node in the queue R, changing R by adding a new term
	    // will not cause problems because we will not iterate
	    // further on R now (insertedTu is set to true)

	    reachedFP = false;
	    insertedTu = true;
	    
	  }

	  Rnode = Rnode -> next;

	} // end while (Rnode != NULL)

      } // end if (c -> constr == constraint_leq)

      Qnode = Qnode -> next;
      
    } // end while (Qnode != NULL) 

  } // end while (!reachedFP)


  delete R; // Do not delete the data in R since it is held elsewhere
  return false;
  
}

//------------------------------------------

/* 
   Now we repeat similar code for checking Q |= stronger K1 K2.
*/

static inline bool directStrongerPrincipal(Term * t1, Term * t2)
{
  return (t1 -> constr == constr_loca || t1 -> equal(t2));
} 


// Function checkHypConstraintPrincipal(), similar to
// checkHypConstraintTime described above, but works for
// principals. It checks that Q |= stronger K1 K2. 

static bool checkHypConstraintPrincipal(Queue <Constraint *> * Q, Term * K1, Term * K2)
{
  if (directStrongerPrincipal(K1, K2)) return true;

  Queue<Term *> * R = new Queue<Term *>;
  
  R -> push(K1);  // R = {K1}

  int reachedFP = false;  // We have not reached a fixed point yet.

  while (!reachedFP) {

    reachedFP = true;  
    // Assume for next iteration that fixed point has been
    // reached. This assumption may be falsified if something in R
    // changes in this iteration.

    // Iterate over Q.
    QueueNode <Constraint *> * Qnode = Q -> getHead() -> next;
    while (Qnode != NULL) {

      Constraint * c = Qnode -> d;
      
      // Check if current node of Q is of the form stronger Kl Ku
      if (c -> constr == constraint_stronger) {
	
	//Yes it is! So extract Kl and Ku
	Term * Kl = ((ConstraintStronger *) c) -> arg1;
	Term * Ku = ((ConstraintStronger *) c) -> arg2;

	// Now iterate over R
	bool insertedKu = false;  // This will be set to true as soon as we find a match in R

	QueueNode <Term *> * Rnode = R -> getHead() -> next;

	while (Rnode != NULL && !insertedKu) {
	  
	  Term * K = Rnode -> d;

	  if (directStrongerPrincipal(K, Kl) &&
	      !termInQueue(Ku, R)) {

	    if (directStrongerPrincipal(Ku, K2)) {
	      delete R;
	      return true;
	    }

	    R -> push (Ku); 
	    // Note that although we have an explicit pointer Rnode to
	    // a node in the queue R, changing R by adding a new term
	    // will not cause problems because we will not iterate
	    // further on R now (insertedTu is set to true)

	    reachedFP = false;
	    insertedKu = true;
	    
	  }

	  Rnode = Rnode -> next;

	} // end while (Rnode != NULL)

      } // end if (c -> constr == constraint_stronger)

      Qnode = Qnode -> next;
      
    } // end while (Qnode != NULL) 

  } // end while (!reachedFP)


  delete R; // Do not delete the data in R since it is held elsewhere
  return false;
  
}

//---------------------------------------------------


// Function to check a hypothetical constraint h. Note that the second
// argument is mandatory, but is used only if h has the form Q |= leq
// T1 T2. In other cases, it is okay to pass NULL for the second
// argument.

bool checkHypConstraint(HypConstraint * h, TermPrim_date2time * vctime) 
{
  Queue <Constraint *> * hyps = h -> hyps;
  
  Constraint * c = h -> goal;

  switch (c -> constr) {

  case constraint_leq:
    {
      ConstraintLeq * cl = (ConstraintLeq *) c;
      
      return checkHypConstraintTime (hyps, cl -> arg1, cl -> arg2, vctime);
    }

  case constraint_stronger:
    {
      ConstraintStronger * cs = (ConstraintStronger *) c;
      
      return checkHypConstraintPrincipal (hyps, cs -> arg1, cs -> arg2);
    }

  case constraint_other:
    {
      // In this case return true iff c is in hyps
      QueueNode <Constraint *> * qn = hyps -> getHead() -> next;
      
      while (qn != NULL) {
	if (c -> equal (qn -> d))
	  return true;
	qn = qn -> next;
      }

      return false;
    }
  }

  // Should never be reached
  return false;
}


// Function to check an entire list of hypothetical constraints
bool checkHypConstraintList (Queue<HypConstraint *> * hcl, TermPrim_date2time * vctime) 
{
  QueueNode <HypConstraint *> * qn;

  for (qn = hcl -> getHead() -> next; 
       qn != NULL;
       qn = qn -> next) {
    
    if (!checkHypConstraint (qn -> d, vctime)) 
      return false;
  }

  return true;
}



//-------------------------------------------------------------------------------

/* Now we turn to checking state conditions. There are only two types
   of conditions that are understood: owner F K and has_xattr F A
   V. Given below are functions to check both of these state conditions 
*/

// Check that owner of file represented by term F is the principal
// represented by K. F must have form prim_str2file (prim_str "...")
// where "..." is the path of the file, relative to the third argument
// pathPrefix. K must have the form prim_int2principal (prim_int
// ...). 

static bool checkStateOwner (Term * F, Term * K, const char * pathPrefix) 
{
  if (F -> constr == constr_prim_str2file &&
      K -> constr == constr_prim_int2principal) {
    
    Term * Farg = ((TermPrim_str2file *) F) -> arg1;
    Term * Karg = ((TermPrim_int2principal *) K) -> arg1;

    if (Farg -> constr == constr_prim_str &&
	Karg -> constr == constr_prim_int) {
      
      char * path = ((TermPrim_str *) Farg) -> str;
      int uid = ((TermPrim_int *) Karg) -> i;

      charbuf fullPath(MAX_PATH_LENGTH);
      fullPath << pathPrefix << path;

      struct stat st;
      if (lstat(fullPath.getHead(), &st) != 0) {
	// File does not exist
	return false;
      }
      
      return (((int) (st.st_uid)) == uid);
    }
  }

  return false;
}


/* Check that the file represented by term F has extended attribute A
   = V, where A and V are also terms. F must have the form
   prim_str2file (prim_str "...") where ... is the name of a file. A
   must have the form (prim_str "..."), where "..." is the name of an
   attribute. Note that "..." must not contain the prefix
   "user.#pcfs." as this is automatically added. V can be just about
   any term. Returns true on success, false on failure. As in the
   above function, pathPrefix is appended before the path contained in
   F. 
*/


static bool checkStateHas_xattr (Term * F, Term * A, Term * V, const char * pathPrefix) 
{
  // F must have form prim_str2file (...) and
  // A must have form prim_str (...)
  if (F -> constr == constr_prim_str2file &&
      A -> constr == constr_prim_str) {
    
    // Get argument of F
    Term * Farg = ((TermPrim_str2file *) F) -> arg1;
   
    // This argument must have form prim_str (...)
    if (Farg -> constr == constr_prim_str) {
      
      // Extract name of attribute from A
      char * attr_name = ((TermPrim_str *) A) -> str;

      // Extract name of file from F's argument
      char * fname = ((TermPrim_str *) Farg) -> str;


      charbuf fullPath(MAX_PATH_LENGTH);
      fullPath << pathPrefix << fname;

      //------------------------------------------

      // The following code can also be rewritten a different way
      // (shown immediately afterwards), which is slightly less
      // portable but much faster.

      // Read attribute's value from file.
      Term * v = getXAttrTerm(fullPath.getHead(), attr_name);

      if (v == NULL) {
	// Attribute does not exist, or its value does not represent a
	// term.
	return false;
      }
      
      // Okay, check if the value matches the given value
      bool b = V -> equal(v);

      delete v;
      return b;

      //-------------------------------------------

      /*
      // Alternate code for the above. This code reads the attribute's
      // value as a string (without parsing) and compares it to the
      // given term by converting the given term to a string, and
      // comparing the strings. This saves the time of parsing the
      // newly read value (which is huge), at the cost of printing the
      // given term (which is small). Although much faster than the
      // code above, its correctness assumes that the attribute was
      // written using the exact same print function that is used to
      // print the given term. Since print functions can vary without
      // varying the semantic meaning of terms (say by introducing or
      // reducing parentheses or whitespace), this is less robust and
      // less elegant. However, the performance difference is huge. In
      // one test the earlier code takes almost 20us per call (with a
      // flex-lemon based lexer-parser), where as this one takes only
      // 2us. A more elegant way to optimize the time may be to use
      // the above code, but improve the lexer-parser.

      char * v = getXAttr(fullPath.getHead(), attr_name);

      if (v == NULL) return false;

      charbuf ss(2048);
      V -> print(ss);

      bool b = !strcmp (ss.getHead(), v);

      delete [] v;
      return b;
      
      */

      //-------------------------------------------

    }
  }

  return false;
}


// The general function to check a State condition.

bool checkState (State * st, const char * pathPrefix) 
{
  switch(st -> constr) {
    
  case state_owner: 
    {
      StateOwner * sto = (StateOwner *) st;
      Term * F = sto -> arg1;
      Term * K = sto -> arg2;
      return checkStateOwner (F, K, pathPrefix);
    }

  case state_has_xattr:
    {
      StateHas_xattr * sth = (StateHas_xattr *) st;
      Term * F = sth -> arg1;
      Term * A = sth -> arg2;
      Term * V = sth -> arg3;

      return checkStateHas_xattr (F, A, V, pathPrefix);
    }

  default:
    {
      return false;
    }
  }

  return false;
}


// Function to check an entire list of State Conditions
bool checkStateList (Queue <State *> * sl, const char * pathPrefix) 
{
  QueueNode <State *> * qn = sl -> getHead() -> next;

  while (qn != NULL) {
    
    if (!checkState (qn -> d, pathPrefix))
      return false;

    qn = qn -> next;

  }

  return true;
}


//-------------------------------------------------------------------------------

/* Finally we integrate it all into one function that checks the qhcl
   and the hypothetical constraints of a ProCap
*/

int checkProCapConditions(ProCap * procap, 
			  const char * pathPrefix, 
			  TermPrim_date2time * vctime)
{
  Queue<HypConstraint *> * hcl = procap -> q -> hcs;
  
  if (!checkHypConstraintList(hcl, vctime)) return 0;

  Queue <State *> * stl = procap -> s;

  if (!checkStateList(stl, pathPrefix)) return 0;

  return 1;
}


