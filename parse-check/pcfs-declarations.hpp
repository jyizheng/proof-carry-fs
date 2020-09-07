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

#ifndef __DECLARATIONS_H
#define __DECLARATIONS_H

/* A description of declarations that may be read from a file of
   declarations. There are five types of declarations allowed (Note:
   an id (identifier) is a string)

   id: sort.   %% Sort declaration
   id: term id1 -> ... -> term idn -> term id.    %% term constructor; id1 ... idn, id must be sorts
   id: term id1 -> ... -> term idn -> constraint. %% constraint declaration; id1 ... idn must be sorts
   id: term id1 -> ... -> term idn -> pred.       %% predicate declaration; id1 ... idn must be sorts
   id: term id1 -> ... -> term idn -> state.      %% state predicate declaration; id1 ... idn must be sorts
   
   Declarations fall into two distinct categories:

   1. Pre-defined. This includes all ids that are fixed in
   advance. These are always available. Here is a list of these:
   
   any: sort.        %% Every well-formed term has this sort.
   principal: sort.
   time: sort.
   exp: sort.        %% Expressions over time
   file: sort.
   perm: sort.       %% permissions
   int: sort.        %% primitive integers, injected into pcfs
   str: sort.        %% primitive strings, injected into pcfs; of the form "abc"
   date: sort.       %% primitive dates, injected into pcfs. Of the forms yyyy:mm:dd[:hh:mm:ss]

   loca: term sort.                  %% local authority
   ctime: term time.                 %% current time
   ninfty: term time.                %% Time - infinity
   pinfty: term time.                %% Time + infinity

   prim_str2file: prim_str -> term file.
   prim_date2time: prim_date -> term time. 
   prim_int2principal: prim_int -> term principal.  %% convert a uid to a principal.

   %% time represented as an integer (measured in seconds). Note that
   %% this is used exclusively to represent relative time, often used
   %% to add a certain number of seconds to another time point.
   int2tile: prim_int -> term time.

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

   
   stronger: term principal -> term principal -> constraint.
   leq: term time -> term time -> constraint.

   owner: term file -> term principal -> state.
   has_xattr: term file -> term prim_str -> term any -> state.
   %% has_xattr F A V -- file F has the extended attribute A = V
   
   has_perm: term principal -> term file -> term perm -> pred.  
   %% has_perm K F P -- principal K has permission P on file F

   2. System specific declarations, which are read from a file. The
   file does not have to contain the above declarations. These are
   available in all PCFS systems. Note that although state predicates
   may be defined in a file, they will be essentially useless, unless
   PCFS is extended to verify them. The two state predicates owner and
   has_xattr are natively supported in PCFS.

   The list of all pre-defined identifiers is stored in a Patricia
   Trie, indexed by the identifier for easy lookup. The data stored
   with an identifier is an object of (a subclass of) the class
   Declaration, which is defined below.
   
*/

#include <stdio.h>
#include "../Queue.hpp"
#include "../nPatriciaTrie.h"
#include "../charbuf.hpp"

//----------------------------------------------------------------------------------

/* Data structures for representing sorts. Sorts are of two types:
   1. Pre-defined:
   
   principal: sort.
   time: sort.
   exp: sort.        %% Expressions over time
   file: sort.
   perm: sort.       %% permissions
   
   %% These are sorts for primitive types
   date: sort.
   int: sort.
   str: sort.

   2. User defined (represented as a string).
 */

enum SortName {
  sortname_any,
  sortname_principal,
  sortname_time,
  sortname_exp,
  sortname_file,
  sortname_perm,
  sortname_str,
  sortname_int,
  sortname_date,
  sortname_other
};


class Sort 
{
protected:
  Sort(SortName s) {sort = s;}

public:
  SortName sort;

  virtual ~Sort() { }
  virtual ostream & print (ostream & out) = 0;
  virtual charstream & print (charstream & out) = 0;
 
  // Override this method for all but primitively defined sorts
  virtual bool equal (Sort * s) {
    return sort == s -> sort;
  }
};

//---------------------------------------

class SortAny: public Sort
{
public:
  SortAny() : Sort(sortname_any) { }

  ostream & print (ostream & out) {
    out << "any";
    return out;
  }

  charstream & print (charstream & out) {
    out << "any";
    return out;
  }
}; 


//---------------------------------------

class SortPrincipal: public Sort
{
public:
  SortPrincipal() : Sort(sortname_principal) { }
  
  ostream & print (ostream & out) {
    out << "principal";
    return out;
  }

  charstream & print (charstream & out) {
    out << "principal";
    return out;
  }
}; 


//---------------------------------------

class SortTime: public Sort
{
public:
  SortTime() : Sort(sortname_time) { }

  ostream & print (ostream & out) {
    out << "time";
    return out;
  }


  charstream & print (charstream & out) {
    out << "time";
    return out;
  }
}; 


//---------------------------------------

class SortExp: public Sort
{
public:
  SortExp() : Sort(sortname_exp) { }

  ostream & print (ostream & out) {
    out << "exp";
    return out;
  }

  charstream & print (charstream & out) {
    out << "exp";
    return out;
  }

}; 


//---------------------------------------

class SortFile: public Sort
{
public:
  SortFile() : Sort(sortname_file) { }

  ostream & print (ostream & out) {
    out << "file";
    return out;
  }

  charstream & print (charstream & out) {
    out << "file";
    return out;
  }

}; 


//---------------------------------------

class SortPerm: public Sort
{
public:
  SortPerm() : Sort(sortname_perm) { }

  ostream & print (ostream & out) {
    out << "perm";
    return out;
  }
  
  charstream & print (charstream & out) {
    out << "perm";
    return out;
  }
  
}; 


//---------------------------------------

class SortStr: public Sort
{
public:
  SortStr() : Sort(sortname_str) { }

  ostream & print (ostream & out) {
    out << "str";
    return out;
  }
  
  charstream & print (charstream & out) {
    out << "str";
    return out;
  }
  
}; 


//---------------------------------------

class SortInt: public Sort
{
public:
  SortInt() : Sort(sortname_int) { }

  ostream & print (ostream & out) {
    out << "int";
    return out;
  }
  
  charstream & print (charstream & out) {
    out << "int";
    return out;
  }
  
}; 


//---------------------------------------

class SortDate: public Sort
{
public:
  SortDate() : Sort(sortname_date) { }

  ostream & print (ostream & out) {
    out << "date";
    return out;
  }
  
  charstream & print (charstream & out) {
    out << "date";
    return out;
  }
  
}; 


//---------------------------------------

class SortOther: public Sort
{
public:
  SortOther(char * s) : Sort(sortname_other) 
  {
    name = s;
  }

  char * name;

  ~SortOther() 
  {
    delete [] name;
  }

  ostream & print (ostream & out) {
    out << name;
    return out;
  }
  
  charstream & print (charstream & out) {
    out << name;
    return out;
  }
  
  virtual bool equal (Sort * s) {
    if (s -> sort != sortname_other) return false;
    return (!strcmp (name, ((SortOther*) s) -> name));
  }
}; 


//--------------------------------------------------------------------------------------------

enum DeclType 
{
    decl_sort,          // Sort declaration; id: sort.
    decl_term,          // Term declaration; id: term id1 -> ... -> term idn -> term id.
    decl_constraint,    // Constraint declaration; id: term id1 -> ... -> term idn -> constraint.
    decl_predicate,          // Predicate declaration;  id: term id1 -> ... -> term idn -> pred.
    decl_state          // State predicate declaration; id: term id1 -> ... -> term idn -> state.
};


//--------------------------------------------------------

class Declaration
{
protected:
  DeclType type;

  Declaration(DeclType t) {type = t; }

public:
  virtual ~Declaration() { } 

  virtual DeclType getType() { return type; }

  virtual ostream & print(ostream & out) = 0;
  virtual charstream & print(charstream & out) = 0;

};



//---------------------------------------------------------

class DeclarationSort : public Declaration
{
public: 
  DeclarationSort () 
  : Declaration (decl_sort) 
  { }

  ostream & print (ostream & out) {
    out << "sort";
    return out;
  }

  charstream & print (charstream & out) {
    out << "sort";
    return out;
  }

};



//---------------------------------------------------------

class DeclarationTerm: public Declaration
{
public:
  Queue <Sort * > * sortsOfArgs; // Queue containing sorts of
				 // arguments; the head side contains
				 // the sort of the first argument,
				 // the tail the last. Cannot be NULL,
				 // but may be empty.

  Sort * sortOfResult;             // Sort of the result. Cannot be NULL.

  DeclarationTerm (Queue <Sort *> * q, Sort * r) 
    // Do not pass stack allocated arguments. Do not deallocate the
    // arguments after passing to the constructor since they are NOT
    // copied, but aliased. These will be deallocated by the
    // destructor automatically.
    : Declaration (decl_term)
  {
    sortsOfArgs = q;
    sortOfResult = r;
  }

  ~DeclarationTerm ()
  {
    delete sortOfResult;
    freeQueueWithData(sortsOfArgs);
  }

  ostream & print (ostream & out) {
    printQueue (out, sortsOfArgs, "term ", " -> ");
    out << "term ";
    sortOfResult -> print(out);
    return out;
  }

  charstream & print (charstream & out) {
    printQueue (out, sortsOfArgs, "term ", " -> ");
    out << "term ";
    sortOfResult -> print(out);
    return out;
  }

};


//------------------------------------------------------------

class DeclarationConstraint: public Declaration
{
public:
  Queue <Sort * > * sortsOfArgs; 

  DeclarationConstraint (Queue <Sort *> * q) 
    // Do not pass stack allocated arguments. Do not deallocate the
    // arguments after passing to the constructor since they are NOT
    // copied, but aliased. These will be deallocated by the
    // destructor automatically.
    : Declaration (decl_constraint)
  {
    sortsOfArgs = q;
  }

  ~DeclarationConstraint ()
  {
    freeQueueWithData(sortsOfArgs);
  }

  ostream & print (ostream & out) {
    printQueue (out, sortsOfArgs, "term ", " -> ");
    out << "constraint";
    return out;
  }
    
  charstream & print (charstream & out) {
    printQueue (out, sortsOfArgs, "term ", " -> ");
    out << "constraint";
    return out;
  }
    
};


//------------------------------------------------------------

class DeclarationPredicate: public Declaration
{
public:
  Queue <Sort * > * sortsOfArgs; 

  DeclarationPredicate (Queue <Sort *> * q) 
    // Do not pass stack allocated arguments. Do not deallocate the
    // arguments after passing to the constructor since they are NOT
    // copied, but aliased. These will be deallocated by the
    // destructor automatically.
    : Declaration (decl_predicate)
  {
    sortsOfArgs = q;
  }

  ~DeclarationPredicate ()
  {
    freeQueueWithData(sortsOfArgs);
  }

  ostream & print (ostream & out) {
    printQueue (out, sortsOfArgs, "term ", " -> ");
    out << "pred";
    return out;
  }
    
  charstream & print (charstream & out) {
    printQueue (out, sortsOfArgs, "term ", " -> ");
    out << "pred";
    return out;
  }
    
};


//------------------------------------------------------------

class DeclarationState: public Declaration
{
public:
  Queue <Sort * > * sortsOfArgs; 

  DeclarationState (Queue <Sort *> * q) 
    // Do not pass stack allocated arguments. Do not deallocate the
    // arguments after passing to the constructor since they are NOT
    // copied, but aliased. These will be deallocated by the
    // destructor automatically.
    : Declaration (decl_state)
  {
    sortsOfArgs = q;
  }

  ~DeclarationState ()
  {
    freeQueueWithData(sortsOfArgs);
  }

  ostream & print (ostream & out) {
    printQueue (out, sortsOfArgs, "term ", " -> ");
    out << "state";
    return out;
  }
    
  charstream & print (charstream & out) {
    printQueue (out, sortsOfArgs, "term ", " -> ");
    out << "state";
    return out;
  }
    
};


//-----------------------------------------------------------------------------------------

/* A data structure for holding the data read from a declaration
   file. The core is a Patricia Trie, indexed by identifiers.
*/

class DeclarationIndex
{
protected:
  nPatriciaTrie<Declaration *> * index;

  Queue<char *> * idList; 
  // List of identifiers defined here. This also includes identifiers
  // that have been deleted from the index.

public:

  DeclarationIndex()
  {
    index = new nPatriciaTrie<Declaration *>;
    idList = new Queue <char *>;
  }

  // Return a queue of all declarations in the index. This list is
  // stored internally in the index, and must NOT be freed by the
  // caller. Further, there may be some deleted/duplicate entries in
  // the list. These can be determined by calling lookup.
  Queue <char *> * listOfDeclarations() {
    return idList;
  }

  // Add a declaration. Returns 1 on success, 0 on failure.  If the
  // declaration exists, a 0 will be returned.  This method will store
  // the declaration directly, so this must not be freed by the caller
  // unless the method returns 0.
  int add(char * id, Declaration * decl)
  {
    if (index -> Insert(id, decl) == NULL) {
      // Insert has failed so identifier exists. 
      return 0;
    }
    
    char * nid = new char[strlen(id) + 1];
    strcpy(nid, id);
    idList -> push (nid);
    return 1;
  }

  // Remove a declaration. Return the stored declaration on success,
  // NULL if the declaration does not exist. The use of this method is
  // not encouraged because the memory held by the identifier in
  // idList is freed only when the trie is deleted.
  Declaration * remove(char * id) {
    nPatriciaTrieNode<Declaration *> * node = index -> LookupNode(id);
    if (id == NULL) return NULL;
    Declaration * d = (node -> GetData());
    index -> Delete(id);
    return d;
  }

  // Find a declaration. Return declaration if successful, NULL if it
  // does not exist
  Declaration * find(char * id) {
    return index -> Lookup(id);
  }

  ~DeclarationIndex()
  {
    QueueNode <char *> * qn = idList -> getHead() -> next;
    while (qn != NULL) {
      nPatriciaTrieNode <Declaration * > * tn = index -> LookupNode(qn -> d);
      if (tn != NULL) delete (tn -> GetData());
      index -> Delete(qn -> d);
      delete [] (qn -> d);
      qn = qn -> next;
    }
    delete idList;
    delete index;
  }

  ostream & print (ostream & out) {
    QueueNode <char *> * qn = idList -> getHead() -> next;
    while (qn != NULL) {
      nPatriciaTrieNode <Declaration * > * tn = index -> LookupNode(qn -> d);
      if (tn != NULL) {
	out << (qn -> d) << " : " ;
	tn -> GetData() -> print(out);
	out << ".\n";
      }
      qn = qn -> next;
    }
    return out;
  }

  charstream & print (charstream & out) {
    QueueNode <char *> * qn = idList -> getHead() -> next;
    while (qn != NULL) {
      nPatriciaTrieNode <Declaration * > * tn = index -> LookupNode(qn -> d);
      if (tn != NULL) {
	out << (qn -> d) << " : " ;
	tn -> GetData() -> print(out);
	out << ".\n";
      }
      qn = qn -> next;
    }
    return out;
  }

};



// Checks a declaration index to make sure that all sorts mentioned in
// it actually declared.
bool sortCheckDeclarationIndex(DeclarationIndex * index);

// Instances of pre-defined sorts
extern SortAny sort_global_any;
extern SortPrincipal sort_global_principal;
extern SortTime sort_global_time;
extern SortExp sort_global_exp;
extern SortFile sort_global_file;
extern SortPerm sort_global_perm;
extern SortStr sort_global_str;
extern SortInt sort_global_int;
extern SortDate sort_global_date;

#endif
