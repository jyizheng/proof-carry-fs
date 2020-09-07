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

#include "pcfs-declarations.hpp"

SortAny sort_global_any;
SortPrincipal sort_global_principal;
SortTime sort_global_time;
SortExp sort_global_exp;
SortFile sort_global_file;
SortPerm sort_global_perm;
SortStr sort_global_str;
SortInt sort_global_int;
SortDate sort_global_date;


// Check that a sort is well-formed, i.e., that it is either
// pre-defined or present in a declaration index
static inline bool checkSort(Sort * s, DeclarationIndex * index)
{
    if (s -> sort == sortname_other) {

      // Sort is a declared sort. First get its name (id)
      char * id = ((SortOther *) s) -> name;

      // Now find its declaration
      Declaration * d = index -> find(id);

      // If declaration does not exist or it is not a sort
      // declaration, return false
      if (d == NULL || d -> getType() != decl_sort) return false;

    }
    
    return true;
}

// Check that all sorts in a list of sorts are declared in an index.
static bool checkSortList (Queue <Sort *> * q, DeclarationIndex * index)
{
  QueueNode <Sort *> * node = q -> getHead() -> next;

  while (node != NULL) {

    Sort * s = node -> d;

    if (!checkSort(s, index)) return false;

    node = node -> next;
  }

  return true;
}

// Check that a single declaration is well-defined in a signature
static bool checkDeclaration(Declaration * d, DeclarationIndex * index)
{
  switch (d -> getType()) {
    
  case decl_sort:
    // If declaration is a sort declaration, it is always valid
    return true;

  case decl_term:
    {
      // We need to check the sorts of arguments
      // And he sort of the target
      DeclarationTerm * dt = (DeclarationTerm *) d;
      
      return (checkSortList(dt -> sortsOfArgs, index) &&
	      checkSort(dt -> sortOfResult, index));
    }
    
  case decl_constraint:
    {
      // We need to check the sorts of arguments
      DeclarationConstraint * dc = (DeclarationConstraint *) d;
      return (checkSortList(dc -> sortsOfArgs, index));
    }

  case decl_predicate:
    {
      // We need to check the sorts of arguments
      DeclarationPredicate * dp = (DeclarationPredicate *) d;
      return (checkSortList(dp -> sortsOfArgs, index));
    }

  case decl_state:
    {
      // We need to check the sorts of arguments
      DeclarationState * ds = (DeclarationState *) d;
      return (checkSortList(ds -> sortsOfArgs, index));
    }

  default:
    cerr << "Reached unreachable default case\n";
    return false;
  }
}


bool sortCheckDeclarationIndex (DeclarationIndex * index)
{
  Queue <char *> * list = index -> listOfDeclarations();

  QueueNode <char *> * node = list -> getHead() -> next;
  while (node != NULL) {
    char * id = node -> d;
    Declaration * d = index -> find(id);
    
    if (d != NULL) {
      // Okay this was not a deleted declaration, so check it
      if (!checkDeclaration(d, index)) return false;
    }
    
    node = node -> next;
  }

  return true;
}

