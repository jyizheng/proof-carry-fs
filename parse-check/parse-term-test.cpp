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

#include "pcfs-file-parsers.hpp"
#include "pcfs-syn.hpp"

int main()
{
  cout << "Parsing term\n";

  DeclarationIndex * di = parseFileDeclarations ("sample-decl.d");
  Term * t = parseFileTerm ("sample-term.t");

  Queue <IdSortPair *> * local = new Queue<IdSortPair*>;
  char * id = new char[10];
  strcpy (id, "somelevel");
  local -> push (new IdSortPair (id, new SortPrincipal));

  if (t != NULL) {
    cerr << "Term is: " << "\n";
    t -> print(cerr) << "\n";

    if (t -> check(&sort_global_any, di, local))
      cerr << "Type check succeeded\n";
    else
      cerr << "Type check failed\n";
  }
  else {
    cerr << "No term found\n";
    return 1;
  }

  delete t;
  delete di;
  freeQueueWithData(local);

  return 0;
}
