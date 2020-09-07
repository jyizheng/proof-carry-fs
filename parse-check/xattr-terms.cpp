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

#include <sys/types.h>
#include <attr/xattr.h>
#include <iostream>
#include <string>

#include "xattr-terms.hpp"
#include "../paths-common.hpp"
#include "../charbuf.hpp"
#include "pcfs-file-parsers.hpp"
#include "pcfs-syn.hpp"

using namespace std;

char * getXAttr (const char * fname, const char * attr_name) 
{

  charbuf full_attr_name(MAX_ATTR_NAME_SIZE + 12);
  // The extra twelve are needed to accomodate the prefix
  // "user.#pcfs." and the '\0' at the end

  full_attr_name << "user.#pcfs." << attr_name;

  char dummy; // Just a dummy value
  int size = lgetxattr (fname, full_attr_name.getHead(), &dummy, 0);

  if (size == -1) {
    return NULL;
  }

  char * val = new char[size + 1];

  int r = lgetxattr (fname, full_attr_name.getHead(), val, size);

  if (r != size) {
    delete [] val;
    return NULL;
  }

  val[size] = '\0';
  return val;
}


Term * getXAttrTerm(const char * fname, const char * attr_name)
{
  char * val = getXAttr(fname, attr_name);
  
  if (val == NULL) return NULL;
  
  Term * term = parseStringTerm(val);

  delete [] val;

  return term;
}



int setXAttr (const char * fname, const char * attr_name, const char * val) 
{

  charbuf full_attr_name(MAX_ATTR_NAME_SIZE + 12);
  // The extra twelve are needed to accomodate the prefix
  // "user.#pcfs." and the '\0' at the end

  full_attr_name << "user.#pcfs." << attr_name;

  int r = setxattr(fname, full_attr_name.getHead(), val, strlen(val), 0);
  
  if (r == 0) return 1;

  return 0;
}


int setXAttrTerm (const char * fname, const char * attr_name, Term * val)
{

  charbuf ss (2048); // 2048 is an upper bound on textual size of a term.

  val -> print(ss);

  return (setXAttr (fname, attr_name, ss.getHead()));
}

