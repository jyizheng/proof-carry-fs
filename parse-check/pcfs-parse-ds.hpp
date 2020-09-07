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

#ifndef __PCFS_PARSE_SYN_H
#define __PCFS_PARSE_SYN_H

#include <stdio.h>
#include "pcfs-declarations.hpp"
#include "pcfs-syn.hpp"
#include "pcfs-time.hpp"

struct ParserOutput
{
  enum ParserType {
      parser_declaration, 
      parser_procap, 
      parser_term,
      parser_error
    };
    
  ParserType type;

  union {
    DeclarationIndex * index;
    ProCap * procap;
    Term * term;
  } data;

  ParserOutput()
  {
    type = parser_error;
  }
};

struct PrimitiveValue
{
  enum {integer, string, date} type;

  union {
    int i;
    char * str;
    NativeTime * t;
  } data;

  PrimitiveValue (int v) {
    type = integer;
    data.i = v;
  }

  PrimitiveValue (char * c) {
    type = string;
    data.str = c;
  }

  PrimitiveValue (NativeTime * t) {
    type = date;
    data.t = t;
  }

  ~PrimitiveValue()
  {
    // We don't explicitly delete any data in PrimitiveValue since it
    // is held in the parser's data structures
  }
};

inline void freeDataPrimitiveValue(PrimitiveValue * pv)
{
  if (pv -> type == PrimitiveValue :: string) delete [] (pv -> data.str);
  else if (pv -> type == PrimitiveValue :: date) delete (pv -> data.str);
}

#endif
