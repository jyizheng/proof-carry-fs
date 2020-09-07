/*
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

#include "xattr-c-wrap.h"

Int32 get_xattr_size (Pointer fname, Pointer attr, Pointer err)
{
  const char * file = (const char *) fname;
  const char * attribute = (const char *) attr;
  int * errp = (int *) err;

  char dummy;

  int size = lgetxattr (file, attribute, &dummy, 0);

  if (size >= 0) {
    *err = 0; // No error
    return size;
  }

  else {
    if (errno == EACCES) {
      *err = 1; // 1 is a code for an access error
      return -1; 
    }
    else {
      *err = 2; // 2 is a code for a non-existence error
      return -1;
    }
  }
}

Int32 get_xattr (Pointer fname, Pointer attr, Pointer buf, Int32 size)
{
  const char * filename = (const char *) fname;
  const char * attribute = (const char *) attr;
  
  if (lgetxattr (filename, attribute, (void *) buf, (int) size) == size) {
    return 1;
  }
  else {
    return 0;
  }
}
