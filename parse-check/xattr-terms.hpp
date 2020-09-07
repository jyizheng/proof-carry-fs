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

#ifndef __XATTR_TERMS_HPP
#define __XATTR_TERMS_HPP

/* This file contains functions to read and write extended attributes
   to files, where the attributes are strings and values written are
   terms. It is possible to check that the value written parses as a
   term before writing it. 
*/

#include "pcfs-syn.hpp"

#define MAX_ATTR_NAME_SIZE   64  
// Maximum length of the name of an attribute, excluding the
// "user.#pcfs." prefix

// Function to read a given attribute from a file as a string. Returns
// the value of the attribute if it succeeds, NULL otherwise.
char * getXAttr(const char * fname, const char * attr_name);

// Read the attribute with name 'attr_name' from 'fname', parse it as
// a term and return it. If any of these steps fail, return NULL.
Term * getXAttrTerm(const char * fname, const char * attr_name);

// Write the attribute-value pair 'attr_name'-'val' to file
// 'fname'. Return 1 on success, 0 on failure.
int setXAttr(const char * fname, const char * attr_name, const char * val);

// Write the attribute-value pair 'attr_name'-term to file
// 'fname'. Return 1 on success, 0 on failure. Note that val is not
// sort-checked.
int setXAttrTerm(const char * fname, const char * attr_name, Term * val);



#endif
