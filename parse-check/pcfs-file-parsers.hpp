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

#ifndef PCFS_FILE_PARSERS_HPP
#define PCFS_FILE_PARSERS_HPP

#include <iostream>
#include <stdio.h>
#include "pcfs-parse-ds.hpp"

using namespace std;

#define MAX_FILE_LENGTH 16384 // 16K

// Parse a file as a ProCap .. return a ProCap on success, NULL on
// error. This function does not verify the MAC or verify the ProCap
// for well-formedness.
ProCap * parseFileProCap(const char * fname);

// Parse a file containing declarations. Return a DeclarationIndex on
// success, NULL on failure. 
DeclarationIndex * parseFileDeclarations(const char * fname);

// Parse a file containing a term. Return a Term on success, NULL on
// failure.
Term * parseFileTerm(const char * fname);

// Parse a file and return the entire data as a string.  If there is
// an error, or if file length exceeds MAX_FILE_LENGTH, return
// NULL. Note that this limit does not apply to the other functions
// above.
char * parseFileTrivial (const char * fname);

// Parse a string containing a term. Return a Term on success, NULL on
// failure.
Term * parseStringTerm(const char * str);

#endif
