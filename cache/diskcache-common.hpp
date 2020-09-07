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

#ifndef __DISK_CACHE_COMMON_H
#define __DISK_CACHE_COMMON_H

#include "../paths-common.hpp"

// Error codes
#define CACHE_ERR_NOFILE -1    // File/directory does not exist or cannot read/write it
#define CACHE_ERR_FILEISDIR -2 // Was expecting a file, but found a directory
#define CACHE_ERR_DIRISFILE -4 // Was expecting a directory, but found a file
#define CACHE_ERR_NOMEM -8     // Not enough memory
#define CACHE_ERR_FILEIO -16   // File input/output
#define CACHE_ERR_FORMAT -32   // String format error
#define CACHE_ERR_INTERNAL -64   // An internal error (invariant was violated)

class DiskCacheError { };

#endif
