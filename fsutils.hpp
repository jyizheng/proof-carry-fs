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

#ifndef __FS_UTILS_H
#define __FS_UTILS_H

#include <iostream>

#include "charbuf.hpp"
#include "paths-common.hpp"
#include "logstream.hpp"

using namespace std;

// Recursively remove path, and all its subdirectories. This function
// will "try" to remove as much as possible. Some subpaths may not be
// removable because of permission issues. In this sense, its behavior
// is like rm -rf. path must not end in '/'. Note that this function
// is actually recursive, not iterative, so it may crash the stack if
// the directories are too deep. The 'out' argument is used to write a
// trace of files removed to the log
void recursiveRemove(const char * path, logstream * out);

// Similar to the above function, but takes charbuf as argument. The
// charbuf is reused and must have length MAX_PATH_LENGTH
void recursiveRemove(charbuf * path, logstream * out);

#endif
