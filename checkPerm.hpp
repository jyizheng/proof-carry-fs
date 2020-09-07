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

#ifndef __CHECK_PERM_HPP
#define __CHECK_PERM_HPP

#include "parse-check/pcfs-time.hpp"
#include "parse-check/pcfs-syn.hpp"
#include "pcfs-runtime.hpp"
#include "pcfs-procaps.hpp"

/* Function to check given permission on a given path for a given
   user. The three arguments are the path, uid of user, and permission
   to check. The function will determine whether the call is to a
   configuration file or to a regular file/directory by checking
   isAncestor("/#config", path). If the check is true, the call is to
   configuration, else it is to data. path must not have a trailing
   '/', but must have a leading '/'.
*/

int checkPerm (const char * path, int uid, Term * perm);

#endif
