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

#ifndef __CHECK_PROCAP_CONDITIONS_HPP
#define __CHECK_PROCAP_CONDITIONS_HPP

#include "pcfs-syn.hpp"

// Check whether the conditions (constraints and state conditions) of
// a procap hold in the current environment. The second argument is
// the prefix that is attached to all file names found in the
// conditions. Usually, this will be the physical path to the root of
// the pcfs file system. Note that no attempt is made to insert (or
// remove) any path separators ('/') between the prefix and the files
// found. So it must be ensured that either prefixPath ends in a '/'
// or each file name mentioned in the procap starts with a '/' (but
// not both). The third argument is a term representing the current
// time. It is expected that this term will be created like : new
// TermPrim_date2time (new TermPrim_date (new NativeTime ((long)
// time(NULL)))) where time() is defined in <time.h>.

// Returns 1 on success, 0 on failure.
int checkProCapConditions(ProCap * procap, 
			  const char * pathPrefix, 
			  TermPrim_date2time * vctime);


bool checkState (State * st, const char * pathPrefix) ;
bool checkStateList (Queue <State *> * sl, const char * pathPrefix);

bool checkHypConstraint(HypConstraint * h, TermPrim_date2time * vctime) ;
bool checkHypConstraintList (Queue<HypConstraint *> * hcl, TermPrim_date2time * vctime) ;

#endif

