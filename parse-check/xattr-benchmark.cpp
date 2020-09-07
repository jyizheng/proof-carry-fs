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

#include <sys/types.h>
#include <attr/xattr.h>

#include <iostream>
#include <string>
#include "xattr-terms.hpp"
#include "pcfs-file-parsers.hpp"

using namespace std;

int main() 
{
  Term * term = new TermExp_add (new TermTime2exp (new TermPrim_int2time (new TermPrim_int (201))), 
				 new TermTime2exp (new TermPrim_date2time 
						   (new TermPrim_date (new NativeTime (2008, 12, 29)))));

  cerr << "Trying to write to file sample-procap.cap, the pair last_mod_time: ";
  term -> print (cerr) << "\n";
  
  int r = setXAttrTerm ("sample-procap.cap", "last_mod_time", term);

  if (r != 1) {
    cerr << "Writing of attribute failed\n";
    delete term;
    return EXIT_FAILURE;
  }
  
  cerr << "Writing of attribute succeeded\n";

  cerr << "Trying to read the attribute\n";

  Term * nterm = getXAttrTerm ("sample-procap.cap", "last_mod_time");

  if (nterm == NULL) {
    cerr << "Reading of attribute failed\n";
    delete term;
    delete nterm;
    return EXIT_FAILURE;
  }

  cerr << "Term obtained is: ";
  nterm -> print(cerr) << "\n";

  cerr << "Checking term equality\n";

  if (term -> equal(nterm)) {
    cerr << "Equality test okay\n";
  }
  else 
    cerr << "Equality test not okay\n";

  delete nterm;

  //-----------------------------------------------------------------------

  {
    cerr << "Timing 1000000 reads of attribute\n";
    
    struct timeval init_time, final_time;
    
    gettimeofday(&init_time, NULL);
    
    int i;
    for (i = 0; i < 1000000; i++) {
      char * t = getXAttr ("sample-procap.cap", "last_mod_time");
      if (t != NULL) {
	delete [] t;
      }
    }

    gettimeofday(&final_time, NULL);
    
    double diffw = (final_time.tv_sec - init_time.tv_sec) + 0.000001 * (final_time.tv_usec - init_time.tv_usec);
    cout << "Time taken = " << diffw << " seconds\n";
  }
  

  /*
  {
    char * ttext = parseFileTrivial("sample-term2.t");
    cerr << "sample-term2.t contains: " << ttext << "\n";

    cerr << "Timing 1000000 parses of term string\n";
    
    struct timeval init_time, final_time;
    
    gettimeofday(&init_time, NULL);
    
    int i;
    for (i = 0; i < 1000000; i++) {
      Term * t = parseStringTerm (ttext);
      if (t != NULL) {
	delete t;
      }
    }

    gettimeofday(&final_time, NULL);
    
    double diffw = (final_time.tv_sec - init_time.tv_sec) + 0.000001 * (final_time.tv_usec - init_time.tv_usec);
    cout << "Time taken = " << diffw << " seconds\n";

    delete [] ttext;
  }
  */

  delete term;
  return EXIT_SUCCESS;

}
