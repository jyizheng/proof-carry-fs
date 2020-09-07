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

#include "loadProCap.hpp"
#include "loadDeclarations.hpp"

int main()
{
  FILE * keyfile = fopen ("mac-key.key", "r");
  if (keyfile == NULL) return 1;
  char key[41];
  fscanf(keyfile, "%40s", key);
  key[40] = '\0';
  fclose(keyfile);

  cerr << "Key is: " << key << "\n";

  // Convert key to binary form
  unsigned char binkey[SYM_KEY_LENGTH];
  if (hexToBinary(key, 2 * SYM_KEY_LENGTH, binkey, true) != 1) {
    cerr << "Key format error\n";
    return EXIT_FAILURE;
  }
  

  DeclarationIndex * di = loadFileDeclarations ("sample-decl.d");
  if (di == NULL) {
    cerr << "Error parsing declarations file\n";
    return EXIT_FAILURE;
  }
  
  ProCap * p = loadFileProCap ("sample-procap.cap.crp", binkey, di);
  if (p!= NULL) {
    p -> print(cout);
    delete p;
  }

  cout << "Timing 100000 reads, parses, and type checks\n";

  struct timeval init_time, final_time;

  gettimeofday(&init_time, NULL);
  
  int i;
  for (i = 0; i < 100000; i++) {
    ProCap * p = loadFileProCap ("sample-procap.cap.crp", binkey, di);
    if (p != NULL) {
      //      cerr << "Found procap\n";
      // p -> print(cerr);
      delete p;
    }
    
  }

  gettimeofday(&final_time, NULL);

  double diffw = (final_time.tv_sec - init_time.tv_sec) + 0.000001 * (final_time.tv_usec - init_time.tv_usec);
  cout << "Time taken = " << diffw << " seconds\n";

  return EXIT_SUCCESS;
}
