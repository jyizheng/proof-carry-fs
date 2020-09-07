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

#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>
#include <time.h>
#include <iostream>

#include "DiskCacheTrivial.hpp"
#include "../paths-common.hpp"

using namespace std;

/* Benchmarking the speed of disk stat
   
   usage:
   stat-benchmark numberOfFiles times 

   numberOfFiles: total number of files to test on (2 <= numberOfFiles)
   times: total number of times the test has to be run (Any number)
   
   The test runs as follows:
   1. A list L of size <times> containing random numbers between 1 and <numberOfFiles> is generated
   2. For each i in L, the file cache-testing/procaps/home/dg/1K/<i> is 'stat'ed using stat (2)
   3. The total time for step (3) is printed

   The directory cache-testing/procaps/home/dg/1K/ actually contains
   files named 1 -- 1000. So if the test is run with more than 1000
   files, there will be misses. In this way, both the speed of miss
   and hit can be measured.
*/

void doStat(char * path) 
{
  struct stat filedata;
  if (lstat(path, &filedata) == -1) {
    return;
  }
}
    

int main(int argc, char * argv[])
{
  if (argc != 3) return -1;

  srand48(time(NULL));
  
  int numberOfFiles = atoi (argv[1]);
  int times = atoi (argv[2]);

  char subpath[MAX_PATH_LENGTH] = "cache-testing/procaps/home/dg";
  addNewComponentToPath(subpath, argv[4]);

  char ** list = new char *[times];

  int i;
  for (i = 0; i < times; i++) {
    int temp = (int)(drand48() * numberOfFiles) + 1;
    list[i] = new char[7];
    sprintf(list[i], "%d", temp);
    cerr << list[i] << "\n";
  }

  cout << "Running test " << times;
  cout << " times with <";
  cout << "numberOfFiles = " << numberOfFiles << ">\n";

  struct timeval init_time, final_time;

  gettimeofday(&init_time, NULL);
  
  // Run test without cache
  for (i = 0; i < times; i++) {
    addNewComponentToPath(subpath,list[i]);
    doStat(subpath);
    removeLastComponentOfPath(subpath);
  }  

  gettimeofday(&final_time, NULL);

  
  cout << "Timing ... \n";
  double diffw = (final_time.tv_sec - init_time.tv_sec) + 0.000001 * (final_time.tv_usec - init_time.tv_usec);
  cout << "Time taken = " << diffw << " seconds\n";

  //  for (i=0;i<times;i++) cerr << output[i];

  //-------------------------------------


  // Deallocation

  for (i = 0; i <times; i++) {
    delete [] list[i];
  }
  delete [] list;

  return 1;
}
