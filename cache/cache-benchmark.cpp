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

/* Benchmarking the DiskCache
   
   usage:
   cache-benchmark cacheSize numberOfFiles times fileSize

   cacheSize: size of cache to use (1 <= cacheSize <= 100)
   numberOfFiles: total number of files to test on (2 <= numberOfFiles <= 10000)
   times: total number of times the test has to be run (Any number)
   fileSize: 1K, 2K, 4K, or 8K 
   
   The test runs as follows:
   1. A cache of size <cacheSize> is initialized with rootPath = ./
   2. A list L of size <times> containing random numbers between 1 and <numberOfFiles> is generated
   3. For each i in L, the file cache-testing/procaps/home/dg/<fileSize>/<i> is read /using/ the cache
   4. The total time for step (3) is printed
   5. For each i in L, the file procaps/home/dg/<fileSize>/<i> is read /without/ the cache
   6. The total time for step (5) is printed
*/

char * getData(char * path) 
{
  int fd = open (path, O_RDONLY);

  if (fd == -1) return NULL;

  struct stat filedata;
  if (fstat(fd, &filedata) == -1) {
    close (fd);
    return NULL;
  }
    
  if (S_ISDIR(filedata.st_mode)) {
    close (fd);
    return NULL;
  }

  int length = filedata.st_size;
    
  char * data = new char[length + 1024]; 
  // This extra space of 1024 is needed, just in case file is longer
  // than specified by its length
  if (data == NULL) return NULL;

  int pos = 0;
  int done = false;
  while (!done && pos < length)
    {
      int c;
      if ((c = read (fd, data + pos, 512)) > 0)
	{
	  pos = pos + c;
	}
      else 
	{
	  done = true;
	}
    }
   
  if (pos != length) {
    close (fd);
    delete [] data;
    return NULL;
  }
  data[pos] = '\0';

  close(fd);

  return data;
}


int main(int argc, char * argv[])
{
  if (argc != 5) return -1;
  if (strcmp(argv[4], "1K") && strcmp(argv[4], "2K") &&
      strcmp(argv[4], "4K") && strcmp(argv[4], "8K")) 
    return -1;

  srand48(time(NULL));
  
  unsigned cacheSize = (unsigned) (atoi (argv[1]));
  int numberOfFiles = atoi (argv[2]);
  int times = atoi (argv[3]);

  char rootpath[MAX_PATH_LENGTH] = ".";

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
  cout << " times with <cache size = " << cacheSize;
  cout << ", fileSize = " << argv[4];
  cout << ", numberOfFiles = " << numberOfFiles << ">\n";



  //-----------------------------------------------

  DiskCacheTrivial * cache = new DiskCacheTrivial(cacheSize, rootpath);

  struct timeval init_time;
  struct timeval final_time;

  gettimeofday(&init_time, NULL);

  
  
  // Run test with cache
  for (i = 0; i < times; i++) {
    addNewComponentToPath(subpath,list[i]);
    char * p = (char *)(cache -> getData(subpath));
    
    removeLastComponentOfPath(subpath);
  }
  
  gettimeofday(&final_time, NULL);

  cout << "Timing with cache ... \n";
  double diff = (final_time.tv_sec - init_time.tv_sec) + 0.000001 * (final_time.tv_usec - init_time.tv_usec);
  cout << "Time taken = " << diff << " seconds\n";

  gettimeofday(&init_time, NULL);
  
  // Run test without cache
  for (i = 0; i < times; i++) {
    addNewComponentToPath(subpath,list[i]);
    char * p = getData(subpath);
    delete [] p;
    removeLastComponentOfPath(subpath);
  }  

  gettimeofday(&final_time, NULL);

  
  cout << "Timing without cache ... \n";
  double diffw = (final_time.tv_sec - init_time.tv_sec) + 0.000001 * (final_time.tv_usec - init_time.tv_usec);
  cout << "Time taken = " << diffw << " seconds\n";

  delete cache;

  //  for (i=0;i<times;i++) cerr << output[i];

  //-------------------------------------


  // Deallocation

  for (i = 0; i <times; i++) {
    delete [] list[i];
  }
  delete [] list;

  return 1;
}
