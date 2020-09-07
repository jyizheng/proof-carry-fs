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

/* Trivial parsers for cache:

   In case of a file, it just returns a null-terminated string containing the contents of the file.
   In case of a directory, it returns a Queue with a list of files in it.

*/

#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <dirent.h>

#include "trivial-parser.hpp"

#include "../Queue.hpp"
#include "DiskCache.hpp"

static char * trivial_parser_cache_file(const char * path)
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


static Queue<char *> * trivial_parser_cache_dir(const char * path)
{
  DIR * dirp = opendir(path);
    
  if (dirp == NULL) return NULL;

  Queue<char *> * children = new Queue<char*>;
    

  struct dirent * entry;
  while ((entry = readdir(dirp)) != NULL) {
      
    // check that entry does not start with '.'
    // We want to ignore files that start with '.'
    if (entry -> d_name[0] != '.') {
      char * name = new char[strlen(entry -> d_name) + 1];
      strcpy(name, entry -> d_name);
      children -> push (name);
    }
  }
  
  closedir (dirp);
  return children;
}


void * trivial_parser_cache(NodeType type, const char * path)
{
  if (type == file) return trivial_parser_cache_file(path);
  else if (type == directory) return trivial_parser_cache_dir(path);
  else return NULL;
}

void trivial_deallocate_cache(NodeType type, void * p)
{
  if (p == NULL) return;
  if (type == file) delete [] ((char *) p);

  if (type == directory) {
    Queue<char *> * q = (Queue<char *> *) p;
    while (q -> size () > 0) {
      delete [] (q -> pop());
    }
    delete q;
  }
}

