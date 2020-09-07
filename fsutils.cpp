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

#include <iostream>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>

#include "charbuf.hpp"
#include "fsutils.hpp"

using namespace std;

// Recursively remove given path and all its subpaths. The charbuf
// path must be of length MAX_PATH_LENGTH.

void recursiveRemove (charbuf * path, logstream * out) 
{
  const char * p = path -> getHead() ;

  struct stat stbuf;

  if (lstat(p, &stbuf) != 0) return ;

  else if (!S_ISDIR(stbuf.st_mode)) {
    // p is not a directory

    (*out) << "Deleting: " << p << "\n";

    unlink (p);
    return;
  }

  // p is a directory
  DIR * dirp = opendir(p);

  if (dirp == NULL) return;
  
  struct dirent * ent;
  while ((ent = readdir(dirp)) != NULL) {
    const char * name = ent -> d_name;

    if (!strcmp (name, ".") || !strcmp (name, ".."))
      continue;

    (*path) << '/' << name;

    recursiveRemove(path, out);

    path -> rollback ('/');
  }

  closedir(dirp);

  (*out) << "Deleting: " << p << "\n";

  rmdir (p);
}


void recursiveRemove (const char * path, logstream * out)
{
  charbuf buf(MAX_PATH_LENGTH);
  buf << path;
  recursiveRemove (&buf, out);
}

