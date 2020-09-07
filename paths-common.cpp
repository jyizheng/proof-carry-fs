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

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <fcntl.h>

#include <string.h>
#include <stdlib.h>

#include <iostream>

#include "paths-common.hpp"

using namespace std;

const char * findFirstComponent(const char * path, char * buf, int buflen) {
  int count = 0;
  while (*path != '\0' && *path != '/' && count < buflen) 
    buf[count++] = *(path++);
  
  if (count == buflen)  // Not enough memory
    return NULL;

  buf[count] = '\0';
  return path;  // Return a pointer to the next component on path
}

int removeLastComponentOfPath(char * path) 
{
  char * lastSlash = NULL;
  while (*path != '\0') {
    if (*path == '/' && *(path + 1) != '\0') {
      lastSlash = path;
    }
    path ++;
  }
  
  if (lastSlash == NULL) return PATHS_ERR_FORMAT;
  
  *lastSlash = '\0';
  return 1;
}

int addNewComponentToPath(char * path, const char * pathname)
{
  // First find the position of terminating '/' or '\0' in path

  char * ptr = path;
  while (*ptr != '\0') ptr ++;

  // if path == "" return error
  if (ptr == path) return PATHS_ERR_FORMAT;
  
  char * restorept = ptr;

  if (*(ptr - 1) != '/') {
    *ptr = '/';
    ptr ++;
  }

  while (*pathname != '\0' && *pathname != '/') {
    *(ptr ++) = *(pathname ++);
  }

  if (*pathname == '\0') {
    *ptr = '\0';
    return 1;
  }
  else { 
    // *pathname == '/'
    // Now check if this was at the end or not

    if (*(pathname + 1) == '\0') {
      *(ptr ++) = '/';
      *ptr = '\0';
      return 1;
    }
    else {
      // Error
      *restorept = '\0';
      return PATHS_ERR_FORMAT;
    }
  }
}

int checkOrCreatePath(char * path, PathType type, int uid)
{
  char * iter = path;
  
  bool done = false;

  // Skip a leading '/', since '/' always exists
  if (*iter == '/') iter ++;

  while (!done) {

    while (*iter != '/' && *iter != '\0') iter ++;
    
    if (*iter == '\0' || *(iter + 1) == '\0') {
      // We have reached the last component, so break the loop
      done = true;
    }
    else {
      // We have not reached the last component yet. It must be the
      // case that *iter = '/'. Temporarily set *iter to '\0'.
      *iter = '\0';
      
      // Check if path exists. Note that since we set *iter to '\0',
      // we are only checking a subpath here.

      struct stat statbuf;

      // cerr << "Checking for directory: " << path;
      if (!lstat(path, &statbuf)) {
	// Path exists, check that it is a directory
	
	if (S_ISDIR(statbuf.st_mode)) {
	  // cerr << "... found.\n";
	  // Path is a directory, so just continue
	  ;
	}
	
	else {
	  // Path is not a directory, but it exists, so we have a problem
	  // cerr << "... found file not directory.\n";
	  * iter = '/';
	  return 0;
	}

      }

      else {
	// Path does not exist, so create a directory
	
	// cerr << "... not found. Creating";
	if (!mkdir (path, 0700)) {
	  // Directory created, so try to set ownership to uid
	  chown (path, uid, -1);
	  // cerr << "...okay\n";
	}
	else {
	  // Could not create directory. This is an error
	  // cerr << "...error creating.\n";
	  * iter = '/';
	  return 0;
	}
      }

      *iter = '/';
    }
    
    iter ++; // Skip the '/'
  }

  // Now we are at the last component. Check that it exists
  
  // cerr << "Checking for file/directory: " << path;
  struct stat statbuf;
  
  if (!lstat(path, &statbuf)) {
    // Path exists. Check its type
    // cerr << "...found. Checking type";

    if ((type == type_file && S_ISREG(statbuf.st_mode)) ||
	(type == type_directory && S_ISDIR(statbuf.st_mode))) {

      // Types match, so continue
      // cerr << "...okay\n";
      ;
    }

    else {

      // Types do not match. This is an error.
      // cerr << "...type mismatch\n";
      return 0;
    }
  }

  else {
    
    // Path does not exist, so create it
    
    // cerr << "...not found. Creating";
    if (type == type_file) {
      
      umask(0);
      int fd = creat (path, 0600);
      
      if (fd == -1) {
	// Could not create file, so this is an error
	// cerr << "...error creating.\n";
	return 0;
      }
      else {
	// File successfully created
	// cerr << "...okay.\n";
	chown (path, uid, -1);
	close (fd);
      }
    }

    else {
      if (!mkdir (path, 0777)) {
	  // Directory created, so continue
	// cerr << "...okay.\n";
	chown (path, uid, -1);
      }
      else {
	// Could not create directory. This is an error
	// cerr << "...error creating.\n";
	return 0;
      }
    }
  }

  return 1;
}

/*
int isAncestor(const char * ac, const char * ch)
{
  int ac_length = strlen(ac);
  int ch_length = strlen(ch);
  
  if (ac[ac_length - 1] == '/') ac_length --;

  return ((ac_length <= ch_length) &&
	  (!strncmp (ac, ch, ac_length)) &&
	  (ch[ac_length] == '/' || ch[ac_length] == '\0'));
}

*/

int isAncestor(const char * ac, const char * ch)
{
  // Match ac and ch till either of them ends or they fail to match

  // while (*ac == *ch && *ac != '\0' && *ch != '\0') {
  
  while (*ac == *ch && *ac != '\0') { 
    // This check is equivalent to the one commented above, but faster
    
    ac ++;
    ch ++;
  }

  // If ac ended, then we must only check that ch is at the end of a
  // component. This is to eliminate matches like /abc/def <=
  // /abc/defghi. There are two possibilities: if *(ac - 1) == '/',
  // then *ch is always at the end of a component (because the last
  // character matched was '/'). Else, we must check that *ch is either
  // '/' or '\0'.

  if (*ac == '\0') {

    if ( *(ac - 1) == '/') 
      return 1;
    
    if (*ch == '/' || *ch == '\0')
      return 1;
    else 
      return 0;

  }
  
  // If ch ended, it better be the case *ac == '\0', or at the most
  // have an extra trailing '/'. The case where *ac == '\0' is already
  // covered above. So we need only check that *ac == '/' and *(ac +
  // 1) == '\0'.

  if (*ch == '\0') {
    if (*ac == '/' && *(ac + 1) == '\0') 
      return 1;
    else 
      return 0;
  }

  // If *ac != *ch, and both *ac and *ch are not '\0', there is no
  // possibility that ac is an ancestor of ch.
  return 0;
}


/*

// Test cases for isAncestor
 
int main()
{
  const char acs[11][100] = {"/usr0/abc",
			     "/usr0/abc",
			     "/usr0/abc/",
			     "/usr0/abc/", 
			     "/usr0/abc", 
			     "/usr0/abc", 
			     "/usr0/abc/", 
			     "/usr0/abcde", 
			     "/usr0/abcde/",
			     "/usr0/abc/def",
			     "/usr0/abc/def"};

  const char chs[11][100] = {"/usr0/abcdef", 
			     "/usr0/abc",
			     "/usr0/abc/",
			     "/usr0/abc", 
			     "/usr0/abc/", 
			     "/usr0/abc/def", 
			     "/usr0/abc/def", 
			     "/usr0/abc", 
			     "/usr0/abc/",
			     "/usr0/abc",
			     "/usr0/abc/"};

  int i;
  for (i = 0; i < 11 ; i ++ ) {
    cerr << "Checking if " << acs[i] << " is an ancestor of " << chs[i] << "...";
    cerr << (isAncestor (acs[i], chs[i]) == 1 ? "yes" : "no") << "\n";
  }
}

*/
