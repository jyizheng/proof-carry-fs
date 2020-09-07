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

#ifndef __PATHS_COMMON_HPP
#define __PATHS_COMMON_HPP

#define MAX_PATH_LENGTH 1024         // Maximum length of a path
#define MAX_COMPONENT_LENGTH 256     // Maximum length of a directory or file name

#define PATHS_ERR_NOMEM -1
#define PATHS_ERR_FORMAT -2

// Finds first component of path and stores in buf. Returns a pointer
// to the character immediately following the first component in path
// (or NULL if buffer is too small to contain it). Usually this
// character will be a '/'. It may be '\0' if path had no slashes.
const char * findFirstComponent(const char * path, char * buf, int buflen);

// The following functions modify a given path: they add/subtract a
// component at the end. These are designed to be very fast -- they
// are guaranteed to make one pass only

// Remove the last component of a path. The resulting path is
// guaranteed not to have a trailing '/'.  Returns 1 if successful,
// error code if not. In particular, if there is only one component,
// then it will return an error
int removeLastComponentOfPath(char * path) ;

// Adds a new component to the path (does not check for size violations)
// Return 1 on success, error code on failure. In particular, if the
// new component has a slash (except at the end), it will return an
// error. The resulting path will have a slash at the end if and only
// if newcomponent has a slash at the end
int addNewComponentToPath(char * path, const char * newcomponent);


// An enumeration type to indicate files and directories.
enum PathType {type_file = 0, type_directory = 1};


// This function checks that path exists and is of type. If the check
// fails, it attempts to create path, by creating any sub components
// if necessary as well. The program will try to set the uid of any
// new component to uid, although this may fail, and the function will
// not complain. If uid is -1, no change is made, and all
// directories/files created have the uid of the calling
// process. Returns 1 on success, 0 on failure. Failure will usually
// happen only if permissions are denied, or path exists but its type
// does not match type, or if one of the components except the last in
// path is actually a file.
int checkOrCreatePath(char * path, PathType type, int uid = -1);

// Check that path ac is an ancestor directory of path ch. Return 1 if
// this is the case, 0 otherwise. Does not check paths for
// format. This function is highly unoptimized.
int isAncestor(const char * ac, const char * ch);



// Check whether a given path is root "/". The function does not check
// for format. Thus a path is root iff it has exactly one character
// (which has to be '/' if the path is well formed)

inline int isRoot(const char * path) {
  return (path[1] == '\0');
}

#endif
