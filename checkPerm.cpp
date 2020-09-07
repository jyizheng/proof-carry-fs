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

#include <time.h>
#include "parse-check/pcfs-syn.hpp"
#include "checkPerm.hpp"
#include "pcfs-runtime.hpp"
#include "paths-common.hpp"
#include "parse-check/checkProCapConditions.hpp"
#include "pcfs-procaps.hpp"
#include "Thread.hpp"

/* 
   Function to check permissions when access is to a file in
   <src>/#config. In this case special rules apply. See comments in
   pcfs-main.cpp for details. The rules are:

   1. All access to root and SYSTEM_UID for any file/directory
      beginning with /#config.

   2. Read and execute access to everyone for all files/directories in
      /#config, except /#config/shared-key

   3. All access to uid for any file/directory starting with
      /#config/procaps/<uid>


   This function takes three arguments.  

   path: must have the form /#config... 

   uid: uid of the calling process

   perm: the permission to check 

   Return 1 on success, 0 on failure.
*/

static int checkPermConfig(const char * path, int uid, Term * perm) 
{
  // If uid is root or SYSTEM_UID, skip all checks and return 1
  // immediately.
  if (uid == 0 || uid == global_uid_of_system) {
    (*global_log_stream) <<  "Succeeding: Path starts with /#config, and uid is 0 (root) or SYSTEM_UID.\n";
    return 1;
  }

  // We can assume that path begins with /#config. We want to skip
  // this prefix. Since strlen ("/#config") == 8, we increment path by
  // 8.

  path = path + 8; 
  
  // Now if next character on path is '\0', then the path was exactly
  // "/#config". So we need to check that perm is either execute or
  // read.

  if (*path == '\0') {
    if (perm -> equal (&global_perm_execute) || perm -> equal (&global_perm_read)) {
      (*global_log_stream) <<  "Succeeding: Path is /#config and permission is execute or read.\n";
      return 1;
    }

    else {
      (*global_log_stream) << "Failing: Path is /#config, permission is neither execute nor read, " 
			   << "and uid is neither root nor SYSTEM_UID.\n";
      return 0;
    }
  }
  
  // Next check if the remaining path begins with "/procaps".

  if (isAncestor ("/procaps", path)) {
    // Path begins with "/#config/procaps

    // Skip the prefix "/procaps"
    path = path + 8;

    // If path is now '\0', then it was exactly "/#config/procaps"
    if (*path == '\0') {
      if (perm -> equal (&global_perm_execute) || perm -> equal (&global_perm_read)) {
	(*global_log_stream) << "Succeeding: Path is /#config/procaps and permission is execute or read.\n";
	return 1;
      }
      
      else {
	(*global_log_stream) << "Failing: Path is /#config/procaps, permission is neither execute nor read, "
			     << "and uid is neither root nor SYSTEM_UID.\n";
	return 0;
      }
    }

    else {
      // Path has the form /#config/procaps/X, where X is
      // non-empty. If X has ancestor <uid>, then allow access, else
      // deny it.

      // First convert uid to a string. uid_str is a buffer to hold
      // the string. We will write uid to its FAR END, starting at
      // uid_str[18] (uid_str[19] is '\0') and moving back. This is
      // because it is easier to convert a number to a string
      // backwards.

      char uid_str [20];
      uid_str[19] = '\0' ;
      int uid_count = 18;
      while (uid > 0) {
	uid_str[uid_count--] = (uid % 10) + '0';
	uid = uid / 10;
      }

      // Prepend a '/' to uid_str
      uid_str[uid_count] = '/';
      
      // At this point, uid_str + uid_count points to "/<uid>".
      const char * uid_s = uid_str + uid_count;

      // Check if path starts with /<uid>
      if (isAncestor (uid_s, path)) {
	// Allow access
	(*global_log_stream) << "Succeeding: Path begins with /#config/procaps/<uid>.\n";
	return 1;
      }
      
      else {
	// Deny access
	*global_log_stream << "Failing: Path begins with /#config/procaps, but not with /#config/procaps/<uid>, "
			   << "and uid is neither root nor SYSTEM_UID.\n";
	return 0;
      }
    }
  }

  else {
    // Path is something of the form /#config/X where X does not start
    // with procaps/. Check if it starts with shared-key.

    if (isAncestor("/shared-key", path)) {
      // Okay, so path is /#config/shared-key. Since uid is neither
      // root, nor SYSTEM_UID, deny access.
      *global_log_stream << "Failing: Path begins with /#config/shared-key, "
			 << "and uid is neither root nor SYSTEM_UID.\n";
      return 0;
    }

    else {
      // Path is some other file/directory in /#config. So allow read
      // and execute access only.

      if (perm -> equal (&global_perm_execute) || perm -> equal (&global_perm_read)) {
	(*global_log_stream) << "Succeeding: Path is in /#config/, but is not /#config/shared-key, nor is it "
			     << "/#config/procaps/* and permission is execute or read.\n";
	return 1;
      }
      else {
	(*global_log_stream) << "Failing: Path is in /#config/, but not /#config/procaps/<uid>/, "
			     << "uid is neither root nor SYSTEM_UID and permission is not execute or read.\n";
	
	return 0;
      }
    }   
  }
}

/*
  Function to check permissions on non-configuration files (i.e., most
  files). This function will be called when path does not start with
  /#config, and is not "/". In this case the call will eventually get
  redirected to the directory <src>/. This function takes three
  arguments:

  path: path of file being accessed, relative to the pcfs file
  system. Must start with '/'

  uid: uid of calling process
  
  perm: permission to check. Must be an instance of TermExecute,
  TermIdentity, TermRead, TermGovern, or TermWrite.

  Returns 1 if permission is granted, 0 if it is not.

  The function works as follows. 

  If uid is 0 (root), it returns 1 (allowed) immediately.
  
  If uid is SYSTEM_UID: if permission is execute it returns 1 immediately, else it returns 0 (denied) immediately.
  
  If uid is not zero, but path is "/", it returns 1 iff perm is
  execute or read, else it returns 0.

  If uid is not zero, it looks for the procap
  <src>/#config/procaps/<uid>/<path>.perms/<perm>, using the cache. If
  a procap is found, its constraints are checked. If that succeeds it
  is checked that the procap authorizes the correct file, uid and
  permission. If all this succeeds 1 is returned, else 0 is returned.
  
 */
static int checkPermData (const char * path, int uid, Term * perm)
{
  // If uid is root, skip all checks and return 1 immediately.
  if (uid == 0) {
    *global_log_stream << "Succeeding: uid is 0 (root).\n";
    return 1;
  }
  
  // If uid is SYSTEM_UID, and permission is execute return 1 immediately
  // If uid is SYSTEM_UID, and permission is not execute return 0 immediately
  if (uid == global_uid_of_system) {
    if (perm -> equal (&global_perm_execute)) {
      *global_log_stream << "Succeeding: Path is not / or /#config, uid is SYSTEM_UID, and permission is execute.\n";
      return 1;
    }
    else {
      *global_log_stream << "Failing: Path is not / or /#config, uid is SYSTEM_UID, and permission is not execute.\n";
      return 0;
    }
  }

  // uid is not root or SYSTEM_UID, so we must lookup a procap

  // Create the path where ProCap is located. This path will be used
  // to lookup the cache.
  charbuf fullpath(MAX_PATH_LENGTH);
  
  makeProCapPathForCache(path, uid, perm, & fullpath);

  *global_log_stream << "Looking for ProCap at location "
		     << fullpath.getHead()
		     << "\n";

  // Find the ProCap
  ProCap * procap = (ProCap *) (global_procap_cache -> getData (fullpath.getHead())); 

  if (procap == NULL) {
    *global_log_stream << "Failing: ProCap not found, procap unparseable, mac failure, or procap does not typecheck.\n";
    return 0;
  }

  // Check the ProCap
  TermPrim_date2time * vctime = 
    new TermPrim_date2time (new TermPrim_date (new  NativeTime ((long) time(NULL))));

  int check = checkProCapConditions (procap, global_src, vctime);

  delete vctime;

  if (!check) {
    *global_log_stream << "Failing: ProCap conditions could not be verified.\n";
    return 0;
  }

  // Check that the permission allowed in the ProCap matches <perm>
  if (! procap -> p -> equal (perm)) {
    *global_log_stream << "Failing: ProCap authorizes a different permission.\n";
    return 0;
  }

  // Check that the uid authorized in the ProCap matches <uid>

  if (procap -> k -> constr == constr_prim_int2principal) {

    Term * arg = ((TermPrim_int2principal *) (procap -> k)) -> arg1;

    if (arg -> constr == constr_prim_int) {
      int u = ((TermPrim_int *) arg) -> i;
      
      if (uid != u) { 
	*global_log_stream << "Failing: ProCap authorizes a different uid.\n";
	return 0;      
      }
    }
    else {
      *global_log_stream << "Failing: ProCap authorizes some unrecognized uid.\n";
      return 0;
    }
  }
  else {
    *global_log_stream << "Failing: ProCap authorizes some unrecognized uid.\n";
    return 0;
  }

  // Check that the file authorized in the ProCap matches
  // <path>. (Note that ProCaps are required to authorize absolute
  // paths, relative to <src>, which is also the exact form of <path>,
  // so we can syntactically match them)


  if (procap -> f -> constr == constr_prim_str2file) {

    Term * arg = ((TermPrim_str2file *) (procap -> f)) -> arg1;

    if (arg -> constr == constr_prim_str) {
      const char * str = ((TermPrim_str *) arg) -> str;
      
      if (strcmp(str, path)) { 
	*global_log_stream << "Failing: ProCap authorizes a different file/directory.\n";
	return 0;      
      }
    }
    else {
      *global_log_stream << "Failing: ProCap authorizes some unrecognized file/directory.\n";
      return 0;
    }
  }
  else {
    *global_log_stream << "Failing: ProCap authorizes some unrecognized file/directory.\n";
    return 0;
  }

  *global_log_stream << "Succeeding: Procap exists and checks.\n";
  return 1;
}


/* Function to check permission perm for the / directory of file
   system. Returns 1 if allowed, zero otherwise. The rules used to
   decide is simple:

   1. If uid is root (0) access is immediately allowed
   2. Else, if permission is execute or read, permission is allowed, else denied

 */
int checkPermRoot (int uid, Term * perm) 
{
  // If uid is root, allow 
  if (uid == 0) {
    *global_log_stream << "Succeeding: uid is 0 (root).\n";
    return 1;
  }

  // Else, check if permission is excute or read (then allow, else deny)
  if (perm -> equal (&global_perm_execute) || perm -> equal (&global_perm_read)) {
    *global_log_stream << "Succeeding: path is / and permission requested is read or execute.\n";
    return 1;
  }
  else {
    *global_log_stream << "Failing: path is /, but permission is not read or execute, and uid is not root.\n";
    return 0;
  }
}

/* Function to check given permission on a given path for a given
   user. The three arguments are the path, uid of user, and permission
   to check. The function will determine whether the call is to a
   configuration file or to a regular file/directory by checking
   isAncestor("/#config", path). If the check is true, this function
   calls checkPermConfig, else it calls checkPermData. path should not
   have a trailing '/', but must have a leading '/'.
*/

int checkPerm (const char * path, int uid, Term * perm)
{
  // Write entry to log
  *global_log_stream << "Checking permission ("
		     << perm
    		     << ") for uid "
		     << uid
		     << " on file "
		     << path
		     << "\n";

  if (isRoot (path)) {
    return checkPermRoot (uid, perm);
  }
  
  else if (isAncestor("/#config", path)) {
    return checkPermConfig (path, uid, perm);
  }

  else {
    return checkPermData (path, uid, perm);
  }
}

