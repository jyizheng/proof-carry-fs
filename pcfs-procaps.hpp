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

#ifndef __PCFS_PROCAPS_HPP
#define __PCFS_PROCAPS_HPP

#include <iostream>
#include "parse-check/pcfs-syn.hpp"
#include "logstream.hpp"

using namespace std;

/* Writes <uid>/<path>.perm.<perm> to <buf>, assuming that <path>
   starts with '/'. This is the exact path that must be sent to the
   ProCap cache to lookup the ProCap authorizing <uid> permission
   <perm> to <path>. (The exact location of the ProCap is
   <src>/#config/procaps/<uid>/<path>.perm.<perm>. However the
   rootPath of the ProCap cache is <src>/#config/procaps/, so we need
   to send only /<uid>/<path>.perm.<perm>)
*/
void makeProCapPathForCache(const char * path, int uid, Term * perm, charbuf * buf);

/* 
   Create a default ProCap authorizing permission perm to user uid for
   path. The ProCap is dynamically allocated and must be freed by
   caller. A default procap gives access for 30 days from date of
   creation of procap (i.e., the current time), and includes a check
   to ensure that the pcfs attribute newfile (i.e. the actual
   attribute user.#pcfs.newfile) is set to (TermPrim_int 1). If the
   last argument is true, the attribute check will be skipped.

   ** This function will not create the MAC on the procap **. Instead,
   it will set the MAC field to "" (the empty string).

*/

ProCap * makeDefaultProCap (const char * path, int uid, Term * perm, bool skipAttributeCheck);


/*
  This function writes procap to the file
  <prefix>/<uid>/<path>.perm.<perm>. Returns 1 on success, 0 on
  error. If any component of <uid>/<path>.perm.<perm> is missing, it
  is created. New components are created with ownership and umask of
  the calling process. If any existing component has a wrong type, an
  error is returned. If <prefix>/ is missing or is not a directory, an
  error is returned. Note that if a file named
  <prefix>/<uid>/<path>.perm.<perm> exists, it will be
  overwritten. If any permissions are denied, an error is returned.

  This function will ignore the "MAC" field of the ProCap, if any, and
  will instead compute the MAC on its own (after writing the rest of
  the ProCap to the file).

  Arguments:

  procap -- procap to write

  path -- path that is authorized by the procap. Must start with /
  
  uid -- uid authorized by the procap.

  perm -- permission authorized by the procap.

  prefix -- directory prepended to path to write procap. Must NOT end in /

  mackey -- the key to be used to create the mac on the procap. Must
  have length SYM_KEY_LENGTH.
  
 */
int writeProCapToFile (ProCap * procap, const char * path, int uid, Term * perm, 
		       const char * prefix, unsigned char * mackey);


/* 
   This function will attempt to remove all ProCaps associated with
   'path'. 'path' must be relative to the <src> directory of the file
   system, and must start with / and must not end with a / unless the
   path is exactly "/". The second argument 'procaps' must be the path
   to the procaps directory i.e. it must equal
   <src>/#config/procaps. Thus it is an absolute path on the
   underlying FS. It must not end in a /.

   Specifically, this function does the following:

   foreach (directory d in 'procaps') { // d must represent a uid
       recursiveRemove ( 'procaps'/d/'path'.perm.* );
       recursiveRemove ( 'procaps'/d/'path' );
   }
   
   if path is "/" or starts with "/#config", the function will do
   nothing at all.

*/

void removeProCaps (const char * path, const char * procaps, logstream * out);

#endif
