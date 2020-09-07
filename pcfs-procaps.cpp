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

#include <fstream>
#include <iostream>

#include <sys/time.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>
#include <dirent.h>

#include "parse-check/pcfs-time.hpp"
#include "parse-check/pcfs-syn.hpp"
#include "parse-check/pcfs-file-parsers.hpp"

#include "pcfs-procaps.hpp"
#include "paths-common.hpp"
#include "crypto-common.hpp"
#include "charbuf.hpp"
#include "fsutils.hpp"

using namespace std;

/*
  Writes <uid>/<path>.perm.<perm> to <buf>, assuming that <path>
  starts with '/'. This is the exact path that must be sent to the
  ProCap cache to lookup the ProCap authorizing <uid> permission
  <perm> to <path>. (The exact location of the ProCap is
  <src>/#config/procaps/<uid>/<path>.perm.<perm>. However the
  rootPath of the ProCap cache is <src>/#config/procaps/, so we need
  to send only /<uid>/<path>.perm.<perm> to it)
*/

void makeProCapPathForCache(const char * path, int uid, Term * perm, charbuf * buf)
{
  
  charbuf & out = *buf;
  out << uid ;
  out << path;
  out << ".perm.";

  switch( perm -> constr ) {

  case constr_identity:
    out << "identity";
    break;

  case constr_govern:
    out << "govern";
    break;

  case constr_execute:
    out << "execute";
    break;

  case constr_write:
    out << "write";
    break;

  case constr_read:
    out << "read";
    break;
    
  default:
    // Should never happen
    break;
  }
}

//-----------------------------------------------------------------------------------------


ProCap * makeDefaultProCap(const char * path, int uid, Term * perm, bool skipAttributeCheck)
{
  int path_length = strlen(path);
  // Term representing path
  char * pp = new char[path_length + 1];
  strcpy(pp, path);
  Term * f = new TermPrim_str2file (new TermPrim_str (pp));

  // Term representing uid
  Term * k = new TermPrim_int2principal (new TermPrim_int (uid));

  // Term representing perm
  Term * p = NULL;
  switch (perm -> constr) {

  case constr_identity:
    p = new TermIdentity;
    break;

  case constr_govern:
    p = new TermGovern;
    break;

  case constr_execute:
    p = new TermExecute;
    break;

  case constr_read:
    p = new TermRead;
    break;

  case constr_write:
    p = new TermWrite;
    break;

  default:
    // Error
    delete f;
    delete k;
    return NULL;
  }

  // Hypothetical Constraints

  long curr_time = (long) (time(NULL));

  Queue <HypConstraint *> * hcs = new Queue<HypConstraint *>;
  
  hcs -> push (new HypConstraint 
	       (new Queue <Constraint *>,
		new ConstraintLeq
		(new TermPrim_date2time (new TermPrim_date (new NativeTime (curr_time))),
		 new TermCtime)));

  hcs -> push (new HypConstraint 
	       (new Queue <Constraint *>,
		new ConstraintLeq
		(new TermCtime,
		 new TermPrim_date2time (new TermPrim_date 
					 (new NativeTime (curr_time + 30 * 24 * 60 * 60))))));

  
  // Make the Qhcl
  Qhcl * q = new Qhcl (new Queue<IdSortPair *>, hcs);

  // Make the state list
  Queue <State *> * ss = new Queue <State *> ;

  char * ff = new char [path_length + 1];
  strcpy(ff, path);

  char * attr_name = new char [8];
  strcpy (attr_name, "newfile");
  
  if (!skipAttributeCheck) {
    ss -> push (new StateHas_xattr (new TermPrim_str2file (new TermPrim_str(ff)),
				    new TermPrim_str (attr_name),
				    new TermPrim_int (1)));
  }

  // Make an empty MAC
  char * mac = new char[1];
  mac[0]  = '\0';

  return new ProCap (k, f, p, q, ss, mac);
}

//---------------------------------------------------------------------------------------------


int writeProCapToFile (ProCap * procap, const char * path, int uid, Term * perm, 
		       const char * prefix, unsigned char * mackey)
{
  // First check that prefix exists and is a directory
  struct stat statbuf;
  
  if (lstat(prefix, &statbuf) != 0 || !S_ISDIR(statbuf.st_mode)) {
    return 0;
  }

  // Write the full path of the procap's location to a new buffer,
  // fullpath
  charbuf fullpath(MAX_PATH_LENGTH);
  
  fullpath << prefix << '/';
  makeProCapPathForCache (path, uid, perm, &fullpath);

  // check that procap path exists, or create a new file there.
  int r = checkOrCreatePath (fullpath.getHead(), type_file, uid);

  if (r == 0) return 0;

  // Write the procap to a new buffer.
  charbuf pcbuf(8192); // An 8K buffer

  procap -> printSpecial (pcbuf, true); 
  // The true indicates that we do not want to print the procap's mac


  // Now we have to generate the MAC. So we need to read the ProCap,
  // and find its first non-white character
  char * startText = pcbuf.getHead();

  // Skip to first non-whitespace character
  while (*startText == ' ' || *startText == '\t' 
	 || *startText == '\n' || *startText == '\r') {
    startText++;
  }

  // Next we need to get to the end of the procap.
  char * endText = pcbuf.getTail();
  
  int textLength = endText - startText;
  
  // Compute the MAC
  unsigned char mac[MAX_MAC_LENGTH];
  int size;
  computeHMAC(mackey, SYM_KEY_LENGTH, startText, textLength, mac, &size);
  
  if (size != MAC_LENGTH) {
    return 0; // This should not happen
  }

  // Convert MAC to hex.
  char machex[2 * MAX_MAC_LENGTH + 1];

  binaryToHex (mac,MAC_LENGTH, machex);
  machex[2 * MAC_LENGTH] = '\0';  
  
  // Write MAC to buffer.
  pcbuf << "#\n" << machex << '\n';

  // Write procap to file

  int fd = open (fullpath.getHead(), O_WRONLY | O_TRUNC, S_IRUSR | S_IWUSR);
  if (fd == -1) return 0;

  r = pcbuf.writeToFile (fd);

  chown (fullpath.getHead(), uid, -1);

  close (fd);
  
  return r;
  
}


void removeProCaps (const char * path, const char * procaps, logstream * out)
{
  (*out) << "(removeProCaps) path is: " << path << "\n";

  if (isRoot(path) || isAncestor ("/#config", path)) {
    (*out) << "Path is / or starts with /#config ... nothing to do.\n";
    return;
  }

  charbuf buf(MAX_PATH_LENGTH);
  buf << procaps;

  // Open the procaps directory, so we can read what is in it.

  DIR * dirp = opendir (procaps);

  if (dirp == NULL) {
    return ;
  }

  struct dirent * ent;

  while ((ent = readdir(dirp)) != NULL) {
    const char * uid = ent -> d_name;
    
    if (!strcmp (uid, ".") || !strcmp(uid, "..")) {
      continue;
    }

    buf << '/' << (uid); // buf contains procaps/uid

    struct stat stbuf;
    
    if (lstat(buf.getHead(), &stbuf) == 0) {

      if (S_ISDIR(stbuf.st_mode)) {
	// uid is a directory.

	charbuf fullpath(MAX_PATH_LENGTH);
	fullpath << (buf.getHead()) << (path);
	// fullpath contains procaps/uid/path

	recursiveRemove (& fullpath, out);


	fullpath << ".perm";

	fullpath << ".read";
	recursiveRemove (& fullpath, out);
	fullpath.rollback('.');

	fullpath << ".execute";
	recursiveRemove (& fullpath, out);
	fullpath.rollback('.');

	fullpath << ".write";
	recursiveRemove (& fullpath, out);
	fullpath.rollback('.');

	fullpath << ".govern";
	recursiveRemove (& fullpath, out);
	fullpath.rollback('.');

	fullpath << ".identity";
	recursiveRemove (& fullpath, out);
	//	fullpath.rollback('.');

      }

    }

    buf.rollback('/'); // buf contains procaps

  }

  closedir(dirp);

}
