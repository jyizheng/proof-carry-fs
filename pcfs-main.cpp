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

/* The main program of pcfs, that mounts a file system.

   The program is used as follows (only to be run as root):

   sudo ./pcfs-main <src> <mount point>

   <src> is the directory that contains the pcfs-file system. It must
   be specified as a full pathname (relative path names are not
   allowed). <mount point> is the point where the file system will be
   visible. This must also be a full pathname. In general, any calls
   made via <mount point> will be directed to <src> after appropriate
   access control checks.

   

   Structure of <src>.

   A pcfs file system requires a specific structure. 

   <src>/#config/ -- This directory contains the pcfs configuration
   data as well as the procap store. 

   <src>/ -- All files/directories in <src>/ other than #config are
   assumed to be data that is subject to access control checks and
   visible at <mount point>/ (#config is also visible at <mount
   point>/ but is subject to special checks listed below)

   The #config directory must contain the following:

   <src>/#config/config-file -- This file contains the configuration
   data for pcfs. This data is public: anyone may read it.

   <src>/#config/declarations -- This file contains declarations of
   sorts, term constructors, state constructors, and constraint
   constructors. It is parsed using the function
   loadFileDeclarations(). This file must also be protected from
   tampering by allowing write access to the root only on the
   underlying file system. Note however, that there is no need to
   restrict read access to this file, and in fact, ordinary users may
   have a need to know the contents of this file.

   <src>/#config/shared-key -- This file contains the secret key
   shared between the verifier and PCFS that is used to encrypt
   procaps. It must be protected at all times, i.e., on the underlying
   system, only root should have any access to it. PCFS will only
   allow SYSTEM_UID to read this file.

   <src>/#config/ca-pubkey.pem -- This file contains the public key of
   the certifying authority who certifies keys of other
   principals. The key is PEM encoded with header lines "-----BEGIN
   PUBLIC KEY-----" and "-----END PUBLIC KEY-----". It is readable by
   everyone.

   <src>/#config/procaps/ -- This directory contains the procaps. The
   details of how procaps are stored follow later. There is nothing
   secret per say about procaps and they may be left unprotected in
   the underlying file system. However, in order to prevent sabotage
   by other users, it is recommended that on the underlying file
   system, this directory also remain under the control of root only.

   

   //--------------------------------------


   Configuration file <src>/#config/config-file

   This file has a simple structure. It consists of attribute-value
   pairs written as follows (one pair per line):

   <attribute> <value>

   The attribute and value may be separated by any amount of
   whitespace (space and tab) but not lines. Both attributes and
   values are case sensitive and spaces are not allowed in
   either. Lines whose first non-whitespace character is a # are
   assumed to be comments and are ignored. (Note that it is impossible
   to put a comment at the end of a line because # is a valid
   character in a file name and may occur in the log file or temporary
   directory).

   The following attributes must be defined at the least:

   LOG_FILE    Full path to logfile on the underlying file system. This
               file may be anywhere on the underlying system. If it
               does not exist, it will be created with no data.

   ADMIN_UID   uid of the user "admin" who gives all access. This is a
               positive integer

   SYSTEM_UID  uid of the user "system" who states all borderline
               facts, and runs the verifier. SYSTEM_UID has full
               access to procaps.
               
   TEMP_DIR    Full path on the underlying file system to a directory, in
               which temporary files may be created. This directory
               will be created if it is missing

   LOG_SIZE    Size of logfile in KB. Recommended value is 32768 (32 MB
               logfile). Note that the log is overwritten in LRU
               fashion, unless committed. So allocate enough
               space. Also note that log is held in memory, so do not
               allocate too much space.

   CACHE_SIZE  Number of entries to be held in procap
               cache. Recommended numbers are 20-100. Change this
               based on expected size of working file set. Each procap
               in cache may take a few KB of space.

   In addition the following optional flags may be defined. Flags
   don't take values and must be written on lines by themselves (if
   there is anything else after the flag, it is ignored). By default
   all flags are off. The sample configuration file generated will set
   the flags marked RECOMMENDED below.

   NO_LOG_COMMIT          Prevent committing the log on every operation. The
                          log is held in a memory buffer and written on
                          unmount. This makes the fs (slightly) faster but may
                          cause loss of log on crash. NOT RECOMMENDED

   NO_DATACHECK           Skips permission check (and logging) on read() and
                          write() calls. RECOMMENDED

   NO_INSERT_AUTO_PROCAPS Skips inserting default procaps. NOT RECOMMENDED

   NO_INSERT_ATTR_NEWFILE Skips insertion of attribute
                          "user.#pcfs.newfile"=1 in newly created
                          files/directories. Set this flag if needed
                          by policies. Generally if policies do not
                          distinguish between newly created files, and
                          old ones, set this flag. 

   NO_REMOVE_AUTO_PROCAPS Do not automatically remove procaps when
                          files/directories are removed. NOT RECOMMENDED

   None of these attributes may be defined twice. Any other
   attributes, if defined, will be ignored, and a warning message will
   be printed. If any of the required attributes are missing, it will
   result in an error, and mount will fail. Also note that the file
   system itself does not use TEMP_DIR. This is for future use.
   
   //---------------------------------------------------

   Structure of file <src>/#config/shared-key

   Any lines whose first non-whitespace character is #, and lines that
   have only whitespace are assumed to be comments and
   ignored. Besides comments, this file must contain only one usable
   piece of data: a 160 bit (40 hex digit) key written in hex. It must
   satisfy the regex [a-zA-Z0-9]{40}.

   //--------------------------------------------

   Access to data files and directories (i.e., files/directories in
   <src>/, with the exception of <src>/#config and <src>/ itself).

   This access is based on ProCaps, that are stored in the directory
   <src>/#config/procaps/. Within this directory, the procaps are
   indexed as follows:

   - The procap authorizing permission <p> (p = identity, govern,
     execute, read, or write) to user with user id <uid> for the file
     <path> is stored in the file
     <src>/#config/procaps/<uid>/<path>.perm.<p>.

   For example, the procap authorizing read permission to uid 500 for
   the file visible as <mount point>/foo/bar.txt (and actually present
   at <src>/foo/bar.txt) is stored in the file
   <src>/#config/procaps/500/foo/bar.txt.perm.read. Note that given a
   file/directory, a uid, and a permission, there can be at most one
   procap to allow access.

   An important exception to procap based permissions is the
   following: the user SYSTEM_UID always has execute permission on
   every file/directory. This is because SYSTEM_UID is the user that
   runs the proof verifier and hence it must be able to read
   attributes of files. Further, all other permissions (read, write,
   identity, govern) are denied to SYSTEM_UID in descendants of <src>/
   even if Procaps exist. This is done to prevent SYSTEM_UID from
   abusing its power and gaining control over user data.

   //---------------------------------------------

   The "root" directory <src>/ (visible at <mount point>/) has
   special permissions: root is allowed all access to it. Other users
   have only read and execute access. This directory is not subject to
   procap checks. 


   //---------------------------------------------

   Access to <src>/#config. 

   Special access rules apply to calls made to <src>/#config. These
   are the following: 

   1. All access to root and SYSTEM_UID for any file/directory
   beginning with /#config.

   2. All access (including govern) to <uid> for any file/directory
   starting with /#config/procaps/<uid>

   3. Read and execute access to everyone for all files and
   directories immediately in /#config (including itself) except
   /#config/shared-key. Access to the latter is allowed only by (1).

   
   //-----------------------------------------------

   Summary of access:

   - If call is made to <mount point>/ , it is redirected to <src>/
     with checks for "root" directory as described above

   - If call is made to <mount point>/X where the first component in X
     does not start with #config and X in not "", the call is
     redirected to <src>/X, with procap based checks. The only
     exception is that SYSTEM_UID has execute permission on all
     files/directories regardless of procaps.
   
   - If call is made to <mount point>/X where X has ancestor #config/,
     it is redirected to <src>/X with checks as described in (1)--(3)


   //------------------------------------------------

   Roles of ADMIN_UID and SYSTEM_UID.

   SYSTEM_UID is a special priviledged and trusted principal. Although
   it never gets any access to user files (or even their metadata),
   i.e., it cannot run any fs operation in <mount point>/, it has all
   permissions over /#config and its subdirectories. In particular it
   can do anything in /#config/procaps, which means that it can set
   and remove procaps (as described in (3) above, individual users
   have full access to their *own* procaps). As a result, the proof
   verifier which generates and injects procaps must run with
   SYSTEM_UID uid. For this reason SYSTEM_UID must have acces to the
   symmetric key. PCFS will give it this access, but it may also be
   given access outside. In addition SYSTEM_UID gets execute access on
   all files/directories so that it can read file attributes to verify
   proofs.

   Note that in order to prevent the SYSTEM_UID from injecting procaps
   for itself, the file system prohibits SYSTEM_UID from accessing any
   data (via permission read, write, identity or govern) in
   directories other than /#config. Even if SYSTEM_UID injects its
   procaps, it cannot access the data. In particular, it cannot be an
   ordinary user of the system.

   ADMIN_UID is just another user of the file system. It has no
   special priviledges, except for one: whenever a new object is
   created in the FS (through create, link, symlink, rename, or
   mkdir), ADMIN_UID is automatically given execute and govern
   permissions to it. These allow it to read the file's metadata, and
   set metadata that is used for pcfs policies. Note that it does not
   get any access to the file's contents, nor the ability to delete or
   create files. The execute and govern permissions are enough to
   "administrate" the file, i.e., adjust its policies, but nothing
   else. Note also that since ADMIN_UID has no access to the symmetric
   key, it cannot increase its own permissions. Thus it is limited to
   an "administrator" in the strictest sense (unless the policy allows
   otherwise).

   By default therefore (i.e., unless the policy allows otherwise),
   FS enforces a principle of least privilege on three classes of
   users:

   1. Ordinary users who get read, write, execute and identity
      permissions, that allow normal use of fs, but cannot be used to
      change attributes that policies depend on. In other words,
      ordinary users can't choose how policies affect their files
      (unless there is a meta policy that gives discretionary control
      to users).
   
   2. ADMIN_UID who gets govern permission to change attributes that
      affect policy, and hence change what policies apply to data, but
      cannot otherwise read or write data.

   3. SYSTEM_UID who only manages the *enforcement of permissions*,
      i.e., procaps but has no ability to change data or attributes
      that policies depend on.
   
   //----------------------------------------------

   Recommendations for the underlying system.
   
   It is recommended that permissions on the underlying system be kept
   as few as possible. The best recommendation is to turn off all
   access to src/, except for the root (i.e., set its ownership to
   root, and permissions to rwx------). For practical reasons, if a
   more permissive setup is used, #config/config-file must definitely
   not be readable by users since it has a secret key,
   #config/declarations, #config/ca-pubkey.pem must not be writable by
   ordinary users, and #config/procaps should be protected from
   sabotage by users to prevent denial of service attacks where one
   user deletes another user's procaps.

*/

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <time.h>

#include <fstream>
#include <sstream>
#include <iostream>

#include "paths-common.hpp"
#include "crypto-common.hpp"
#include "parse-check/loadDeclarations.hpp"

#include "pcfs-runtime.hpp"
#include "checkPerm.hpp"

using namespace std;

//--------------------------------------------------------------------------------

/* Function to parse and load the configuration file. src must be the
   full path of the file. Returns 1 and sets global configuration
   variables if parse is successful. Returns 0 on failure. Assumes
   that src is a valid file.
 */

static int parseConfigurationFile (char * src)
{
  // Flags that keep track of what attributes have been set already

  bool set_logfile = false;
  bool set_adminuid = false;
  bool set_systemuid = false;
  bool set_tempdir = false;
  bool set_logsize = false;
  bool set_cachesize = false;

  cerr << "Parsing configuration file " << src << "\n";
  fstream fin(src, fstream :: in);

  char line[2048];

  int linecount = 0;
  
  while (!fin.eof()) {
  
    fin.getline (line, 2048);
    linecount ++;

    // Skip white space on the beginning of the line
    char * p = line;
    while (*p == ' ' || *p == '\t') p++;

    // If the first character after the whitespace is '\0', then the
    // line is empty.  If the first character after the whitespace is
    // '#', the line is a comment. In either case, continue to the
    // next line.
    if (*p == '\0' || *p == '#') continue;
    
    // Convert the remaining line to a stringstream
    string sline (p);
    stringstream ssline (sline, stringstream :: in);
    char attribute[128];
    
    ssline >> attribute;

    if (!strcmp (attribute, "LOG_FILE")) {
      if (set_logfile) {
	cerr << "Redefinition of attribute LOG_FILE in line " << linecount << "\n";
	return 0;
      }

      cerr << "Found attribute: " << attribute << "\n";
      char logpath[MAX_PATH_LENGTH];
      ssline >> logpath;

      if (strlen(logpath) == 0 || logpath[0] != '/') {
	cerr << "Bad logfile path in line " << linecount << " (path must be absolute)\n";
	return 0;
      }

      if (checkOrCreatePath(logpath, type_file)) {
	strcpy(global_logfile, logpath);
	set_logfile = true;
      }

      else {
	cerr << "Bad logfile path in line " << linecount << "\n";
	return 0;
      }
    }

    else if (!strcmp (attribute, "TEMP_DIR")) {
      if (set_tempdir) {
	cerr << "Redefinition of attribute TEMP_DIR in line " << linecount << "\n";
	return 0;
      }

      cerr << "Found attribute: " << attribute << "\n";
      char temppath[MAX_PATH_LENGTH];
      ssline >> temppath;

      if (strlen(temppath) == 0 || temppath[0] != '/') {
	cerr << "Bad temp dir path in line " << linecount << " (path must be absolute)\n";
	return 0;
      }

      if (checkOrCreatePath(temppath, type_directory)) {
	strcpy(global_tempdir, temppath);
	set_tempdir = true;
      }

      else {
	cerr << "Bad temp dir path in line " << linecount << "\n";
	return 0;
      }
    }

    else if (!strcmp (attribute, "ADMIN_UID")) {
      if (set_adminuid) {
	cerr << "Redefinition of attribute ADMIN_UID in line " << linecount << "\n";
	return 0;
      }

      cerr << "Found attribute: " << attribute << "\n";
      int i;
      ssline >> i;
      if (i >= 0) {
	global_uid_of_admin = i;
	set_adminuid = true;
      }

      else {
	cerr << "Bad uid of admin in line " << linecount << "\n";
	return 0;
      }
    }

    else if (!strcmp (attribute, "SYSTEM_UID")) {
      if (set_systemuid) {
	cerr << "Redefinition of attribute SYSTEM_UID in line " << linecount << "\n";
	return 0;
      }

      cerr << "Found attribute: " << attribute << "\n";
      int i;
      ssline >> i;

      if (i >= 0) {
	global_uid_of_system = i;
	set_systemuid = true;
      }

      else {
	cerr << "Bad uid of system in line " << linecount << "\n";
	return 0;
      }
    }

    else if (!strcmp (attribute, "LOG_SIZE")) {
      if (set_logsize) {
	cerr << "Redefinition of attribute LOG_SIZE in line " << linecount << "\n";
	return 0;
      }

      cerr << "Found attribute: " << attribute << "\n";
      int i;
      ssline >> i;

      if (i >= 0) {
	global_log_size = i;
	set_logsize = true;
      }

      else {
	cerr << "Bad LOG_SIZE in line " << linecount << "\n";
      }
    }

    else if (!strcmp (attribute, "CACHE_SIZE")) {
      if (set_cachesize) {
	cerr << "Redefinition of attribute CACHE_SIZE in line " << linecount << "\n";
	return 0;
      }

      cerr << "Found attribute: " << attribute << "\n";
      int i;
      ssline >> i;

      if (i >= 0) {
	global_cache_size = i;
	set_cachesize = true;
      }

      else {
	cerr << "Bad CACHE_SIZE in line " << linecount << "\n";
      }
    }

    // Check for flags

    else if (!strcmp (attribute, "NO_LOG_COMMIT")) {
      global_no_log_commit = true;
    }

    else if (!strcmp (attribute, "NO_DATACHECK")) {
      global_no_datacheck = true;
    }

    else if (!strcmp (attribute, "NO_INSERT_AUTO_PROCAPS")) {
      global_no_insert_auto_procaps = true;
    }

    else if (!strcmp (attribute, "NO_INSERT_ATTR_NEWFILE")) {
      global_no_insert_attr_newfile = true;
    }

    else if (!strcmp (attribute, "NO_REMOVE_AUTO_PROCAPS")) {
      global_no_remove_auto_procaps = true;
    }

    else {
      cerr << "Warning: unknown attribute: " << attribute << " in line " <<  linecount << "\n";
    }

  }

  // Check for missing attributes
  int r = 1;
  if (!set_logfile) {
    cerr << "Mandatory attribute LOG_FILE not found\n";
    r = 0;
  }
  if (!set_tempdir) {
    cerr << "Mandatory attribute TEMP_DIR not found\n";
    r = 0;
  }
  if (!set_adminuid) {
    cerr << "Mandatory attribute ADMIN_UID not found\n";
    r = 0;
  }
  if (!set_systemuid) {
    cerr << "Mandatory attribute SYSTEM_UID not found\n";
    r = 0;
  }
  if (!set_logsize) {
    cerr << "Mandatory attribute LOG_SIZE not found\n";
    r = 0;
  }
  if (!set_cachesize) {
    cerr << "Mandatory attribute CACHE_SIZE not found\n";
    r = 0;
  }
  return r;
}



/* Function to parse the shared-key file and load the shared key. src
   must be the full path of the file. Returns 1 and sets global
   configuration variable global_mackey if parse is
   successful. Returns 0 on failure. Assumes that src is a valid file.
 */

static int parseSharedKeyFile (char * src)
{
  cerr << "Parsing shared-key file " << src << "\n";
  fstream fin(src, fstream :: in);

  char line[2048];

  int linecount = 0;
  
  bool set_mackey = false;

  while (!fin.eof()) {
    
    fin.getline (line, 2048);
    linecount ++;
    
    // Skip white space on the beginning of the line
    char * p = line;
    while (*p == ' ' || *p == '\t') p++;
    
    // If the first character after the whitespace is '\0', then the
    // line is empty.  If the first character after the whitespace is
    // '#', the line is a comment. In either case, continue to the
    // next line.
    if (*p == '\0' || *p == '#') continue;
    
    // Convert the remaining line to a stringstream
    string sline (p);
    stringstream ssline (sline, stringstream :: in);
    
    if (set_mackey) {
      cerr << "Redefinition of shared-key in line " << linecount << "\n";
      return 0;
    }
    
    char key [128];
    ssline >> key;

    if (strlen(key) == 40) {

      if (hexToBinary(key, 2 * SYM_KEY_LENGTH, global_mackey, true) != 1) {
	cerr << "Bad shared-key in line " << linecount << "\n";
	return 0;
      }
      
      set_mackey = true;
    }

    else {
      cerr << "Bad shared-key in line " << linecount << "\n";
      return 0;
    }
  }
  
  if (!set_mackey) {
    cerr << "Shared-key not found\n";
    return 0;
  }
  
  return 1;
}

//--------------------------------------------------------------------------------


/* Function to check that the pathname src contains a pcfs file
   system, and to load configuration settings. The specific checks
   made are:

   - src/#config exists, and is a directory 

   - src/#config/procaps exists, and is a directory.

   - src/#config/declarations exists and is a file. This file is
     parsed and the declarations stored in the global variable
     global_declaration_index.

   - src/#config/config-file exists, and is a file. This file is
     parsed and the configuration is stored in global variables. If
     parsing fails, an error is reported.

   - src/#config/shared-key exists and is a file. This file is parsed
     and the shared key is stored in global_mackey.


   Returns 1 on success and 0 on error. If src ends with a '/' and
   this function succeeds with 1, the trailing '/' would have been
   deleted when this function returns.
*/

static int checkPcfsFs(char * src)
{
  struct stat statbuf;

  // Check that src/#config exists, and it is a directory
  addNewComponentToPath(src, "#config");
  
  if (lstat(src, &statbuf) != 0 || !S_ISDIR(statbuf.st_mode)) {
    cerr << src << " does not exist or it is not a directory\n";
    return 0;
  }

  // Check that src/#config/procaps exists, and it is a directory
  addNewComponentToPath(src, "procaps");

  if (lstat(src, &statbuf) != 0 || !S_ISDIR(statbuf.st_mode)) {
    cerr << src << " does not exist or it is not a directory\n";
    return 0;
  }

  removeLastComponentOfPath(src); // remove "procaps"

  // Check that src/#config/declarations exists and is a regular file
  addNewComponentToPath(src, "declarations");

  if (lstat(src, &statbuf) != 0 || !S_ISREG(statbuf.st_mode)) {
    cerr << src << " does not exist or it is not a file\n";
    return 0;
  }

  // Parse the declarations file

  cerr << "Parsing declarations file: " << src << " ";
  global_declarations_index = loadFileDeclarations(src);
  if (global_declarations_index == NULL) {
    cerr << "...failed\n";
    return 0;
  }
  else {
    cerr << "\nDeclarations are:\n";
    global_declarations_index -> print(cerr);
  }

  removeLastComponentOfPath(src); // remove "declarations"

  cerr << "\n------------------------------------------------\n";

  // Check that src/#config/config-file exists, and it is a regular file
  addNewComponentToPath(src, "config-file");

  if (lstat(src, &statbuf) != 0 || !S_ISREG(statbuf.st_mode)) {
    cerr << src << " does not exist or it is not a file\n";
    return 0;
  }

  // Parse the configuration file now. 
  
  if (! parseConfigurationFile (src)) {

    removeLastComponentOfPath(src); // remove "config-file"
    removeLastComponentOfPath(src); // remove "#config"

    return 0;
  }

  removeLastComponentOfPath(src); // remove "config-file"

  // Check that src/#config/shared-key exists, and it is a regular file
  addNewComponentToPath(src, "shared-key");

  if (lstat(src, &statbuf) != 0 || !S_ISREG(statbuf.st_mode)) {
    cerr << src << " does not exist or it is not a file\n";
    return 0;
  }

  // Parse the shared-key file

  int r = 1;
  if (!parseSharedKeyFile(src)) {
    r = 0;
  }

  removeLastComponentOfPath(src); // remove "shared-key"
  removeLastComponentOfPath(src); // remove "#config"

  return r;

}




//--------------------------------------------------------------------------------

/* Some helper functions */

// Print a usage message on cerr
inline void errUsage (const char * progname) 
{
    cerr << "Usage is:\n\t" << progname << " <src> <mountpoint>\n";
    cerr << "<src> and <mountpoint> must be absolute path names starting with '/'\n";
    cerr << "Please run as root, and make sure that <src> is a valid pcfs file system, and that <mount point> exists.\n";
    cerr << "You should have received a script to create a pcfs file system.\n";
}

int main (int argc, char * argv[])
{
  if (argc != 3) {
    errUsage(argv[0]);
    return EXIT_FAILURE;
  }

  // Check that this program is run by root only
  if (geteuid() != 0) {
    cerr << "Please run as root, else files may not be accessible. PCFS implements its own access checks.\n";
    return EXIT_FAILURE;
  }

  // Check that both src and mount point are absolute path names
  char * src = new char [MAX_PATH_LENGTH];
  strcpy(src, argv[1]);
  char * mtpt = new char[MAX_PATH_LENGTH];
  strcpy(mtpt, argv[2]);

  if (src[0] != '/' || mtpt[0] != '/') {
    errUsage(argv[0]);
    return EXIT_FAILURE;
  }

  // Check that src and mount point exist, and both are directories
  struct stat statbuf;
  cerr << "Checking that " << src << " and " << mtpt << " exist, and are both directories...\n";
  if (lstat(src, &statbuf) != 0 ||  !S_ISDIR(statbuf.st_mode)) {
    cerr << src << " does not exist or it is not a directory\n";
    return EXIT_FAILURE;
  }
  if (lstat(mtpt, &statbuf) != 0 ||  !S_ISDIR(statbuf.st_mode)) {
    cerr << mtpt << " does not exist or it is not a directory\n";
    return EXIT_FAILURE;
  }

  cerr << "\n";

  // Confirm from user before proceeding
  cerr << "You have elected to mount " << src << " at mount point " << mtpt << ".\n";
  cerr << "Is that correct? (enter yes to accept): ";
  char response[4];
  cin >> response;
  if (strcmp (response, "yes")) {
    cerr << "Goodbye\n";
    return EXIT_SUCCESS;
  }

  cerr << "\n";

  // Check that src is not a prefix of the mount point, because that
  // would allow the possibility of recursion.
  if (isAncestor(src, mtpt)) {
    cerr << "The source " << src << " is an ancestor of the mount point " << mtpt <<".\n";
    cerr << "This is illegal, since it allows the possibility of recursion in the file system.\n";
    return EXIT_FAILURE;
  }

  // Check that src is a valid pcfs file system, and load the
  // configuration from <src>/#config/config-file
  if (!checkPcfsFs (src)) {
    return EXIT_FAILURE;
  }

  // Initialize global variables
  strcpy(global_mountpoint, mtpt);
  strcpy(global_src,src);
  
  strcpy(global_procaps,src);
  addNewComponentToPath(global_procaps, "#config");
  addNewComponentToPath(global_procaps, "procaps");

  delete [] src;
  delete [] mtpt;


  // Print the configuration
  cerr << "\n---------------------------------------\n";
  cerr << "Configuration is ... \n";
  printConfiguration(cerr);
  cerr << "\n";

  // Set up the log stream. Right now this is set to stderr, later this
  // should be changed to point to an fstream object derived from
  // global_logfile.

  cerr << "Creating a log stream...\n";

  int fd = open (global_logfile, O_WRONLY | O_TRUNC);
  if (fd == -1) {
    cerr << "Failed to open log file for writing; redirecting log to stderr\n";
    global_log_stream = new logstream(global_log_size * 1024,2) ;
  }
  else {
    global_log_stream = new logstream(global_log_size * 1024, fd) ;
  }

  cerr << "Initializing ProCap cache...\n";

  global_procap_cache = new ProtectedCache(global_cache_size, global_procaps) ;

  cerr << "\n";
  
  //------------------------------------------------------

  // Check that permissions on "/" and "/#config" work okay

  int const random_user = 1;

  if (checkPerm("/", random_user, &global_perm_execute)) {
    cerr << "Permission check successful\n";
  }
  else {
    cerr << "Permission check failed\n";
  }


  if (checkPerm("/", random_user, &global_perm_write)) {
    cerr << "Permission check successful\n";
  }
  else {
    cerr << "Permission check failed\n";
  }
 
  if (checkPerm("/#config", global_uid_of_system, &global_perm_write)) {
    cerr << "Permission check successful\n";
  }
  else {
    cerr << "Permission check failed\n";
  }
 
  if (checkPerm("/#config", global_uid_of_admin, &global_perm_write)) {
    cerr << "Permission check successful\n";
  }
  else {
    cerr << "Permission check failed\n";
  }
 
  if (checkPerm("/#config", random_user, &global_perm_execute)) {
    cerr << "Permission check successful\n";
  }
  else {
    cerr << "Permission check failed\n";
  }
 
  if (checkPerm("/#config", random_user, &global_perm_identity)) {
    cerr << "Permission check successful\n";
  }
  else {
    cerr << "Permission check failed\n";
  }
 
  if (checkPerm("/#config/procaps", random_user, &global_perm_execute)) {
    cerr << "Permission check successful\n";
  }
  else {
    cerr << "Permission check failed\n";
  }
 
  if (checkPerm("/#config/procaps", random_user, &global_perm_identity)) {
    cerr << "Permission check successful\n";
  }
  else {
    cerr << "Permission check failed\n";
  }
 
  if (checkPerm("/#config/procaps/1000", random_user, &global_perm_write)) {
    cerr << "Permission check successful\n";
  }
  else {
    cerr << "Permission check failed\n";
  }
 
  if (checkPerm("/#config/procaps/0", random_user, &global_perm_write)) {
    cerr << "Permission check successful\n";
  }
  else {
    cerr << "Permission check failed\n";
  }
 
  if (checkPerm("/#config/config-file", random_user, &global_perm_write)) {
    cerr << "Permission check successful\n";
  }
  else {
    cerr << "Permission check failed\n";
  }
 
  //------------------------------------------------------


  cerr << "\n";

  // Call fuse
  int ret = startFuse (argv[0]);


  //-------------------------------------------------------

  // Perform cleanup


  delete global_procap_cache;
  delete global_declarations_index;
  // delete global_log_stream;
  delete global_log_stream;
  
  // close log file
  if (fd != -1) close (fd);

  return ret;
}
