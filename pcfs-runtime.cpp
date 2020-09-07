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

#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <fuse.h>

#include <iostream>

#include "pcfs-runtime.hpp"
#include "parse-check/pcfs-syn.hpp"

using namespace std;

// Initialized by main()
char global_mountpoint[MAX_PATH_LENGTH] = {0};  // Dir where pcfs is mounted
char global_src[MAX_PATH_LENGTH] = {0};         // Dir where data and config is stored
char global_procaps[MAX_PATH_LENGTH] = {0};     // Dir where config is located. Must equal global_pcfs_src/#config/procaps

//--------------------------------------------------------------------------------------------------------

// Initialized from the configuration file

char global_logfile[MAX_PATH_LENGTH] = {0};     // Full path of logfile

unsigned char global_mackey[SYM_KEY_LENGTH] = {0,0,0,0,0,0,0,0,0,0,
					       0,0,0,0,0,0,0,0,0,0};
// Symmetric key shared with verifier. Stored as a 20 byte binary.

int global_uid_of_admin = -1;                   // Uid of "admin"

int global_log_size = -1;  // Size of log file in KB
int global_cache_size = -1; // Size of procap cache


// Flags that govern behavior of runtime on files
bool global_no_log_commit = false;          // Do not commit log to disk after each operation
bool global_no_datacheck = false;           // Do not check permissions in read() and write()
bool global_no_insert_auto_procaps = false; // Do not insert default procaps for new objects
bool global_no_insert_attr_newfile = false; // Do not insert attribute "user.#pcfs.newfile"=1 for new objects
bool global_no_remove_auto_procaps = false; // Do not remove procaps when objects are deleted

// These variables are not currently used, but may be needed by the proof search and proof verifier
int global_uid_of_system= -1;                   // Uid of "system"
char global_tempdir[MAX_PATH_LENGTH] = {0};     // Dir where temporary files may be stored


//---------------------------------------------------------------------------------------------------------

// Global variables for permissions. Initialized right here.

TermIdentity global_perm_identity;
TermGovern global_perm_govern;
TermExecute global_perm_execute;
TermRead global_perm_read;
TermWrite global_perm_write;

//---------------------------------------------------------------------------------------------------------

// Global variable for the Declaration Index. Initialized by main()

DeclarationIndex * global_declarations_index = NULL;

// Global variable for the ProCap cache. Initialized by main()

ProtectedCache * global_procap_cache = NULL;


//---------------------------------------------------------------------------------------------------------


void * procap_parser_cache (NodeType type, const char * path)
{
  if (type == file) {
    return loadFileProCap (path,global_mackey,global_declarations_index);
  }
  else return NULL;
}

void procap_deallocate_cache (NodeType type, void * procap)
{
  if (procap != NULL) {
    delete ((ProCap *) procap);
  }
}

//---------------------------------------------------------------------------------------------------------


void printConfiguration(ostream & out)
{
  out << "Mount point: " << ((global_mountpoint == NULL) ? "(NULL)" : global_mountpoint) << "\n";
  out << "Source: " << ((global_src == NULL) ? "(NULL)" : global_src) << "\n";
  out << "Procaps directory: " << ((global_procaps == NULL) ? "(NULL)" : global_procaps) << "\n";
  out << "Logfile: " << ((global_logfile == NULL) ? "(NULL)" : global_logfile) << "\n";
  out << "Temporary directory: " << ((global_tempdir == NULL) ? "(NULL)" : global_tempdir) << "\n";

  if (global_mackey == NULL) 
    cerr << "MAC Key: (NULL)\n";
  else {
    char key[2 * SYM_KEY_LENGTH + 1];
    binaryToHex(global_mackey, SYM_KEY_LENGTH, key);
    key[40] = '\0';
    out << "MAC Key: " << key << "\n";
  }

  out << "admin uid: " << global_uid_of_admin << "\n";
  out << "system uid: " << global_uid_of_system << "\n";

  out << "Log buffer size: " << global_log_size << " KB\n";
  out << "Procap cache size: " << global_cache_size << " entries\n";

  out << "Flags:\n";
  out << (global_no_log_commit ? "NO_LOG_COMMIT\n" : "");
  out << (global_no_datacheck ? "NO_DATACHECK\n" : "" );
  out << (global_no_insert_auto_procaps ? "NO_INSERT_AUTO_PROCAPS\n" : "" );
  out << (global_no_insert_attr_newfile ? "NO_INSERT_ATTR_NEWFILE\n" : "" );
  out << (global_no_remove_auto_procaps ? "NO_REMOVE_AUTO_PROCAPS\n" : "" );
  out << "\n";

}

//-------------------------------------------------------------------------------------------------------------

logstream * global_log_stream = NULL;
// A stream where the log is written. It must in general be derived
// from the file global_logfile, but for debugging purposes may
// instead be set to something else (like cerr).

//-------------------------------------------------------------------------------------------------------------

/*
  Entry point to start the fuse loop. This function will set up the
  fuse_operations structure, initialize fuse arguments and call
  fuse_main. binaryName must be the name of the binary used to invoke
  this program, as obtained from argv[0]. Returns the return value of
  fuse_main().
*/

int startFuse(const char * binaryName) 
{
  // Set up and obtain the handlers
  cerr << "Setting up PCFS handlers...\n";
  struct fuse_operations * opers = initPcfsHandlers();

  /* 
     Set up the arguments. Here is what we want to pass to fuse:
     -o allow_other -f -o nonempty <mount point> (if NO_BACKGROUND is defined)
     -o allow_other -o nonempty <mount point>    (if NO_BACKGROUND is not defined)
  */

  cerr << "Setting up fuse arguments...\n";

#ifdef NO_BACKGROUND
  int const nargs = 7;
#else
  int const nargs = 6;
#endif

  char ** arg_v = new char * [nargs];

  arg_v[0] = new char[strlen(binaryName) + 1];
  strcpy(arg_v[0], binaryName);

  arg_v[1] = new char[strlen("-o") + 1];
  strcpy(arg_v[1], "-o");

  arg_v[2] = new char[strlen("allow_other") + 1];
  strcpy(arg_v[2], "allow_other");

  arg_v[3] = new char[strlen(global_mountpoint) + 1];
  strcpy(arg_v[3], global_mountpoint);

  arg_v[4] = new char[strlen("-o") + 1];
  strcpy(arg_v[4], "-o");
  
  arg_v[5] = new char[strlen("nonempty") + 1];
  strcpy(arg_v[5], "nonempty");
  
#ifdef NO_BACKGROUND
  // NO_BACKGROUND is defined, so run in foreground (pass -f)
  arg_v[6] = new char[strlen("-f") + 1];
  strcpy(arg_v[6], "-f");
#endif


  cerr << "Setting umask...\n";
  // clear umask so that files can be created with any permission
  umask(0);

  cerr << "Starting fuse...\n";

  // Call fuse. 

  int r = fuse_main(nargs, arg_v, opers, NULL);

  // Clean up

  cerr << "Fuse finished. Cleaning up arguments ...\n";

  for (int i = 0; i < nargs; i++) {
    delete [] (arg_v[i]);
  }

  delete [] arg_v;

  return r;
}
