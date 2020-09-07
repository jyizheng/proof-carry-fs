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

#ifndef __PCFS_COMMON_HPP
#define __PCFS_COMMON_HPP

#include <fstream>
#include "parse-check/pcfs-syn.hpp"
#include "parse-check/pcfs-declarations.hpp"
#include "crypto-common.hpp"
#include "cache/DiskCache.hpp"
#include "parse-check/loadProCap.hpp"
#include "paths-common.hpp"
#include "pcfs-handlers.hpp"
#include "logstream.hpp"
#include "Thread.hpp"

// Initialized by main()
extern char global_mountpoint[MAX_PATH_LENGTH];  // Dir where pcfs is mounted
extern char global_src[MAX_PATH_LENGTH];         // Dir where data and config is stored
extern char global_procaps[MAX_PATH_LENGTH];     // Dir where config is located. Must equal global_pcfs_src/#config/procaps

//--------------------------------------------------------------------------------------------------------

// Initialized from the configuration file

extern char global_logfile[MAX_PATH_LENGTH];     // Full path of logfile
extern unsigned char global_mackey[SYM_KEY_LENGTH];    // Symmetric key shared with verifier. Stored as a 20 byte binary

extern int global_uid_of_admin;   // Uid of "admin"

extern int global_log_size;  // Size of log file in KB
extern int global_cache_size; // Size of procap cache


// Flags that govern behavior of runtime on files
extern bool global_no_log_commit;          // Do not commit log to disk after each operation
extern bool global_no_datacheck;           // Do not check permissions in read() and write()
extern bool global_no_insert_auto_procaps; // Do not insert default procaps for new objects
extern bool global_no_insert_attr_newfile; // Do not insert attribute "user.#pcfs.newfile"=1 for new objects
extern bool global_no_remove_auto_procaps; // Do not remove procaps when objects are deleted

// These variables are not currently used, but may be needed by the proof search and proof verifier
extern int global_uid_of_system;                 // Uid of "system"
extern char global_tempdir[MAX_PATH_LENGTH];     // Dir where temporary files may be stored

//---------------------------------------------------------------------------------------------------------

// Global variables for permissions. Initialized when they are created.

extern TermIdentity global_perm_identity;
extern TermGovern global_perm_govern;
extern TermExecute global_perm_execute;
extern TermRead global_perm_read;
extern TermWrite global_perm_write;

//---------------------------------------------------------------------------------------------------------

// Global variable for the Declaration Index. Initialized by main()

extern DeclarationIndex * global_declarations_index;


//---------------------------------------------------------------------------------------------------------

// A cache for ProCaps. Extends DiskCache

// Functions to allocate and deallocate procaps for the cache. They
// use the global variables global_declarations_index and
// global_mackey.

void * procap_parser_cache (NodeType type, const char * path);

void procap_deallocate_cache (NodeType type, void * procap);


class ProCapCache : public DiskCache
{

 public:

 ProCapCache(unsigned maxE, const char * path) 
   : DiskCache (maxE,  & procap_parser_cache, & procap_deallocate_cache, path) 
    { }
};

// A wrapper around ProCapCache that protects all operations via a mutex
class ProtectedCache
{
protected:
  ProCapCache * cache;
  Mutex * lock;

public:
  ProtectedCache(unsigned maxE, const char * path)
  {
    cache = new ProCapCache (maxE, path);
    lock = new Mutex;
  }

  ~ProtectedCache() 
  {
    delete cache;
    delete lock;
  }

  int markDirty(char * path) {
    lock -> lock();
    int r = cache -> markDirty(path);
    lock -> unlock();
    return r;
  }

  void * getData(char * path) {
    lock -> lock();
    void * r = cache -> getData(path);
    lock -> unlock();
    return r;
  }
};

// Global variable for the ProCap cache. Initialized by main()

extern ProtectedCache * global_procap_cache;


//---------------------------------------------------------------------------------------------------------

// Function to print the configuration

extern void printConfiguration(ostream & out);


//---------------------------------------------------------------------------------------------------------

extern logstream * global_log_stream; 
// A stream where the log is written. It must in general be derived
// from the file global_logfile, as in new logstream (new fstream
// (global_logfile, fstream::out)) but for debugging purposes may
// instead be set to something else (like cerr).


/*
// Write data to the global log file. Assumes that global_log_stream
// is correctly set. The data is written up to the first '\0'. If the
// optional second argument len is positive, then exactly len bytes
// are written, without any regard for '\0' that may occur in between.
inline void writeToLog(const char * data, int len = 0) {
#ifndef NO_LOG
  if (len <= 0) len = strlen (data);

  global_log_stream -> write (data, len);
  global_log_stream -> flush ();
#endif
}

// Similar function for writing an integer to the log
inline void writeToLog(int i) {
#ifndef NO_LOG
  (*global_log_stream) << i;
  global_log_stream -> flush ();  
#endif
}


// Similar function for writing a term to the log
inline void writeToLog(Term * t) {
#ifndef NO_LOG
  t -> print (*global_log_stream);
  global_log_stream -> flush ();  
#endif
}

*/

//-----------------------------------------------------------------------------------------------------------

/*
  Entry point to start the fuse loop. This function will set up the
  fuse_operations structure, initialize fuse arguments and call
  fuse_main. binaryName must be the name of the binary used to invoke
  this program, as obtained from argv[0]. Returns the return value of
  fuse_main().
*/

int startFuse(const char * binaryName);

#endif

