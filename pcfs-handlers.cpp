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

#include <fuse.h>
#include <ulockmgr.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <dirent.h>
#include <errno.h>
#include <sys/time.h>
#include <sys/xattr.h>

#include <iostream>

#include "pcfs-handlers.hpp"

#include "pcfs-runtime.hpp"
#include "charbuf.hpp"
#include "checkPerm.hpp"
#include "pcfs-procaps.hpp"
#include "parse-check/pcfs-syn.hpp"

using namespace std;

/* 
   PCFS' use of permissions and rwxrwxrwx bits.

   PCFS defines and uses 5 permissions for each file/directory

   1. execute (read meta data; this is the permission to stat the
      file/directory, it is needed to "execute" any operation on the
      file/directory). Note that in the absence of execute permission,
      users cannot read the attributes on files. In particular, if
      access policies depend on file attributes, proofs using them
      cannot be constructed. It is therefore essential that the
      policies giving "execute" permission to a file not depend on
      attributes of the file.

   2. read (read file contents or directory contents)

   3. write (for files -- modify contents, truncate, or change
      attributes that have no bearing on policies, i.e., attributes
      that begin with "user." but not "user.#pcfs.". Also included are
      the nine protection bits, so write permission allows chmod. For
      directories -- change the same attributes, and add new files to
      the directory. Note that this permission does not allow deletion
      of files from a directory)

   4. identity (allow changes to the identity of the object in the
      filesystem, i.e., unlink, rmdir, rename, and create hard links
      pointing to the object)

   5. govern (change file/directory meta data that affects policies,
      i.e., chown, and changes to extended attributes starting with
      "user.#pcfs.")
 
   Note that permissions are exclusive: no permission implies the
   other. The execute permission is needed to perform almost any
   operation on a file/directory. In particular, to govern a file, you
   must have both the execute and govern permission to it.

   All these are implemented either via procaps (for ordinary data) or
   via fixed rules (for paths beginning with /#config ). As a result
   the ordinary protection bits are useless for PCFS. The only place
   where they have some meaning is for the actual ability to execute
   files. On the other hand, some software check permission bits
   before continuing execution. To encourage these software to
   continue running, any stat structure returned from PCFS is modified
   to appear to be excessively permissive as follows (see the function
   modifyStat below):

   - For directories and soft links, all nine bits rwxrwxrwx are
     always set.

   - For files, all 6 read and write bits are always set. In addition,
     all three x bits are set if any one is.

   ---------------------------------------------------------------------

   The permissions used for various operations are as follows:

   stat/lstat/fstat (getattr/fgetattr) -- execute permission on the
   file/directory. 

   access -- no permission needed a priori

   readlink (read a soft link) -- read permission on the link (this
   deviates from POSIX which requires no permission to read a soft
   link)

   openddir -- read permission on directory (opening a directory is for reading only)

   readdir -- read permission on directory

   mkdir -- write permission on parent directory. 

   rmdir -- identity permission on directory being removed. 

   unlink -- identity permission on file being removed. 

   symlink -- write permission on parent directory where link is
   created.

   rename -- write permissions on parent directory of new path, write
   permission on new path if it exists, and identity permission on the
   old path.

   link (create a hard link) -- write permission to parent directory
   of new link, and identity permission on source of the link. (The
   new link must not already exist -- this is required by POSIX).

   chmod -- write permission (pcfs does not use protection bits, so
   these have no bearing on policies)

   chown -- govern permission

   truncate/ftruncate -- write permission  

   utimes (utimens) -- write permission 

   create -- write permission to parent of path being created

   open -- depends on mode in which file is opened:
     O_WRONLY -- wrtite permission
     O_RDONLY -- read permission
     O_RDWR -- read and write permission

   read -- read permission

   write -- write permission

   statvfs -- no checks needed

   getxattr -- execute permission 

   setxattr -- write permission if attribute starts with "user." but
   not "user.#pcfs.", govern permission if attribute starts with
   "user.#pcfs.". Namespaces other than "user." are not supported and
   will fail with EACCES.

   listxattr -- execute permission

   removexattr -- write permission if attribute starts with "user."
   but not "user.#pcfs.", govern permission if attribute starts with
   "user.#pcfs.". Namespaces other than "user." are not supported and
   will fail with EACCES.
   
   Note that as per fuse design, getattr (stat) is called on a
   file/directory before any other operation. As a result, if execute
   permission is denied to a file/directory, almost certainly every
   other operation is bound to fail. Besides this, execute permissions
   in PCFS are useless. In particular, PCFS does not have a recursive
   check for permissions on ancestor directories. These may be
   implemented using an iterative policy if needed.

   -----------------------------------------------------------------------

   Lack of recursive permissions.

   By default, PCFS does not implement recursive permissions on parent
   directories. This is not a missing feature since recursive checks
   can be implemented in the policy itself. For policies that do not
   do this, this may have some unconventional consequences.

   -----------------------------------------------------------------------

   Execute permission for directories. 

   In PCFS, execute permission on a directory is needed only to read
   its metadata, not for any access in the directory, as is the case
   in POSIX systems. As a result, in theory, the execute permission
   may not be very relevant. None the less, most shell commands or
   FUSE will stat directories recursively, so execute permissions for
   directories are practically essential.

   -----------------------------------------------------------------------

   Default permissions.

   When a file or directory is created (via create, mkdir, rename,
   symlink, or link), it will essentially be useless until procaps are
   generated. Ideally, after file creation, the calling program should
   generate proofs and procaps to access the file. However, existing
   programs will not do this. So, to keep things neat for them, when a
   new fs object is created, some default settings are made on the
   file, and some default procaps are inserted. These are the
   following:

   1. A special pcfs attribute "newfile" (actual attribute
      user.#pcfs.newfile) is set to (TermPrim_int 1). This attribute
      value pair signifies that the file/directory is in its default
      state as created. For any reasonable administration, this
      attribute should be unset (or set to another value) as soon as
      the file is properly classified. (Note: this attribute is not
      set for symbolic links)

   2. The ownership of the new object is set to the uid of the calling
      process (gid, which is not used by pcfs remains root)

   3. Procaps giving excute, read, write, and identity permissions for
      the uid of the calling process are injected. These procaps are
      valid for 30 days, and only if the "newfile" attribute is 1. (In
      case of symbolic links the newfile check is not inserted)

   4. Procaps giving execute and govern permissions to ADMIN_UID are
      injected. These are also valid for 30 days, and only if the
      "newfile" attribute is 1. (In case of symbolic links the newfile
      check is not inserted)

   It should be noted that the govern permission available to admin
   does not allow it to read/write/delete the object. Similarly the
   permissions available to the owner will not allow it to change the
   policy affecting attributes or even to transfer ownership to
   another person. Consequently, the "newfile" attribute must be
   changed by ADMIN_UID, after which it will itself lose all access to
   the file (unless allowed otherwise by the policy).

   The generation of default procaps and the setting of "newfile" to 1
   can be suppressed by setting the flag NO_INSERT_AUTO_PROCAPS in the
   config file.

   ------------------------------------------------------------------------
   
   Deletion of procaps with objects.

   When a file system object is deleted (via unlink, rmdir, or
   rename), its procaps become useless, and occupy unnecessary
   space. By default pcfs will delete all these procaps when the
   object is deleted. 

   This is a somewhat time costly affair. It can be suppressed by
   setting the flag NO_REMOVE_AUTO_PROCAPS in the config file.
*/

inline void modifyStat(struct stat * stbuf)
{
  // Set all r and w permissions
  stbuf -> st_mode |= (S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
  
  // Set all x permissions if any one is set, or path is a directory or a link
  if ((stbuf -> st_mode & (S_IXUSR | S_IXGRP | S_IXOTH)) || 
      (S_ISDIR(stbuf -> st_mode)) || (S_ISLNK(stbuf -> st_mode))) {
    stbuf -> st_mode |= (S_IXUSR | S_IXGRP | S_IXOTH);
  }
}

// Add default ProCaps for path and uid. This will add procaps
// allowing execute, read, write and identity permissions to owner of
// the file. It will also inject execute and govern permissions for
// ADMIN_UID. Note that default permissions will not be inserted if
// the path is "/" or it begins with /#config because these
// directories have special access. If the last argument is true, then
// the check for attribute "newfile" is skipped.
static int injectDefaultProCaps (const char * path, int uid, bool skipAttributeCheck)
{
  *global_log_stream << "(Injecting default procaps) path: " 
		     << path << "\n";

  // Do not inject default permissions if path is / or begins with /#config

  if (isRoot (path) || isAncestor ("/#config", path)) {
    *global_log_stream << "Path is / or begins with /#config ... no procaps are needed.\n";
    return 1;
  }

  if (uid == 0) {
    *global_log_stream << "<uid> is 0 (root) ... no procaps are needed.\n";
    return 1;
  }

  // First the read permission for owner
  ProCap * pc = makeDefaultProCap (path, uid, &global_perm_read, skipAttributeCheck);
  int r = writeProCapToFile (pc, path, uid, &global_perm_read, global_procaps, global_mackey);

  if (r == 0) {
    delete pc;
    return r;
  }

  // Now the write permission for owner
  delete pc -> p; 

  pc -> p = new TermWrite;

  r = writeProCapToFile (pc, path, uid, &global_perm_write, global_procaps, global_mackey);

  if (r == 0) {
    delete pc;
    return r;
  }

  // Next the execute permission for owner

  delete pc -> p;

  pc -> p = new TermExecute;

  r = writeProCapToFile (pc, path, uid, &global_perm_execute, global_procaps, global_mackey);

  if (r == 0) {
    delete pc;
    return r;
  }

  // Next the identity permission for owner

  delete pc -> p;

  pc -> p = new TermIdentity;

  r = writeProCapToFile (pc, path, uid, &global_perm_identity, global_procaps, global_mackey);


  // The govern permission for ADMIN_UID

  delete pc -> p;
  delete pc -> k;

  pc -> p = new TermGovern;

  pc -> k = new TermPrim_int2principal (new TermPrim_int (global_uid_of_admin));

  r = writeProCapToFile (pc, path, global_uid_of_admin, &global_perm_govern, global_procaps, global_mackey);

  // ADMIN_UID must also have execute permission, else it cannot govern (stats will fail)

  delete pc -> p;

  pc -> p = new TermExecute;

  r = writeProCapToFile (pc, path, global_uid_of_admin, &global_perm_execute, global_procaps, global_mackey);

  delete pc;
  return r;

}

//---------------------------------------------------------------------------------------------------

// A function that commits the log and returns its argument. If
// NO_LOG_COMMIT is defined in the config file, the function does
// nothing at all

inline int commitlog_and_return (int k) {

  if (!global_no_log_commit) {
    global_log_stream -> commit ();
    global_log_stream -> reset ();
  }

  return k;
}

//---------------------------------------------------------------------------------------------------

static int pcfs_getattr(const char *path, struct stat *stbuf)
{
  global_log_stream -> begin();
  *global_log_stream << "(getattr) path is: "
		     << path << "\n";

  // It is better to stat the file before checking permissions because
  // this is a cheap check. Further, if the file does not exist, we
  // want to return the correct error, not EACCES.  

  // Construct the full path of the file.
  charbuf fullpath (MAX_PATH_LENGTH);
  fullpath << global_src << path;
  
  // Perform io
  int res = lstat(fullpath.getHead(), stbuf);
  if (res == -1) {
    return (commitlog_and_return(-errno));
  }

  // In order to get the attributes of a file, the calling user must
  // have execute access on the file

  if (! checkPerm (path, fuse_get_context() -> uid, &global_perm_execute)) {
    // Permission is denied
    return (commitlog_and_return(-EACCES));
  }

  modifyStat(stbuf);

  return (commitlog_and_return(0));
}

static int pcfs_fgetattr(const char *path, struct stat *stbuf,
			struct fuse_file_info *fi)
{
  global_log_stream -> begin();
  *global_log_stream << "(fgetattr) path is: "
		     << path
		     << "\n";

  // Perform io
  int res = fstat(fi -> fh, stbuf);
  if (res == -1)
    return (commitlog_and_return(-errno));

  // In order to get the attributes of a file, the calling user must
  // have execute access on the file

  if (checkPerm (path, fuse_get_context() -> uid, &global_perm_execute) != 1) {
    // Permission is denied
    return (commitlog_and_return(-EACCES));
  }

  modifyStat(stbuf);

  return (commitlog_and_return(0));
}

static int pcfs_access(const char *path, int mask)
{
  global_log_stream -> begin();
  *global_log_stream << "(access) path is: "
		     << path
		     << "\n";
  
  // No permission is needed a priori to call access

  if (mask  == F_OK) {
    charbuf fullpath(MAX_PATH_LENGTH);
    fullpath << global_src << path;

    if (access (fullpath.getHead(), F_OK) != 0) 
      return (commitlog_and_return(-errno));

    return (commitlog_and_return(0));
  }

  if ((mask & R_OK) && 
      !checkPerm (path, fuse_get_context() -> uid, &global_perm_read)) {
    return (commitlog_and_return(-EACCES));
  }
  
  if ((mask & W_OK) && 
      !checkPerm (path, fuse_get_context() -> uid, &global_perm_write)) {
    return (commitlog_and_return(-EACCES));
  }
  
  if ((mask & X_OK)) {
    
    // This execute permission is the permission to execute the file,
    // or look into a directory, not the PCFS permission to read its
    // attributes. This is checked from the x permission bits.
    
    struct stat statbuf;
    
    charbuf fullpath(MAX_PATH_LENGTH);
    fullpath << global_src << path;

    int res = lstat (fullpath.getHead(), &statbuf);

    if (res == -1) return (commitlog_and_return(-errno));
    
    // if path is a regular file, and none of the x bits are set, return error
    if (S_ISREG(statbuf.st_mode) && 
	!(statbuf.st_mode & (S_IXUSR | S_IXGRP | S_IXOTH))) {
      return (commitlog_and_return(-EACCES));
    }

    // if path is a directory or a soft link, x permission is always
    // allowed.
  }
  
  return (commitlog_and_return(0));
}


static int pcfs_readlink(const char *path, char *buf, size_t size)
{
  global_log_stream -> begin();
  *global_log_stream << "(readlink) path is: "
		     << path
		     << "\n";

  // In order to read a link, the calling process must have read
  // permission on it
  
  if (checkPerm (path, fuse_get_context() -> uid, &global_perm_read) != 1) {
    // Permission is denied
    return (commitlog_and_return(-EACCES));
  }

  // Construct the full path
  charbuf fullpath(MAX_PATH_LENGTH);
  fullpath << global_src << path;
  
  // Perform io
  int res = readlink(fullpath.getHead(), buf, size - 1);
  if (res == -1)
    return (commitlog_and_return(-errno));
  
  buf[res] = '\0';

  return (commitlog_and_return(0));
}

static int pcfs_opendir(const char *path, struct fuse_file_info *fi)
{
  global_log_stream -> begin();
  *global_log_stream << "(opendir) path is: "
		     << path
		     << "\n";

  // In order to open a dir, the calling process must have read
  // permission on it.

  if (checkPerm (path, fuse_get_context() -> uid, &global_perm_read) != 1) {
    // Permission is denied. 
    return (commitlog_and_return(-EACCES));
  }

  // Construct full path
  charbuf fullpath(MAX_PATH_LENGTH);
  fullpath << global_src << path;

  // Perform io
  DIR *dp = opendir(fullpath.getHead());
  if (dp == NULL)
    return (commitlog_and_return(-errno));
  
  fi->fh = (unsigned long) dp;
  return (commitlog_and_return(0));
}

static inline DIR *get_dirp(struct fuse_file_info *fi)
{
  return ((DIR *) (uintptr_t) fi->fh);
}

static int pcfs_readdir(const char *path, void *buf, fuse_fill_dir_t filler,
		       off_t offset, struct fuse_file_info *fi)
{
  global_log_stream -> begin();
  *global_log_stream << "(readdir) path is: "
		     << path
		     << "\n";

  // In order to read a dir, the calling process must have read
  // permission on it.

  if (checkPerm (path, fuse_get_context() -> uid, &global_perm_read) != 1) {
    // Permission is denied
    return (commitlog_and_return(-EACCES));
  }

  (void) path;

  DIR *dp = get_dirp(fi);
  struct dirent *de;
  
  seekdir(dp, offset);
  while ((de = readdir(dp)) != NULL) {
    struct stat st;
    memset(&st, 0, sizeof(st));
    st.st_ino = de->d_ino;
		st.st_mode = de->d_type << 12;
		if (filler(buf, de->d_name, &st, telldir(dp)))
		  break;
  }
  
  return (commitlog_and_return(0));
}

static int pcfs_releasedir(const char *path, struct fuse_file_info *fi)
{
  /* 
     // No need to log -- releasedir has no security implication 
  global_log_stream -> begin();
  *global_log_stream << "(releasedir) path is: "
		     << path
		     << "\n";
  */

  // No access check is needed to release (close) a dir

  DIR *dp = get_dirp(fi);
  closedir(dp);
  return (commitlog_and_return(0));
}

/*

// PCFS does not support special files, so this method is not needed

static int pcfs_mknod(const char *path, mode_t mode, dev_t rdev)
{
  writeToLog ("---------------------------------------------------\n");
  writeToLog ("(opendir) path is: "); 
  writeToLog (path);
  writeToLog ("\n");

	int res;

	if (S_ISFIFO(mode))
		res = mkfifo(path, mode);
	else
		res = mknod(path, mode, rdev);
	if (res == -1)
		return (commitlog_and_return(-errno));

	return (commitlog_and_return(0));
}
*/

static int pcfs_mkdir(const char *path, mode_t mode)
{
  global_log_stream -> begin();
  *global_log_stream << "(mkdir) path is: "
		     << path
		     << "\n";

  // If path is "/", return with error, since "/" cannot be created.
  if (isRoot (path)) return (commitlog_and_return(-EEXIST));

  // To create a new directory, the calling process must have write
  // permission on the parent directory. So we must first obtain the
  // parent directory.

  charbuf parent(path); // initialize a charbuf with path
  parent.rollback ('/'); // roll back to previous '/', thus removing the last component
  if (parent.size() == 0) {
    // parent was '/', so we must push back the '/'
    parent << '/';
  }

  if (!checkPerm (parent.getHead(), fuse_get_context() -> uid, &global_perm_write)) {
    return (commitlog_and_return(-EACCES));
  }
  
  // Create the full path to create
  charbuf fullpath(MAX_PATH_LENGTH);
  fullpath << global_src << path;
  
  // Perform io
  
  int res = mkdir(fullpath.getHead(), mode);
  if (res == -1)
    return (commitlog_and_return(-errno));
  
  // Set ownership of new directory to calling process

  res = chown (fullpath.getHead(), fuse_get_context() -> uid, fuse_get_context() -> gid);
  if (res == -1) 
    return (commitlog_and_return(-errno));

  if (!global_no_insert_attr_newfile) {
    // Set "user.#pcfs.newfile" = 1
    lsetxattr(fullpath.getHead(), "user.#pcfs.newfile", "1", 1, 0);
  }

  if (!global_no_insert_auto_procaps) {
    injectDefaultProCaps (path, fuse_get_context() -> uid, global_no_insert_attr_newfile);
  }

  return (commitlog_and_return(0));
}

static int pcfs_unlink(const char *path)
{
  global_log_stream -> begin();
  *global_log_stream << "(unlink) path is: "
		     << path
		     << "\n";

  // If path is "/", return with error because we cannot remove "/"
  if (isRoot (path)) return (commitlog_and_return(-EACCES));
  
  // To delete a file, the calling process must have identity permission
  // on it. 

  if (!checkPerm (path, fuse_get_context() -> uid, &global_perm_identity)) {
    return (commitlog_and_return(-EACCES));
  }

  // Create the full path to remove
  charbuf fullpath(MAX_PATH_LENGTH);
  fullpath << global_src << path;
  
  // Perform io

  int res = unlink(fullpath.getHead());
  if (res == -1)
    return (commitlog_and_return(-errno));

  // If unlinked file is a procap, mark it dirty in the cache
  if (isAncestor ("/#config/procaps", path) && *(path + 16) != '\0') {
    global_procap_cache -> markDirty ((char *) (path + 17));
  }

  // Remove procaps associated with path. Remove this if you need a
  // more efficient system.
  if (!global_no_remove_auto_procaps) {
    removeProCaps (path, global_procaps, global_log_stream);
  }

  return (commitlog_and_return(0));
}

static int pcfs_rmdir(const char *path)
{
  global_log_stream -> begin();
  *global_log_stream << "(rmdir) path is: "
		     << path
		     << "\n";

  // If path is "/", return with error
  if (isRoot (path)) return (commitlog_and_return(-EACCES));
  
  // To delete a directory, the calling process must have identity
  // permission on the directory.

  if (!checkPerm (path, fuse_get_context() -> uid, &global_perm_identity)) {
    return (commitlog_and_return(-EACCES));
  }

  // Create the full path to remove
  charbuf fullpath(MAX_PATH_LENGTH);
  fullpath << global_src << path;
  
  // Perform io

  int res = rmdir(fullpath.getHead());
  if (res == -1)
    return (commitlog_and_return(-errno));

  // remove associated procaps
  if (!global_no_remove_auto_procaps) {
    removeProCaps (path, global_procaps, global_log_stream);
  }

  return (commitlog_and_return(0));
}

static int pcfs_symlink(const char *from, const char *to)
{
  global_log_stream -> begin();
  *global_log_stream << "(symlink) "
		     << to
		     << " --> "
		     << from
		     << "\n";

  // If to is "/", return with error
  if (isRoot (to)) return (commitlog_and_return(-EACCES));
  
  // The calling process must have write permission to the parent
  // directory of 'to'

  charbuf parent(to); // initialize a charbuf with to
  parent.rollback ('/'); // roll back to previous '/', thus removing the last component
  if (parent.size() == 0) {
    // parent was '/', so we must push back the '/'
    parent << '/';
  }

  if (!checkPerm (parent.getHead(), fuse_get_context() -> uid, &global_perm_write)) {
    return (commitlog_and_return(-EACCES));
  }
  
  // Create the full path of target 
  charbuf fullto(MAX_PATH_LENGTH);
  fullto << global_src << to;

  /*
  // Create the full path of src
  charbuf fullfrom(MAX_PATH_LENGTH);
  fullfrom << global_src << from;
  */

  int res = symlink(from, fullto.getHead());
  if (res == -1)
    return (commitlog_and_return(-errno));

  // Set ownership of new link to calling process
  res = lchown (fullto.getHead(), fuse_get_context() -> uid, fuse_get_context() -> gid);
  if (res == -1) 
    return (commitlog_and_return(-errno));

  // The attribute "newfile" = 1 is never inserted for a symlink

  if (!global_no_insert_auto_procaps) {
    injectDefaultProCaps (to, fuse_get_context() -> uid, true);
  }


  return (commitlog_and_return(0));
}


static int pcfs_rename(const char *from, const char *to)
{
  global_log_stream -> begin();
  *global_log_stream << "(rename) "
		     << from
		     << " to "
		     << to
		     << "\n";

  // If either from or to is "/", return with error
  if (isRoot (from) || isRoot (to)) 
    return (commitlog_and_return(-EACCES));

  
  // The calling process must have write permission to the parent
  // directory of 'to', and in addition it must have write permission
  // to 'to' if it exists. We can check whether 'to' exists by simply
  // stating it. Since we are root, an error should happen only in
  // case of non-existence.

  // Check write permission on parent directory of 'to'
  charbuf toparent(to); // initialize a charbuf with 'to'
  toparent.rollback ('/'); // roll back to previous '/', thus removing the last component
  if (toparent.size() == 0) {
    // parent was '/', so we must push back the '/'
    toparent << '/';
  }
  
  if (!checkPerm (toparent.getHead(), fuse_get_context() -> uid, &global_perm_write)) {
    return (commitlog_and_return(-EACCES));
  }
  
  // Check identity permission on path to be renamed.
  if (!checkPerm (from, fuse_get_context() -> uid, &global_perm_identity)) 
    return (commitlog_and_return(-EACCES));

  
  // Create the full path of target 
  charbuf fullto(MAX_PATH_LENGTH);
  fullto << global_src << to;
  
  // Create the full path of src
  charbuf fullfrom(MAX_PATH_LENGTH);
  fullfrom << global_src << from;
  
  // Check write permission on 'to', if it exists
  struct stat statbuf;
  if (stat (fullto.getHead(), &statbuf) == 0) {
    // 'to' exists, so check write permission on it.
    if (!checkPerm (to, fuse_get_context() -> uid, &global_perm_write)) 
      return (commitlog_and_return(-EACCES));
  }

  int res = rename(fullfrom.getHead(), fullto.getHead());
  if (res == -1)
    return (commitlog_and_return(-errno));
  
  
  // Set ownership of new path to calling process
  res = lchown (fullto.getHead(), fuse_get_context() -> uid, fuse_get_context() -> gid);
  if (res == -1) 
    return (commitlog_and_return(-errno));


  // Now we need to set attribute "user.#pcfs.newfile"=1 and inject
  // default procaps.  This depends on the type of object we moved. If
  // it is a symbolic link, we never set the attributes, else we may
  // (depending on flags)

  struct stat stbuf;
  if (lstat (fullto.getHead(), &stbuf) == 0) {
    // Okay the new object exists

    if (S_ISREG (stbuf.st_mode) || S_ISDIR (stbuf.st_mode)) {
      // to is a file or a directory, so set attribute and inject procaps
      
      if (!global_no_insert_attr_newfile) {
	lsetxattr(fullto.getHead(), "user.#pcfs.newfile", "1", 1, 0);
      }
      
      if (!global_no_insert_auto_procaps) {
	injectDefaultProCaps (to, fuse_get_context() -> uid, global_no_insert_attr_newfile);
      }
      
    }
    
    else {
      // to is a symbolic link, so we do not have to set the attributes
      
      if (!global_no_insert_auto_procaps)
	injectDefaultProCaps (to, fuse_get_context() -> uid, true);
    }
  } // if (lstat (...))

  // If from is a procap, mark its cache entry dirty
  if (isAncestor ("/#config/procaps", from) && *(from + 16) != '\0') {
    global_procap_cache -> markDirty ((char *) (from + 17));
  }

  // remove procaps associated with from
  if (!global_no_remove_auto_procaps) {
    removeProCaps (from, global_procaps, global_log_stream);  
  }

  return (commitlog_and_return(0));
}

static int pcfs_link(const char *from, const char *to)
{
  global_log_stream -> begin();
  *global_log_stream << "(link) "
		     << to
		     << " --> "
		     << from
		     << "\n";


  // If to is "/", return with error
  if (isRoot (to)) return (commitlog_and_return(-EACCES));
  
  // The calling process must have write permission to the parent
  // directory of 'to', and identity permission on 'from'

  // Check write permission to parent of 'to'
  charbuf parent(to); // initialize a charbuf with to
  parent.rollback ('/'); // roll back to previous '/', thus removing the last component
  if (parent.size() == 0) {
    // parent was '/', so we must push back the '/'
    parent << '/';
  }

  if (!checkPerm (parent.getHead(), fuse_get_context() -> uid, &global_perm_write)) {
    return (commitlog_and_return(-EACCES));
  }
  

  // Check identity permission on from
  if (!checkPerm (from, fuse_get_context() -> uid, &global_perm_identity)) {
    return (commitlog_and_return(-EACCES));
  }
  
  // Create the full path of target 
  charbuf fullto(MAX_PATH_LENGTH);
  fullto << global_src << to;
  
  // Create the full path of src
  charbuf fullfrom(MAX_PATH_LENGTH);
  fullfrom << global_src << from;
  
  int res = link(fullfrom.getHead(), fullto.getHead());
  if (res == -1)
    return (commitlog_and_return(-errno));

  struct stat stbuf;
  if (lstat (fullto.getHead(), &stbuf) == 0) {
    // Okay the new object exists

    if (S_ISREG (stbuf.st_mode) || S_ISDIR (stbuf.st_mode)) {
      // to is a file or a directory, so set attribute and inject procaps

      if (!global_no_insert_attr_newfile) {
	lsetxattr(fullto.getHead(), "user.#pcfs.newfile", "1", 1, 0);
      }

      if (!global_no_insert_auto_procaps) {
	injectDefaultProCaps (to, fuse_get_context() -> uid, global_no_insert_attr_newfile);
      }
    }

    else {
      // to is a symbolic link, so we do not have to set the attributes
      if (!global_no_insert_auto_procaps) {
	injectDefaultProCaps (to, fuse_get_context() -> uid, true);
      }

    }
  } // if (lstat (...))

  return (commitlog_and_return(0));
}

static int pcfs_chmod(const char *path, mode_t mode)
{
  global_log_stream -> begin();
  *global_log_stream << "(chmod) path is: "
		     << path
		     << ". Mode is (in decimal): "
		     << (int) mode
		     << "\n";

  // To change modes, the calling process must have write permission
  // on path

  if (!checkPerm (path, fuse_get_context() -> uid, &global_perm_write)) {
    return (commitlog_and_return(-EACCES));
  }

  // Create full path 
  charbuf fullpath(MAX_PATH_LENGTH);
  fullpath << global_src << path;

  int res = chmod(fullpath.getHead(), mode);
  if (res == -1)
    return (commitlog_and_return(-errno));
  
  return (commitlog_and_return(0));
}

static int pcfs_chown(const char *path, uid_t uid, gid_t gid)
{
  global_log_stream -> begin();
  *global_log_stream << "(chown) path is: "
		     << path
		     << ". New uid is: "
		     << (int) uid
		     << "\n";

  // To change ownership, the calling process must have govern
  // permission on path
  if (!checkPerm (path, fuse_get_context() -> uid, &global_perm_govern)) {
    return (commitlog_and_return(-EACCES));
  }
  
  // Create full path 
  charbuf fullpath(MAX_PATH_LENGTH);
  fullpath << global_src << path;

  int res = lchown(fullpath.getHead(), uid, gid);
  if (res == -1)
    return (commitlog_and_return(-errno));
  
  return (commitlog_and_return(0));
}

static int pcfs_truncate(const char *path, off_t size)
{
  global_log_stream -> begin();
  *global_log_stream << "(truncate) path is: "
		     << path
		     << "\n";

  // To change file length, the calling process must have write
  // permission on path
  if (!checkPerm (path, fuse_get_context() -> uid, &global_perm_write)) {
    return (commitlog_and_return(-EACCES));
  }
  
  // Create full path 
  charbuf fullpath(MAX_PATH_LENGTH);
  fullpath << global_src << path;

  int res = truncate(fullpath.getHead(), size);
  if (res == -1)
    return (commitlog_and_return(-errno));

  // If path is a procap, mark its cache entry dirty
  if (isAncestor ("/#config/procaps", path) && *(path + 16) != '\0') {
    global_procap_cache -> markDirty ((char *) (path + 17));
  }

  return (commitlog_and_return(0));
}

static int pcfs_ftruncate(const char *path, off_t size,
			 struct fuse_file_info *fi)
{
  global_log_stream -> begin();
  *global_log_stream << "(ftruncate) path is: "
		     << path
		     << "\n";

  // To change file length, the calling process must have write
  // permission on path
  if (!checkPerm (path, fuse_get_context() -> uid, &global_perm_write)) {
    return (commitlog_and_return(-EACCES));
  }
  
  // Create full path 
  charbuf fullpath(MAX_PATH_LENGTH);
  fullpath << global_src << path;

  int res = ftruncate(fi->fh, size);
  if (res == -1)
    return (commitlog_and_return(-errno));

  // If path is a procap, mark its cache entry dirty
  if (isAncestor ("/#config/procaps", path) && *(path + 16) != '\0') {
    global_procap_cache -> markDirty ((char *) (path + 17));
  }
  
  return (commitlog_and_return(0));
}

static int pcfs_utimens(const char *path, const struct timespec ts[2])
{
  global_log_stream -> begin();
  *global_log_stream << "(utimens) path is: "
		     << path
		     << ". New access time is "
		     << (int) ts[0].tv_sec
		     << ". New modification time is "
		     << (int) ts[1].tv_sec
		     << "\n";

  struct timeval tv[2];
  
  tv[0].tv_sec = ts[0].tv_sec;
  tv[0].tv_usec = ts[0].tv_nsec / 1000;
  tv[1].tv_sec = ts[1].tv_sec;
  tv[1].tv_usec = ts[1].tv_nsec / 1000;
  
  // To change the time stamp, the calling process must have write
  // permission on path
  if (!checkPerm (path, fuse_get_context() -> uid, &global_perm_write))
    return (commitlog_and_return(-EACCES));
  
  // Create full path 
  charbuf fullpath(MAX_PATH_LENGTH);
  fullpath << global_src << path;

  int res = utimes(fullpath.getHead(), tv);
  if (res == -1)
    return (commitlog_and_return(-errno));
  
  return (commitlog_and_return(0));
}

static int pcfs_create(const char *path, mode_t mode, struct fuse_file_info *fi)
{
  global_log_stream -> begin();
  *global_log_stream << "(create) path is: "
		     << path
		     << ". Mode is (in decimal): "
		     << (int) mode
		     << "\n";

  // To create a new file, the calling process must have write
  // permission on the parent directory. So we must first obtain the
  // parent directory.

  charbuf parent(path); // initialize a charbuf with path
  parent.rollback ('/'); // roll back to previous '/', thus removing the last component
  if (parent.size() == 0) {
    // parent was '/', so we must push back the '/'
    parent << '/';
  }

  if (!checkPerm (parent.getHead(), fuse_get_context() -> uid, &global_perm_write)) {
    return (commitlog_and_return(-EACCES));
  }
  
  // Create the full path to create
  charbuf fullpath(MAX_PATH_LENGTH);
  fullpath << global_src << path;
  
  int fd = open(fullpath.getHead(), fi->flags, mode);
  if (fd == -1)
    return (commitlog_and_return(-errno));
  
  
  // Set ownership of new file to calling process

  int res = chown (fullpath.getHead(), fuse_get_context() -> uid, fuse_get_context() -> gid);
  if (res == -1) {
    close(fd);
    return (commitlog_and_return(-errno));
  }

  fi->fh = fd;


  if (!global_no_insert_attr_newfile) {
    lsetxattr(fullpath.getHead(), "user.#pcfs.newfile", "1", 1, 0);
  }

  if (!global_no_insert_auto_procaps) {
    injectDefaultProCaps (path, fuse_get_context() -> uid, global_no_insert_attr_newfile);
  }

  return (commitlog_and_return(0));
}

static int pcfs_open(const char *path, struct fuse_file_info *fi)
{
  global_log_stream -> begin();
  *global_log_stream << "(open) path is: "
		     << path
		     << "\n";


  // Check permissions based on flags in fi -> flags
  
  if ((((fi -> flags) & O_ACCMODE) == O_RDONLY) || (((fi -> flags) & O_ACCMODE) == O_RDWR)) {
    if (!checkPerm (path, fuse_get_context() -> uid, &global_perm_read)) {
      return (commitlog_and_return(-EACCES));
    }
  }
  
  if ((((fi -> flags) & O_ACCMODE) == O_WRONLY) || (((fi -> flags) & O_ACCMODE) == O_RDWR)) {
    if (!checkPerm (path, fuse_get_context() -> uid, &global_perm_write)) {
      return (commitlog_and_return(-EACCES));
    }
  }
  
  // Create the full path to open
  charbuf fullpath(MAX_PATH_LENGTH);
  fullpath << global_src << path;
  
  int fd = open(fullpath.getHead(), fi->flags);
  if (fd == -1)
    return (commitlog_and_return(-errno));
  
  fi->fh = fd;

  // If path is a procap, mark its cache entry dirty, if
  // O_WRONLY or O_RDWR is set
  if (fi -> flags & O_WRONLY || fi -> flags & O_RDWR) {
    if (isAncestor ("/#config/procaps", path) && *(path + 16) != '\0') {
      global_procap_cache -> markDirty ((char *) (path + 17));
    }
  }


  return (commitlog_and_return(0));
}

static int pcfs_read(const char *path, char *buf, size_t size, off_t offset,
		    struct fuse_file_info *fi)
{
  if (!global_no_datacheck) {
    global_log_stream -> begin();
    *global_log_stream << "(read) path is: "
		       << path
		       << "\n";
    
    if (!checkPerm (path, fuse_get_context() -> uid, &global_perm_read)) {
      return (commitlog_and_return(-EACCES));
    }
  }

  int res = pread(fi->fh, buf, size, offset);
  if (res == -1)
    res = -errno;
  
  return (commitlog_and_return(res));
}

static int pcfs_write(const char *path, const char *buf, size_t size,
		     off_t offset, struct fuse_file_info *fi)
{
  if (!global_no_datacheck) {
    global_log_stream -> begin();
    *global_log_stream << "(write) path is: "
		       << path
		       << "\n";
    
    if (!checkPerm (path, fuse_get_context() -> uid, &global_perm_write)) {
      return (commitlog_and_return(-EACCES));
    }
  }
  
  int res = pwrite(fi->fh, buf, size, offset);
  if (res == -1)
    res = -errno;

  return (commitlog_and_return(res));
}

static int pcfs_statfs(const char *path, struct statvfs *stbuf)
{
  global_log_stream -> begin();
  *global_log_stream << "(statfs) path is: "
		     << path
		     << "\n";

  // No checks are needed to statfs

  
  // Create the full path to create
  charbuf fullpath(MAX_PATH_LENGTH);
  fullpath << global_src << path;
  
  int res = statvfs(fullpath.getHead(), stbuf);

  if (res == -1)
    return (commitlog_and_return(-errno));
  
  return (commitlog_and_return(0));
}

/*
static int pcfs_flush(const char *path, struct fuse_file_info *fi)
{
  writeToLog ("---------------------------------------------------\n");
  writeToLog ("(opendir) path is: "); 
  writeToLog (path);
  writeToLog ("\n");

  int res;
  
  (void) path;

  // This is called from every close on an open file, so call the
  // close on the underlying filesystem. But since flush may be called
  // multiple times for an open file, this must not really close the
  // file.  This is important if used on a network filesystem like NFS
  // which flush the data/metadata on close()

  res = close(dup(fi->fh));
  if (res == -1)
    return (commitlog_and_return(-errno));

  return (commitlog_and_return(0));
}
*/


static int pcfs_release(const char *path, struct fuse_file_info *fi)
{
  /* 
     // No need to log -- release has no security implications
  global_log_stream -> begin();
  *global_log_stream << "(release) path is: "
		     << path
		     << "\n";
  */

  // No access check is needed to release (close) a file

  (void) path;
  close(fi->fh);
  
  return (commitlog_and_return(0));
}

static int pcfs_fsync(const char *path, int isdatasync,
		     struct fuse_file_info *fi)
{
  /*
    // No need to log -- fsync has no security implications
  global_log_stream -> begin();
  *global_log_stream << "(fsync) path is: "
		     << path
		     << "\n";
  */
  // No access check is needed to sync

  int res;
  
#ifndef HAVE_FDATASYNC
  (void) isdatasync;
#else
  if (isdatasync)
    res = fdatasync(fi->fh);
  else
#endif
    res = fsync(fi->fh);
  if (res == -1)
    return (commitlog_and_return(-errno));
  
  return (commitlog_and_return(0));
}

static int pcfs_setxattr(const char *path, const char *name, const char *value,
			size_t size, int flags)
{
  global_log_stream -> begin();
  *global_log_stream << "(setxattr) path is: "
		     << path 
		     << ". Attribute name is: "
		     << name 
		     << "\n";

  // If attribute starts with "user.#pcfs." govern permission is
  // needed. Else if it starts with "user.", write permission is
  // needed. Else we return an error.

  if (!strncmp (name, "user.#pcfs.", 11))  {
    if (!checkPerm (path, fuse_get_context() -> uid, &global_perm_govern)) {
      return (commitlog_and_return(-EACCES));
    }
  }

  else if (!strncmp (name, "user.", 5)) {
    if (!checkPerm (path, fuse_get_context() -> uid, &global_perm_write)) {
      return (commitlog_and_return(-EACCES));
    }
  }
  
  else return (commitlog_and_return(-EACCES));

  // Create the full path to setxattr
  charbuf fullpath(MAX_PATH_LENGTH);
  fullpath << global_src << path;
  
  int res = lsetxattr(fullpath.getHead(), name, value, size, flags);
  if (res == -1)
    return (commitlog_and_return(-errno));
  return (commitlog_and_return(0));
}

static int pcfs_getxattr(const char *path, const char *name, char *value,
			size_t size)
{
  global_log_stream -> begin();
  *global_log_stream << "(getxattr) path is: "
		     << path
		     << ". Attribute name is: "
		     << name
		     << "\n";

  if (!checkPerm (path, fuse_get_context() -> uid, &global_perm_execute)) {
    return (commitlog_and_return(-EACCES));
  }
  
  // Create the full path to getxattr
  charbuf fullpath(MAX_PATH_LENGTH);
  fullpath << global_src << path;
  
  int res = lgetxattr(fullpath.getHead(), name, value, size);
  if (res == -1)
    return (commitlog_and_return(-errno));

  return (commitlog_and_return(res));
}

static int pcfs_listxattr(const char *path, char *list, size_t size)
{
  global_log_stream -> begin();
  *global_log_stream << "(listxattr) path is: "
		     << path
		     << "\n";

  if (!checkPerm (path, fuse_get_context() -> uid, &global_perm_execute)) {
    return (commitlog_and_return(-EACCES));
  }
  
  // Create the full path to listxattr
  charbuf fullpath(MAX_PATH_LENGTH);
  fullpath << global_src << path;
  
  int res = llistxattr(fullpath.getHead(), list, size);
  if (res == -1)
    return (commitlog_and_return(-errno));

  return (commitlog_and_return(res));
}

static int pcfs_removexattr(const char *path, const char *name)
{
  global_log_stream -> begin();
  *global_log_stream << "(removexattr) path is: "
		     << path
		     << ". Attribute name is: "
		     << name
		     << "\n";

  // If attribute starts with "user.#pcfs." govern permission is
  // needed. Else if it starts with "user.", write permission is
  // needed. Else we return an error.

  if (!strncmp (name, "user.#pcfs.", 11))  {
    if (!checkPerm (path, fuse_get_context() -> uid, &global_perm_govern)) {
      return (commitlog_and_return(-EACCES));
    }
  }

  else if (!strncmp (name, "user.", 5)) {
    if (!checkPerm (path, fuse_get_context() -> uid, &global_perm_write)) {
      return (commitlog_and_return(-EACCES));
    }
  }
  
  else return (commitlog_and_return(-EACCES));

  // Create the full path to removexattr
  charbuf fullpath(MAX_PATH_LENGTH);
  fullpath << global_src << path;
  
  int res = lremovexattr(fullpath.getHead(), name);
  if (res == -1)
    return (commitlog_and_return(-errno));

  return (commitlog_and_return(0));
}

/*
static int pcfs_lock(const char *path, struct fuse_file_info *fi, int cmd,
		     struct flock *lock)
{
	(void) path;

	return ulockmgr_op(fi->fh, cmd, lock, &fi->lock_owner,
			   sizeof(fi->lock_owner));
}
*/


static struct fuse_operations pcfs_oper;


struct fuse_operations * initPcfsHandlers()
{

  memset (&pcfs_oper, 0, sizeof(pcfs_oper));
  
  pcfs_oper.getattr	= pcfs_getattr;
  pcfs_oper.fgetattr	= pcfs_fgetattr;
  pcfs_oper.access	= pcfs_access;
  pcfs_oper.readlink	= pcfs_readlink;
  pcfs_oper.opendir	= pcfs_opendir;
  pcfs_oper.readdir	= pcfs_readdir;
  pcfs_oper.releasedir	= pcfs_releasedir;
  //  pcfs_oper.mknod	= pcfs_mknod;
  pcfs_oper.mkdir	= pcfs_mkdir;
  pcfs_oper.symlink	= pcfs_symlink;
  pcfs_oper.unlink	= pcfs_unlink;
  pcfs_oper.rmdir	= pcfs_rmdir;
  pcfs_oper.rename	= pcfs_rename;
  pcfs_oper.link	= pcfs_link;
  pcfs_oper.chmod	= pcfs_chmod;
  pcfs_oper.chown	= pcfs_chown;
  pcfs_oper.truncate	= pcfs_truncate;
  pcfs_oper.ftruncate	= pcfs_ftruncate;
  pcfs_oper.utimens	= pcfs_utimens;
  pcfs_oper.create	= pcfs_create;
  pcfs_oper.open	= pcfs_open;
  pcfs_oper.read	= pcfs_read;
  pcfs_oper.write	= pcfs_write;
  pcfs_oper.statfs	= pcfs_statfs;
  //  pcfs_oper.flush	= pcfs_flush;
  pcfs_oper.release	= pcfs_release;
  pcfs_oper.fsync	= pcfs_fsync;
  pcfs_oper.setxattr	= pcfs_setxattr;
  pcfs_oper.getxattr	= pcfs_getxattr;
  pcfs_oper.listxattr	= pcfs_listxattr;
  pcfs_oper.removexattr	= pcfs_removexattr;
  //  pcfs_oper.lock        = pcfs_lock;

  return &pcfs_oper;
}
