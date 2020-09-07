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

#ifndef __DISK_CACHE_H
#define __DISK_CACHE_H

#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <iostream>

#include "../nPatriciaTrie.h"
#include "diskcache-common.hpp"
#include "BiQueue.hpp"
#include "../charbuf.hpp"

using namespace std;

enum NodeType {file = 0, directory = 1};

struct NodeData
{
public:
  bool hasData;  
  // true if data has been read
  
  bool isCurrent; 
  // false if data on disk has changed

  void * data; 
  // Opaque pointer to parsed data; 
  // may be NULL if there is a parse error or if hasData == false
  
  NodeType type;
  // type of node

  BiQueueNode<char *> * qNode;
  // entry of the current node in the priority Queue

  NodeData() {
    hasData = false;
    isCurrent = true;
    data = NULL;
    type = file;
    qNode = NULL;
  }

  NodeData(NodeType t) {
    hasData = false;
    isCurrent = true;
    data = NULL;
    type = t;
    qNode = NULL;
  }

};


class DiskCache
{
 protected:
  
  nPatriciaTrie<NodeData*> * trie;
  // Trie containing data

  char * rootPath;
  // Path where it all begins; must be a directory
  // We will make sure that rootPath has a trailing '/'

  unsigned nEntries;
  // Number of entries held currently

  unsigned const maxEntries;
  // Maximum number of entries allowed

  void * (*const parser)(NodeType, const char *);
  // A parser that given a type (file or directory) and the name on
  // disk, parses the data and returns it (as a void *). In simple
  // cases, the parser may simply return NULL for directories, and the
  // contents of the file as a char* (string).
    
  void (*const dealloc)(NodeType, void *);
  // A deallocator for parsed data. The deallocator is passed the type
  // of node (file or directory), and the data. It is guaranteed that
  // the data was earlier parsed using the parser function above,
  // using the same type. Note that if the parser had returned NULL
  // during parsing, the data passed in will also be NULL (i.e., for
  // the cache, NULL is a valid value of data).

  BiQueue<char *>  * priorityQ;
  // This queue contains the index of every string that is in the
  // trie. The data in the queue are arranged in LRU order: the node
  // at the tail is always the most recently used node. This queue
  // serves two purposes: to keep track of strings in the trie (needed
  // when the destructor is called), and to allow for cache
  // replacement.


  // Methods


  // Find the type (file or directory) for a pathname
  // Write output to *type.
  // Return 1 on success, error code on error (e.g., path does not exist)
  static int determineType (const char * path, NodeType * type) ;

  // Find a node in the trie, and return it. If node is not in trie,
  // but createIfNeeded is true, then create a new node and return
  // that (provided path exists on disk). If moveToTail is true, the
  // returned node will also be moved to the tail of the priorityQ. If
  // a new node has to be created, it will always be at the tail of
  // the priorityQ.
  // Return NULL in case of failure
  NodeData * findNode(char * path, bool moveToTail, bool createIfNeeded);

  // Add a new node, only if the path actually exists on disk. The new
  // node is returned in newnode Return 1 on success, error code
  // otherwise. If there is not enough space in the Cache, then a node
  // will be deleted according to LRU.  (It is a bad idea to call
  // createNode on a path that already exists in the trie. Although
  // this is not an error, it may cause some other node to be paged
  // out unnecessarily)
  int createNode (char * path, NodeData ** newnode);
 
  // We keep the constructor protected to prevent direct use of this class.
  // The class must be extended to provide functionality with specific parsers.
  DiskCache(unsigned maxE, void * (*p) (NodeType, const char *), void (*d)(NodeType, void*), const char * path);
  
 public:
  
  unsigned getNumberOfEntries();

  char * getPathOfRoot();

  // DEBUG function
  BiQueue<char *> * getPriorityQ() {
    return priorityQ;
  }

  // Mark an entry as dirty (modified on disk). Returns 1 if path is
  // valid, error code otherwise
  int markDirty (char * path);

  // Refresh the data in node 'nodedata' by reading 'path'. If
  // nodedata is NULL, then the correct NodeData is determined by
  // calling findNode on 'path'. If you pass in a non-NULL value of
  // nodedata, then you must also push it to the end of
  // priorityQ. forceRefresh will not do that. If forceRefresh cannot
  // find the data on disk or the node's type is inconsistent with the
  // type of path on disk, forceRefresh will delete the node from
  // memory, and return an error.  Return 1 on success, error code
  // otherwise.
  int forceRefresh(char * path, NodeData * nodedata = NULL);

  // Get data from a node. May refresh the data if data has not been
  // read, or is not current, or it may also create a new node if one
  // does not exist.
  void * getData(char * path);

  // Get the type of a node. It may add a new node to the trie if the
  // node does not exist, but path exists on disk. Writes the type to
  // the output parameter ret. Returns 1 on success, error code
  // otherwise.
  int getType(char * path, NodeType * ret);
  
  // Delete data from node corresponding to path. Note that this
  // function will NOT create a node in the trie if it does not
  // already exist. Returns 1 on success, 0 on error. (Currently this
  // function never returns 0).
  int flush (char * path);


  // Remove a node from the trie. If any data is associated with the
  // node, that is deleted as well. Returns 1 if node is in the trie,
  // error code otherwise.
  int remove(char * path);
  
  virtual ~DiskCache();

};


//------------------------------------------------------------------------------------

inline int DiskCache::determineType(const char * path, NodeType * type) 
{
  /// cerr << "DiskCache::determineType() : determining type of node : " << path << "\n";
  struct stat fileData;
  if (lstat(path, &fileData) == -1) return CACHE_ERR_NOFILE;
  
  if (S_ISDIR(fileData.st_mode)) {
    *type = directory;
    return 1;
  }
  else if (S_ISREG(fileData.st_mode)) {
    *type = file;
    return 1;
  }

  return CACHE_ERR_NOFILE;
}



inline NodeData * DiskCache::findNode (char * path, bool moveToTail, bool createIfNeeded)
{
  /// cerr << "DiskCache::findNode() : finding node : " << path << "\n";

  NodeData * data = trie -> Lookup(path) ;
  if (data != NULL) {
    if (moveToTail) priorityQ -> moveToTail(data -> qNode);
    return data;
  }

  // Okay data path does not exist, so create a new node if
  // createIfNeeded is true

  if (createIfNeeded) {
    NodeData * newNode;

    int r = createNode (path, &newNode);
    
    if (r == 1) {
      return newNode;
      // Note: newNode should never come back as NULL if r is 1
      // There is no need to move newNode to tail, since createNode
      // always adds new nodes to the tail
    }
    else return NULL;
  }

  else return NULL;
}



inline unsigned DiskCache::getNumberOfEntries() 
{
  return nEntries;
}


inline char * DiskCache::getPathOfRoot() 
{
  return rootPath;
}


inline int DiskCache::markDirty(char * path) 
{
  /// cerr << "DiskCache::markDirty() : marking node : " << path << "\n";

  NodeData * n = findNode(path, false, false);
  if (n == NULL) return CACHE_ERR_NOFILE;

  n -> isCurrent = false;
  return 1;

}


inline int DiskCache::forceRefresh(char * path, NodeData * nodedata)
{
  /// cerr << "DiskCache::forceRefresh() : forcing node : " << path << "\n";

  NodeData * n;

  if (nodedata == NULL) {
    n = findNode(path, true, true);
    if (n == NULL) return CACHE_ERR_NOFILE;
  }
  else {
    n = nodedata;
    // There is no need to push n to the tail of priorityQ; the
    // calling function must do that
  }

  charbuf fullpath(MAX_PATH_LENGTH);
  fullpath << rootPath << path;

  NodeType t;
  int r = determineType (fullpath.getHead(), &t);
  if (r != 1) {
    // Okay the node has disappeared from disk, so we must clear out
    // this node from the tree
    remove (path);
    return CACHE_ERR_NOFILE;
  }

  // In the following two cases, the type of node in memory and disk
  // is inconsistent, so we must remove from memory
  if (t == file && n -> type == directory) {
    remove (path);
    return CACHE_ERR_DIRISFILE;
  }
  if (t == directory && n -> type == file) {
    remove (path);
    return CACHE_ERR_FILEISDIR;
  }

  void *newdata = (*parser) (n -> type, fullpath.getHead());
  
  // Deallocate existing data if any
  if (n -> hasData) ((*dealloc) (n -> type, n -> data));

  n -> data = newdata;

  n -> isCurrent = true;
  n -> hasData = true;

  return 1;
}



inline void * DiskCache::getData(char * path)
{
  /// cerr << "DiskCache::getData() : getting node : " << path << "\n";

  NodeData * n = findNode(path, true, true);
  if (n == NULL) return NULL;
  
  if (!(n -> hasData) || !(n -> isCurrent)) {
    int r = forceRefresh(path, n);
    if (r == 1) return n -> data;
    else return NULL;
  }
  else return n -> data;
}


inline int DiskCache::getType(char * path, NodeType * ret)
{
  /// cerr << "DiskCache::getType() : getting type of node : " << path << "\n";
  
  NodeData * n = findNode(path, true, true);
  
  if (n == NULL) return CACHE_ERR_NOFILE;

  *ret =  (n -> type);

  return 1;
}


inline int DiskCache::flush(char * path)
{
  /// cerr << "DiskCache::flush() : flushing node : " << path << "\n";
  NodeData * n = findNode(path, false, false); 
  // The false means that the node n should not be pushed to the end of the queue
  
  if (n == NULL) return CACHE_ERR_NOFILE;

  if (n -> hasData) {
    ((*dealloc) (n -> type, n -> data));
    n -> data = NULL; // This is completely optional
  }
  n -> isCurrent = true;
  n -> hasData = false;
  return 1;
}

inline int DiskCache::remove (char * path) 
{
  /// cerr << "DiskCache::remove() : removing node : " << path << "\n";

  NodeData * nodeData = trie -> Lookup (path);

  if (nodeData == NULL)  return CACHE_ERR_NOFILE; // path was not in the trie
  
  trie -> Delete(path);

  if (nodeData -> hasData) {
    dealloc (nodeData -> type, nodeData -> data);
  }
  
  // Remove node from priority queue.
  // This must always succeed
  priorityQ -> remove (nodeData -> qNode);
  
  // delete the char pointer in the queue node
  delete [] (nodeData -> qNode -> d);
  
  // delete the queuenode
  delete (nodeData -> qNode);

  nEntries --;

  delete nodeData;
  /// cerr << "Remove done\n";
  return 1;
}

inline DiskCache::~DiskCache() 
{
  /// cerr << "DiskCache::~DiskCache entered\n";
  delete [] rootPath;

  while (priorityQ -> size() > 0) {
    char * index = priorityQ -> top();
    //    cerr << "Removing node: " << index << "\n";

    remove (index);

  }
  delete trie;
  delete priorityQ;
}

#endif
