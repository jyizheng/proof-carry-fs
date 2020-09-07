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

#include <iostream>
#include "DiskCache.hpp"
#include "../charbuf.hpp"

using namespace std;

int DiskCache::createNode(char * path, NodeData ** output) 
{
  /// cerr << "DiskCache::createNode() : creating node : " << path << "\n";
  charbuf fullpath(MAX_PATH_LENGTH);
  fullpath << rootPath << path;

  NodeType type;
  if (determineType (fullpath.getHead(), &type) != 1) {
    return CACHE_ERR_NOFILE;
  }

  // Check if there is space for another node, else we will remove a node
  if (nEntries >= maxEntries) {
    // Must deallocate
    char * p = priorityQ -> top(); 
    // get the oldest node (the one at the head)

    remove(p);
  }

  NodeData * newNode = new NodeData(type);

  if (trie -> Insert(path, newNode) == NULL) {
    // Node already existed in trie, so deallocate
    delete newNode;
    *output = NULL;
  }
  else {
    char * npath = new char[strlen(path) + 1];
    strcpy(npath, path);
    BiQueueNode<char *> * qNode = priorityQ -> push(npath);
    newNode -> qNode = qNode;

    nEntries ++;
    *output = newNode;
  }
  
  return 1;
}



DiskCache::DiskCache(unsigned maxE, void * (*p) (NodeType, const char *), void (*d) (NodeType,void*), const char * path)
: maxEntries(maxE), parser(p), dealloc (d)
{ 
  NodeType type;

  if (maxE == 0) 
    throw DiskCacheError();
  // The cache must allow at least one entry, else it cannot be used

  if (determineType(path, &type) != 1 || type == file) 
    throw DiskCacheError();

  else { 
    // cerr << "DiskCache::DiskCache() : Determined type of Node\n";

    
    rootPath = new char[strlen(path) + 2];
    
    char * t = rootPath;

    while (*path != '\0') {
      *(t ++) = *(path++);
    }

    if (*(--path) != '/') {
      * (t++) = '/';
    }
    
    * t = '\0';

    // cerr << "DiskCache::DiskCache() : rootPath is : " << rootPath << "\n";
  }
  
  trie = new nPatriciaTrie<NodeData*> ();

  priorityQ = new BiQueue<char *>();

  nEntries = 0;
} 


