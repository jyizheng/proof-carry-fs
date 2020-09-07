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
#include "../paths-common.hpp"
#include "DiskCacheTrivial.hpp"
#include "../Queue.hpp"

using namespace std;

int main()
{
  DiskCacheTrivial * cache = new DiskCacheTrivial(2, ".");
  cout << cache -> getPathOfRoot() << "\n";

  NodeType type = file;

  char currpath[1024] = "data";

  bool done = false;

  while (!done) {
    cout << "Menu: \n";
    cout << "1. ls current node (directory only)\n";
    cout << "2. cat current node (file only)\n";
    cout << "3. List type of current node\n";
    cout << "4. Navigate to a child node\n";
    cout << "5. Navigate to parent\n";
    cout << "6. Force refresh on current node\n";
    cout << "7. Force data flush on current node\n";
    cout << "8. Force complete flush on current node\n";
    cout << "9. Mark current node dirty\n";
    cout << "10. Quit\n";
    cout << "-----------------------------------------------\n";
    
    cout << "Current number of cached nodes: " << cache -> getNumberOfEntries() << "\n";
    cout << "Current number of nodes in queue: " << cache -> getPriorityQ() -> size() << "\n";
    cout << currpath << "> " ;
    int choice;
    cin >> choice;
    
    switch (choice) {

    case 1:
      cache -> getType (currpath, &type);
      if (type == file)
	{
	  cout << "Current node is a file .. you may want to try choice 2\n";
	}
      else
	{
	  Queue<char *> * names = (Queue<char *> *)(cache -> getData(currpath));
	  if (names == NULL) {
	    cout << "Sorry, some error happened\n";
	  }
	  else {
	    QueueNode <char *> * n = names -> getHead() -> next;
	    while (n != NULL) {
	      cout << n -> d << "\n";
	      n = n -> next;
	    }
	  }
	}
      break;

    case 2:
      cache -> getType (currpath, &type);
      if (type == directory)
	{
	  cout << "Current node is a directory .. you may want to try choice 1\n";
	}
      else
	{
	  const char * data = (char *) (cache -> getData(currpath));
	  if (data == NULL) {
	    cout << "Sorry, some error happened (did the file on disk get deleted?)\n";
	  }
	  else {
	    cout << data << "\n";
	  }
	}
      break;

    case 3:
      cache -> getType (currpath, &type);
      cout << ((type == file) ? "file" : "directory") << "\n";
      break;

    case 4:
      cache -> getType (currpath, &type);
      if (type != directory) {
	cout << "Current node is a file\n";
      }
      else {
	cout << "Enter name of child: ";
	char name[256];
	cin >> name;
	
	if (addNewComponentToPath(currpath, name) != 1) {
	  cout << "Some error happened (does the child name have a slash in between?)\n";
	}
	else {
	  // Check if the new path exists
	  if (cache -> getType(currpath, &type) != 1) {
	    // New path does not exist: print error and roll back
	    cout << "Some error happened (does the child exist?)\n";
	    removeLastComponentOfPath(currpath);
	  }
	  else {
	    cout << "New path is: " << currpath << "\n";
	  }
	}
      }
      break;

    case 5:
      if (!strcmp(currpath, "data")) {
	cout << "We are at the root, there is no parent\n";
      }
      else {
	removeLastComponentOfPath(currpath);
	cout << "New Node is: " << currpath << "\n";
      }
      break;

    case 6:
      {
	int r = cache -> forceRefresh(currpath);
	if (r == 1) {
	  cout << "Refresh done\n";
	}
	else {
	  cout << "Sorry, some error happened\n";
	}
      }
      break;

    case 7:
      cache -> flush(currpath);
      cout << "Data Clear done\n";
      break;

    case 8:
      if (cache -> remove(currpath) != 1) {
	cout << "Sorry, some error happened (did you already flush completely earlier?)\n";
      }
      else {
	cout << "Remove done\n";
      }
      break;

    case 9:
      if (cache -> markDirty(currpath) != 1) {
	cout << "Sorry, some error happened (may be the node was flushed out earlier?)\n";
      }
      else {
	cout << "Marked dirty\n";
      }
      break;

    case 10:
      done = true;
      break;

    default:
      break;
    }
    cout << "-----------------------------------------------\n";
  }


  delete cache;

  return 0;
}
