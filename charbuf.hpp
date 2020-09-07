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

#ifndef __STRING_BUF_HPP
#define __STRING_BUF_HPP

#include <iostream>

#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>

using namespace std;

/* An abstract class of character streams. This is an interface for charstreams
 */

class charstream 
{
public:
  charstream() { }
  virtual charstream & operator << (char c) = 0;
  virtual charstream & operator << (const char * c) = 0;
  virtual charstream & operator << (int i) = 0;
  virtual ~charstream() { };
};

/* A class to hold buffers of characters. Each buffer is a bounded
   length array of characters, which can be used like a stack. The
   base end (0) is called the head, and the other the tail. There are
   functions to add and remove characters from the tail end. The tail
   end is guaranteed to be null terminated (i.e., the tail pointer
   always points to a null character), so head can be used as a null
   terminated string. The insertion functions don't keep track of
   length, and it is possible to run over the allocated length.
*/

class charbuf: public charstream
  {
protected:
  char * head;
  char * tail;

public:

  // Create a buffer of length size + 1 that can hold size characters. 
  charbuf (int size) {
    head = new char[size + 1];
    head[0] = '\0';
    tail = head;
  }

  // Create a buffer by copying a string
  charbuf (const char * str) {
    int l = strlen(str);
    head = new char[l + 1];
    strcpy(head, str);
    tail = head + l;
  }

  // Add a character to the tail. 
  charbuf & operator << (char c) {
    *tail++ = c;
    *tail = '\0';
    return * this;
  }


  // Add a character string to the tail. 
  charbuf & operator << (const char * str) {
    while (*str != '\0') {
      *tail++ = *str++;
    }
    *tail = '\0';
    return * this;
  }
  
  // Add an integer to the tail. 
  charbuf & operator << (int n) {
    int pos = (n >= 0) ? n : (-n);

    char buf[20];

    buf[19] = '\0';
    int i = 18;
    while (pos > 0) {
      buf[i] = (char)(pos % 10 + '0');
      i -- ;
      pos = pos / 10;
    }

    if (n == 0) { 
      buf[i] = '0';
    }

    else if (n < 0) {
      buf [i] = '-';
    }
    
    else {
      i++;
    }

    for (; i <= 18; i++) {
      *tail++ = buf[i];
    }

    *tail = '\0';

    return * this;
  }

  // Remove from tail end, all characters upto and including the first
  // occurence of c. If c is not found, tail is set equal to head
  // (buffer is emptied). Note that if c == '\0', this call has no
  // effect.
  void rollback(char c) {
    while (tail != head && *tail != c) {
      tail --;
    }
    
    *tail = '\0';
  }

  // Use this function to convert to char *
  char * getHead() const {
    return head;
  }

  // Use this function if you want to iterate backwards.
  char * getTail() const {
    return tail;
  }

  int size() const {
    return tail - head;
  }

  ~charbuf() {
    delete [] head;
  }

  // Write the buffer to a file pointed by descriptor fd. Returns 1 on
  // success, 0 on failure
  int writeToFile (int fd) {
    int l = size();
    int niters = l / 512;
    int leftover = l % 512;

    char * root = head;
    for (int i = 0; i < niters; i++) {
      if (write (fd, root, 512) != 512) {
	return 0;
      }

      root = root + 512;
    }

    if (leftover > 0) {
      if (write (fd, root, leftover) != leftover) {
	return 0;
      }
    }

    return 1;
  }


};

inline ostream & operator << (ostream & out, const charbuf & cb) {
  return (out << (cb.getHead()));
}

#endif
