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

#ifndef __LOGSTREAM_HPP
#define __LOGSTREAM_HPP

#include <iostream>

#include <sys/time.h>
#include <time.h>


#include "parse-check/pcfs-syn.hpp"
#include "charbuf.hpp"

/* 
   logstream is a class similar to charbuf. It is also derived from
   charstream. In this case, the buffer loops, overwriting itself,
   when the end is reached.

   The stream is also associated with a file, which is written
   whenever commit() is called. Note that commit() does not reset the
   buffer. So calling commit() again, will write the entire buffer
   again, unless reset() is called in between.

   deleting a logstream causes an automatic commit().
*/

class logstream: public charstream
{
protected:
  char * head; // beginning of buffer
  char * tail; // tail of buffer. This is where the next character is
	       // written

  char * end; // Always points to the last character of the buffer,
	      // i.e., head + size. If tail reaches this value,
	      // tail is set to head.

  bool notfull; // This is true iff the tail has not wrapped around.

  int fd; // File descriptor to which commits are made.

  // protected function to add a character to the current buffer. It
  // will increment the tail intelligently
  void addchar (char c) {
    * tail = c;
    if (tail == end) {
      tail = head;
      notfull = false;
    }
    else {
      tail ++;
    }
  }

public:

  // Create a buffer of length msize+1, that will hold msize
  // characters (the tail always points to an empty cell)
  logstream (int msize, int file) {
    head = new char[msize+1];
    end = head + msize;
    tail = head;
    notfull = true;
    fd = file;
  }

  // Add a character to the tail. 
  logstream & operator << (char c) {
#ifndef NO_LOG
    addchar (c);
#endif

    return * this;
  }

  // Add a term to the tail. 
  logstream & operator << (Term * t) {

#ifndef NO_LOG
    t -> print (*this);
#endif

    return * this;
  }

  // Add a character string to the tail. 
  logstream & operator << (const char * str) {
#ifndef NO_LOG
    while (*str != '\0') {
      addchar (*str ++);
    }
#endif

    return * this;
  }
  
  // Add an integer to the tail. 
  logstream & operator << (int n) {
#ifndef NO_LOG
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
      addchar(buf[i]);
    }
      
#endif

    return * this;
  }

  // Start a new log transaction
  void begin() {
#ifndef NO_LOG
    struct timeval this_time;
    gettimeofday(&this_time, NULL);
  
    (*this) << "----------------------\n"
	    << "[BT] Time: " 
	    << ((int)this_time.tv_sec) << "(s) + " 
	    << ((int)this_time.tv_usec) << "(us)\n"; 
#endif    
  }
  
  // Return buffer size, i.e., number of characters it can hold
  int size() {
    return (end - head);
  }

  // Reset the buffer, i.e., set it to empty
  void reset() {
#ifndef NO_LOG
    tail = head;
    notfull = true;
#endif
  }

  // Write the buffer to file. Return 1 on success, 0 on failure
  int commit () {
#ifndef NO_LOG
    if (!notfull) {
      // Buffer was full. Tail has wrapped around. So write [tail+1,
      // end] first. Then write [head, tail-1];
      int l = end - tail; 
      
      if (write(fd, tail + 1, l) < l) {
	return 0;
      }
      
      l = tail - head;
      if (write(fd, head, l) < l) {
	return 0;
      }
      
      return 1;
    }

    else {
      // Buffer was not full, so write [head,tail]
      int l = tail - head;
      if (write(fd, head, l) < l) {
	return 0;
      }
      return 1;
    }
#else
    return 1;
#endif
  }


  ~logstream() { 
    commit();
    delete [] head;
  }
};


#endif
