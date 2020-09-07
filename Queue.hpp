/*
The MIT License

Copyright (c) <2001,2008,2009> <Deepak Garg dg@cs.cmu.edu>

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

#ifndef __QUEUE_H
#define __QUEUE_H

#include <iostream>

#include "charbuf.hpp"

using namespace std;

template<class Datatype>
struct QueueNode {
  Datatype d;
  QueueNode * next;
};

template <class Datatype>
class Queue
{

 private:
  QueueNode<Datatype> * head;
  QueueNode<Datatype> * tail;
  int length;

 public:
  Queue() {
    head = tail = new QueueNode<Datatype>;
    head -> next = NULL;
    length = 0;
  }      

  QueueNode<Datatype> * getHead() {
    return head;
  }

  Datatype top() {
    if (length > 0) return head -> next -> d;
    else return head -> d;
  }

  Datatype pop() {
    if (length > 0) {
      Datatype d = head -> next -> d;
      QueueNode<Datatype> * t = head;
      head = head -> next;
      delete t;
      length --;
      return d;
    }
    else return head -> d;
  }

  void push (Datatype d) {
    tail -> next = new QueueNode<Datatype>;
    tail = tail -> next;
    tail -> d = d;
    tail -> next = NULL;
    length ++;
  }

  void pushHead(Datatype d) {
    QueueNode<Datatype> * n = new QueueNode<Datatype>;
    n -> next = head;
    head -> d = d;
    head = n;
    length ++;
  }

  bool isEmpty() {
    return length == 0;
  }

  ~Queue() {
    while (head != tail) {
      QueueNode<Datatype> * p = head;
      head = head -> next;
      delete p;
    }
    delete head;
  }

  int size() {
    return length;
  }

};


template <class DataType>
static void freeQueueWithData(Queue <DataType *> * q)
{
  while (q -> size() > 0) {
    DataType * d = q -> pop();
    delete d;
  }
  delete q;
}

/* 
// No code is currently using this function. Uncomment if needed

template <class DataType>
static void freeQueueWithDataArray(Queue <DataType *> * q)
{
  while (q -> size() > 0) {
    DataType * d = q -> pop();
    delete [] d;
  }
  delete q;
}
*/

 // Print a queue, assuming that a print method is defined for the
 // elements. 'prefix' is printed before each element, and 'suffix'
 // after each element
template <class DataType>
static ostream & printQueue(ostream & out, Queue <DataType *> * q, const char * prefix, const char * suffix)
{
  QueueNode <DataType *> * node = q -> getHead() -> next;
  
  while (node != NULL) {
    out << prefix;
    node -> d -> print(out); 
    out << suffix;
    node = node -> next;
  }

  return out;
}

template <class DataType>
static charstream & printQueue(charstream & out, Queue <DataType *> * q, const char * prefix, const char * suffix)
{
  QueueNode <DataType *> * node = q -> getHead() -> next;
  
  while (node != NULL) {
    out << prefix;
    node -> d -> print(out); 
    out << suffix;
    node = node -> next;
  }

  return out;
}

#endif

