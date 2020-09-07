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

#ifndef __BIQUEUE_H
#define __BIQUEUE_H

// An implementation of a bi-directional Queue.

template <class T>
struct BiQueueNode
{
  T d;  // Data
  BiQueueNode<T> * next;
  BiQueueNode<T> * prev;

  BiQueueNode() 
  {
    next = prev = NULL;
  }

  BiQueueNode(T data, BiQueueNode<T> * n, BiQueueNode<T> * p)
  {
    d = data;
    next = n;
    prev = p;
  }

  ~BiQueueNode() { }

};


template <class T>
class BiQueue
{
 protected:
  
  BiQueueNode<T> * head;
  BiQueueNode<T> * tail;
  int length;
  
 public:
  
  BiQueue() {
    head = tail = new BiQueueNode<T>;
    length = 0;
  }

  int size() {
    return length;
  }

  // Add at the tail
  BiQueueNode<T> * push(T data) {
    tail -> next = new BiQueueNode<T> (data, NULL, tail);
    tail = tail ->  next;
    length ++;
    return tail;
  }

  // Add node to tail
  void pushNode(BiQueueNode<T> * node) {
    tail -> next = node;
    node -> prev = tail;
    node -> next = NULL;
    tail = tail -> next;
    length ++;
  }


  // Remove from head, return data
  T pop() {
    // Empty queue
    if (head == tail) return head -> d;
    
    T d = head -> next -> d;
    BiQueueNode<T> * t = head;

    head = head -> next;
    head -> prev = NULL;

    delete t;
    length --;

    return d;
  }

  // Return data at the head
  T top() {
    if (head == tail) return head -> d;
    else return head -> next -> d;
  }

  BiQueueNode<T> * getHead() {
    return head;
  }

  BiQueueNode<T> * getTail() {
    return tail;
  }
  
  // Remove the given node from the queue 
  // Do NOT pass in a node from one queue to another queue
  // Return 1 on success, something else otherwise
  int remove(BiQueueNode<T> * node) {
    
    // Can't remove the head, it's a blank node
    if (node == head) return 0;

    node -> prev -> next = node -> next;
    if (node -> next != NULL) {
      // Node is not the tail
      node -> next -> prev = node -> prev;
    }
    else {
      // Node is tail
      tail = node -> prev;
    }
    
    length --;

    // Isolate the node
    node -> next = node -> prev = NULL;
    
    return 1;
  }

  // Remove a node and move it to the tail
  int moveToTail(BiQueueNode<T> * node) {
    int r = remove (node);
    if (r != 1) return 0;
    
    pushNode(node);
    return 1;
  }
    
  ~BiQueue() {
    BiQueueNode<T> * t = head;
    while (t != NULL) {
      BiQueueNode<T> * t1 = t -> next;
      delete t;
      t = t1;
    }
  }

};

#endif
