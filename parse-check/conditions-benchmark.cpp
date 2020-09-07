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

#include <sys/time.h>
#include <time.h>

#include <iostream>

#include "checkProCapConditions.hpp"
#include "pcfs-syn.hpp"

using namespace std;

static char * dupstring (const char * a) {
  char * c = new char[strlen(a)  + 1];
  strcpy(c,a);
  return c;
}


int main()
{
  {
    // Test of leq constraints
    cout << "Timing 1000000 hypothetical constraints of form leq T1 T2\n";
    
    Queue<Constraint *> * Q = new Queue<Constraint *>;
    
    Q -> push (new ConstraintLeq (new TermPrim_date2time (new TermPrim_date (new NativeTime(2009,12,31))),
				  new TermOther(dupstring("b"), new Queue<Term *>)));

    Q -> push (new ConstraintLeq (new TermOther(dupstring("c"), new Queue<Term *>),
				  new TermPrim_date2time (new TermPrim_date (new NativeTime(2009,12,31)))));
    
    Q -> push (new ConstraintLeq (new TermOther(dupstring("c"), new Queue<Term *>),
				  new TermOther(dupstring("d"), new Queue<Term *>)));
    
    Q -> push (new ConstraintLeq (new TermPrim_date2time (new TermPrim_date (new NativeTime(2008,12,31))),
				  new TermOther(dupstring("c"), new Queue<Term *>)));

    Q -> push (new ConstraintLeq (new TermOther(dupstring("a"), new Queue<Term *>),
				  new TermCtime));
    
    Term * t1 = new TermOther(dupstring("a"), new Queue<Term *>);
    Term * t2 = new TermOther(dupstring("b"), new Queue<Term *>);
    
    HypConstraint * hc = new HypConstraint (Q, new ConstraintLeq(t1, t2));
    
    struct timeval init_time, final_time;
    gettimeofday(&init_time, NULL);
    
    for (int i = 0; i < 1000000; i++) {
      TermPrim_date2time * vctime = new TermPrim_date2time(new TermPrim_date(new NativeTime((long)(time(NULL)))));
      checkHypConstraint(hc, vctime);
      delete vctime;
    }
    
    gettimeofday(&final_time, NULL);
    double diffw = (final_time.tv_sec - init_time.tv_sec) + 0.000001 * (final_time.tv_usec - init_time.tv_usec);
    cout << "Time taken = " << diffw << " seconds\n";
    
    delete hc;

  }

  {
    // Test of stronger constraints
    cout << "Timing 1000000 hypothetical constraints of form stronger K1 K2\n";
    
    Queue<Constraint *> * Q = new Queue<Constraint *>;
    
    Q -> push (new ConstraintStronger (new TermPrim_int2principal (new TermPrim_int (500)),
				       new TermOther (dupstring ("c"), new Queue<Term *>)));
    
    Q -> push (new ConstraintStronger (new TermOther(dupstring("c"), new Queue<Term *>),
    				       new TermOther(dupstring("b"), new Queue<Term *>)));
    
    Q -> push (new ConstraintStronger (new TermOther(dupstring("a"), new Queue<Term *>),
    				       new TermPrim_int2principal (new TermPrim_int (500))));
    
    
    Term * t1 = new TermOther(dupstring("a"), new Queue<Term *>);
    Term * t2 = new TermOther(dupstring("b"), new Queue<Term *>);
    
    HypConstraint * hc = new HypConstraint(Q, new ConstraintStronger(t1, t2));
    struct timeval init_time, final_time;
    gettimeofday(&init_time, NULL);
    
    for (int i = 0; i < 1000000; i++) {
      checkHypConstraint(hc, NULL);
    }
    
    gettimeofday(&final_time, NULL);
    double diffw = (final_time.tv_sec - init_time.tv_sec) + 0.000001 * (final_time.tv_usec - init_time.tv_usec);
    cout << "Time taken = " << diffw << " seconds\n";
    
    delete hc;
  }

  {
    // Test of state condition owner
    
    cout << "Timing 1000000 state conditions of form owner F K\n";
    
    Term * F = new TermPrim_str2file (new TermPrim_str (dupstring ("/sample-procap.cap")));
    Term * K = new TermPrim_int2principal (new TermPrim_int (1000));

    StateOwner * st = new StateOwner (F, K);
    struct timeval init_time, final_time;
    gettimeofday(&init_time, NULL);
    
    for (int i = 0; i < 1000000; i++) {
      checkState(st, ".");
    }
    
    gettimeofday(&final_time, NULL);
    double diffw = (final_time.tv_sec - init_time.tv_sec) + 0.000001 * (final_time.tv_usec - init_time.tv_usec);
    cout << "Time taken = " << diffw << " seconds\n";
    
    delete st;
  }

  {
    // Test of state condition has_xattr
    
    cout << "Timing 1000000 state conditions of form has_xattr F A V\n";
    
    Term * F = new TermPrim_str2file (new TermPrim_str (dupstring ("/sample-procap.cap")));
    Term * A = new TermPrim_str (dupstring ("last_mod_time"));
    Term * V = new TermExp_add (new TermTime2exp (new TermPrim_int2time (new TermPrim_int (201))), 
				 new TermTime2exp (new TermPrim_date2time 
						   (new TermPrim_date (new NativeTime (2008, 12, 29)))));

    StateHas_xattr * st = new StateHas_xattr (F, A, V);

    struct timeval init_time, final_time;
    gettimeofday(&init_time, NULL);
    
    for (int i = 0; i < 1000000; i++) {
      checkState (st, ".");
    }
    
    gettimeofday(&final_time, NULL);
    double diffw = (final_time.tv_sec - init_time.tv_sec) + 0.000001 * (final_time.tv_usec - init_time.tv_usec);
    cout << "Time taken = " << diffw << " seconds\n";
    
    delete st;
  }
}
