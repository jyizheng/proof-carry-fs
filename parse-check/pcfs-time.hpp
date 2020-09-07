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

#ifndef __PCFS_TIME_H
#define __PCFS_TIME_H

#include <iostream>
#include <time.h>
#include <stdio.h>

#include "../charbuf.hpp"

using namespace std;

class NativeTime
{

public:
  int year, month, day;
  int hour, minute, second;
  long sinceEpoch;  // Time since the last epoch

  NativeTime(int yy, int mm, int dd) 
  {
    year = yy;
    month = mm;
    day = dd;
    hour = minute = second = 0;
    calcSinceEpoch();
  }

  NativeTime(int yy, int mm, int dd, int h, int m, int s)
  {
    year = yy;
    month = mm;
    day = dd;
    hour = h;
    minute = m;
    second = s;
    calcSinceEpoch();
  }

  NativeTime (long time) 
  {
    setFromEpoch(time);
  }

  long getSinceEpoch() 
  {
    return sinceEpoch;
  }

  // Caculate, store and return the time since the last epoch
  long calcSinceEpoch() 
  {
    struct tm mytime;
    mytime.tm_sec = second;
    mytime.tm_min = minute;
    mytime.tm_hour = hour;
    mytime.tm_mday = day;
    mytime.tm_mon = month - 1;
    mytime.tm_year = year - 1900;
    mytime.tm_isdst = -1;

    time_t t = mktime (&mytime);
    sinceEpoch = ((long) t);
    return sinceEpoch;
  }
  
  void setFromEpoch(long t)
  {
    struct tm mytime;

    localtime_r((const time_t *) &t, &mytime);

    year = 1900 + mytime.tm_year;
    month = mytime.tm_mon + 1; // tm_mon is in the range 0 to 11
    day = mytime.tm_mday;
    hour = mytime.tm_hour;
    minute = mytime.tm_min;
    second = mytime.tm_sec;

    calcSinceEpoch();
  }

  ostream & print (ostream & out) {
    char s[256];
    toString(s);
    out << s;
    return out;
  }

  charstream & print (charstream & out) {
    out << year << ':';
    out << month << ':';
    out << day << ':';
    out << hour << ':';
    out << minute << ':';
    out << second;
    return out;
  }

  void toString (char * out)
  {
    sprintf(out, "%04d:%02d:%02d:%02d:%02d:%02d", 
	    year, month, day,
	    hour, minute, second);
  }

};

#endif
