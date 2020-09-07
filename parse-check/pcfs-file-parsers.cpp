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

#include <sys/stat.h>
#include <stdlib.h>
#include <fcntl.h>
#include <iostream>
#include <stdio.h>
#include <unistd.h>
#include <limits.h>

#include "pcfs-parse-ds.hpp"
#include "pcfs-lex.hpp"
#include "pcfs-gram.h"

#include "pcfs-file-parsers.hpp"

using namespace std;

/* Prototypes of parser functions */
void *PcfsCParseAlloc(void *(*mallocProc)(size_t));

void PcfsCParseFree(
  void *p,                    /* The parser to be deleted */
  void (*freeProc)(void*)     /* Function used to reclaim memory */
);

void PcfsCParse(
  void *yyp,                   /* The parser */
  int yymajor,                 /* The major token code number */
  PrimitiveValue * yyminor,       /* The value for the token */
  ParserOutput * output               /* Optional %extra_argument parameter */
);

//--------------------------------------------------------------------------------

// Helper function that can parse declaration files, procaps, and
// terms.  Returns a ParserOutput on success, NULL on error.  The
// ParserOutput is dynamically allocated and must be freed by
// caller. But please note that you must separately free the index,
// procap, or term present in the ParserOutput, since the destructor
// won't do this (this should usually be unnecessary because these
// will probably be stored somewhere in the cache).  

// First argument: file name to parse. 

// Second argument: type of data to expect. 

// Third argument: if true, then the file name is interpreted as the
// entire data that is to be parsed. There is a limit on how much this
// data can be. This is the limit of a Linux pipe buffer, currently
// 64K. If data is more than that, NULL is returned.

// It is an error if data expected (as given in the second argument)
// does not match the data in the file. If the second argument is
// ParserOutput :: parser_error, then this function returns
// immediately with NULL.

static ParserOutput * parseCommon(const char * fname, 
				  ParserOutput :: ParserType type, bool nameIsData)
{
  if (type == ParserOutput :: parser_error) return NULL;

  FILE * fp = NULL;
  
  if (!nameIsData) {
    fp = fopen(fname,"r");
    if (fp == NULL) return NULL;
  }

  else {
    // We have to interpret the file name as a string

    // We will open a pipe, write the data to its write end, and
    // convert the read end to a file. We will then pass this file to
    // the lexer.

    // Make a new pipe.
    int pipefd[2];
    if (pipe (pipefd) != 0) 
      return NULL;

    int readfd = pipefd[0];
    int writefd = pipefd[1];

    // We will write data in chunks of size PIPE_BUF. This is the
    // largest size for a write that is guaranteed to be atomic. Since
    // our application is single threaded, it won't make a difference,
    // but the hope is that writing in this chunk size may also be
    // time efficient.

    int maxlen = strlen(fname);
    int niters = maxlen / PIPE_BUF;
    int rem = maxlen % PIPE_BUF;
    
    for (int i = 0; i < niters; i++) {
      int r = write(writefd, fname + i * PIPE_BUF, PIPE_BUF);
      if (r != PIPE_BUF) {
	close(writefd);
	close(readfd);
	return NULL;
      }
    }
    
    if (write(writefd, fname + niters * PIPE_BUF, rem) != rem) {
	close(writefd);
	close(readfd);
	return NULL;
    }      
    
    // Close the write end of the pipe
    close (writefd);

    // Convert the read end of the pipe to a FILE * object.  When this
    // FILE * object is closed, the read end of the pipe will be
    // closed automatically
    fp = fdopen(readfd, "r");

    if (fp == NULL) {
      close(readfd);
      return NULL;
    }
  }

  // Structure passed to parser
  ParserOutput * output = new ParserOutput;

  // Make parser
  void * parser = PcfsCParseAlloc( malloc );
  
  
  // Structure passed to lexer
  PrimitiveValue * pv = new PrimitiveValue(0);

  // Make lexer
  yyscan_t lexer;
  PcfsClex_init_extra(pv, &lexer);
  PcfsCset_in(fp, lexer);
  //  PcfsCset_debug(1, lexer); // set debugging mode

  // Feed initial token to parser
  switch (type) {
  case ParserOutput :: parser_procap:
    PcfsCParse(parser, PCFS_TOKEN_C_BEGIN_PROCAP, new PrimitiveValue(0), output);
    break;
  case ParserOutput :: parser_declaration:
    output -> data.index = new DeclarationIndex;
    PcfsCParse(parser, PCFS_TOKEN_C_BEGIN_DECLARATION, new PrimitiveValue(0), output);
    break;
  case ParserOutput :: parser_term:
    PcfsCParse(parser, PCFS_TOKEN_C_BEGIN_TERM, new PrimitiveValue(0), output);
    break;
  default:
    // Should never be reached
    break;
  }
    
    
  // Lex the stream, and feed tokens to parser
  int c;
  while ((c = PcfsClex(lexer)) != 0) {

    PrimitiveValue * pv_p;
    if (c == PCFS_TOKEN_C_STRING || c == PCFS_TOKEN_C_ID || c == PCFS_TOKEN_C_MAC) {
      pv_p = new PrimitiveValue(pv -> data.str);
    }
    else if (c == PCFS_TOKEN_C_INTEGER) {
      pv_p = new PrimitiveValue (pv -> data.i);
    }
    else if (c == PCFS_TOKEN_C_DATE) {
      pv_p = new PrimitiveValue (pv -> data.t);
    }
    else {
      pv_p = new PrimitiveValue (0);
    }

    PcfsCParse(parser, c, pv_p, output);
  }

  // Close file
  fclose(fp);

  // Construct the parse tree
  PcfsCParse(parser, 0, pv, output);

  //free lexer
  PcfsClex_destroy(lexer);
  
  //free parser
  PcfsCParseFree(parser, free);
  
  //free memory
  delete pv;

  if (output -> type == ParserOutput::parser_error) {
    if (type == ParserOutput :: parser_declaration) {
      delete output -> data.index;
    }
    delete output;
    return NULL;
  }
  else return output;
}

// Function to parse a ProCap file
ProCap * parseFileProCap (const char * fname) {
  ParserOutput * po = parseCommon(fname, ParserOutput :: parser_procap, false);
  if (po == NULL) return NULL;

  ProCap * p = po -> data.procap;
  delete po;
  return p;
}


// Function to parse a Declarations file
DeclarationIndex * parseFileDeclarations (const char * fname) {
  ParserOutput * po = parseCommon(fname, ParserOutput :: parser_declaration, false);
  if (po == NULL) return NULL;

  DeclarationIndex * p = po -> data.index;
  delete po;
  return p;
}

// Function to parse a Term file
Term * parseFileTerm(const char * fname) {
  ParserOutput * po = parseCommon(fname, ParserOutput :: parser_term, false);
  if (po == NULL) return NULL;

  Term * t = po -> data.term;
  delete po;
  return t;

}

// Function to parse a string containing a term
Term * parseStringTerm(const char * str) {
  ParserOutput * po = parseCommon(str, ParserOutput :: parser_term, true);

  if (po == NULL) return NULL;

  Term * t = po -> data.term;
  delete po;
  return t;

}

//--------------------------------------------------------------------------------

// Function to just read a file and return its contents The maximum
// size of file it will read is MAX_FILE_LENGTH. Anything longer will
// result in NULL being returned.
char * parseFileTrivial(const char * path)
{
  int fd = open (path, O_RDONLY);

  if (fd == -1) return NULL;

  struct stat filedata;
  if (fstat(fd, &filedata) == -1) {
    close (fd);
    return NULL;
  }
    
  if (S_ISDIR(filedata.st_mode)) {
    close (fd);
    return NULL;
  }

  int length = filedata.st_size;

  if (length > MAX_FILE_LENGTH) {
    close(fd);
    return NULL;
  }

  char * data = new char[length + 1024]; 
  // This extra space of 1024 is needed, just in case file is longer
  // than specified by its length
  if (data == NULL) return NULL;

  int pos = 0;
  int done = false;
  while (!done && pos < length)
    {
      int c;
      if ((c = read (fd, data + pos, 512)) > 0)
	{
	  pos = pos + c;
	}
      else 
	{
	  done = true;
	}
    }
   
  if (pos != length) {
    close (fd);
    delete [] data;
    return NULL;
  }

  data[pos] = '\0';

  close(fd);

  return data;
}

