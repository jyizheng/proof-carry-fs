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

#include "loadProCap.hpp"

// Checks the MAC on a ProCap. The ProCap is passed as a string that
// contains the entire file. The MAC is a 160 bit HMAC. The MAC to be
// checked against must be passed as a 40 digit (2 * MAC_LENGTH) hex
// string (the format of the MAC is not checked). The MAC is taken
// over the portion of fileContents beginning with its first
// non-whitespace character, and ending at its last '#' (including the
// first non-whitespace character, but not the '#')

// Returns 1 on success, 0 on failure, error code on error.

static int checkProCapMAC (const char * fileContents,  // contents of the file
			   const unsigned char * MACkey,        
			   // symmetric key to verify the MAC. Must be
			   // SYM_KEY_LENGTH bytes long and in binary format
			   const char * MAC)           // MAC to check against
{
  // Find the first non-white space character, and name it startText.
  const char * c = fileContents;
  while (*c == '\r' || *c == '\n' || *c == ' ' || *c == '\t') c++;

  const char * startText = c;

  // Now we need to find the last '#' in the file.  So we scan the
  // file to the end ('\0'), and then scan backwards to the '#' Note
  // that this must succeed because the file parsed correctly.

  while (*c != '\0') c++;
  while (*c != '#') c--;

  // length is the length of text over which MAC was calculated
  int length = c - startText;
  
  unsigned char mac[MAX_MAC_LENGTH];

  int maclen;
  computeHMAC (MACkey, SYM_KEY_LENGTH, startText, length, mac, &maclen);
  
  if (maclen != MAC_LENGTH) 
    {
      return CRYPTO_ERR_INTERNAL;
    }

  // Now check that this MAC is correct. So convert the given MAC to
  // binary and verify that the output is correct.

  unsigned char given_mac[MAC_LENGTH];
  hexToBinary(MAC, 2 * MAC_LENGTH, given_mac);

  int i;
  for (i = 0; i < MAC_LENGTH; i++) {
    if (mac[i] != given_mac[i]) {
      return 0;
    }
  }
  
  return 1;
}


ProCap * loadFileProCap (const char * fname,
			 const unsigned char * MACkey,
			 // Key to check MAC. Must be SYM_KEY_LENGTH
			 // long
			 DeclarationIndex * di
			 )
{
  // Parse the file
  ProCap * procap = parseFileProCap (fname);
  if (procap == NULL) return NULL;

  // Read the raw file
  char * fileContents = parseFileTrivial(fname);
  if (fileContents == NULL) return NULL;

  if (checkProCapMAC(fileContents, MACkey, procap -> mac) != 1) {
    delete [] fileContents;
    delete procap;
    return NULL;
  }

  delete [] fileContents;

  if (!procap -> check (di)) {
    delete procap;
    return NULL;
  }

  return procap;
}


