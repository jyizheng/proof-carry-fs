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

/* A program to generate a HMAC on a file.

   It reads the entire file, ignores any whitespace in the beginning;
   computes a HMAC from the first non-whitespace character to the last
   character of the file, and outputs '#' followed by a '\n' followed
   by the HMAC in hex, followed by a '\n'. An HMAC is 40 hex digits
   i.e. 160 bits.

   Input: argv[1] is filename to read. argv[2] is the key, written as
   hex digits. It must be exactly 160 bits i.e. 40 hex digits. If
   argv[2] is '-' the key is read from stdin.

   A typical way to generate a MAC on a file <fname> would be:
   cat <keyfile> | generate-mac <fname> - | cat <fname> - > <outfile>
*/

#include <iostream>
#include <stdio.h>

#include "../crypto-common.hpp"
#include "pcfs-file-parsers.hpp"

using namespace std;

int main(int argc, char * argv[])
{
  if (argc != 3) {
    cout << "usage is:\ngenerate-mac <filename> <key>\n";
  }
  
  // Read the key in hex from argv[2] or standard input
  char * keyhex;
  int lenhex;

  if (!strcmp (argv[2], "-")) {
    keyhex = new char[2 * SYM_KEY_LENGTH + 1];
    if (scanf("%40s", keyhex) != 1) {
      delete [] keyhex;
      return EXIT_FAILURE;
    }
    keyhex[2 * SYM_KEY_LENGTH] = '\0';
    lenhex = 2 * SYM_KEY_LENGTH;
    //    cerr << keyhex << "\n";
  }
  else {
    lenhex = strlen(argv[2]);
    if (lenhex != 2 * SYM_KEY_LENGTH) return EXIT_FAILURE;
    keyhex = new char[2 * lenhex + 1];
    strcpy(keyhex, argv[2]);
  }
    
  // Convert key to binary
  unsigned char key[SYM_KEY_LENGTH]; 
  // The key doesn't have to be NULL terminated, so no need to add 1 here

  if (hexToBinary(keyhex, 2 * SYM_KEY_LENGTH, key, true) != 1) {
    delete [] keyhex;
    cerr << "Wrong key format\n";
    return EXIT_FAILURE;
  }

  delete [] keyhex;

  // Now read the file
  char * fileData = parseFileTrivial(argv[1]);

  if (fileData == NULL) return EXIT_FAILURE;

  // Find the first non-whitespace character
  char * c = fileData;
  while (*c == ' ' || *c == '\t' || *c == '\n' || *c == '\r') {
    c++;
  }
  char * startText = c;

  // Now we need to get to the end of the file.
  while (*c != '\0') c++;
  
  int fileLength = c - startText;
  
  cerr << "Length of data is: " << fileLength << "\n";
  // Compute the MAC
  unsigned char mac[MAX_MAC_LENGTH];
  int size;
  computeHMAC(key, SYM_KEY_LENGTH, startText, fileLength, mac, &size);
  
  delete [] fileData;

  if (size != MAC_LENGTH) {
    return EXIT_FAILURE; // This should not happen
  }
 
  // Print the MAC
  printf ("#\n");
  char hex[2 * MAC_LENGTH + 1];
  
  binaryToHex(mac, MAC_LENGTH, hex);
  hex [2 * MAC_LENGTH] = '\0';
  printf ("%s\n", hex);

  // Check the hex representation by converting back to binary
  unsigned char bin[MAC_LENGTH];
  hexToBinary(hex, 2 * MAC_LENGTH, bin);

  int i;
  for (i = 0; i < MAC_LENGTH; i++) {
    if (bin[i] != mac[i]) {
      cerr << "Error at position: " << i << "\n";
    }
  }

  return EXIT_SUCCESS;
}
