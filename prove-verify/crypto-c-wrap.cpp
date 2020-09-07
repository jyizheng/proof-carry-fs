/*
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

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <malloc.h>

#include <openssl/hmac.h>
#include <openssl/rsa.h>
#include <openssl/pem.h>

#include "../crypto-common.hpp"
#include "crypto-c-wrap.h"



/**********************************************************************************/

extern "C" {

  int pemLength_SML (int len) 
  {
    return pemLength(len);
  }
  
  int pemEncode_SML (Pointer in, int inlen, Pointer buf)
  {
    pemEncode ((const char *) in, inlen, (char *) buf);
    return 1;
  }
  
  int pemDecode_SML (Pointer in, int inlen, Pointer buf)
  {
    return pemDecode ((const char *) in, inlen, (char *) buf);
  }
  
  //-------------------------------------------------------

  Pointer privkeyToPubkey_SML(Pointer privkey) {
    return ((Pointer) privKeyToPubKey((PrivKey *) privkey));
  }

  int eq_pubkey_SML(Pointer k1, Pointer k2) {
    if (eq_PubKey((PubKey *) k1, (PubKey *) k2)) 
      return 1;
    else return 0;
  }

  int correspond_SML(Pointer privkey, Pointer pubkey) {
    if (correspond((PrivKey *) privkey, (PubKey *) pubkey))
      return 1;
    else return 0;
  }

  //-------------------------------------------------------
  
  Pointer readPrivkeyFromFile_SML(Pointer fname) {
    return ((Pointer) (readPrivKeyFromFile ((const char *) fname)));
  }

  int writePrivkeyToFile_SML (Pointer key, Pointer fname) {
    const char * f = (const char *) fname;
    PrivKey * pkey = (PrivKey *) key;

    return writePrivKeyToFile (pkey, f);
  }

  Pointer stringToPrivkey_SML (Pointer str, int len) {
    return ((Pointer) (readPrivKeyFromString ((const char *) str, len)));
  }
  
  int privkeyToString_SML (Pointer key, Pointer buf, int len) {
    return writePrivKeyToString ((PrivKey *) key, (char *) buf, len);
  }

  int destroyPrivkey_SML (Pointer key) {
    delete ((PrivKey *) key);
    return 1;
  }



  //-----------------------------------------------------------------------


  Pointer readPubkeyFromFile_SML(Pointer fname) {
    return ((Pointer) (readPubKeyFromFile ((const char *) fname)));
  }

  int writePubkeyToFile_SML (Pointer key, Pointer fname) {
    const char * f = (const char *) fname;
    PubKey * pkey = (PubKey *) key;

    return writePubKeyToFile (pkey, f);
  }

  Pointer stringToPubkey_SML (Pointer str, int len) {
    return ((Pointer) (readPubKeyFromString ((const char *) str, len)));
  }
  
  int pubkeyToString_SML (Pointer key, Pointer buf, int len) {
    return writePubKeyToString ((PubKey *) key, (char *) buf, len);
  }

  int destroyPubkey_SML (Pointer key) {
    delete ((PubKey *) key);
    return 1;
  }


  int sign_length_SML (Pointer privkey) {
    return getSignatureLength ((PrivKey *) privkey);
  }

  int sign_SML (Pointer privkey, Pointer str, int len, Pointer buf, Pointer buflen) {
    return createSignature ((PrivKey *) privkey,
			    (const char *) str,
			    len,
			    (unsigned char *) buf,
			    (int *) buflen);
  }

  int verify_SML (Pointer pubkey, Pointer data, int len,
		  Pointer sign, int signlen) {
    return verifySignature ((PubKey *) pubkey,
			    (const char *) data,
			    len,
			    (unsigned char *) sign,
			    signlen);
  }

  
  int symkeyLength_SML (int i) {
    return SYM_KEY_LENGTH;
  }

  int hexToBinary_SML (Pointer hex, int hexlen, Pointer outbuf) {
    return hexToBinary((char *) hex, hexlen, (unsigned char *) outbuf, false);
  }

  int binaryToHex_SML (Pointer sym, int symlen, Pointer outbuf) {
    binaryToHex((unsigned char *) sym, symlen, (char *) outbuf);
    return 1;
  }
 
  int hmac_SML (Pointer key, int keylen, Pointer data, int datalen, Pointer outbuf, Pointer outbuflen) {
    computeHMAC((unsigned char *) key, keylen, 
		(char *) data, datalen, 
		(unsigned char *) outbuf, (int *) outbuflen);
    return 1;
  }
}


/***********************************************************************************/



