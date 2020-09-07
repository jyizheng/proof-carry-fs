(*
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
*)

(* A certificate is a LIST of ELEMENTS of data, each of which is
   signed. All the data as well as signatures are written with base64
   (PEM) encoding. Newlines and whitespace are ignored by the
   parser. Public keys may optionally have headers: "-----BEGIN PUBLIC
   KEY-----" and "-----END PUBLIC KEY-----" (This is provided for
   compatibility with OpenSSL PEM printing routines)

 * An element contains three components:
 * 1. The data
 * 2. The public key that signs it
 * 3. The signature (the signature is taken over the original data, not its PEM encoded form)
 * 4. The type

   The type may be any ASCII string not containing newlines and double
   quotes. It must be enclosed in double quotes "...". It is returned
   as is by the parser with the data (without the quotes)

 * These are written with XML notation:
   <element>
       <data>
          abcdef ... 
       </data>
       <type>
          "[A-Za-z0-9_:]"
       </type>
       <pubkey>
	  [-----BEGIN PUBLIC KEY-----]
          abcdef ...
          [-----END PUBLIC KEY-----]
       </pubkey>
       <signature>
          abcdef ...
       </signature>
    </element>

    The order of the components in an element is important

  * A certificate is a list of elements, written as follows:
    
       <element>
          ...
       </element>
          .
          .
          .
       <element>
          ...
       </element>

    It is possible to concatenate certificates by just concatenating
    their lists of elements

  * The parser for certificates will return a list of three tuples of the form:
    (data, type, pubkey, sign),
    where data: string, type: string, pubkey:Crypto.pubkey, and sign: string

    The strings will NOT be base64 encoded, i.e., the parser will
    decode them before returning them.

  * The type of these tuples is abstractly called elem 

  * An elem can be created by giving a data, its type, and a private key to sign the data

  * Comments may be written anywhere between <comment> and
    </comment>. Comments may be nested. But observe that if the text
    inside a comment contains <comment> or </comment>, they may
    interfere with the tags.
    
    It is expected that comments will be used to describe data or
    keys, which are otherwise base64 encoded.

  * The parser for certs is in the XML parser

 *)


structure Certs = struct

abstype str = Str of string 
with

local 

(* Checks that a string is a valid type by making sure that it
contains only alphanumeric characters, '_', ':', and spaces. Note that
a string is a CharVector.vector as per SML specifications *)

    fun checkStr(s: string) : bool =
	let fun checkChar (c: char) = (c <> #"\"" andalso c <> #"\n" andalso c <> #"\r")
	in
	    CharVector.foldl (fn (c, b:bool) => (checkChar c) andalso b) 
			     true s
	end


in
exception BadStr of string

fun strToString (Str s) = s

fun stringToStr s = 
    case (checkStr s) of
	false => raise BadStr s
      | true => Str s
	
end (* local *)
	
end (* abstype typ *)

type elem = string * str * Crypto.pubkey * string
            (* data, type, public key, signature *)

type cert = elem list

exception BadPubKey of Crypto.pubkey
exception BadPrivKey of Crypto.privkey

(* Convert an element to a string. If the second argument
   include_raw_data is true, then the actual data is also included as
   a comment. Be careful before using this option: if the actual data
   contains mismatched <comment> or </comment>, then the printed
   string may be unparseable *)

fun makestring_elem ((data, typ, pubkey, sign): elem, include_raw_data: bool): string = 
    let val data_base64 = Crypto.pemEncode data
	val sign_base64 = Crypto.pemEncode sign
    in
	case (Crypto.pubkeyToString pubkey) of
	    NONE =>  raise BadPubKey (pubkey) 
	  | SOME pubkey_base64 =>  
	    "<element>\n\t<data>\n" ^ data_base64 ^ "\t</data>\n" ^
	    (if include_raw_data 
	     then "\t<comment>\n" ^ data ^ "\t</comment>\n"
	     else "") ^
	    "\t<type>\n\t\t\"" ^ (strToString typ) ^ "\"\n\t</type>\n" ^ 
	    "\t<pubkey>\n" ^ pubkey_base64 ^ "\t</pubkey>\n" ^
	    "\t<signature>\n" ^ sign_base64 ^ "\t</signature>\n" ^ 
	    "</element>\n"
    end
	    
fun makestring_cert ([], include_raw_data) = ""
  | makestring_cert (e :: cert, include_raw_data) = 
    (makestring_elem (e, include_raw_data)) ^ "\n" ^ (makestring_cert (cert, include_raw_data))

(* Take data, its type, a private key and convert to an element by signing the data *)
fun createElemFromData (data: string, typ: string, pkey: Crypto.privkey): elem =
    case Crypto.sign (pkey, data) of 
	NONE => raise BadPrivKey pkey
      | SOME sign => 
	case (Crypto.privkeyToPubkey pkey) of
	    NONE => raise BadPrivKey pkey
	  | SOME pubkey => (data, stringToStr typ, pubkey, sign)
			   

(* Used by the parser *)
exception BadPubKeyFormat of string
exception BadBase64 of string

end (* structure Certs *)
