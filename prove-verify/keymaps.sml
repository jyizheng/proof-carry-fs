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

(*
 A keymap is a map from a key to a principal, written in XML as follows:

 <keymap>
    <subject>
      "..."
    </subject>
    <pubkey>
	  [-----BEGIN PUBLIC KEY-----]
          abcdef ...
          [-----END PUBLIC KEY-----]      
    </pubkey>
    <from>
       date1
    </from>
    <to>
       date2
    </to>
 </keymap>

 The subject is any string enclosed in double quotes. It may not
 contain newlines and double quotes. date1 and date2 are dates in one
 of the formats yyyy:mm:dd:hh:mm:ss or yyyy:mm:dd

 The parser for keymaps is combined with the XML parser
					      
*)

structure KeyMaps = struct

type str = Certs.str

val strToString = Certs.strToString

val stringToStr = Certs.stringToStr

type keymap = str * Crypto.pubkey * Time.time * Time.time
(* subject, its assigned key, from, to *)


exception BadPubKey of Crypto.pubkey

fun makestring_keymap (subj, key, t1, t2) =
    case (Crypto.pubkeyToString key) of
	NONE => raise BadPubKey key
      | SOME key_base64 =>
	"\t<keymap>\n" ^
	"\t\t<subject>\n" ^
	"\t\t\t\"" ^ (strToString subj) ^ "\"\n" ^ 
	"\t\t</subject>\n" ^
	"\t\t<pubkey>\n" ^
	key_base64 ^
	"\t\t</pubkey>\n" ^
	"\t\t<from>\n" ^
	"\t\t\t" ^ (Date.fmt "%Y:%m:%d:%H:%M:%S" (Date.fromTimeLocal t1)) ^ "\n" ^ 
	"\t\t</from>\n" ^
	"\t\t<to>\n" ^
	"\t\t\t" ^ (Date.fmt "%Y:%m:%d:%H:%M:%S" (Date.fromTimeLocal t2)) ^ "\n" ^
	"\t\t</to>\n" ^
	"\t</keymap>\n"

end (* structure KeyMaps *)
