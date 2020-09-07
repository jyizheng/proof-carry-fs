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

(* A program for taking a file and appending to its mac generated with
   a symmetric key *)

structure GenMac = struct

fun print_err s = TextIO.output (TextIO.stdErr, s)

fun error () = 
    (print_err ("Usage: \n  genmac <file-to-sign> <symmetric-key>\n");
     print_err ("   <symmetric-key> must be exactly 40 hex digits\n");
     OS.Process.exit(OS.Process.failure))
;

val filename = ref "";
val key_hex = ref "";

(* Get the arguments *)
val _ = case (CommandLine.arguments()) of
	    fname :: key :: [] => (filename := fname; key_hex := key)
	  | _ => error ()
;

(* Get the symmetric key *)
val symkey = case (Crypto.hexToSymkey (!key_hex)) of
		 NONE => error ()
	       | SOME k => k
;

(* Read entire file as a string -- be careful, this may cause memory exhaustion *)
fun readFile (f: string): string =
    if (f <> "-") then
	let val inp = BinIO.openIn f
	    val s = BinIO.inputAll inp
	    val s' = Byte.bytesToString s
	in 
	    (BinIO.closeIn(inp); s')
	end
    else (* read stdIn *)
	let fun recread s = 
		let val snew = TextIO.inputLine (TextIO.stdIn)
		in
		    case snew of 
			NONE => s
		      | SOME s' => recread (s ^ s')
		end
	in
	    recread ""
	end

val data = (readFile (!filename)) handle IO.Io _ => 
					 (print_err ("Unable to read file " ^ (!filename) ^ "\n");
					  error())

(* Generate mac *)
val mac = Crypto.binaryToHex (Crypto.hmac (symkey, data))

(* Print file and mac *)
val _ = print data;
val _ = print "#\n";
val _ = print (mac^"\n");

end (* struct *)
