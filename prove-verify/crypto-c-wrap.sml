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

(* Wrapper around C code to Crypto functions. Will work with Mlton, but not with SML/NJ.  
*)

signature CRYPTO = sig

(* PEM here refers to base64 with newlines every 64 characters. The
PEM encode function will take binary data, encode it in base64, and
add newlines every 64 characters. The PEM decode function will take
any string that has base64 and whitespace characters, ignore the
latter and then decode the resulting base64 string. *)

val pemLength: int -> int (* Input: length of string to PEM encode; Output: length of encoded data *)
val pemEncode: string -> string
val pemDecode: string -> string Option.option (* Ignores whitespace completely *)

(* Take a string, and insert a newline every 64 characters *)
val formatPem: string -> string 

(* Opaque types for keys *)
type privkey
type pubkey

val eq_pubkey: pubkey * pubkey -> bool (* Are two public keys equal? *)
val correspond: privkey * pubkey -> bool (* Does a public key correspond to a private key? *)

val privkeyToPubkey: privkey -> pubkey Option.option (* May fail if privkey is invalid, but that should not happen *)

(* String representations of keys are PEM encoded with headers
"-----BEGIN XXX KEY-----" and "-----END XXX KEY------". Do not use
them for comparison *)

val readPrivkeyFromFile: string -> privkey Option.option (* Arg: string == file *)
val writePrivkeyToFile: privkey * string -> bool (* Return false on error *)
val stringToPrivkey: string -> privkey Option.option
val privkeyToString: privkey -> string Option.option 
(* NONE may be returned if key is too big but that should not happen *)

val readPubkeyFromFile: string -> pubkey Option.option (* Arg: string == file *)
val writePubkeyToFile: pubkey * string -> bool (* Return false on error *)
val stringToPubkey: string -> pubkey Option.option
val pubkeyToString: pubkey -> string Option.option 
(* NONE may be returned if key is too big but that should not happen *)


val sign: privkey * string -> string Option.option
(* Args: private key, data to sign; returned string may contain
unprintable characters. Error happens only in case of internal openSSL
failures *)

val verify: pubkey * string * string -> bool 
(* Args: public key, data, signature. Signature must be raw binary,
 not hex coded. Returns true on success, false on failure *)



val binaryToHex: string -> string
val hexToBinary: string -> string Option.option


(* Opaque type for a symmetric key *)
type symkey

(* The length of a symmetric key in bytes. Its length in hex will be
   double since 1 byte --> 2 hex *)
val symkeyLength: int

val hexToSymkey: string -> symkey Option.option
val symkeyToHex: symkey -> string

(* Length of HMACs *)
val hmacLength: int

(* Computer HMAC of a string; the output is binary *)
val hmac: symkey * string -> string


end (* signature CRYPTO *)


structure Crypto:> CRYPTO = struct

val pemLength = _import "pemLength_SML": int -> int;

local 
	
    val pemEncode_h = _import "pemEncode_SML": string * int * char array -> int;
	(* string to encode, its length, buffer for output; return value is meaningless *)
    val pemDecode_h = _import "pemDecode_SML": string * int * char array -> int;
	(* string to encode, its length, buffer for output; returns length of output or negative number on error *)

in

fun pemEncode (s: string) : string = 
    let val size = String.size s
	val bufsize = pemLength size
	val buf = Array.array (bufsize , #"\000") (* Create a buffer *)
	val _ = pemEncode_h (s, size, buf) 
    in
	CharVector.tabulate (bufsize, fn i => Array.sub(buf,i)) 
    end
 


fun pemDecode (s: string): string Option.option = 
    let val size = String.size s
	val bufsize = size
	val buf = Array.array (bufsize, #"\000")
	val n = pemDecode_h (s, size, buf)
    in
	if (n < 0) then NONE 
	else SOME (CharVector.tabulate (n, fn i => Array.sub(buf, i)))
    end

end (* local *)


fun formatPem (s: string) = 
    if (String.size s <= 64) then s ^ "\n"
    else 
	(String.substring (s, 0, 64) ^ "\n" ^ (formatPem (String.extract(s,64,NONE))))


(* Both privkey and pubkey are C++ heap allocated objects, represented
here as opaque types, that have associated destructors *)

type privkey = MLton.Pointer.t MLton.Finalizable.t
type pubkey = MLton.Pointer.t MLton.Finalizable.t

local 

    val destroyPrivkey_h = _import "destroyPrivkey_SML": MLton.Pointer.t -> int;
    (* Return value is meaningless *)
	
    val destroyPubkey_h = _import "destroyPubkey_SML": MLton.Pointer.t -> int;
    (* Return value is meaningless *)

in

fun destroyPrivkey (k: MLton.Pointer.t) = (destroyPrivkey_h k; ())

fun destroyPubkey (k: MLton.Pointer.t) = (destroyPubkey_h k; ())


(* Associate a destructor with a pointer and return a key *)
fun wrapprivkey (t: MLton.Pointer.t): privkey = 
    let val key = MLton.Finalizable.new t
	val _ = MLton.Finalizable.addFinalizer (key, destroyPrivkey) 
    in 
	key
    end


fun wrappubkey (t: MLton.Pointer.t): pubkey = 
    let val key = MLton.Finalizable.new t
	val _ = MLton.Finalizable.addFinalizer (key, destroyPubkey) 
    in 
	key
    end

end (* local *)



val key_buf_size = 1024 (* Maximum size of a printed key *)


local
    val privkeyToPubkey_h = _import "privkeyToPubkey_SML": MLton.Pointer.t -> MLton.Pointer.t;
    val eq_pubkey_h = _import "eq_pubkey_SML": MLton.Pointer.t * MLton.Pointer.t -> int;
    val correspond_h = _import "correspond_SML": MLton.Pointer.t * MLton.Pointer.t -> int;
in

fun privkeyToPubkey (key: privkey): pubkey Option.option =
    MLton.Finalizable.withValue
	(key, fn key => 
		 let val p = privkeyToPubkey_h (key)
		 in
		     if (p = MLton.Pointer.null) then NONE
		     else SOME (wrappubkey p)
		 end)
				
fun eq_pubkey (k1: pubkey, k2: pubkey): bool = 
    MLton.Finalizable.withValue
	(k1, fn k1 =>
		MLton.Finalizable.withValue(k2, fn k2 => 
						   eq_pubkey_h(k1,k2) = 1))
    
fun correspond (priv: privkey, pub: pubkey): bool = 
    MLton.Finalizable.withValue
    (priv,fn priv =>
	     MLton.Finalizable.withValue(pub, fn pub => 
						 correspond_h (priv, pub) = 1))
end (* local *)


local
    val readPrivkeyFromFile_h = _import "readPrivkeyFromFile_SML": string -> MLton.Pointer.t;
    (* return null on error *)
									     
    val writePrivkeyToFile_h = _import "writePrivkeyToFile_SML": MLton.Pointer.t * string -> int;
    (* return 1 on success, negative number on error *)
											     
    val stringToPrivkey_h = _import "stringToPrivkey_SML": string * int -> MLton.Pointer.t;
    (* return null on error *)

    val privkeyToString_h = _import "privkeyToString_SML": MLton.Pointer.t * (char array) * int -> int;
    (* return length on success, -1 on error *)

    val readPubkeyFromFile_h = _import "readPubkeyFromFile_SML": string -> MLton.Pointer.t;
    (* return null on error *)
									     
    val writePubkeyToFile_h = _import "writePubkeyToFile_SML": MLton.Pointer.t * string -> int;
    (* return 1 on success, negative number on error *)
											     
    val stringToPubkey_h = _import "stringToPubkey_SML": string * int -> MLton.Pointer.t;
    (* return null on error *)

    val pubkeyToString_h = _import "pubkeyToString_SML": MLton.Pointer.t * (char array) * int -> int;
    (* return length on success, -1 on error *)

in

fun readPrivkeyFromFile(fname: string): privkey Option.option =
    let val ptr = readPrivkeyFromFile_h (fname ^ "\000")
    in 
	if (ptr = MLton.Pointer.null)
	then 
	    NONE
	else
	    SOME (wrapprivkey ptr)
    end

fun writePrivkeyToFile (k: privkey, fname: string): bool =
    MLton.Finalizable.withValue (k, fn k =>
				       case (writePrivkeyToFile_h (k, fname ^ "\000")) of
					   1 => true
					 | _ => false
				)
	     

fun stringToPrivkey (s: string): privkey Option.option = 
    let val ptr = stringToPrivkey_h (s, String.size s)
    in 
	if (ptr = MLton.Pointer.null)
	then
	    NONE 
	else 
	    SOME (wrapprivkey ptr)
    end

fun privkeyToString (k: privkey): string Option.option =
    MLton.Finalizable.withValue(k, fn k =>
				      let val buf = Array.array (key_buf_size, #"\000")
					  val length = privkeyToString_h (k, buf, key_buf_size)
				      in 
					  if (length < 0) then NONE
					  else SOME (CharVector.tabulate (length, fn i => Array.sub(buf,i)))
				      end
			       )

fun readPubkeyFromFile(fname: string): pubkey Option.option =
    let val ptr = readPubkeyFromFile_h (fname ^ "\000")
    in 
	if (ptr = MLton.Pointer.null)
	then 
	    NONE
	else
	    SOME (wrappubkey ptr)
    end

fun writePubkeyToFile (k: pubkey, fname: string): bool =
    MLton.Finalizable.withValue (k, fn k =>
				       case (writePubkeyToFile_h (k, fname ^ "\000")) of
					   1 => true
					 | _ => false
				)
	     

fun stringToPubkey (s: string): pubkey Option.option = 
    let val ptr = stringToPubkey_h (s, String.size s)
    in 
	if (ptr = MLton.Pointer.null)
	then
	    NONE
	else 
	    SOME (wrappubkey ptr)
    end

fun pubkeyToString (k: pubkey): string Option.option =
    MLton.Finalizable.withValue (k, fn k =>
				       let val buf = Array.array (key_buf_size, #"\000")
					   val length = pubkeyToString_h (k, buf, key_buf_size)
				       in 
					   if (length < 0) then NONE
					   else SOME (CharVector.tabulate (length, fn i => Array.sub(buf,i)))
				       end
				)

end (* local *)



local

    val sign_length = _import "sign_length_SML": MLton.Pointer.t -> int;
    (* Args: private key
     * Returns: length of signatures created with this key, or negative number on error
     *)

    val sign_h = _import "sign_SML": MLton.Pointer.t * string * int * char array * int ref -> int;
    (* Args: privkey, string to sign, length of string, output buffer, cell for length of generated signature.
     * Returns: 1 on success, negative value on error (error will usually only happen if buffer is small) 
     *)

    val verify_h = _import "verify_SML": MLton.Pointer.t * string * int * string * int -> int;
    (* Args: pubkey, data, length of data, signature, length of signature
     * Returns: 1 on success, 0 on failure, negative number on error
     *)

in

fun sign (key: privkey, str: string): string Option.option = 
    MLton.Finalizable.withValue 
    (key, 
  fn key =>
     let val l = sign_length key
     in
	 if ( l < 0 ) then NONE (* Should not happen! *)
	 else 
	     let val size = String.size str
		 val buf = Array.array (l, #"\000")
		 val outlen = ref ~1
		 val ret = sign_h (key, str, size, buf, outlen)
	     in 
		 case ret of
		     1 => SOME (CharVector.tabulate (!outlen, fn i => Array.sub(buf,i)))
		   | _ => NONE (* Should not happen normally *)
	     end
     end
    )

fun verify (key: pubkey, str: string, sign: string): bool = 
    MLton.Finalizable.withValue 
    (key, 
  fn key =>
     let val r = verify_h (key, str, String.size str, sign, String.size sign)
     in 
	 case r of
	     1 => true
	   | _ => false
     end
    )

end (* local *)



type symkey = string (* A vector of binary bytes *)

local
    (* Input argument is meaningless *)
    val symkeyLength_h = _import "symkeyLength_SML": int -> int;
    
    (* Returns value is meaningles; Input must be proper hex; we assume that char array is long enough  to hold output *)
    val hexToBinary_h = _import "hexToBinary_SML": string * int * char array -> int;
    
    (* Return value is meaningless; we assume that char array is long enough to hold output *)
    val binaryToHex_h = _import "binaryToHex_SML": string * int * char array -> int;
		
    (* Return value is meaningless; inputs are key, keylen, string, stringlen, outbuf, outbuflen* *)
    val hmac_h = _import "hmac_SML": string * int * string * int * char array * int ref -> int;
in

fun hexToBinary (hex: string) = 
	(* Make sure that all characters are valid hex *)
	let val allHex: bool = CharVector.foldr (fn (c, b:bool) 
						    => b andalso (Char.isHexDigit(c))) 
						true hex
    	in 
	    case allHex of
		false => NONE
	      | true => 
		(* Create a buffer *)
		let val outlen = (String.size hex) div 2
		    val buf = Array.array (outlen, #"\000")
		    val _  = hexToBinary_h (hex, String.size hex, buf)
		    val out = CharVector.tabulate (outlen, fn i => Array.sub(buf,i))
		in
		    SOME out
		end
	end

fun binaryToHex (bin: string) = 
    let val outlen = 2 * String.size bin
	val buf = Array.array (outlen, #"\000")
	val _ = binaryToHex_h (bin, String.size bin, buf)
    in
	CharVector.tabulate (outlen, fn i => Array.sub(buf, i))
    end
    

val symkeyLength = symkeyLength_h 0
val hmacLength = symkeyLength

fun hexToSymkey (hex: string) = 
    
    (* Make sure that hex has the right length for a symmetric key *)
    if (String.size hex <> (2 * symkeyLength))
    then
	NONE
    else
	hexToBinary hex



fun symkeyToHex (sym: symkey) = (* A symkey is a string of raw binary data*)
    binaryToHex sym


fun hmac (key: symkey, data: string) = 
    let	val buf = Array.array (hmacLength, #"\000")
	val outlen = ref 0
	val _ = hmac_h (key, String.size key, data, String.size data, buf, outlen)
    in
	CharVector.tabulate (!outlen, fn i => Array.sub(buf, i))
    end

end (* local *)


end (* structure Crypto *)

