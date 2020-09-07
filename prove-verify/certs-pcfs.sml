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


(* Certificates for PCFS. This builds on the existing structures for
certificates, keymaps and propositions. Start by looking at the
definition of certificates (certs.sml). For PCFS, the following two
types of elements are supported (without quotes):

 * "type:policy" -- the data of this element must be a hypotheses, as
   parseable by Parser.parse_program. Further, each hypothesis must be
   a hyp_claims (no hyp_true) and all hypothesis must be stated by the
   same principal k.

 * "type:keymap" -- the data of this element must be a keymap, as
   parseable by XmlParser.parse_keymap. 

  A certificate is checked as follows. Each of its elements digital
  signatures are checked. Then the data fields are parsed according to
  their types as above. Now for each element E of "type:policy", it is
  checked that there is another element E' of "type:keymap" which
  states that the principal k stating all of the claims in E actually
  owns the key that signs E. For each element of "type:policy", it is
  checked that the signing key matches a fixed key belonging to the
  CA, which is passed from outside. If everything succeeds, the
  content of the certificate is assumed to be the concatenated list of
  all hypotheses from certificates of the form "type:policy", together
  with an intersection of the validity of certificates of the form
  "type:keymap".

*) 

structure CertsPCFS = struct

(* A datatype to represent the data in an element. It may be a
hypotheses or a keymap *)

datatype info = Ctx of Proofs.ctx
	      | Keymap of Terms.term * Crypto.pubkey * Time.time * Time.time
			  (* principal, its assigned public key, start time of certificate, expiration of certificate *)
 
(* A type similar to Certs.elem, but we replace the data field with info, and remove the type *)
type local_elem = info * Crypto.pubkey * string
type local_cert = local_elem list

exception CertCheckError of string


(* Check the signature on an element *)
fun checkSignatureElem ((data: string, _, key: Crypto.pubkey, sign: string): Certs.elem): unit = 
    case Crypto.verify (key, data, sign) of
	true => ()
      | false => raise CertCheckError ("Failed to check signature on data:\n" ^ (Crypto.pemEncode data))

fun checkSignatureCert (c: Certs.cert) = (List.map checkSignatureElem c; ())



(* Convert a Certs.elem to local_elem by parsing based on types *)
fun toInfoElem ((data, typ, key, sign) : Certs.elem): local_elem = 
    case (Certs.strToString typ) of
	"type:policy" => 
	(case (Parser.parse_program data) of
	     SOME p => (Ctx p, key, sign)
	   | NONE => raise CertCheckError ("Unable to parse the policy:\n" ^ (Crypto.pemEncode data))
	)
	
      | "type:keymap" => 
	(case (XmlParser.parse_keymap data) of
	     NONE => raise CertCheckError ("Unable to parse the keymap:\n" ^ (Crypto.pemEncode data))
	   | SOME (subject, key', from, to) => 
	     (case (Parser.parse_term (KeyMaps.strToString subject)) of
		  NONE => raise CertCheckError ("Unable to parse the subject in the keymap:\n" ^ (Crypto.pemEncode data))
		| SOME t => (Keymap (t, key', from, to), key, sign)
	     )
	)
      | s => raise CertCheckError ("Bad type: " ^ s)


fun toInfoCert (c: Certs.cert): local_cert = List.map toInfoElem c


(* Check that all assumptions in a context are claimed by exactly one
principal, and return that principal. NONE should be returned iff p is []
*)
fun checkCtx_for_claims (p: Proofs.ctx): Terms.term Option.option =
    
    (* Check that a each assumption in a context is claimed by the
    principal in the second argument (if it has form SOME k), else
    check that all assumptions in the context are claimed by the same
    principal *)

    let fun checkCtx_h [] kopt = kopt
	  | checkCtx_h (h :: hs) kopt =
	    case h of
		Proofs.hyp_true _ => raise CertCheckError ("\"Truth\" assumption in certificate: \n" ^
							   (Proofs.makestring_hyp h))
	      | Proofs.hyp_claims (_, _, k', _, _, _) =>
		(case kopt of
		     NONE => checkCtx_h hs (SOME k')
		   | SOME k => 
		     (case Terms.eq(k,k') of
			  true => checkCtx_h hs (SOME k)
			| false => 
			  raise CertCheckError 
				    ("Hypotheses in a single element are claimed by at least two different principals: " ^
				     (Terms.makestring_term k) ^ " and " ^ (Terms.makestring_term k'))
		     )
		)
    in
	case p of
	    [] => NONE
	  | (h as (Proofs.hyp_true _)) :: _ => raise CertCheckError ("\"Truth\" assumption in certificate: \n" ^
								     (Proofs.makestring_hyp h))
	  | (Proofs.hyp_claims (_,_,k,_,_,_)) :: hs => checkCtx_h hs (SOME k)
						       
    end


(* Check a certificate chain. 

  * For each element of type "type:policy", do the following:
    - Let k be the principal making all its claims
    - Let key be the signing key
    - Check that (k, key) occurs in some element of type "type:keymap"

  * For each element of type "type:keymap", do the following:
    - Check that is signed by the CA

  Return true or raise an exception
*)

fun certChainCheck (c: local_cert) (CA: Crypto.pubkey): unit = 
    
    (* Check a single element by the rules above *)
    let fun elemChainCheck_h e = 

	    case e of

		(Keymap _, key, _) => 
		if (Crypto.eq_pubkey (CA, key)) 
		then ()
		else raise CertCheckError ("Following signing key for a keymap does not match CA's key\n " ^
					   (Option.valOf (Crypto.pubkeyToString key)))

	      | (Ctx p, key ,s) =>
		(case (checkCtx_for_claims p) of
		     NONE => ()
		   | SOME k => case (List.exists (fn (Keymap (k',key',_,_), _,_) 
						     => Terms.eq(k,k') andalso Crypto.eq_pubkey(key,key')
						   | (Ctx _, _,_) => false) c) of
				   false => 
				   raise CertCheckError ("Keymap for the following principal, key pair is missing\n" ^
							 "Principal: " ^ (Terms.makestring_term k) ^ "\n" ^
							 "Key:\n" ^ (Option.valOf (Crypto.pubkeyToString key)))
				 | true => ()
		)

    in
	List.map (elemChainCheck_h) c; ()
    end


(* The content of a certificate is a concatenated list of all contexts
in it, together with the intersection of validities of all keymaps
in it *)

fun extractContent (c: local_cert): (Proofs.ctx * Time.time * Time.time) = 
    let fun min(t1: Time.time, t2: Time.time) = if (Time.compare (t1,t2) = LESS) then t1 else t2
	fun max(t1: Time.time, t2: Time.time) = if (Time.compare (t1,t2) = LESS) then t2 else t1
    in
	case c of
	    [] => ([], Time.zeroTime, Time.fromReal 2149000000.0) 
	  | (e :: c) =>
	    let val (ctx, t1, t2) = extractContent c
	    in
		case e of
		    (Ctx p, _, _) => (p @ ctx, t1, t2)
		  | (Keymap (_,_,t1',t2'), _, _) => (ctx, max(t1,t1'), min(t2,t2'))
	    end

    end


(* 
   Do the following:
   * Check the signatures in a certificate
   * Parse all data fields based on their types
   * Do a chain check for signing keys and claims
   Return the content of the certificate as defined in extractContent()
*)

fun getCertContent (c: Certs.cert, CA: Crypto.pubkey) =
    (checkSignatureCert c;
     let val c' = toInfoCert c
     in
	 certChainCheck c' CA;
	 extractContent c'
     end
    )

(* Main functions: Check a number of certificates and extract their content using getCertContent 
   - decode_certs works from Certs.cert list
   - decode_certs_file parses the certificates
   - decode_certs_file_file parses the certificates and the key in addition						   
*) 
fun decode_certs (cl: Certs.cert list, CA: Crypto.pubkey) =
    getCertContent (List.concat cl, CA)

fun decode_certs_file (fl: string list, CA: Crypto.pubkey) = 
    let val cl = 
	    List.foldr 
		(fn (f, cl') => case (XmlParser.parse_cert_file f) of
				    NONE => raise CertCheckError ("Unable to parse certificate file " ^ f)
				  | SOME c => (c :: cl')) 
		([]: Certs.cert list) fl
    in
	decode_certs (cl, CA)
    end

fun decode_certs_file_file (fl: string list, CAf: string) = 
    case (Crypto.readPubkeyFromFile CAf) of
	NONE => (raise CertCheckError ("Unable to parse CA public key from file " ^ CAf))
      | SOME CA => decode_certs_file (fl, CA)


end (* structure CertsPCFS *)
