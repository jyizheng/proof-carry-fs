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

(* Code for creating and checking certificates used by PCFS *)
structure PCFSCert = struct

								
fun readFile f =
    let val inp = BinIO.openIn f
	val s = BinIO.inputAll inp
	val s' = Byte.bytesToString s
    in 
	(BinIO.closeIn(inp); s')
    end


fun print_err s = TextIO.output (TextIO.stdErr, s)

fun error () = 
    (print_err ("Usage: \n   pcfs-cert -check [-o] <ca-pubkey-file> [<cert1> <cert2> ...]");
     print_err ("       \n   pcfs-cert -signkey [-o] <ca-privkey-file> <pubkey-file> {-u <uid> | <principal>} <from> <to>");
     print_err ("       \n   pcfs-cert -signpolicy [-o] <privkey-file> <policyfile>");
     print_err ("\nDescription of -check:\n");
     print_err ("   Checks a certificate list to make sure that all policy elements are correctly signed, and that all public keys are attested by the CA's key.\n");
     print_err ("   -o (optional) causes the policies (if any) and the intersection of time validities of all keys to be printed on the standard output.\n");
     print_err ("   <ca-pubkey-file> should contain the CA's public key that attested all public keys in the certificates.\n");
     print_err ("   <cert1> <cert2> ... are the certificates that are simultaneously checked. Each certificate may contain any number of policy and/or key elements.\n");

     print_err ("Description of -signkey:\n");
     print_err ("   Creates a single-element certificate that associates a public key with a principal and prints it on the standard output.\n");
     print_err ("   -o (optional) includes an XML parseable description of the contents as a comment in the certificate.\n");
     print_err ("   <ca-privkey-file> should contain the CA's private key that is used to sign the certificates.\n");
     print_err ("   <pubkey-file> should contain the public key to be associated.\n");
     print_err ("   <uid> is the user-id (positive number) of the user with whom the key is to be associated.\n");
     print_err ("   <principal> is a PCFS Terms.term that represents the principal with whom the key is to be associated.\n");
     print_err ("   <from> is the earliest time at which the association is valid. Written as yyyy:mm:dd or yyyy:mm:dd:hh:mm:ss.\n");
     print_err ("   <to> is the latest time at which the association is valid. Written as yyyy:mm:dd or yyyy:mm:dd:hh:mm:ss.\n");


     print_err ("Description of -signpolicy:\n");
     print_err ("   Signs a file containing policy statements with a private key to create a single-element certificate and prints it on the standard output.\n");
     print_err ("   -o (optional) includes the original text of the policy as a comment in the certificate.\n");
     print_err ("   <privkey-file> should contain the private key that is to be used to sign the certificate.\n");
     print_err ("   <policyfile> should contain the policy to be signed. The policy is parsed to ensure its syntactic correctness.\n");
     OS.Process.exit (OS.Process.failure))
    

fun check (capubfile: string, cl: string list, print_policies: bool) = 
    (let val (hyps, tb, te) = CertsPCFS.decode_certs_file_file (cl, capubfile) 
     in
	 print_err ("Certificate(s) check OK.\n");
	 case print_policies of
	     true => (print (Proofs.makestring_ctx (hyps));
		      print ("Begin time for keymaps is: " ^ 
				(Date.fmt "%Y:%m:%d:%H:%M:%S" (Date.fromTimeLocal tb)) ^ "\n");
		       print ("End time for keymaps is: " ^ 
			      (Date.fmt "%Y:%m:%d:%H:%M:%S" (Date.fromTimeLocal te)) ^ "\n")
		     )
		      
	   | false  => ()
     end) handle CertsPCFS.CertCheckError s => print_err (s ^ "\n")
    
fun signkey (caprivfile: string, pubfile: string, principal: Terms.term, from: string, to: string, include_raw: bool) =
    case (Crypto.readPrivkeyFromFile caprivfile) of
	NONE => (print_err ("Unable to read CA's private key from: " ^ caprivfile ^ "\n"); error())
      | SOME CA =>
	case (Crypto.readPubkeyFromFile pubfile) of
	    NONE => (print_err ("Unable to read public key from: " ^ pubfile ^ "\n"); error())
	  | SOME pubkey =>
	    case (XmlParser.parse_date from) of
		NONE => (print_err ("Unable to parse <from>: " ^ from ^ "\n"); error())
			
	      | SOME dfrom => 
		case (XmlParser.parse_date to) of
		    NONE => (print_err ("Unable to parse <to>: " ^ to ^ "\n"); error())
			    
		  | SOME dto =>
		    let val subject = KeyMaps.stringToStr (Terms.makestring_term principal)
			val keymap_str = KeyMaps.makestring_keymap (subject, pubkey, dfrom, dto)
			val elem = Certs.createElemFromData (keymap_str, "type:keymap", CA)
			val cert_str = Certs.makestring_elem (elem, include_raw)
		    in
			print cert_str
		    end

fun signpolicy (privfile: string, policyfile: string, include_raw: bool) =
    case (Crypto.readPrivkeyFromFile privfile) of
	NONE => (print_err ("Unable to read private key from: " ^ privfile ^ "\n"); error())
      | SOME privkey =>
	(let val policies = readFile policyfile
	 in
	     case (Parser.parse_program policies) of
		 NONE => (print_err ("Unable to parse policies in: " ^ policyfile ^ "\n"); error())
	       | SOME ctx => 
		 (case CertsPCFS.checkCtx_for_claims ctx of
		      _ => (* Okay the ctx is fine, if there were some error, an exception would be raised *)
		      let val elem = Certs.createElemFromData (policies, "type:policy", privkey)
			 val cert_str = Certs.makestring_elem (elem, include_raw)
		      in
			  print cert_str
		      end
		 ) handle CertsPCFS.CertCheckError s => (print_err (s ^ "\n"); error())
	 end
	) handle IO.Io _ => (print_err ("Error reading policy file: " ^ policyfile ^ "\n"); error())
			  
val _ = case CommandLine.arguments() of
	    "-check" :: "-o" :: ca_pubkey_f :: L => check(ca_pubkey_f, L, true)
	  | "-check" :: ca_pubkey_f :: L => check(ca_pubkey_f, L, false)
	  | "-signkey" :: "-o" :: ca_privkey_f :: pubkey_f :: "-u" :: uid :: from :: to :: [] =>
	    (case (Int.fromString uid) of
		 NONE => (print_err ("Unable to read uid: " ^ uid ^ "\n"); error())
	       | SOME i => signkey (ca_privkey_f, pubkey_f, Terms.Prim_int2principal (Terms.Prim_int i), 
				    from, to, true)
	    )
	  | "-signkey" :: "-o" :: ca_privkey_f :: pubkey_f :: k :: from :: to :: [] =>
	    (case (Parser.parse_term k) of
		 NONE => (print_err ("Unable to parse principal: " ^ k ^ "\n"); error())
	       | SOME i => signkey (ca_privkey_f, pubkey_f, i, from, to, true)
	    )
	  | "-signkey" :: ca_privkey_f :: pubkey_f :: "-u" :: uid :: from :: to :: [] =>
	    (case (Int.fromString uid) of
		 NONE => (print_err ("Unable to read uid: " ^ uid ^ "\n"); error())
	       | SOME i => signkey (ca_privkey_f, pubkey_f, Terms.Prim_int2principal (Terms.Prim_int i), 
				    from, to, false)
	    )
	  | "-signkey" :: ca_privkey_f :: pubkey_f :: k :: from :: to :: [] =>
	    (case (Parser.parse_term k) of
		 NONE => (print_err ("Unable to parse principal: " ^ k ^ "\n"); error())
	       | SOME i => signkey (ca_privkey_f, pubkey_f, i, from, to, false)
	    )
	  | "-signpolicy" :: "-o" :: privkey_f :: policy_f :: [] => signpolicy (privkey_f, policy_f, true)
	  | "-signpolicy" :: privkey_f :: policy_f :: [] => signpolicy (privkey_f, policy_f, false)
	  | _ => error ()

end (* struct *)
