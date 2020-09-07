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

(* This module provides top level functions for checking
   proofs. Functions are categorized along three orthogonal axis:

   (1) Whether it allows the lists of assumed parameters, constraints,
   and state conditions to be non-empty or not. Those that DO, contain
   the string _with_constraints in their names.

   (2) Whether it takes the hypotheses as a list, or as a list of file
   name. In the latter case, the file may either contain a raw list of
   assumptions, or they may be certificates (certs.sml). If the
   function takes a file of raw hypotheses, it contains the string
   _file_raw_ in its name. If it takes certificates, it contains
   _file_cert_ in its name. If certificates are taken, then the output
   constraints will limit the validity to the validity of
   certificates.

   Note that this choice only governs how the hypotheses are
   provided. Declarations, parameters, constraints, and state
   conditions (if applicable) must be provided pre-parsed.

   (3) Whether it will take a general proposition that is proved or a
   pcfs-specific argument. The latter is a four tuple (a, k, f, p),
   where a: Terms.term is the administrator of PCFS, k: Terms.term is
   a principal, f: Terms.term is a filename, and p: Terms.term is a
   permission. It will check that the proof establishes:
   
   at (says a (atom (may k f p)) ctime ctime)

   PCFS-specific functions have the string "pcfs" in their name. They
   return a qprocap. The sigma of the qprocap will be nil if the
   function is NOT of the form with_constraints. Other functions
   return (sigma * Procaps.qhcl * States.state_ctx)

*)

structure VerifierTop = struct

type sigma = Verifier.sigma

fun verify_with_constraints (decls: Declarations.decls) (S: sigma) (C: Constraints.constraint_ctx) (E: States.state_ctx)
			    (G: Proofs.ctx) (N: Proofs.pfn) (A: Props.prop) = 
    Verifier.pfn_check decls S C E G N A



fun verify decls G N A = verify_with_constraints decls [] Constraints.empty_ctx [] G N A

fun verify_with_constraints_file_raw decls S C E (G: string list) N A =
    let val program = 
	    let fun parsetc_cl [] = []
		  | parsetc_cl (f :: L) = 
		    case ((Parser.parse_program_file f) handle IO.Io _ => NONE) of
			NONE => raise Verifier.VerificationError ("Proof Verifier: Failed to parse raw hypotheses file: " 
								  ^ f)
		      | SOME prog => prog @ (parsetc_cl L)
	in
		parsetc_cl G
	end 
	    
    in
	verify_with_constraints decls S C E program N A
    end

fun verify_file_raw decls (G: string list) N A =
    verify_with_constraints_file_raw decls [] Constraints.empty_ctx [] G N A

fun verify_with_constraints_file_cert decls S C E (G: string list, CA: Crypto.pubkey) N A =
    (let val (program, Tl, Tu) = CertsPCFS.decode_certs_file(G, CA)
	 val (S', (S'', HCL), SL) = verify_with_constraints decls S C E program N A
	 val Tl' = Terms.Prim_date2time (Terms.Prim_date Tl)
	 val Tu' = Terms.Prim_date2time (Terms.Prim_date Tu)
	 val HCL' = (C, Constraints.c_leq (Tl', Terms.Ctime)) :: (C, Constraints.c_leq (Terms.Ctime, Tu')) :: HCL
     in
	 (S', (S'', HCL'), SL)
     end
    ) handle CertsPCFS.CertCheckError s => raise Verifier.VerificationError s


fun verify_file_cert decls (G: string list, CA: Crypto.pubkey) N A =
    verify_with_constraints_file_cert decls [] Constraints.empty_ctx [] (G, CA) N A


local
    fun make_prop_pcfs (a: Terms.term, k: Terms.term, f: Terms.term, p: Terms.term) = 
	Props.p_at (Props.p_says (a, Props.p_atomic ("may", [k,f,p])), Terms.Ctime, Terms.Ctime)

in

fun verify_with_constraints_pcfs decls S C E G N (a, k, f, p) = 
    let val (S', Q, SL) = verify_with_constraints decls S C E G N (make_prop_pcfs (a,k,f,p))
    in
	(S', (k,f,p,Q,SL,NONE)): Procaps.qprocap
    end

fun verify_pcfs decls G N (a, k, f, p) = 
    verify_with_constraints_pcfs decls [] Constraints.empty_ctx [] G N (a,k,f,p)


fun verify_with_constraints_file_raw_pcfs decls S C E (G: string list) N (a,k,f,p) = 
    let val (S',Q,SL) = verify_with_constraints_file_raw decls S C E G N (make_prop_pcfs (a,k,f,p))
    in
	(S', (k,f,p,Q,SL,NONE)): Procaps.qprocap
    end

fun verify_file_raw_pcfs decls (G: string list) N (a,k,f,p) =
    verify_with_constraints_file_raw_pcfs decls [] Constraints.empty_ctx [] G N (a,k,f,p)

fun verify_with_constraints_file_cert_pcfs decls S C E (G: string list, CA: Crypto.pubkey) N (a,k,f,p) =
    let val (S',Q,SL) = verify_with_constraints_file_cert decls S C E (G,CA) N (make_prop_pcfs (a,k,f,p))
    in
	(S', (k,f,p,Q,SL,NONE)): Procaps.qprocap
    end

fun verify_file_cert_pcfs decls (G: string list, CA: Crypto.pubkey) N (a,k,f,p) = 
    verify_with_constraints_file_cert_pcfs decls [] Constraints.empty_ctx [] (G,CA) N (a,k,f,p)
    

end (* local *)

end (* struct *)
