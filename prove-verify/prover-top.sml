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

(* This module provides top level functions to search for
   proofs. Functions are categorized along three orthogonal axis:

   (1) Whether it allows the lists of assumed parameters, constraints,
   and state conditions to be non-empty or not. Those that DO, contain
   the string _with_constraints in their names.

   (2) Whether it takes the hypotheses as a list, or as a list of file
   name. In the latter case, the file may either contain a raw list of
   assumptions, or they may be certificates (certs.sml). If the
   function takes a file of raw hypotheses, it contains the string
   _file_raw_ in its name. If it takes certificates, it contains
   _file_cert_ in its name. 

   Note that this choice only governs how the hypotheses are
   provided. Declarations, parameters, constraints, and state
   conditions (if applicable) must be provided pre-parsed.

   (3) Whether it will take a general proposition that is proved or a
   pcfs-specific argument. The latter is a six tuple (a, k, f, p, tb,
   te), where a: Terms.term is the administrator of PCFS, k:
   Terms.term is a principal, f: Terms.term is a filename, p:
   Terms.term is a permission, tb: Terms.term is a time point, and te:
   Terms.term is also a time point. It will construct a proof that
   establishes:
   
   at (says a (atom (may k f p)) tb te)

   PCFS-specific functions have the string "pcfs" in their
   name. 

*)

structure ProverTop = struct

fun print_err s = TextIO.output (TextIO.stdErr, s)

type sigma = (Terms.term * Sorts.sort) list (* Map from uvars to sorts *)

(* Type check the arguments to the prover. However, failure of
   typechecking will not cause an exception, instead it will print a
   warning and continue.
 *)

fun type_check (decls: Declarations.decls) (S: sigma) (C: Constraints.constraint_ctx) (E: States.state_ctx)
	       (G: Proofs.ctx) (A: Props.prop) =
    let
	(* Type check the decls *)
	val _ = (Tc.tc_decls [] decls) handle Tc.TypeCheckError s => print_err ("Warning: " ^ s ^ "\n")
	(* Type check sigma *)
	val _ = (Tc.tc_sigma decls S) handle Tc.TypeCheckError s => print_err ("Warning: " ^ s ^ "\n")
        (* Type check the goal *)
        val _ = (Tc.tc_prop decls S [] A) handle Tc.TypeCheckError s => print_err ("Warning: " ^ s ^ "\n")
	(* Type check the hypotheses *)
	val _ = (Tc.tc_hyp_list decls S [] G) handle Tc.TypeCheckError s => print_err ("Warning: " ^ s ^ "\n")
	(* Type check the constraint ctx *)
	val _ = (Tc.tc_constraint_ctx decls S [] C) handle Tc.TypeCheckError s => print_err ("Warning: " ^ s ^ "\n")
	(* Type check the states ctx *)
	val _ = (Tc.tc_state_list decls S [] E) handle Tc.TypeCheckError s => print_err ("Warning: " ^ s ^ "\n")
    in
	()
    end
    

exception ProverError of string

fun prove_with_constraints (decls: Declarations.decls) (S: sigma) (C: Constraints.constraint_ctx) (E: States.state_ctx)
			   (G: Proofs.ctx) (A: Props.prop): Proofs.pfn Option.option =
    let val _ = type_check decls S C E G A
    in
	Prover.solve_top (List.map (fn (u, _) => u) S) C E G A
    end


fun prove decls G A = prove_with_constraints decls [] (Constraints.empty_ctx) [] G A

fun prove_with_constraints_file_raw decls S C E (G: string list) A =
        let val program = 
	    let fun parsetc_cl [] = []
		  | parsetc_cl (f :: L) = 
		    case ((Parser.parse_program_file f) handle IO.Io _ => NONE) of
			NONE => raise ProverError ("Prover: Failed to parse raw hypotheses file: " ^ f)
		      | SOME prog => prog @ (parsetc_cl L)
	in
		parsetc_cl G
	end 
	    
    in
	prove_with_constraints decls S C E program A
    end

fun prove_file_raw decls (G: string list) A =
    prove_with_constraints_file_raw decls [] (Constraints.empty_ctx) [] G A

fun prove_with_constraints_file_cert decls S C E (G: string list, CA: Crypto.pubkey) A =
    (let val (program, _, _) = CertsPCFS.decode_certs_file(G, CA)
     in
	 prove_with_constraints decls S C E program A
     end
    ) handle CertsPCFS.CertCheckError s => raise ProverError s
				       

fun prove_file_cert decls (G: string list, CA: Crypto.pubkey) A =
    prove_with_constraints_file_cert decls [] (Constraints.empty_ctx) [] (G, CA) A


local
    fun make_prop_pcfs (a: Terms.term, k: Terms.term, f: Terms.term, p: Terms.term, from:Terms.term, to:Terms.term) = 
	Props.p_at (Props.p_says (a, Props.p_atomic ("may", [k,f,p])), from, to)

in

fun prove_with_constraints_pcfs decls S C E G (a, k, f, p, from, to) = 
    prove_with_constraints decls S C E G (make_prop_pcfs (a,k,f,p, from,to))
    
fun prove_pcfs decls G N (a, k, f, p, from, to) = 
    prove_with_constraints_pcfs decls [] Constraints.empty_ctx [] G (a,k,f,p,from,to)


fun prove_with_constraints_file_raw_pcfs decls S C E (G: string list) (a,k,f,p,from,to) = 
    prove_with_constraints_file_raw decls S C E G (make_prop_pcfs (a,k,f,p,from,to))

fun prove_file_raw_pcfs decls (G: string list) (a,k,f,p,from,to) =
    prove_with_constraints_file_raw_pcfs decls [] Constraints.empty_ctx [] G (a,k,f,p,from,to)

fun prove_with_constraints_file_cert_pcfs decls S C E (G: string list, CA: Crypto.pubkey) (a,k,f,p,from,to) =
    prove_with_constraints_file_cert decls S C E (G,CA) (make_prop_pcfs (a,k,f,p,from,to))

fun prove_file_cert_pcfs decls (G: string list, CA: Crypto.pubkey) (a,k,f,p,from,to) = 
    prove_with_constraints_file_cert_pcfs decls [] Constraints.empty_ctx [] (G,CA) (a,k,f,p,from,to)
    

end (* local *)

end
