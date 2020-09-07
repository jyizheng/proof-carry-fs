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

(* A description of Procaps (Proven Capabilities) and their substructures 

 * A Procap contains the following:
 * The principal it authorizes (Terms.term) 
 * The file it authorizes (Terms.term)
 * The permission it authorizes (Terms.term)
 * A list of hypothetical constraints with universally quantified parameters, called a qhcl
 *                 (((uvar * Sorts.sort) list) * (Constraints.hypconstraint list))
 * A list of state constraints (States.state list)
 * A MAC (string Option.option: the string contains 40 hexadecimal characters for a total of 160 bits)

 * A procap missing only the MAC (i.e., with MAC = NONE) is called a
   pre-procap. This is merely a notation used in comments, it does not
   have a separate datatype.

 * A procap with an additional list of bound parameters at its
   beginning is called a quantified procap or qprocap. PCFS by itself
   does not use qprocaps, but for integrating with a programming
   language, for instance, they may be very useful. In a qprocap, the
   MAC must be taken over a string that includes the list of bound
   parameters. In general, we use only qprocaps, not procaps. However,
   PCFS' backend understands only those qprocaps whose parameter list
   is empty.

*)

structure Procaps = struct

type hcl = Constraints.hypconstraint list

type qhcl = (Terms.term * Sorts.sort) list  * hcl

fun makestring_qhcl ([], hcl) = "(qhcl_base " ^ (Constraints.makestring_hypconstraint_list hcl) ^ ")"
  | makestring_qhcl (((u,s) :: ul), hcl) = "(qhcl_all ([" ^ (Terms.makestring_term u) ^ ": " ^ 
					   (Sorts.makestring_sort s) ^ "] " ^ (makestring_qhcl (ul, hcl)) ^
					   "))"

(* The type procap is for internal use only. Externally, the
   equivalent form is qprocap with sigma = [] *)

type procap = Terms.term (* Principal authorized *)
	      * Terms.term (* File authorized *)
	      * Terms.term (* Permission authorized *)
	      * qhcl  (* Constraints *)
	      * States.state list (* State conditions *)
	      * string Option.option (* MAC, in hex *)

type sigma = (Terms.term * Sorts.sort) list (* map from Terms.Uvar to sorts *)

type qprocap = sigma * procap



(* Convert a pre-procap to a string *)
fun makestring_preprocap (k,f,p,q,sl) = 
    "procap\n" ^ 
    "\t" ^ (Terms.makestring_term k) ^ "    %% Principal authorized\n" ^
    "\t" ^ (Terms.makestring_term f) ^ "    %% File authorized\n" ^
    "\t" ^ (Terms.makestring_term p) ^ "    %% Permission authorized\n" ^
    "\n" ^ 
    "%% Conditions that must hold\n" ^ 
    (makestring_qhcl q) ^ "\n" ^ 
    "\n" ^ 
    "%% State conditions that must hold\n" ^ 
    (States.makestring_state_list sl) ^ "\n" ^
    "\n" ^
    "%% The following period marks the end of the body of the procap.\n" ^
    ".\n" ^ 
    "\n" ^
    "%% MAC\n" 

(*
fun makestring_procap ((k,f,p,q,sl,_): procap, symkey: Crypto.symkey) = 
    let
	val pre = makestring_preprocap(k,f,p,q,sl)
	val mac = Crypto.hmac (symkey, pre)
	val mac_hex = Crypto.binaryToHex mac
    in
	pre ^ "#\n" ^ mac_hex ^ "\n"
    end
*)
    
(* Make a string from a qprocap. The MAC component is ignored, since it
   is sensitive to the exact printed pre-procap. Instead it is
   regenerated from the key.
*)

fun makestring_qprocap ((sigma, (k,f,p,q,sl,_)): qprocap, symkey: Crypto.symkey) = 
    let 
	fun makestring_elem (u,so) = "[" ^ (Terms.makestring_term u) ^ ": " ^ (Sorts.makestring_sort so) ^ "]\n"

	fun makestring_sigma [] = "" (* Should never be called directly *)
	  | makestring_sigma ((u,so) :: sigma) = makestring_elem (u,so) ^ (makestring_sigma sigma) 

	val pre = case sigma of 
		      [] => ""
		    | _ => "parameters\n" ^ (makestring_sigma sigma)
	val main = makestring_preprocap (k,f,p,q,sl)
	val body = pre ^ main
	val mac = Crypto.hmac (symkey, body)
	val mac_hex = Crypto.binaryToHex mac
    in
	body ^ "#\n" ^ mac_hex ^ "\n"
    end


(* Representation of parsed qprocaps: When a qprocap is parsed, its
   bound list of parameters must be considered a list of named
   arguments (since we wish to substitute them by name). As a result,
   the representation of a parsed procap is different from that of an
   ordinary qprocap. In this case, the each pair in sigma must be
   augmented with an additional component of type Terms.varbind that
   is the original name of the parameter. We assume that all
   Terms.varbind occurring in the parameters are distinct, as are the
   uvars.
*)

type sigma_parsed = (Terms.varbind * Terms.term * Sorts.sort) list (* original name, uvar, sort *)

type qprocap_parsed = sigma_parsed * procap

fun sigma_parsed_to_sigma (SP: sigma_parsed): sigma = List.map (fn (_,u,so) => (u,so)) SP

fun qprocap_parsed_to_qprocap ((sp, pr): qprocap_parsed): qprocap = (sigma_parsed_to_sigma sp, pr)

(* Substitute a term t for a uvar u in a parsed qprocap *)

fun subst_term_for_uvar_qhcl t u ([], hcl) = 
    ([], Constraints.subst_term_for_uvar_hcl t u hcl)
  | subst_term_for_uvar_qhcl t u (q as ((u', so) :: S, hcl)) =
    if (Terms.eq (u,u')) then q
    else
	let val (S', hcl') = subst_term_for_uvar_qhcl t u (S, hcl)
	in
	    ((u',so) :: S', hcl')
	end
				  

fun subst_term_for_uvar_procap t u (k,f,p,q,sl,SIG) =
     (Terms.subst_term_for_uvar t u k,
      Terms.subst_term_for_uvar t u f,      
      Terms.subst_term_for_uvar t u p,
      subst_term_for_uvar_qhcl t u q,
      States.subst_term_for_uvar_ctx t u sl,
      SIG)


(* Substitution for parameters in a qprocap *)
type substitution = (Terms.varbind * Terms.term) list

(* Substitute a single named parameter v with term t in a parsed qprocap qp *)

fun subst_term_for_parameter (t: Terms.term) (v: Terms.varbind) ((sp, pr): qprocap_parsed) =
    (* Find uvar that should be substituted *)
    case (List.find (fn (v',u,_) => v=v') sp) of
	NONE => (sp, pr) (* The parameter v is not an argument of the qprocap *)
      | SOME (_,uvar,_) => (List.filter (fn (v',_,_) => v <> v') sp, (* Remove the substituted paramter *) 
			    subst_term_for_uvar_procap t uvar pr)


(* Apply a substitution to a qprocap *)

fun apply_subst (subst: substitution) (qp: qprocap_parsed): qprocap_parsed = 
    List.foldr (fn ((v, t), qp') => subst_term_for_parameter t v qp') qp subst

end (* Structure Procaps *)
