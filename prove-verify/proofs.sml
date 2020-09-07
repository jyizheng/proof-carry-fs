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

structure Proofs = 
struct

datatype pfvar = Pfvar_internal of string * int (* common prefix, timestamp *)
	       | Pfvar_external of string (* User given name *)
					 
local 
    val pfvar_count = ref(0)
in
(* Generate a new internal pfvar, with name as prefix *)
fun new_pfvar name = (pfvar_count := (!pfvar_count) + 1; Pfvar_internal(name, !pfvar_count)) 
end 

fun eq_pfvar (Pfvar_external s, Pfvar_external s') = s=s'
  | eq_pfvar (Pfvar_internal (_, i), Pfvar_internal (_, i')) = i=i'
  | eq_pfvar _ = false

datatype pfr = 
	 pf_hyp of pfvar
       | pf_impE of pfr * pfn * Terms.term * Terms.term
       | pf_conjE1 of pfr 
       | pf_conjE2 of pfr
       | pf_forallE of pfr * Terms.term
       | pf_atE of pfr
       | pf_rhole of pfr option ref (* A hole that contains a proof *)

and pfn = 
    pf_conjI of pfn * pfn 
  | pf_impI of Terms.term * Terms.term * pfvar * pfn (* t1, t2, x, N *)
  | pf_disjI1 of pfn
  | pf_disjI2 of pfn
  | pf_disjE of pfr * (pfvar * pfn) * (pfvar * pfn)
  | pf_botE of pfr
  | pf_topI
  | pf_forallI of Terms.term * pfn (* uvar, proof *)
  | pf_existsI of Terms.term * pfn
  | pf_existsE of pfr * (Terms.term * pfvar * pfn) (* uvar, pfvar, proof *) 
  | pf_synth2check of pfr
  | pf_is
  | pf_cinjI
  | pf_cinjE of pfr * pfn
  | pf_sinjI
  | pf_sinjE of pfr * pfn
  | pf_saysI of pfn
  | pf_saysE of pfr * (pfvar * pfn)
  | pf_atI of pfn
  | pf_nhole of pfn option ref (* A hole that contains a proof *)

fun makestring_pfvar (Pfvar_internal (s, n)) = s ^ (Int.toString n)
  | makestring_pfvar (Pfvar_external s) = s

fun makestring_pfr (pf_hyp v) = "(pf_hyp " ^ makestring_pfvar v ^ ")"
  | makestring_pfr (pf_impE (pfr, pfn,t1,t2)) = "(pf_impE " ^ (makestring_pfr pfr) ^ " " ^ (makestring_pfn pfn) ^ 
						" " ^ (Terms.makestring_term t1) ^ " " ^ (Terms.makestring_term t2) ^ ")"
  | makestring_pfr (pf_conjE1 (pfr)) = "(pf_conjE1 " ^ (makestring_pfr pfr) ^ ")"
  | makestring_pfr (pf_conjE2 (pfr)) = "(pf_conjE2 " ^ (makestring_pfr pfr) ^ ")"
  | makestring_pfr (pf_forallE (pfr, t)) = "(pf_forallE " ^ (makestring_pfr pfr) ^ " " ^ (Terms.makestring_term t) ^ ")"
  | makestring_pfr (pf_atE pfr) = "(pf_atE " ^ (makestring_pfr pfr) ^ ")"
  | makestring_pfr (pf_rhole (ref NONE)) = "???"
  | makestring_pfr (pf_rhole (ref (SOME pfr))) = makestring_pfr pfr

and makestring_pfn (pf_conjI (p1, p2)) = "(pf_conjI " ^ (makestring_pfn p1) ^ " " ^ (makestring_pfn p2) ^ ")"
  | makestring_pfn (pf_impI (u1 as (Terms.Uvar _),u2 as (Terms.Uvar _),v, p)) = 
    "(pf_impI ([" ^ (Terms.makestring_term u1) ^ "][" ^ (Terms.makestring_term u2) ^ "][" ^
    (makestring_pfvar v) ^ "] " ^ (makestring_pfn p) ^ "))"
  | makestring_pfn (pf_impI (_,_,_,_)) = raise Terms.Subtype ("Bound variable in pf_impI is not a Uvar")
  | makestring_pfn (pf_disjI1 p) = "(pf_disjI1 " ^ (makestring_pfn p) ^ ")"
  | makestring_pfn (pf_disjI2 p) = "(pf_disjI2 " ^ (makestring_pfn p) ^ ")"
  | makestring_pfn (pf_disjE (pfr, (v1, pfn1), (v2, pfn2))) =
    "(pf_disjE " ^ (makestring_pfr pfr) ^ " ([" ^ 
    (makestring_pfvar v1) ^ "] " ^ (makestring_pfn pfn1) ^ ") ([" ^
    (makestring_pfvar v2) ^ "] " ^ (makestring_pfn pfn2) ^ "))"
  | makestring_pfn (pf_botE pfr) = "(pf_botE " ^ (makestring_pfr pfr) ^ ")"
  | makestring_pfn (pf_topI) = "pf_topI"
  | makestring_pfn (pf_forallI (u as Terms.Uvar (_), pfn)) = "(pf_forallI ([" ^ (Terms.makestring_term u) ^ "] "^
							     (makestring_pfn pfn) ^ "))"
  | makestring_pfn (pf_forallI (_, pfn)) = raise Terms.Subtype ("Bound variable in pf_forallI is not a Uvar")
  | makestring_pfn (pf_existsI (t, pfn)) = "(pf_existsI " ^ (Terms.makestring_term t) ^ " " ^ (makestring_pfn pfn) ^ ")"
  | makestring_pfn (pf_existsE (pfr, (u as Terms.Uvar (_), pfvar, pfn))) = 
    "(pf_existsE " ^ (makestring_pfr pfr) ^ " ([" ^ (Terms.makestring_term u) 
    ^ "] [" ^ (makestring_pfvar pfvar) ^ "] " ^
    (makestring_pfn pfn) ^ "))"
  | makestring_pfn (pf_existsE (_, (_, pfvar, pfn))) = raise Terms.Subtype ("Bound variable in pf_existsE is not a Uvar")
  | makestring_pfn (pf_synth2check (pfr)) = "(pf_synth2check " ^ (makestring_pfr pfr) ^ ")"
  | makestring_pfn (pf_is) = "pf_is"
  | makestring_pfn (pf_cinjI) = "pf_cinjI"
  | makestring_pfn (pf_cinjE (pfr, pfn)) = "(pf_cinjE " ^ (makestring_pfr pfr) ^ " " ^ (makestring_pfn pfn) ^ ")"
  | makestring_pfn (pf_sinjI) = "pf_sinjI"
  | makestring_pfn (pf_sinjE(pfr, pfn)) = "(pf_sinjE " ^ (makestring_pfr pfr) ^ " " ^ (makestring_pfn pfn) ^ ")"
  | makestring_pfn (pf_saysI pfn) = "(pf_saysI " ^ (makestring_pfn pfn) ^ ")"
  | makestring_pfn (pf_saysE (pfr, (pfvar, pfn))) = "(pf_saysE " ^ (makestring_pfr pfr) ^ " ([" ^
						     (makestring_pfvar pfvar) ^ "] " ^ (makestring_pfn pfn) ^ "))"
  | makestring_pfn (pf_atI pfn) = "(pf_atI " ^ (makestring_pfn pfn) ^ ")"
  | makestring_pfn (pf_nhole (ref NONE)) = "???"
  | makestring_pfn (pf_nhole (ref (SOME pfn))) = makestring_pfn pfn


datatype hyp = hyp_true of Constraints.hypconstraint list (* This is never used in the prover, but the checker uses it *)
			   * pfr  (* Proof of assumption *)
			   * Props.prop (* Proposition *)
			   * Terms.term (* T1 *)
			   * Terms.term (* T2 *)

	     | hyp_claims of Constraints.hypconstraint list (* This is never used in the prover, but the checker uses it *)
			     * pfr  (* Proof of assumption *)
			     * Terms.term (* K *)
			     * Props.prop (* Proposition *)
			     * Terms.term (* T1 *)
			     * Terms.term (* T2 *)


type ctx = hyp list

exception BadContextPfTerm

fun makestring_hyp (hyp_true (_, pf_hyp v, A, t1, t2)) = (makestring_pfvar v) ^ ": hyp_true " ^ 
							 (Props.makestring_prop A) ^ " "  ^
							 (Terms.makestring_term t1) ^ " " ^
							 (Terms.makestring_term t2)
  |  makestring_hyp (hyp_claims (_, pf_hyp v, K, A, t1, t2)) = (makestring_pfvar v) ^ ": hyp_claims " ^ 
							      (Terms.makestring_term K) ^ " " ^ 
							      (Props.makestring_prop A) ^ " "  ^
							      (Terms.makestring_term t1) ^ " " ^
							      (Terms.makestring_term t2)
  | makestring_hyp _ = raise BadContextPfTerm


fun makestring_ctx ([]) = ""
  | makestring_ctx (h :: L) = (makestring_hyp h) ^ ".\n" ^ (makestring_ctx L)



(* Substitute a fresh uvar u' for an existing uvar u *)
fun subst_fresh_pfn  u' u N = 
    let fun subst N =
	    case N of
		pf_conjI (N1, N2) => pf_conjI (subst N1, subst N2)
	      | pf_impI (u1,u2,x, N') => if (Terms.eq(u1,u) orelse Terms.eq(u2,u)) 
					 then N
					 else pf_impI (u1,u2,x, subst N')
	      | pf_disjI1 N' => pf_disjI1 (subst N')
	      | pf_disjI2 N' => pf_disjI2 (subst N')
	      | pf_disjE (R, (x, N1), (y, N2)) => pf_disjE (subst_fresh_pfr u' u R,
							    (x, subst N1), (y, subst N2))
	      | pf_botE R => pf_botE (subst_fresh_pfr u' u R)
	      | pf_topI => pf_topI
	      | pf_forallI (u1, N') => if (Terms.eq(u1,u)) then N else pf_forallI(u1, subst N')
	      | pf_existsI (t, N') => pf_existsI (Terms.subst_term_for_uvar u' u t, subst N')
	      | pf_existsE (R, (u1, x, N')) => pf_existsE (subst_fresh_pfr u' u R,
							   if (Terms.eq (u1, u)) then (u1, x, N')
							   else (u1, x, subst N'))
	      | pf_synth2check R => pf_synth2check (subst_fresh_pfr u' u  R)
	      | pf_is => pf_is
	      | pf_cinjI => pf_cinjI
	      | pf_cinjE (R, N') => pf_cinjE (subst_fresh_pfr u' u R, subst N')
	      | pf_sinjI => pf_sinjI
	      | pf_sinjE (R, N') => pf_sinjE (subst_fresh_pfr u' u R, subst N')
	      | pf_saysI N' => pf_saysI (subst N')
	      | pf_saysE (R, (x, N')) => pf_saysE (subst_fresh_pfr u' u R, (x, subst N'))
	      | pf_atI N' => pf_atI (subst N')
	      | pf_nhole _ => N (* This should never happen *)
    in
	subst N
    end


and subst_fresh_pfr u' u R =
    let fun subst R = 
	    case R of
		pf_hyp _ => R
	      | pf_impE (R', N, t1,t2) => pf_impE (subst R', subst_fresh_pfn u' u N, 
						   Terms.subst_term_for_uvar u' u t1,
						   Terms.subst_term_for_uvar u' u t2)
	      | pf_conjE1 R' => pf_conjE1 (subst R')
	      | pf_conjE2 R' => pf_conjE2 (subst R')
	      | pf_forallE (R', t) => pf_forallE (subst R', Terms.subst_term_for_uvar u' u t)
	      | pf_atE R' => pf_atE (subst R')
	      | pf_rhole _ => R (* Should never happen *)

    in
	subst R
    end

end (* Structure PfTerms *)

