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

signature CONSTRAINTS = sig

datatype constraint = 
	 c_leq of Terms.term * Terms.term
       | c_stronger of Terms.term * Terms.term
       | c_other of string * Terms.term list (* constraint constructor, args *)

val subst : Terms.term -> Terms.varbind -> constraint -> constraint
val eq: (constraint * constraint) -> bool
val makestring_constraint: constraint -> string
val ground: constraint -> constraint option

type constraint_ctx = constraint list * constraint list * constraint list
		      (* leq assumptions, stronger assumptions, other assumptions *)

val empty_ctx: constraint_ctx
val make_ctx: constraint list -> constraint_ctx
val ctx_to_list: constraint_ctx -> constraint list

exception UngroundConstraint of string
val add: (constraint * constraint_ctx) -> constraint_ctx (* May throw exception UngroundConstraint *)

type hypconstraint = constraint_ctx * constraint

(* val check_hc: hypconstraint -> (unit -> unit) -> unit *)

val occurs_uvar_hypconstraint_list: Terms.term * hypconstraint list -> bool

val makestring_hypconstraint_list: hypconstraint list -> string

val simplify: hypconstraint -> hypconstraint

val subst_term_for_uvar_hypconstraint: Terms.term -> Terms.term -> hypconstraint -> hypconstraint
val subst_term_for_uvar_hcl: Terms.term -> Terms.term -> hypconstraint list -> hypconstraint list
end (* signature CONSTRAINTS *)



(*******************************************************************************************************)


structure Constraints :> CONSTRAINTS =  
(* Opaque transcription is needed to hide the implementation of constraint contexts *)
struct

datatype constraint = 
	 c_leq of Terms.term * Terms.term
       | c_stronger of Terms.term * Terms.term
       | c_other of string * Terms.term list (* constraint constructor, args *)


fun subst t x (c_leq (t1, t2)) = c_leq (Terms.subst t x t1, Terms.subst t x t2)
  | subst t x (c_stronger (t1, t2)) = c_stronger (Terms.subst t x t1, Terms.subst t x t2)
  | subst t x (c_other (s, tl)) = c_other(s, List.map (Terms.subst t x) tl)


fun eq (c_leq(t1,t2), c_leq(t1',t2')) = Terms.eq(t1,t1') andalso Terms.eq(t2,t2')
  | eq (c_stronger(t1,t2), c_stronger(t1',t2')) = Terms.eq(t1,t1') andalso Terms.eq(t2,t2')
  | eq (c_other(s, tl), c_other(s', tl')) = (s = s') andalso Terms.eq_list(tl, tl')
  | eq (_, _) = false

fun makestring_constraint (c_leq (t1, t2)) = "(leq " ^ (Terms.makestring_term t1) ^ " " ^ 
					     (Terms.makestring_term t2) ^ ")"
  | makestring_constraint (c_stronger (t1, t2)) = "(stronger " ^ (Terms.makestring_term t1) ^ " " ^ 
						  (Terms.makestring_term t2) ^ ")"
  | makestring_constraint (c_other (s, [])) = s
  | makestring_constraint (c_other (s, tl)) = "(" ^ s ^ " " ^ (Terms.makestring_term_list tl) ^ ")"



(* Return true iff the uvar u occurs free in the constraint c *)
fun occurs_uvar_constraint (u as Terms.Uvar _, c_leq (t1,t2)) = 
    (Terms.occurs_uvar_term (u,t1)) orelse (Terms.occurs_uvar_term (u,t2))
  | occurs_uvar_constraint (u as Terms.Uvar _, c_stronger (t1,t2)) = 
    (Terms.occurs_uvar_term (u,t1)) orelse (Terms.occurs_uvar_term (u,t2))
  | occurs_uvar_constraint (u as Terms.Uvar _, c_other (f,tl)) = Terms.occurs_uvar_term_list (u,tl)
  | occurs_uvar_constraint (_, _) = false

infix >>= ;

fun op >>= (x: 'a option, f: 'a -> 'b option): 'b option = Option.mapPartial f x
						     
(* Ground a constraint by dereferencing all Evars. Return SOME c if
this succeeds, NONE otherwise *)

fun ground (c_leq (t1,t2)) = (Terms.ground t1) 
				 >>= (fn t1' => (Terms.ground t2) 
						    >>= (fn t2' => SOME(c_leq(t1',t2'))))
  | ground (c_stronger (t1,t2)) = (Terms.ground t1) 
				      >>= (fn t1' => (Terms.ground t2) 
							 >>= (fn t2' => SOME(c_stronger(t1',t2'))))
  | ground (c_other (s, tl)) = (Terms.ground_list tl) >>= (fn tl' => SOME (c_other(s, tl')))

type constraint_ctx = constraint list * constraint list * constraint list

val empty_ctx: constraint_ctx = ([], [], [])

(* Add a constraint to a constraint_ctx. Ground the constraint before
   adding it; if grounding fails the exception UngroundConstraint is
   raised *)

exception UngroundConstraint of string

fun add (c:constraint, (C1,C2,C3): constraint_ctx) = 
    case (ground c) of
	SOME (c' as c_leq _) => (c'::C1, C2, C3)
      | SOME (c' as c_stronger _) => (C1, c'::C2, C3)
      | SOME (c' as c_other _) => (C1, C2, c' :: C3)
      | NONE => raise UngroundConstraint ("add: constraint " ^ (makestring_constraint c) ^ " not ground")


fun make_ctx (L: constraint list) =
    let fun make_ctx_iter [] (C1,C2,C3) = (C1,C2,C3)
	  | make_ctx_iter (c :: C) (C1, C2, C3) = make_ctx_iter C (add(c, (C1,C2,C3)))
    in
	make_ctx_iter L empty_ctx
    end 

fun ctx_to_list (C1, C2, C3) = C1 @ C2 @ C3

type hypconstraint = constraint_ctx * constraint (* Assumptions, goal *)


fun occurs_uvar_constraint_list (u, []) = false
  | occurs_uvar_constraint_list (u, c :: cl) = (occurs_uvar_constraint (u,c)) orelse 
					      (occurs_uvar_constraint_list (u, cl))

fun occurs_uvar_constraint_ctx (u, (C1,C2,C3)) = 
    (occurs_uvar_constraint_list (u, C1)) orelse
    (occurs_uvar_constraint_list (u, C2)) orelse
    (occurs_uvar_constraint_list (u, C3))

fun occurs_uvar_hypconstraint (u, (C,c)) = 
    (occurs_uvar_constraint_ctx (u,C)) orelse (occurs_uvar_constraint (u,c))

fun occurs_uvar_hypconstraint_list (u, []) = false
  | occurs_uvar_hypconstraint_list (u, hc :: hcl) = 
    (occurs_uvar_hypconstraint (u, hc)) orelse (occurs_uvar_hypconstraint_list (u, hcl))


(* Although we do not parse constraint lists and hypconstraints ever
   in the prover/verifier, they need to be printed in the Procaps *)

fun makestring_constraint_list [] = "constraint_nil"
  | makestring_constraint_list (c :: cl) = "(constraint_cons " ^ (makestring_constraint c) ^ " " ^
					   (makestring_constraint_list cl) ^ ")"

fun makestring_constraint_ctx (C1, C2, C3) = makestring_constraint_list (C1 @ C2 @ C3)

fun makestring_hypconstraint (C, c) = "(hypconstraint_ " ^ (makestring_constraint_ctx C) ^ " " ^
				      (makestring_constraint c) ^ ")"

fun makestring_hypconstraint_list [] = "hypconstraint_nil"
  | makestring_hypconstraint_list (hc :: hcl) = "(hypconstraint_cons " ^ (makestring_hypconstraint hc) ^
						" " ^ (makestring_hypconstraint_list hcl) ^ ")"


(* Simplify a hypothetical constraint. At present this simply deletes
   irrelevant assumptions, but this can be made more sophisticated. *)

fun simplify (((C1,C2,C3),c): hypconstraint) = case c of
						   c_leq _ => ((C1, [], []), c)
						 | c_stronger _ => (([], C2, []), c)
						 | c_other _ => (([], [], C3), c)

(* Substitute a term u' for a uvar u *)
fun subst_term_for_uvar u' u (c_leq (t1, t2)) = 
    c_leq (Terms.subst_term_for_uvar u' u t1, Terms.subst_term_for_uvar u' u t2)
  | subst_term_for_uvar u' u (c_stronger (t1, t2)) = 
    c_stronger (Terms.subst_term_for_uvar u' u t1, Terms.subst_term_for_uvar u' u t2)
  | subst_term_for_uvar u' u (c_other (f, tl)) =
    c_other (f, Terms.subst_term_for_uvar_list u' u tl)

fun subst_term_for_uvar_list u' u ([]: constraint list) = []
  | subst_term_for_uvar_list u' u (c :: cl) = 
    (subst_term_for_uvar u' u c) :: (subst_term_for_uvar_list u' u cl)

fun subst_term_for_uvar_constraint_ctx u' u (C1,C2,C3) = 
    (subst_term_for_uvar_list u' u C1, 
     subst_term_for_uvar_list u' u C2, 
     subst_term_for_uvar_list u' u C3)

fun subst_term_for_uvar_hypconstraint u' u (C,c) = 
    (subst_term_for_uvar_constraint_ctx u' u C,
     subst_term_for_uvar u' u c)

fun subst_term_for_uvar_hcl u' u hcl = 
    List.map (subst_term_for_uvar_hypconstraint u' u) hcl

end (* structure Constraints *)
