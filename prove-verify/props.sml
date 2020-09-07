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

structure Props =
struct

type pred = string

datatype prop = 
	 p_atomic of pred * Terms.term list
       | p_conj of prop * prop
       | p_disj of prop * prop
       | p_imp of prop * prop
       | p_forall of Sorts.sort * Terms.varbind * prop
       | p_exists of Sorts.sort * Terms.varbind * prop
       | p_top
       | p_bot
       | p_is of Terms.term * Terms.term (* p_is(E, X) -- evaluate expression E and unify with X *)
       | p_cinj of Constraints.constraint
       | p_sinj of States.state
       | p_says of Terms.term * prop  (* says(K, A) *)
       | p_at of prop * Terms.term * Terms.term  (* A at [T1, T2] *)
   

(* Substitution of terms for terms. subst t x A : substitute t for x
   in A. Note that this function does not check for capture, and must
   be used only when t does not contain free bound variables. In
   particular, it may be used if t is a fresh Uvar.
 *)
fun subst t x (p_atomic (P, tl)) = 
    p_atomic (P, map (fn t' => Terms.subst t x t') tl) 

  | subst t x (p_conj (A,B)) = 
    p_conj (subst t x A, subst t x B)

  | subst t x (p_disj (A,B)) = 
    p_disj (subst t x A, subst t x B)

  | subst t x (p_imp (A,B)) = 
    p_imp (subst t x A, subst t x B)

  | subst t x (p_forall (s, y, A)) =
    if (Terms.shadow x y) then p_forall(s,y,A) else p_forall (s,y, subst t x A)

  | subst  t x  (p_exists (s,y, A)) =
    if (Terms.shadow x y) then p_exists(s,y,A) else p_exists (s,y, subst t x A)

  | subst _ _ p_top = p_top

  | subst _ _ p_bot = p_bot

  | subst t x (p_is(t1, t2)) = p_is (Terms.subst t x t1, Terms.subst t x t2)

  | subst t x (p_cinj c) = p_cinj (Constraints.subst t x c)

  | subst t x (p_sinj s) = p_sinj (States.subst t x s)

  | subst t x (p_says(k, A)) = p_says (Terms.subst t x k, subst t x A)

  | subst t x (p_at (A, t1, t2)) = p_at (subst t x A, Terms.subst t x t1, Terms.subst t x t2)


(* Check equality upto alpha-renaming *)

fun eq (p_atomic (p, tl), p_atomic(p', tl')) = p=p' andalso Terms.eq_list (tl,tl')
  | eq (p_conj (A1,A2), p_conj (A1', A2')) = eq(A1, A1') andalso eq(A2,A2')
  | eq (p_disj (A1,A2), p_disj (A1', A2')) = eq(A1, A1') andalso eq(A2,A2')
  | eq (p_imp (A1,A2), p_imp (A1', A2')) = eq(A1, A1') andalso eq(A2,A2')
  | eq (p_forall (s,i,A), p_forall (s',i',A')) = 
    (s = s') andalso
    let val u = Terms.new_uvar i
    in
	eq (subst u i A, subst u i' A')
    end
  | eq (p_exists (s,i,A), p_exists (s',i',A')) = 
    (s = s') andalso
    let val u = Terms.new_uvar i
    in
	eq (subst u i A, subst u i' A')
    end
  | eq (p_top, p_top) = true
  | eq (p_bot, p_bot) = true
  | eq (p_is(t1,t2), p_is(t1',t2')) = Terms.eq(t1,t1') andalso Terms.eq(t2,t2')
  | eq (p_cinj c, p_cinj c') = Constraints.eq (c,c')
  | eq (p_sinj s, p_sinj s') = States.eq (s,s')
  | eq (p_says (k, A), p_says (k', A')) = Terms.eq(k,k') andalso eq(A,A')
  | eq (p_at (A, t1,t2), p_at (A', t1',t2')) = eq(A,A') andalso Terms.eq(t1,t1') andalso Terms.eq(t2,t2')
  | eq _ = false

fun makestring_prop (p_top) = "top"
  | makestring_prop (p_bot) = "bot"
  | makestring_prop (p_atomic (P, [])) = "(atom " ^ P ^ ")"
  | makestring_prop (p_atomic (P, tl)) = "(atom (" ^ P ^ " " ^ (Terms.makestring_term_list tl) ^ "))"
  | makestring_prop (p_conj (A, B)) = "(conj " ^ (makestring_prop A) ^ " " ^ (makestring_prop B) ^ ")"
  | makestring_prop (p_disj (A, B)) = "(disj " ^ (makestring_prop A) ^ " " ^ (makestring_prop B) ^ ")"
  | makestring_prop (p_imp (A, B)) = "(imp " ^ (makestring_prop A) ^ " " ^ (makestring_prop B) ^ ")"
  | makestring_prop (p_forall (s, Terms.Varbind b, A)) = "(forall " ^ (Sorts.makestring_sort s) ^ " ([" ^ b
							 ^ "] " ^ (makestring_prop A) ^ "))"
  | makestring_prop (p_exists (s, Terms.Varbind b, A)) = "(exists " ^ (Sorts.makestring_sort s) ^ " ([" ^ b
							 ^ "] " ^ (makestring_prop A) ^ "))"

  | makestring_prop (p_is (t1, t2)) = "(is " ^ (Terms.makestring_term t1) ^ " " ^ (Terms.makestring_term t2) ^ ")"
  | makestring_prop (p_cinj c) = "(cinj " ^ (Constraints.makestring_constraint c) ^ ")"
  | makestring_prop (p_sinj s) = "(sinj " ^ (States.makestring_state s) ^ ")"
  | makestring_prop (p_says (k, A)) = "(says " ^ (Terms.makestring_term k) ^ " " ^ (makestring_prop A) ^ ")"
  | makestring_prop (p_at (A, t1, t2)) = "(at " ^ (makestring_prop A) ^ " " ^ (Terms.makestring_term t1) ^
					 " " ^ (Terms.makestring_term t2) ^ ")"
end (* Structure Prop *)


