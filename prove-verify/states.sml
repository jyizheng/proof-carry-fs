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

structure States =
struct

datatype state = s_owner of Terms.term * Terms.term (* file, owner *)
	       | s_has_xattr of Terms.term * Terms.term * Terms.term (* file, attribute name, attribute value *)
	       | s_other of string * Terms.term list


fun subst t x (s_owner (t1, t2)) = s_owner (Terms.subst t x t1, Terms.subst t x t2)
  | subst t x (s_has_xattr (t1, t2, t3)) = s_has_xattr (Terms.subst t x t1, Terms.subst t x t2, Terms.subst t x t3)
  | subst t x (s_other (s, tl)) = s_other (s, List.map (Terms.subst t x) tl)


fun eq (s_owner(f,k), s_owner(f',k')) = Terms.eq(f,f') andalso Terms.eq(k,k')
  | eq (s_has_xattr (f,a,v), s_has_xattr(f',a',v')) = Terms.eq(f,f') andalso Terms.eq(a,a') andalso Terms.eq(v,v')
  | eq (s_other (s, tl), s_other(s',tl')) = s=s' andalso Terms.eq_list (tl,tl')
  | eq _ = false

fun makestring_state (s_owner (t1,t2)) = "(owner " ^ (Terms.makestring_term t1) ^ " " ^ (Terms.makestring_term t2) ^ ")"
  | makestring_state (s_has_xattr (t1,t2,t3)) = "(has_xattr " ^ (Terms.makestring_term t1) ^ " " 
					     ^ (Terms.makestring_term t2) ^ " " ^ (Terms.makestring_term t3) ^ ")"
  | makestring_state (s_other (s, [])) = s
  | makestring_state (s_other (s, tl)) = "(" ^ s ^ " " ^ (Terms.makestring_term_list tl) ^ ")"

fun makestring_state_list [] = "state_nil"
  | makestring_state_list (s :: sl) = "(state_cons " ^ (makestring_state s) ^ " " ^ (makestring_state_list sl) ^ ")"


infix >>= ;

fun op >>= (x: 'a option, f: 'a -> 'b option): 'b option = Option.mapPartial f x
						     
(* Ground a state by dereferencing all Evars. Return SOME s if
this succeeds, NONE otherwise *)


fun ground (s_owner (f, k)) = (Terms.ground f) >>=
			      (fn f' => (Terms.ground k) >>=
					(fn k' => SOME(s_owner(f',k'))))

  | ground (s_has_xattr (f,a,v)) = (Terms.ground f) >>=
				   (fn f' => (Terms.ground a) >>=
					     (fn a' => (Terms.ground v) >>=
						       (fn v' => SOME (s_has_xattr (f',a',v')))))

  | ground (s_other (s, tl)) = (Terms.ground_list tl) >>= (fn tl' => SOME (s_other(s, tl')))

type state_ctx = state list

exception UngroundState of string

fun add (s: state, E: state_ctx) = case (ground s) of
				       SOME s' => (s' :: E)
				     | NONE => raise UngroundState ("add: state " ^ (makestring_state s) ^ " not ground")


fun unify_state (s_owner(t1,t2)) (s_owner(t1',t2')) sc = Terms.unify_list [t1,t2] [t1',t2'] sc
  | unify_state (s_has_xattr(t1,t2,t3)) (s_has_xattr(t1',t2',t3')) sc = Terms.unify_list [t1,t2,t3] [t1',t2',t3'] sc
  | unify_state (s_other (s, tl)) (s_other (s', tl')) sc = if (s <> s') then ()
							   else Terms.unify_list tl tl' sc
  | unify_state _ _ sc = ()

fun makestring_state_ctx (sl: state_ctx) = makestring_state_list sl

fun occurs_uvar_state (u, s_owner (t1,t2)) = Terms.occurs_uvar_term (u, t1) orelse Terms.occurs_uvar_term (u,t2)
   | occurs_uvar_state (u, s_has_xattr (t1,t2,t3)) = Terms.occurs_uvar_term (u, t1) orelse 
						     Terms.occurs_uvar_term (u, t2) orelse
						     Terms.occurs_uvar_term (u, t3)
   | occurs_uvar_state (u, s_other (f, tl)) = Terms.occurs_uvar_term_list (u, tl)

fun occurs_uvar_state_list (u, []) = false
  | occurs_uvar_state_list (u, s :: sl) = (occurs_uvar_state (u, s)) orelse (occurs_uvar_state_list (u, sl))

(* Substitute a term u' for an existing uvar u *)
fun subst_term_for_uvar u' u (s_owner (f,k)) = s_owner (Terms.subst_term_for_uvar u' u f, Terms.subst_term_for_uvar u' u k)
  | subst_term_for_uvar u' u (s_has_xattr (f,a,v)) = s_has_xattr (Terms.subst_term_for_uvar u' u f, 
								  Terms.subst_term_for_uvar u' u a,
								  Terms.subst_term_for_uvar u' u v)
  | subst_term_for_uvar u' u (s_other (f, tl)) = s_other (f, Terms.subst_term_for_uvar_list u' u tl)
						 
fun subst_term_for_uvar_ctx u' u [] = []
  | subst_term_for_uvar_ctx u' u (s :: sl) = (subst_term_for_uvar u' u s) :: (subst_term_for_uvar_ctx u' u sl)

end (* structure States *)
