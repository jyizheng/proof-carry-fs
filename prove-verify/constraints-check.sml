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

(* Code for checking constraints *)

signature CONSTRAINTSCHECK = sig

(* Constraint checking function that will cause unification. Must be
   given a continuation that is called on success. On failure, () is
   returned *)

val check_hc : Constraints.hypconstraint -> (unit -> unit) -> unit

(* Constraint checking function that should be called only for ground
   constraints. Will return true or false *)

val check_hc': Constraints.hypconstraint -> bool

end (* sig *)


structure ConstraintsCheck:> CONSTRAINTSCHECK = struct
exception SubType of string

local
    open Time
    open Constraints

    (* Checks if the relation Tl <= Tu meaningfully extends a list L
       of reachable time points. The function can be used for both
       time points and principals. This function returns true iff Tu
       is not in L and there is a T in L such that direct_below(T, Tl)
       holds *)

    fun does_extend direct_below (Tl, Tu) (L as []) = false (* Clearly, there is no meaningful extension *)
      | does_extend direct_below (Tl, Tu) (T :: L) = 
	if (Terms.eq(T, Tu)) then false (* Tu already exists *)
	else if (direct_below(T, Tl)) then not (List.exists (fn T => Terms.eq(T, Tu)) L)
	else does_extend direct_below (Tl, Tu) L
	
    datatype result = DONE | CHANGED | UNCHANGED

    (* Iterates through the context C of leq relations, adding new
       derived reachable points to L. Returns (DONE, L') if Target
       becomes reachable, (CHANGED, L') if a change was made to L, and
       (UNCHANGED, L) otherwise. The last argument is the status so
       far, it should initially be set to UNCHANGED *)
    
    fun iterate_once direct_below (C as []) L Target Flag = (Flag, L)

      | iterate_once direct_below (c_leq(T1, T2) :: C) L Target Flag =
	if (does_extend direct_below (T1, T2) L) 
	then
	    if (direct_below (T2, Target)) 
	    then (DONE, T2 :: L)
	    else iterate_once direct_below C (T2 :: L) Target CHANGED
	else 
	    iterate_once direct_below C L Target Flag

      | iterate_once direct_below (c_stronger(K1,K2) :: C) L Target Flag = 
	if (does_extend direct_below (K1,K2) L)
	then
	    if (direct_below (K2, Target))
	    then (DONE, K2 :: L)
	    else iterate_once direct_below C (K2 :: L) Target CHANGED
	else
	    iterate_once direct_below C L Target Flag

      | iterate_once _ (c :: C) _ _ _ = raise SubType ("iterate_once: constraint list has non-leq constraint")
 

    (* Iterate again and again with the context C, adding more and
       more reachable points to L using iterate_once until either
       UNCHANGED is signalled, or DONE is signalled. Return true if
       Target can be reached, false otherwise *)
 
    fun iterate direct_below C L Target =
	let val (Flag, L') = iterate_once direct_below C L Target UNCHANGED
	in 
	    case Flag of
		UNCHANGED => false
	      | DONE => true
	      | CHANGED => iterate direct_below C L' Target
	end 

    fun direct_leq (Terms.Ninfty, _) = true
      | direct_leq (_, Terms.Pinfty) = true
      | direct_leq (Terms.Prim_date2time (Terms.Prim_date d), Terms.Prim_date2time (Terms.Prim_date d')) = 
	(Time.compare (d,d') = LESS) orelse (Time.compare(d,d') = EQUAL)
      | direct_leq (t1, t2) = Terms.eq (t1,t2)
			      
    fun check_hc_leq ((C1, _, _), c) = 
	case (ground c) of
	    SOME (c_leq (T1, T2)) => (direct_leq(T1, T2)) orelse (iterate direct_leq C1 [T1] T2)
	  | SOME _ => raise SubType ("check_hc_leq: goal constraint is not of form leq(_,_)")	
	  | NONE => raise UngroundConstraint ("check_hc: target constraint : " ^ (makestring_constraint c) ^ " not ground")

    fun direct_stronger (Terms.Loca, _) = true
      | direct_stronger (t1, t2) = Terms.eq (t1,t2)

    fun check_hc_stronger ((_,C2,_), c) = 
	case (ground c) of
	    SOME (c_stronger(K1,K2)) => (direct_stronger(K1,K2)) orelse 
					(iterate direct_stronger C2 [K1] K2)
	  | SOME _ => raise SubType ("check_hc_stronger: goal constraint is not of form stronger(_,_)")
	  | NONE => raise UngroundConstraint ("check_hc: target constraint : " ^ (makestring_constraint c) ^ " not ground")

    fun check_hc_other (C as (_,_,C3), c as (c_other _)) sc = if (List.exists (fn c' => eq(c,c')) C3) 
							 then sc () 
							 else ConstraintsExternal.solve (C, c) sc
      | check_hc_other (_,_) sc = raise SubType ("check_hc_other: goal constraint is not of form other(_,_)")

in

fun check_hc (C, c) sc = 

(* Note that ctime has no special meaning for this constraint
solver. The prover does not use it at all, and the proof checker
treats it like a parameter. The Procap checker (in the core file
system) must interpret ctime as the time of access, but that code is
separately written. *)

    case c of
	c_leq _ => if (check_hc_leq (C, c)) then sc () else ()
      | c_stronger _ => if (check_hc_stronger (C, c)) then sc () else ()
      | c_other _ => check_hc_other (C, c) sc



fun check_hc' (C, c) =
    let 
	exception Success 
    in
	(check_hc (C, c) (fn () => raise Success) ; false)
	handle Success => true
    end
	
end (* local *)			       


end (* struct *)
