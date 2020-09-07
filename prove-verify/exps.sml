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

structure Exps = struct

local 
    open Time
in

exception BadExpression of string
exception UngroundExpression of string

(* Evaluate the arithmetic expression e and unify with x *)

fun simplify e x sc = 

    (* Simplify an expression and return a Time.time object corresponding to it *)
    
    let fun eval_exp (Terms.Time2exp (Terms.Prim_date2time (Terms.Prim_date d))) = d
	  | eval_exp (Terms.Time2exp (Terms.Prim_int2time (Terms.Prim_int i))) = Time.fromReal (Real.fromInt i)
	  | eval_exp (Terms.Exp_add (t1,t2)) = (eval_exp t1) + (eval_exp t2)
	  | eval_exp (Terms.Exp_subtract (t1,t2)) = (eval_exp t1) - (eval_exp t2)

	  | eval_exp (Terms.Exp_max (t1,t2)) = 
	    let val t1' = eval_exp t1
		val t2' = eval_exp t2
	    in 
		if (t1' >= t2') then t1' else t2'
	    end 

	  | eval_exp (Terms.Exp_min (t1,t2)) = 
	    let val t1' = eval_exp t1
		val t2' = eval_exp t2
	    in 
		if (t1' < t2') then t1' else t2'
	    end 

	  | eval_exp e = raise BadExpression ("simplify: Bad arithmetic expression " ^ (Terms.makestring_term e))

    in
	case (Terms.ground e) of
	    NONE => raise UngroundExpression ("simplify: Unground arithmetic expression " ^ (Terms.makestring_term e))

	  | SOME e' =>
	    let (* val _ = print ("Simplifying: " ^ (Terms.makestring_term e') ^ "\n"); *)
		val tt = eval_exp e' 
	    in
		Terms.unify (Terms.Prim_date2time (Terms.Prim_date tt)) x sc
		(* (fn () => (print ("Simplified value is: " ^ (Terms.makestring_term x) ^"\n"); sc ())) *)
	    end
    end


end (* Local *)
end (* structure Exps *)
