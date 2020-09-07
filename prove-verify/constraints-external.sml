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

(* Code to solve constraints other than those that are
   pre-defined. This code can be customized for each installation *)

structure ConstraintsExternal:> sig

val solve: Constraints.hypconstraint -> (unit -> unit) -> unit

end (* sig *)

 = struct

open Constraints

(* Return the parent directory of a given path. Assume that path is
   valid and does not end in "/".  Return NONE if path is already
   root.
*)
   
fun parent_dir (fname: string): string Option.option =
    case fname of 
	"/" => NONE
      | _ =>
	let fun locate_last_slash 0 = 
		if (String.sub (fname, 0) = #"/") then 0 else ~1
	      | locate_last_slash n =
		if (String.sub (fname, n) = #"/") 
		then n else locate_last_slash (n-1)
			    
	    val last_loc = locate_last_slash ((String.size fname) - 1)
	in
	    case last_loc of
		~1 => NONE (* There isn't a slash! This should not happen, unless the pathname is badly formed *)
	      | 0 => SOME "/" (* Last slash is at the beginning *) 
	      | n => SOME (String.substring (fname, 0, n))
	end
	

fun solve (C,c) sc = 
    case c of
	c_leq _ => () (* This should never happen *)
      | c_stronger _ => () (* This should never happen *)
      | c_other ("member", [f, d]) =>
	(case (Terms.ground f) of
	    NONE => raise Constraints.UngroundConstraint ("File argument " ^ (Terms.makestring_term f) ^ 
							  "of member constraint not ground\n")
	  | SOME (Terms.Prim_str2file (Terms.Prim_str fname)) =>
	    let val parent = parent_dir fname
	    in
		case parent of
		    NONE => ()
		  | SOME dir => Terms.unify d (Terms.Prim_str2file (Terms.Prim_str dir)) sc
	    end
	  | SOME _ => ()
	)	  
      | _ => ()

end (* struct *)
