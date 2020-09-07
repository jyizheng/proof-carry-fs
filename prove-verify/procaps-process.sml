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

(* Functions for checking and compressing procaps *)

structure ProcapsProcess = struct

(* Take a qhcl and compress it by solving all constraints that can be
   solved, and by eliminating all uvars that do not occur in it *)
fun compress_qhcl (q: Procaps.qhcl): Procaps.qhcl =
    let
	(* Take a hypconstraint list and return a sublist of unsolvable ones *)
	fun get_unsolvable [] = []
	  | get_unsolvable (hc :: hcl) = 
	    case (ConstraintsCheck.check_hc' hc) of
		true => get_unsolvable hcl
	      | false => (Constraints.simplify hc) :: (get_unsolvable hcl)
			 
	(* Take a list of uvars and hypconstraint list and return only
           those uvars that occur in the hypconstraint list *)
	fun get_occurring ([], hcl) = []
	  | get_occurring ((u,s) :: ul, hcl) = 
	    case (Constraints.occurs_uvar_hypconstraint_list (u, hcl)) of
		true => (u,s) :: (get_occurring (ul, hcl))
	      | false => get_occurring (ul, hcl)
	
	(* Separate the list of uvars and the hcl from q *)
	val (ul, hcl) = q

	(* Now get the unsolvable hypconstraints, rest are ignored *)
	val hcl' = get_unsolvable hcl

	(* Get the relevant uvars *)
	val ul' = get_occurring (ul, hcl')
    in
	(ul', hcl')
    end


(* Parse a qprocap (or a procap), and check that the MAC signature on
   it is correct. The input is a string containing the entire qprocap,
   and a symmetric key using which the qprocap was supposedly
   signed. The output is the parsed qprocap if the string parses as a
   qprocap, and its signature checks; an exception is raised
   otherwise.

   The signature on a qprocap is made over the text starting from its
   first non-whitespace character which is included (usually a 'p'
   from 'parameters' or 'procap', or a % beginning a comment), and
   ending at its last # which is excluded.
*)

exception BadQprocap of string

fun load_qprocap (qprocap: string, symkey: Crypto.symkey): Procaps.qprocap_parsed =
    let
	(* function to find the index of the first non-whitespace
	character of a string. The counter n must initially be 0 *)
	
	fun firstNonWS s n = if (not (Char.isSpace (String.sub(s,n)))) then SOME n
			     else if (n >= (String.size s) - 1) then NONE 
			     else firstNonWS s (n + 1)

	(* function to find the index of the first # from the end of a
	   string. The counter n must initially be (String.size s) -
	   1 *) 
        fun lastHash s n = if (String.sub(s,n) = #"#") then SOME n
			   else if (n <= 0) then NONE
			   else lastHash s (n-1)

				
	val begin = firstNonWS qprocap 0
	val last = lastHash qprocap ((String.size qprocap) - 1)
    in
	case (begin, last) of
	    (SOME b, SOME l) => 
	    let val str = if (l > b) then String.substring(qprocap, b, l - b)
			  else raise BadQprocap ("Qprocap loader error: Qprocap's body is missing?\n")
		val hmac = Crypto.hmac (symkey, str)
		val parsed = Parser.parse_qprocap qprocap
	    in
		case parsed of
		    NONE => raise BadQprocap ("Qprocap loader error: Failed to parse the Qprocap.\n")
		  | SOME (qp as (_, (_,_,_,_,_, SOME sign))) => 
		    if (sign = Crypto.binaryToHex hmac) then qp 
		    else
			raise BadQprocap ("Qprocap loader error: Signature check failed.\n")
		  | SOME (qp as (_, (_,_,_,_,_, NONE))) => 
		    raise BadQprocap ("Qprocap loader error: parsed Qprocap has no signature. This is a bug.\n")
		  (* This should not happen, since a parsed procap will always have a signature *)
	    end
	  | (SOME _, NONE) => 
	    raise BadQprocap ("Qprocap loader error: Qprocap has no #. Could not detect where the signature began.\n")
	  | (NONE, _) => raise BadQprocap ("Qprocap loader error: Qprocap has only whitespace!.\n")
    end


end (* struct *)

