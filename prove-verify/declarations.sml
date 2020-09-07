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

structure Declarations = 
struct


datatype decl = Decl_sort of string (* name *)
	      | Decl_pred of string * (Sorts.sort list) (* name, arg types *)
	      | Decl_state of string * (Sorts.sort list) (* name, arg types *)
	      | Decl_constraint of string * (Sorts.sort list) (* name, arg types *)
	      | Decl_function of string * (Sorts.sort list) * Sorts.sort (* name, arg types, result type *)

local 
    fun makestring_sort_list [] = ""
      | makestring_sort_list (s :: sl) = 
	"term " ^ (Sorts.makestring_sort s) ^ " -> " ^ (makestring_sort_list sl)
in

fun makestring_decl (Decl_sort s) = s ^ ": sort."
  | makestring_decl (Decl_pred (s, sl)) = s ^ ": " ^ (makestring_sort_list sl) ^ "pred."
  | makestring_decl (Decl_state (s, sl)) = s ^ ": " ^ (makestring_sort_list sl) ^ "state."
  | makestring_decl (Decl_constraint (s, sl)) = s ^ ": " ^ (makestring_sort_list sl) ^ "constraint."
  | makestring_decl (Decl_function (s,sl,so)) = s ^ ": " ^ (makestring_sort_list sl) ^ "term " ^ 
						(Sorts.makestring_sort so) ^ "."
end


(* A list of declarations *)
type decls = decl list

fun makestring_decls (ds: decls) = 
    List.foldr (fn (d, s) => (makestring_decl d) ^ "\n" ^ s) "" ds


end (* structure Declarations *)
