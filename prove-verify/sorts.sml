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

structure Sorts = struct

datatype sort = Sort_any
	      | Sort_principal
	      | Sort_time
	      | Sort_exp
	      | Sort_file
	      | Sort_perm
	      | Sort_str
	      | Sort_int
	      | Sort_date
	      | Sort_other of string

fun makestring_sort Sort_any = "any"
  | makestring_sort Sort_principal = "principal"
  | makestring_sort Sort_time = "time"
  | makestring_sort Sort_exp = "exp"
  | makestring_sort Sort_file = "file"
  | makestring_sort Sort_perm = "perm"
  | makestring_sort Sort_str = "str"
  | makestring_sort Sort_int = "int"
  | makestring_sort Sort_date = "date"
  | makestring_sort (Sort_other s) = s

(* Check that a sort s is a supersort of s' *)

fun supersort (Sort_any) _ = true
  | supersort s s' = (s = s')

end
