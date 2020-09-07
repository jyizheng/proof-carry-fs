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
THE SOFTWARE.*)


(* This file is meant to be compiled only with MLton, not SML/NJ. It
   provides SML interfaces to several C functions. This file should
   not reference any other SML code, hence it may be placed at the top
   in the .mlb file.
*)

structure Xattr = struct

(* get_xattr_size (filename: string, attribute: string, errcode: int ref): int 

   return the size of the value of attribute "attribute" on file
   "filename". If the size does not apply, return -1 and an 
   errcode, according to the following table:
   1 : file cannot be accessed
   2 : file does not exist or attribute does not exist on the file

   The error code is important -- it is used by the prover to decide
   how to proceed (the proof checker just ignores the error code).
*)

val get_xattr_size = _import "get_xattr_size": string * string * int ref -> int;

(* get_xattr (filename: string, attribute: string, buf: char array, size: int): int

   write the value of the attribute "attribute" on file "filename" to
   buf, whose maximum size is size. Return 1 on success, 0 on failure
*)

val get_xattr = _import "get_xattr": string * string * char array * int -> int;


(* Change later: add crypto functions here *)

end (* structure CInterface *)
