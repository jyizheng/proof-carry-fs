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

(* Functions to parse symmetric keys and configuration files stored in PCFS formats *)

structure PCFSConfig:> sig
val parse_shared_key_file: string -> Crypto.symkey Option.option (* string = filename *)
val parse_config_file: string -> int Option.option (* string = filename; return value = ADMIN_UID *)
end (* sig *)

  = struct

(* Remove whitespace from beginning of a char list *)
fun removeWhiteSpaceBeginning (s: char list) = 
    case s of
	[] => []
      | c :: s' => if (Char.isSpace c) 
		   then removeWhiteSpaceBeginning s'
		   else (c :: s')


(* Try check if ADMIN_UID matches a line: skip its initial whitespace, then
  check if the beginning matches "ADMIN_UID". If so, return the rest
  of the line *)

fun matchADMIN_UID (s: string) = 
    let val c_list = removeWhiteSpaceBeginning (String.explode s)
    in
	case c_list of
	    [] => NONE
	  | #"A" :: #"D" :: #"M" :: #"I" :: #"N" :: 
	    #"_" :: #"U" :: #"I" :: #"D" :: L => SOME (String.implode L)
	  | _ => NONE
    end

(* Check if a line is a comment, i.e., its first non-whitespace character is #, or there is only whitespace *)

fun isComment (s: string) = 
    case (removeWhiteSpaceBeginning (String.explode s)) of
	[] => true
      | #"#" :: _ => true
      | _ => false

(* Remove all whitespace from a line *)
fun removeWhiteSpace (s: string) = 
    let fun remove [] = []
	  | remove (c :: L) = if (Char.isSpace(c)) then remove L else
			      c :: (remove L)
    in
	(String.implode o remove o String.explode) s
    end


fun print_err s = TextIO.output (TextIO.stdErr, s)

(* Parse a shared key file, and return the key if one is found *)
fun parse_shared_key_file (f: string): Crypto.symkey Option.option = 
    (* The idea is to read the file line by line, ignoring comments,
       and concatenating others after removing whitespace. The string
       so obtained must be the hex of the symmetric key *)

    let
    
    (* Iterate an istream line by line, concatenating non-comments
       into the accumulator s *)
	fun iterate_lines (i: TextIO.instream, s) = 
	    case TextIO.inputLine i of
	    NONE => s
	  | SOME s' => if (isComment s') then iterate_lines (i,s)
		       else iterate_lines (i, s ^ s')
			    
	val i = TextIO.openIn f
	val s = iterate_lines (i, "")
	val _ = TextIO.closeIn i
	val key_opt = Crypto.hexToSymkey (removeWhiteSpace s)
		      
    in
	key_opt
    end



(* Parse a PCFS config file, and return the ADMIN_UID, if one is found *)	     							     
fun parse_config_file (f: string) = 
    (* The idea is to read the file line by line, ignore lines that
       are comments, and try to find ADMIN_UID in others. As soon as
       this succeeds, try to parse an integer (uid) from the line,
       returning the integer if this succeeds *)

    let 
	fun iterate_lines (i: TextIO.instream) =
	    case TextIO.inputLine i of
		NONE => NONE
	      | SOME s => if (isComment s) then iterate_lines i
			  else 
			      case (matchADMIN_UID s) of
				  NONE => iterate_lines i
				| SOME s_rem => Int.fromString s_rem

	val i = TextIO.openIn f
	val uid_opt = iterate_lines i
	val _ = TextIO.closeIn i
		      
    in
	uid_opt
    end
	
			      

end (* struct *)
