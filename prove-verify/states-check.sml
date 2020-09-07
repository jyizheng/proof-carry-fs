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

structure StatesCheck = struct

local 
    open States

in


fun print_err s = TextIO.output (TextIO.stdErr, s)

exception IOError
exception UngroundState of string
exception Impossible 


(* Function to match a state predicate matches with a state
context. For s_owner F K, only s_owner F _ is matched, for s_has_xattr
F A V, s_has_xattr F A _ is matched. For s_other, only the constructor
name is matched. Returns the arguments that are not matched as a list,
or NONE if nothing matches. Note that only the first match is
considered *)

fun match_in_ctx E (s_owner (F, K)) = 
    (case (List.find (fn s_owner(F', _) => Terms.eq(F,F')
			 | _ => false) E) of
	 NONE => NONE
       | SOME (s_owner (_, K')) => SOME [K']
       | SOME _ => raise Impossible 
    )
  | match_in_ctx E (s_has_xattr (F,A,V)) = 
    (case (List.find (fn s_has_xattr (F', A', _) => Terms.eq(F,F') andalso Terms.eq(A,A')
			 | _ => false) E) of
	 NONE => NONE
       | SOME (s_has_xattr (_,_,V')) => SOME [V']
       | SOME _ => raise Impossible
    )
  | match_in_ctx E (s_other (s, TL)) =
    (case (List.find (fn s_other (s',_) => s = s'
			 | _ => false) E) of
	 NONE => NONE
       | SOME (s_other (_,TL')) => SOME TL'
       | SOME _ => raise Impossible
    )

       
(* Path prefix: appended to all files before checking their attributes from disk *)

val path_prefix = ref ""

(* Internal memoization tables *)

val ask_user = ref true (* Should we ask the user ? *)

(* val system_state = ref ([] : state_ctx) *) (* A memo table for state predicates read from the system *)
val user_state = ref ([]: state_ctx) (* A memo table for state predicates as indicated by user *)

val user_state_neg = ref ([]: state_ctx)  
(* A memo table for state predicates that the user feels are
useless. These are stored to ensure that we do not ask the user the
same questions again an again *)

(* Reset all the memoization tables *)
fun reset_cache () = ((* system_state := [] ; *) user_state := [] ; user_state_neg := [])



(**********************************************************************************************************)

(* Functions to match a state by reading file attributes from files directly *)

datatype attr_value = NOEXIST (* File/attribute does not exist, certainly *)
		    | UNKNOWN (* May exist, but access is denied *)
		    | EXIST of Terms.term list (* Value of attribute *)
				   
local 

    fun get_xattr (file: string, attr:string) =
	let val f' = (!path_prefix) ^ file ^ "\000"
	    val a' = "user.#pcfs." ^ attr ^ "\000"
	    val errcode = ref 0
	    val length = Xattr.get_xattr_size(f',a', errcode)
	in 
	    case length of
		~1 => (case (!errcode) of
			   1 => UNKNOWN
			 | 2 => NOEXIST
			 | _ => raise Impossible
		      )
	      | 0 => raise Impossible (* Value cannot have length 0 *)
	      | _ => 
		let val buf = Array.array (length + 1, #"\000")  (* Allocate a buffer *)
		    val ret = Xattr.get_xattr (f',a',buf,length) (* Get the value of the attribute *)
		in
		    case ret of
			0 => NOEXIST
		      | 1 => 
			let val term_s: string = CharVector.tabulate (length, fn i => Array.sub(buf,i)) 
			    val term_t = Parser.parse_term term_s 
			in
			    case term_t of
				NONE => NOEXIST
			      | SOME t => EXIST [t]
			end
		      | _ => raise Impossible
		end
	end

    fun get_owner (f: string) =
	(let 
	     val st = Posix.FileSys.stat ((!path_prefix) ^ f)
	     val uid = Posix.FileSys.ST.uid st
	     val uid_word = Posix.ProcEnv.uidToWord uid
	     val uid_int = Word64.toInt uid_word
	 in 
	     EXIST [(Terms.Prim_int2principal (Terms.Prim_int uid_int))]
	 end
	) handle OS.SysErr (_,NONE) => NOEXIST
	       | OS.SysErr (_,SOME e) => if (e = Posix.Error.acces) 
					 then UNKNOWN
					 else NOEXIST
in

fun match_s_system (States.s_owner (Terms.Prim_str2file (Terms.Prim_str f), _)): attr_value = 
    get_owner f
  | match_s_system (States.s_has_xattr (Terms.Prim_str2file (Terms.Prim_str f), Terms.Prim_str a, _)) =
    get_xattr (f, a)
  | match_s_system _ = UNKNOWN

end (* local *)






(************************************************************************************************************)


(* Match a state predicate by taking user input *)
fun match_s_user s =
    let 

	(* Get a term from the console, and parse it *)

	fun get_term_from_user (max_tries as 0) = NONE
	  | get_term_from_user (max_tries) = 
	    let val _ = print_err "Enter term (without line feeds): "
		val s_opt = TextIO.inputLine (TextIO.stdIn)
	    in 
		case s_opt of
		    NONE => raise IOError
		  | SOME s => 
		    let val t = Parser.parse_term s
		    in 
			case t of 
			    NONE => (print_err "Term does not parse.\n";
				     get_term_from_user (max_tries - 1))
			  | SOME t' => SOME t'
		    end
	    end

	(* Ask the user a yes/no option *)
	fun get_user_yes_no () =
	    let val _ = print_err ("Enter y or Y for yes, anything else for no: ")
		val s_opt = TextIO.inputLine (TextIO.stdIn)
	    in 
		case s_opt of
		    NONE => raise IOError
		  | SOME s =>
		    if (s = "y\n" orelse s = "Y\n") then true else false
	    end 

	(* Find the value of K such that s_owner F K, where F is
           ground. Works as follows: 

           (1) Checks user_state_neg for something that matches
               s_owner F _. If this is the case, it means that in the
               past, the user said that s_owner F _ is irrelevant ot
               the search. So NONE is returned.
		     
           (3) Checks user_state for an entry s_owner F K' (for some
               K'). If there is one, the user indicated in the past
               that owner of F is K'. In that case SOME K' is returned.

           (4) Asks user whether owner of F might be useful for the
               proof. If user says no, it stores (s_owner F _) in
               user_state_neg and returns NONE. If
               user says yes, it asks the user for a value of K'. If
               this input fails, it returns NONE, else it stores
               (s_owner F K') in user_state, and returns SOME K'.
	 *)

	fun match_s_user_owner (F: Terms.term, K: Terms.term) = 
	    if (List.exists (fn (s_owner (F', _)) => Terms.eq(F,F')
			      | _ => false)  (!user_state_neg))
	    then  (* owner F _ exists in user_state_neg *)
		NONE
	    else (* Find owner F K' in user_state *)
		case (List.find (fn (s_owner (F', _)) => Terms.eq(F,F')
				  | _ => false) (!user_state)) of
		    
		    SOME (s_owner (_, K')) => SOME [K']
		  | SOME _ => raise Impossible (* Cannot happen *)
		  | NONE => (* Not found, so ask the user *)
		    let val _ = print_err ("The search tool thinks that the owner of (" ^ 
				       (Terms.makestring_term F) ^ ") may be useful in finding the proof.\n" ^ 
				       "Do you think so?\n")
			val yn = get_user_yes_no ()
		    in 
			if (not yn) 
			then (user_state_neg := (s_owner (F, Terms.Loca) :: (!user_state_neg)) ; 
			      NONE )
			else 
			    let val _ = print_err ("The search tool cannot find the owner. Please help!\n" ^
					       "Enter a term representing the uid of the owner" ^
					       " (e.g., prim_int2principal 1000)\n")
				val t = get_term_from_user 3 
			    in 
				case t of
				    NONE => NONE
				  | SOME K' => (user_state := (s_owner (F, K') :: (!user_state)) ;
						SOME [K'])
			    end
		    end
		    

	fun match_s_user_has_xattr (F: Terms.term, A: Terms.term, V: Terms.term) = 
	    if (List.exists (fn (s_has_xattr (F', A', _)) => Terms.eq(F,F') andalso Terms.eq (A, A')
			      | _ => false)  (!user_state_neg))
	    then  (* has_xattr F A _ exists in user_state_neg *)
		NONE
	    else (* Find has_x_attr F A V' in user_state *)
		case (List.find (fn (s_has_xattr (F', A', _)) => Terms.eq(F,F') andalso Terms.eq (A, A')
				  | _ => false) (!user_state)) of
		    
		    SOME (s_has_xattr (_, _, V')) => SOME [V']  (* Found the term, so return *)
		  | SOME _ => raise Impossible (* Cannot happen *)
		  | NONE => (* Not found, so ask the user *)
		    let val _ = print_err ("The search tool thinks that the attribute of (" ^ 
				       (Terms.makestring_term A) ^ ") of file (" ^ (Terms.makestring_term F) ^ 
				       ") may be useful in finding the proof.\n" ^ 
				       "Do you think so?\n")
			val yn = get_user_yes_no ()
		    in 
			if (not yn) 
			then (user_state_neg := (s_has_xattr (F, A, Terms.Loca) :: (!user_state_neg)) ; 
			      NONE )
			else 
			    let val _ = print_err ("The search tool cannot find the attribute. Please help!\n" ^
					       "Enter a term representing the value of the attribute\n")
				val t = get_term_from_user 3 
			    in 
				case t of
				    NONE => NONE
				  | SOME V' => (user_state := (s_has_xattr (F, A, V') :: (!user_state)) ;
						SOME [V'])
			    end
		    end
		    

    in
	case s of 
	    s_owner (F, K) => match_s_user_owner (F, K)
	  | s_has_xattr (F,A,V) => match_s_user_has_xattr (F,A,V)
	  | s_other _ => NONE
    end



(* Check that the file of s_owner, or file and attribute of
   s_has_xattr are ground. Raise exception otherwise *)

fun get_ground_goal (S as s_owner (F, K)) = 
    (case (Terms.ground F) of
	 NONE => raise UngroundState ("check_hs: File name in " ^ (States.makestring_state S) ^ 
				      " is not ground\n")
       | SOME F' => s_owner (F', K)
    )

  | get_ground_goal (S as s_has_xattr (F, A, V)) = 
    (case (Terms.ground F) of
	 NONE => raise UngroundState ("check_hs: File name in " ^ (States.makestring_state S) ^ 
				      " is not ground\n")
       | SOME F' => case (Terms.ground A) of
			NONE => raise UngroundState ("check_hs: File name in " ^ (States.makestring_state S) ^ 
						     " is not ground\n")
		      | SOME A' => s_has_xattr (F', A', V)
    
    )

  | get_ground_goal (S as s_other _) = S
	 
		    


(* check_hs: checks whether the state formula s can be
satifisfied. First it looks up E to see if s is assumed. Then it uses
its system dependent heuristics to evaluate s. Finally if that fails,
it asks the user for the values. This function can unify some of the
arguments of s, and hence it needs a continuation. The continuation
has the type unit -> unit, and can be passed as is to unification *)

fun check_hs (E: state_ctx, s: state) sc = 
    let val S = get_ground_goal s
	val arglist = 
	    case (match_in_ctx E S) of
		SOME tl => SOME tl
	      | NONE =>
		case (match_s_system S) of
		    EXIST tl => SOME tl
		  | NOEXIST => NONE
		  | UNKNOWN =>
		    if (!ask_user) 
		    then 
			case (match_s_user S) of 
			    SOME tl => SOME tl
			  | NONE => NONE
		    else NONE
    in 
	case arglist of 
	    NONE => ()
	  | SOME tl =>
	    case S of
		s_owner (_, K) => Terms.unify_list [K] tl sc
	      | s_has_xattr (_,_,V) => Terms.unify_list [V] tl sc
	      | s_other (_, tl') => Terms.unify_list tl' tl sc
    end

end (*local*)	    
end (* Structure StatesCheck *)
