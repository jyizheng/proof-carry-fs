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



(* Functions to check well-formedness of all syntactic entities
   w.r.t. a set of declarations and in some cases, a maps from uvars
   and bvars to sorts. Notation:

   decls -- List of declaration of ground constants (sorts, predicates, functions, state predicates, constraints) 
   S -- Map from uvars to sorts
   B -- Map from Bvars to sorts

   All function return () on success and throw an exception on error
*)

structure Tc = struct

exception TypeCheckError of string
(* Check a sort to make sure it is declared. Return unit or raise exception *)

fun tc_sort decls (Sorts.Sort_other s) = (* Check that Decl_sort s exists in decls *)
    (case (List.exists (fn (Declarations.Decl_sort s') => s=s' 
			| _ => false) decls) of
	 true => ()
       | false => (raise TypeCheckError("Type-checker: Undeclared sort: " ^ s))
    )
  | tc_sort decls _ = () (* All other sorts are pre-defined *)

fun tc_sort_list decls [] = ()
  | tc_sort_list decls (s :: sl) = (tc_sort decls s ; tc_sort_list decls sl)

(* Check a declaration against a list of declarations to ensure that
   all sorts in it occur in list 
*)


fun tc_decl decls (Declarations.Decl_sort s) = ()
  | tc_decl decls (Declarations.Decl_pred (p, sl)) = tc_sort_list decls sl
  | tc_decl decls (Declarations.Decl_state (st, sl)) = tc_sort_list decls sl
  | tc_decl decls (Declarations.Decl_constraint (c, sl)) = tc_sort_list decls sl
  | tc_decl decls (Declarations.Decl_function (f, sl, s)) = (tc_sort_list decls sl ; tc_sort decls s)
    
(* Check a list of declarations (ds) against another list of
declarations (decls). When calling this, decls will initially be [] 
*)

fun tc_decls decls (ds as []) = ()
  | tc_decls decls (d :: ds) = (tc_decl decls d ; tc_decls (decls @ [d]) ds)


(* A partial map from uvars to sorts. We do not have to type check
this map; it is always assumed to be well-formed *) 

type sigma = (Terms.term * Sorts.sort) list

(* A partial map from Bvars to sorts. We do not have to type check
this map; it is always assumed to be well-formed *) 

type bsigma = (Terms.varbind * Sorts.sort) list



(* Function to print an error message when the actual sort of a term
   is incompatible with its expected sort *)

fun raiseSortError (t: Terms.term, exp: Sorts.sort, act: Sorts.sort) = 
    raise TypeCheckError("Type-checker: Term " ^ (Terms.makestring_term t) ^ ". " ^ 
			 "Expected sort: " ^ (Sorts.makestring_sort exp) ^ ". " ^
			 "Actual sort: " ^ (Sorts.makestring_sort act) ^ ".")

(* Check a term against a list of declarations, and signatures for
uvars and Bvars, S and B respectively. The last argument is the sort
against which the check is to be made. When a proposition is checked,
S will always be nil; when a proof is checked, B will always be nil*)

exception Impossible 

fun tc_term decls (S: sigma) (B: bsigma) (Terms.Bvar s) so = 
    (case (List.find (fn (Terms.Varbind s', _) => s' = s) B) of
	 SOME (_, so') => if (Sorts.supersort so so') then () 
			  else raiseSortError(Terms.Bvar s, so, so')

       | NONE => raise TypeCheckError("Type-checker: Undeclared variable: " ^ s ) (* Should never happen *)
    )

  | tc_term decls S B (Terms.Evar _) so = raise Impossible 
(* Evars only arise in proof search, which does not use the type checker *)

  | tc_term decls S B (u as Terms.Uvar _) so = 
    (case (List.find (fn (u', _) => Terms.eq(u,u')) S) of
	 SOME (_, so') => if (Sorts.supersort so so') then () 
			  else raiseSortError(u, so, so')

       | NONE => raise TypeCheckError("Type-checker: Undeclared uvar: " ^ (Terms.makestring_term u) ^ "\n")
    )
    
  | tc_term decls S B (t as Terms.Appl (f, tl)) so = 
    (case (List.find (fn (Declarations.Decl_function(f', _, _)) => f=f' | _ => false) decls) of
	 NONE => raise TypeCheckError("Type-checker: Undeclared function symbol/constant: " ^ f ^ "\n")
       | SOME (Declarations.Decl_function(_, sl, so')) => if (Sorts.supersort so so') then tc_term_list decls S B tl sl
							  else raiseSortError (t, so, so')

       | SOME _ => raise Impossible
    )

  | tc_term decls S B (t as Terms.Prim_str s) so = if (Sorts.supersort so (Sorts.Sort_str)) then () 
						   else raiseSortError (t, so, Sorts.Sort_str)
  | tc_term decls S B (t as Terms.Prim_int i) so = if (Sorts.supersort so (Sorts.Sort_int)) then ()
						   else raiseSortError (t, so, Sorts.Sort_int)
  | tc_term decls S B (t as Terms.Prim_date d) so = if (Sorts.supersort so (Sorts.Sort_date)) then ()
						    else raiseSortError (t, so, Sorts.Sort_date)
  | tc_term decls S B (t as Terms.Loca) so = if (Sorts.supersort so (Sorts.Sort_principal)) then ()
					     else raiseSortError (t, so, Sorts.Sort_principal)
  | tc_term decls S B (t as Terms.Ctime) so = if (Sorts.supersort so (Sorts.Sort_time)) then () 
					      else raiseSortError (t, so, Sorts.Sort_time)
  | tc_term decls S B (t as Terms.Ninfty) so = if (Sorts.supersort so (Sorts.Sort_time)) then () 
					       else raiseSortError (t, so, Sorts.Sort_time)
  | tc_term decls S B (t as Terms.Pinfty) so = if (Sorts.supersort so (Sorts.Sort_time)) then () 
					       else raiseSortError (t, so, Sorts.Sort_time)
  | tc_term decls S B (t as Terms.Read) so = if (Sorts.supersort so (Sorts.Sort_perm)) then () 
					     else raiseSortError (t, so, Sorts.Sort_perm)
  | tc_term decls S B (t as Terms.Write) so = if (Sorts.supersort so (Sorts.Sort_perm)) then () 
					      else raiseSortError (t, so, Sorts.Sort_perm)
  | tc_term decls S B (t as Terms.Execute) so = if (Sorts.supersort so (Sorts.Sort_perm)) then () 
						else raiseSortError (t, so, Sorts.Sort_perm)
  | tc_term decls S B (t as Terms.Identity) so = if (Sorts.supersort so (Sorts.Sort_perm)) then () 
						 else raiseSortError (t, so, Sorts.Sort_perm)
  | tc_term decls S B (t as Terms.Govern) so = if (Sorts.supersort so (Sorts.Sort_perm)) then () 
					       else raiseSortError (t, so, Sorts.Sort_perm)
  | tc_term decls S B (t as Terms.Prim_str2file t') so = if (Sorts.supersort so (Sorts.Sort_file)) 
							 then tc_term decls S B t' Sorts.Sort_str
							 else raiseSortError (t, so, Sorts.Sort_file)
  | tc_term decls S B (t as Terms.Prim_date2time t') so = if (Sorts.supersort so  (Sorts.Sort_time)) 
							  then tc_term decls S B t' Sorts.Sort_date
							  else raiseSortError (t, so, Sorts.Sort_time)
  | tc_term decls S B (t as Terms.Prim_int2time t') so = if (Sorts.supersort so (Sorts.Sort_time)) 
							 then tc_term decls S B t' Sorts.Sort_int
							 else raiseSortError (t, so, Sorts.Sort_time)
  | tc_term decls S B (t as Terms.Prim_int2principal t') so = if (Sorts.supersort so (Sorts.Sort_principal)) 
							      then tc_term decls S B t' Sorts.Sort_int
							      else raiseSortError (t, so, Sorts.Sort_principal)
  | tc_term decls S B (t as Terms.Time2exp t') so = if (Sorts.supersort so (Sorts.Sort_exp))  
						    then tc_term decls S B t' Sorts.Sort_time
						    else raiseSortError (t, so, Sorts.Sort_exp)
  | tc_term decls S B (t as Terms.Exp_add (t1,t2)) so = if (Sorts.supersort so (Sorts.Sort_exp)) 
							then 
							    (tc_term decls S B t1 Sorts.Sort_exp ;
							     tc_term decls S B t2 Sorts.Sort_exp)
							else raiseSortError (t, so, Sorts.Sort_exp)

  | tc_term decls S B (t as Terms.Exp_subtract (t1,t2)) so = if (Sorts.supersort so (Sorts.Sort_exp)) 
							then 
							    (tc_term decls S B t1 Sorts.Sort_exp ;
							     tc_term decls S B t2 Sorts.Sort_exp)
							else raiseSortError (t, so, Sorts.Sort_exp)

  | tc_term decls S B (t as Terms.Exp_max (t1,t2)) so = if (Sorts.supersort so (Sorts.Sort_exp)) 
							then 
							    (tc_term decls S B t1 Sorts.Sort_exp ;
							     tc_term decls S B t2 Sorts.Sort_exp)
							else raiseSortError (t, so, Sorts.Sort_exp)

  | tc_term decls S B (t as Terms.Exp_min (t1,t2)) so = if (Sorts.supersort so (Sorts.Sort_exp)) 
							then 
							    (tc_term decls S B t1 Sorts.Sort_exp ;
							     tc_term decls S B t2 Sorts.Sort_exp)
							else raiseSortError (t, so, Sorts.Sort_exp)


and tc_term_list decls S B [] [] = ()
  | tc_term_list decls S B (t::tl) (so::sl) = (tc_term decls S B t so ; tc_term_list decls S B tl sl)
  | tc_term_list decls S B _ _ = raise TypeCheckError ("Type-checker: Number of arguments don't match declaration")
	 
(* Check well-formedness of a sigma, by checking that all sorts in it are defined *)

fun tc_sigma decls ([]: sigma) = ()
  | tc_sigma decls ((_,s) :: sigma) = (tc_sort decls s ; tc_sigma decls sigma)

(* Check well-formedness of a constraint *)

fun tc_constraint decls S B (Constraints.c_leq (t1,t2)) = (tc_term decls S B t1 (Sorts.Sort_time);
							   tc_term decls S B t2 (Sorts.Sort_time)) 

  | tc_constraint decls S B (Constraints.c_stronger (t1,t2)) = (tc_term decls S B t1 (Sorts.Sort_principal);
								tc_term decls S B t2 (Sorts.Sort_principal))
  | tc_constraint decls S B (Constraints.c_other (c, tl)) = 
    (case c of (* "member" is a special constraint we know about *) 
	 "member" => tc_term_list decls S B tl [Sorts.Sort_file, Sorts.Sort_file]
       | _ =>
	 (case (List.find (fn (Declarations.Decl_constraint (c', _)) => c=c' 
			    | _ => false) decls) of
	      NONE => raise TypeCheckError ("Type-checker: Undeclared constraint constructor: " ^ c)
	    | SOME (Declarations.Decl_constraint (_, sl)) => tc_term_list decls S B tl sl
	    | SOME _ => raise Impossible
	 )
    )

fun tc_constraint_list decls S B [] = ()
  | tc_constraint_list decls S B (c :: cl) =
    (tc_constraint decls S B c ; tc_constraint_list decls S B cl)

fun tc_constraint_ctx decls S B (cl1, cl2, cl3) = 
    (tc_constraint_list decls S B cl1; 
     tc_constraint_list decls S B cl2;
     tc_constraint_list decls S B cl3) 

fun tc_hypconstraint decls S B (cl, c) = (tc_constraint_ctx decls S B cl; tc_constraint decls S B c)

fun tc_hypconstraint_list decls S B [] = ()
  | tc_hypconstraint_list decls S B (hc :: hcl) = 
    (tc_hypconstraint decls S B hc ; tc_hypconstraint_list decls S B hcl)

(* Check well-formedness of a state predicate *)

fun tc_state decls S B (States.s_owner (t1,t2)) = (tc_term decls S B t1 (Sorts.Sort_file);
						   tc_term decls S B t2 (Sorts.Sort_principal))
  | tc_state decls S B (States.s_has_xattr (t1,t2,t3)) = (tc_term decls S B t1 (Sorts.Sort_file);
							  tc_term decls S B t2 (Sorts.Sort_str);
							  tc_term decls S B t3 (Sorts.Sort_any))

  | tc_state decls S B (States.s_other (s, tl)) = 
    (case (List.find (fn (Declarations.Decl_state (s', _)) => s = s' 
		       | _ => false) decls) of
	 NONE => raise TypeCheckError ("Type-checker: Undeclared state constructor: " ^ s)
       | SOME (Declarations.Decl_state (_, sl)) => tc_term_list decls S B tl sl
       | SOME _ => raise Impossible
    )

fun tc_state_list decls S B [] = ()
  | tc_state_list decls S B (s :: sl) = (tc_state decls S B s ; tc_state_list decls S B sl)

(* 
   Check well-formedness of a proposition. 
*)

fun tc_prop decls S B (Props.p_atomic (p, tl)) = 
    (case p of (* may is a special predicate, which we know about ! *)
	 "may" => tc_term_list decls S B tl [Sorts.Sort_principal, Sorts.Sort_file, Sorts.Sort_perm]
       | _ =>
	 (case (List.find (fn (Declarations.Decl_pred (p', _)) => p'=p | _ => false) decls) of
	      NONE => raise TypeCheckError ("Type-checker: Undeclared predicate: " ^ p)
	    | SOME (Declarations.Decl_pred (_, sl)) => tc_term_list decls S B tl sl
	    | _ => raise Impossible
	 )
    )
  | tc_prop decls S B (Props.p_conj(p1,p2)) = (tc_prop decls S B p1 ; tc_prop decls S B p2)
  | tc_prop decls S B (Props.p_disj(p1,p2)) = (tc_prop decls S B p1 ; tc_prop decls S B p2)
  | tc_prop decls S B (Props.p_imp(p1,p2)) = (tc_prop decls S B p1 ; tc_prop decls S B p2)
  | tc_prop decls S B (Props.p_forall (so, v, p)) = (tc_sort decls so ;  tc_prop decls S ((v,so) :: B) p)
  | tc_prop decls S B (Props.p_exists (so, v, p)) = (tc_sort decls so ;  tc_prop decls S ((v,so) :: B) p)
  | tc_prop decls S B (Props.p_top) = ()
  | tc_prop decls S B (Props.p_bot) = ()
  | tc_prop decls S B (Props.p_is (t1,t2)) = (tc_term decls S B t1 (Sorts.Sort_exp) ; 
					      tc_term decls S B t2 (Sorts.Sort_time))
  | tc_prop decls S B (Props.p_cinj c) = tc_constraint decls S B c
  | tc_prop decls S B (Props.p_sinj s) = tc_state decls S B s
  | tc_prop decls S B (Props.p_says (t, p)) = (tc_term decls S B t (Sorts.Sort_principal) ; tc_prop decls S B p)
  | tc_prop decls S B (Props.p_at (p,t1,t2)) = (tc_term decls S B t1 (Sorts.Sort_time);
						tc_term decls S B t2 (Sorts.Sort_time);
						tc_prop decls S B p)


(* Check well-formedness of a hypothesis *)
fun tc_hyp decls S B (Proofs.hyp_true (hcl, Proofs.pf_hyp (Proofs.Pfvar_external _), A, t1,t2)) = 
    (tc_hypconstraint_list decls S B hcl ; 
     tc_prop decls S B A ;
     tc_term decls S B t1 (Sorts.Sort_time) ;
     tc_term decls S B t1 (Sorts.Sort_time)) 
  | tc_hyp decls S B (Proofs.hyp_true (hcl, _ , A, t1,t2)) = raise Impossible
  | tc_hyp decls S B (Proofs.hyp_claims (hcl, Proofs.pf_hyp (Proofs.Pfvar_external _), k, A, t1,t2)) = 
    (tc_hypconstraint_list decls S B hcl ;
     tc_term decls S B k (Sorts.Sort_principal) ;
     tc_prop decls S B A ;
     tc_term decls S B t1 (Sorts.Sort_time) ;
     tc_term decls S B t1 (Sorts.Sort_time)) 
  | tc_hyp decls S B (Proofs.hyp_claims (hcl, _ , k, A, t1,t2)) = raise Impossible

(* Check well-formedness of a hypothesis list (a program, for instance) *)

fun tc_hyp_list decls S B [] = ()
  | tc_hyp_list decls S B (h :: hl) = (tc_hyp decls S B h ; tc_hyp_list decls S B hl)

end (* structure Tc *)
