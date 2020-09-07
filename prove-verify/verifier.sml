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

(* Code for proof checking, and generation of parts of procaps. 

   Treatment of state conditions: If a non-empty list of state
   conditions E is passed from outside, it is checked against whenever
   a state condition needs to hold. At the end, E is appended to the
   list of (undischarged) state conditions in the output. As a result,
   the list of output state conditions may contain some of the input
   conditions that never actually occurred in the proof. This is
   somewhat wasteful. A better method is to always pass [] for E. Any
   state conditions occurring in the proof will then arise in the
   output.

   Treatment of constraints: If a non-empty set of constraints C is
   passed in, it will appear as the context of every hypothetical
   constraint in the output set of hypothetical constraints. In
   addition, all members c of C will be added to the output as (. |=
   c). Again this can be wasteful, since many of these c's may never
   be used. Indeed since (. |= c) must be the case, by cut, these are
   never necessary, and hence the initial C passed in should always be
   empty.

   In summary, we do allow non-empty E and C to be passed to the
   verifier, but realisitically, these should always be empty. This
   does not result in loss of expressiveness, and makes the output
   conditions more precise.
 *)

structure Verifier = struct

(* The type sigma represents a context for terms that maps uvars to
   their sorts. This type is equal to Tc.sigma *)

type sigma = (Terms.term * Sorts.sort) list

exception VerificationError of string
exception Unimplemented

(* Typecheck a term *)

fun typecheck_term (decls: Declarations.decls) (S: sigma) (t: Terms.term) (s: Sorts.sort) = 
    ((Tc.tc_term decls S [] t s) handle Tc.TypeCheckError s => raise VerificationError s)

(* Lookup the variable x in G (first occurence) in context K,T1,T2 and
   return SOME (A, T1', T2', HCL), where x: A on [T1', T2'] or x: K'
   claims A' on [T1',T2'], and HCL = [] in the former case, and [C |=
   K' >= K, C |= T1' <= T1, C |= T2 <= T2'] in the latter. If x does
   not occur, return NONE.
 *)

fun lookup (C: Constraints.constraint_ctx) ([]: Proofs.ctx) K T1 T2 (x: Proofs.pfvar) = NONE
  | lookup C (h :: G) K T1 T2 x =
    case h of
	Proofs.hyp_true (HCL, Proofs.pf_hyp v, A', T1', T2') =>
	if (Proofs.eq_pfvar (v,x)) then SOME (A', T1', T2', HCL) else lookup C G K T1 T2 x
      | Proofs.hyp_claims (HCL, Proofs.pf_hyp v, K', A', T1', T2') =>
	if (Proofs.eq_pfvar (v,x)) then 
	    let val HCL' = (C, Constraints.c_leq (T1', T1)) :: (C, Constraints.c_leq (T2, T2')) :: 
			   (C, Constraints.c_stronger (K', K)) :: HCL
	    in
		SOME (A', T1', T2', HCL')
	    end
	else
	    lookup C G K T1 T2 x
      | _ => raise VerificationError ("Proof verifier: Hypothesis in not of form (pf_hyp x). This is a bug\n")


(* The main proof checking/inference functions. We are using
   biredirectional proofs: normal proofs (Proofs.pfn) are checked,
   neutral proofs (Proofs.pfr) are infered. In each case, we produce a
   qhcl and a list of state predicates that must hold in order for the
   proofs to hold. The arguments Q and SL are accumulators to hold
   these respectively. These should initially be [] each. 
				 
   Note also that proof variables (Proofs.pfvar) are not alpha-renamed
   for type-checking binding constructs like pf_impI
   (_,_,x,N). Instead they get shadowed in the hypotheses G (see the
   code below), and hence become unusable. On the other hand, bound
   uvars in proofs such as u1 and u2 in pf_impI(u1,u2,_,_) must be
   alpa-renamed to fresh parameters. This is because after they are
   introduced in the context S in the premise, they may reappear in
   the output qhcl as bound uvars. To allow qhcls from different
   branches to be merged without further alpa-renaming, their uvars
   must be distinct.

   There is no need to "type-check" a proof-term (pfn or pfr) in
   advance, since that is done in the proof checking itself. The
   hypotheses (G, C, E) must be checked, however.
*)

fun pfn_check_core (decls: Declarations.decls) (S: sigma) (C: Constraints.constraint_ctx) (E: States.state_ctx)
		   (G: Proofs.ctx) (K: Terms.term) (T1: Terms.term) (T2: Terms.term) 
		   (N: Proofs.pfn) 
		   (A: Props.prop) (T1': Terms.term) (T2': Terms.term) 
		   (Q: Procaps.qhcl) (SL: States.state list)
    : (Procaps.qhcl * States.state list) = 

    case N of
	Proofs.pf_conjI (N1, N2) => 
	(case A of
	     Props.p_conj (A1, A2) => 
	     let val (Q1, SL1) = pfn_check_core decls S C E G K T1 T2 N1 A1 T1' T2' Q SL
	     in
		 pfn_check_core decls S C E G K T1 T2 N2 A2 T1' T2' Q1 SL1
	     end
	     
	   | _ => raise VerificationError ("Proof verifier: Expected conjuction, found formula:\n" ^ 
					   (Props.makestring_prop A))
	)

      | Proofs.pf_impI (u1, u2, x, N) => 
	(case A of
	     Props.p_imp (A1, A2) =>
	     let
		 (* generate new parameters for u1 and u2 *)
		 val u1' = Terms.new_uvar(Terms.Varbind "t")
		 val u2' = Terms.new_uvar(Terms.Varbind "t")
		 (* Substitute the new parameters into N *)
		 val Nn = Proofs.subst_fresh_pfn u1' u1 (Proofs.subst_fresh_pfn u2' u2 N)
		 (* Add u1' and u2' to the context S *)
		 val S' = (u1', Sorts.Sort_time) :: (u2', Sorts.Sort_time) :: S
		 (* Add x to the context G *)
		 val G' = (Proofs.hyp_true ([], Proofs.pf_hyp x, A1, u1', u2')) :: G
		 (* Add constraints to the context *)
		 val C' = Constraints.add (Constraints.c_leq (T1', u1'),
					   Constraints.add (Constraints.c_leq (u2', T2'), C))
		 (* Check the proof Nn *)
		 val ((S1, HCL1), SL1) = pfn_check_core decls S' C' E G' K T1 T2 Nn A2 u1' u2' Q SL
	     in
		 (* Quantify bound variables in Q1 *)
		 (((u1', Sorts.Sort_time) :: ((u2', Sorts.Sort_time) :: S1), 
		   HCL1),
		  SL1)
	     end
	     
	   | _ => raise VerificationError ("Proof verifier: Expected implication, found formula:\n" ^ 
					   (Props.makestring_prop A))
	)

      | Proofs.pf_disjI1 N1 =>
	(case A of
	     Props.p_disj (A1, A2) => pfn_check_core decls S C E G K T1 T2 N1 A1 T1' T2' Q SL
	   | _ => raise VerificationError ("Proof verifier: Expected disjunction, found formula:\n" ^ 
					   (Props.makestring_prop A))
	)
      | Proofs.pf_disjI2 N2 =>
	(case A of
	     Props.p_disj (A1, A2) => pfn_check_core decls S C E G K T1 T2 N2 A2 T1' T2' Q SL
	   | _ => raise VerificationError ("Proof verifier: Expected disjunction, found formula:\n" ^ 
					   (Props.makestring_prop A))
	)
      | Proofs.pf_disjE (R, (x, N1), (y, N2)) =>
	let
	    (* First infer R *)
	    val (B, t1, t2, Q0, SL0) = pfr_infer_core decls S C E G K T1 T2 R Q SL
	in
	    (* Now case analyze C to check its a disjunction *)
	    case B of 
		Props.p_disj (B1, B2) =>
		let
		    (* Construct the context G for the two branches *)
		    val G1 = (Proofs.hyp_true ([], Proofs.pf_hyp x, B1, t1, t2)) :: G
		    val G2 = (Proofs.hyp_true ([], Proofs.pf_hyp y, B2, t1, t2)) :: G
		    (* Check the two branches *)
		    val (Q1, SL1) = pfn_check_core decls S C E G1 K T1 T2 N1 A T1' T2' Q0 SL0
		    val (Q2, SL2) = pfn_check_core decls S C E G2 K T1 T2 N2 A T1' T2' Q1 SL1
		in
		    (Q2, SL2)
		end
	      | _ => raise VerificationError ("Proof verifier: Expected disjunction, infered formula:\n" ^ 
					      (Props.makestring_prop B))

	end

      | Proofs.pf_botE R => 
	let val (B, t1, t2, Q1, SL1) = pfr_infer_core decls S C E G K T1 T2 R Q SL
	in
	    case B of
		Props.p_bot => (Q1, SL1)
	      | _ => raise VerificationError ("Proof verifier: Expected falsehood, infered formula:\n" ^ 
					      (Props.makestring_prop B))
	end

      | Proofs.pf_topI => 
	(case A of
	     Props.p_top => (Q, SL)
	   | _ => raise VerificationError ("Proof verifier: Expected top, found formula:\n" ^ 
					   (Props.makestring_prop A))
	)

      | Proofs.pf_forallI (u, N') => 
	(case A of 
	     Props.p_forall (s, i, B) =>
	     let val u' = Terms.new_uvar i (* Create new parameter *)
		 val B' = Props.subst u' i B (* Substitute parameter in goal *)
		 val N'' = Proofs.subst_fresh_pfn u' u N' (* Substitute in proof *)
		 val S' = (u', s) :: S (* add parameter to context *)
		 val ((S1, HCL1), SL1) = pfn_check_core decls S' C E G K T1 T2 N'' B' T1' T2' Q SL
	     in
		 (((u',s)::S1, HCL1), SL1)
	     end
	     
	   | _ => raise VerificationError ("Proof verifier: Expected forall, found formula:\n" ^ 
					   (Props.makestring_prop A))
	)

      | Proofs.pf_existsI (t, N') =>
	(case A of
	     Props.p_exists (s, i, B) =>

	     let val _ = typecheck_term decls S t s  
		 val B' = Props.subst t i B 
	     (* Although Terms.subst will not check that free
  	        variables in t do not get bound in B, this
  	        substitution is perfectly safe because t must be
  	        ground (it may contain uvars, but not bvars) *)

	     in
		 pfn_check_core decls S C E G K T1 T2 N' B' T1' T2' Q SL
	     end
	   | _ => raise VerificationError ("Proof verifier: Expected exists, found formula:\n" ^ 
					   (Props.makestring_prop A))
	)
 
      | Proofs.pf_existsE (R1, (u, x, N2)) =>
	let val (B, t1, t2, Q0, SL0) = pfr_infer_core decls S C E G K T1 T2 R1 Q SL
	in
	    case B of 
		Props.p_exists (s, i, B') => 
		let val u' = Terms.new_uvar i
		    val N2' = Proofs.subst_fresh_pfn u' u N2
		    val B'' = Props.subst u' i B'
		    val S' = (u', s) :: S
		    val G' = (Proofs.hyp_true ([], Proofs.pf_hyp x, B'', t1, t2)) :: G
		in
		    pfn_check_core decls S' C E G' K T1 T2 N2' A T1' T2' Q0 SL0
		end

	      | _ =>  raise VerificationError ("Proof verifier: Expected exists, infered formula:\n" ^ 
					       (Props.makestring_prop B))
	end

      | Proofs.pf_synth2check R =>
	let val (A', t1, t2, Q1, SL1) = pfr_infer_core decls S C E G K T1 T2 R Q SL
	in
	    if (Props.eq (A', A)) 
	    then
		let val (S1, HCL1) = Q1
		    val HCL2 = (C, Constraints.c_leq (t1, T1')) :: ((C, Constraints.c_leq(T2',t2)) :: HCL1)
		in
		    ((S1, HCL2), SL1)
		end
	    else 
		raise VerificationError ("Proof verifier: Expected formula:\n" ^ 
					 (Props.makestring_prop A) ^ "\n" ^
					 "Infered formula:\n" ^ 
					 (Props.makestring_prop A'))
	end

      | Proofs.pf_is =>
	let
	    exception Success 
	in
	    case A of
		Props.p_is (e,x) => ((Exps.simplify e x (fn () => raise Success) ; 
				      raise VerificationError ("Proof verifier: Problem with p_is: Expression " ^ 
							       (Terms.makestring_term e) ^ " does not evaluate to " ^ 
							       (Terms.makestring_term x) ^ "\n")
				     ) handle Success => (Q, SL))
							 
	      | _ => raise VerificationError ("Proof verifier: Expected p_is, found formula:\n" ^ 
					      (Props.makestring_prop A))
	end

      | Proofs.pf_cinjI =>
	(case A of
	     Props.p_cinj c => 
	     let val (S1, HCL1) = Q
	     in
		 ((S1, (C,c) :: HCL1), SL)
	     end
	   | _ => raise VerificationError ("Proof verifier: Expected p_cinj, found formula:\n" ^ 
					   (Props.makestring_prop A))
	)

      | Proofs.pf_cinjE (R, N') =>
	let val (B, _, _, Q0, SL0) = pfr_infer_core decls S C E G K T1 T2 R Q SL
	in
	    case B of
		Props.p_cinj c =>
		let val C' = Constraints.add (c, C)
		in
		    pfn_check_core decls S C' E G K T1 T2 N' A T1' T2' Q0 SL0
		end
	      | _ => raise VerificationError ("Proof verifier: Expected p_cinj, infered formula:\n" ^ 
					      (Props.makestring_prop B))
		
	end

      | Proofs.pf_sinjI =>
	(case A of
	     Props.p_sinj s =>
	     (* Now check if s is already in the context E. If it is not, we must put s in SL *)
	     (case (List.exists (fn s' => States.eq(s,s')) E) of
		  true => (Q, SL)
		| false => (Q, s :: SL)
	     )
	   | _ => raise VerificationError ("Proof verifier: Expected p_sinj, found formula:\n" ^ 
					   (Props.makestring_prop A))
	)

      | Proofs.pf_sinjE (R, N') => 
	let val (B, _, _, Q0, SL0) = pfr_infer_core decls S C E G K T1 T2 R Q SL
	in
	    case B of
		Props.p_sinj s =>
		let val E' = s :: E
		in
		    pfn_check_core decls S C E' G K T1 T2 N' A T1' T2' Q0 SL0
		end
	      | _ => raise VerificationError ("Proof verifier: Expected p_sinj, infered formula:\n" ^ 
					      (Props.makestring_prop B))
		
	end


      | Proofs.pf_saysI (N') =>
	(case A of
	     Props.p_says (k, A') =>
	     (* Restrict the context G. A restriction just removes all
	        assumptions of the form hyp_true *)
	     let val G' = List.filter (fn Proofs.hyp_true _ => false
					| Proofs.hyp_claims _ => true) G
	     in
		 pfn_check_core decls S C E G' k T1' T2' N' A' T1' T2' Q SL
	     end
	   | _ => raise VerificationError ("Proof verifier: Expected p_sinj, found formula:\n" ^ 
					   (Props.makestring_prop A))
	)

      | Proofs.pf_saysE (R, (x, N')) =>
	let val (B, t1, t2, Q0, SL0) = pfr_infer_core decls S C E G K T1 T2 R Q SL
	in
	    case B of
		Props.p_says (k, B') => 
		let val G' = (Proofs.hyp_claims ([], Proofs.pf_hyp x, k, B', t1, t2)) :: G
		in
		    pfn_check_core decls S C E G' K T1 T2 N' A T1' T2' Q0 SL0
		end
	      | _ => raise VerificationError ("Proof verifier: Expected p_says, infered formula:\n" ^ 
					      (Props.makestring_prop B))
	end
	
      | Proofs.pf_atI (N') =>
	(case A of
	     Props.p_at (A', t1, t2) => pfn_check_core decls S C E G K T1 T2 N' A' t1 t2 Q SL
	   | _ => raise VerificationError ("Proof verifier: Expected p_at, found formula:\n" ^ 
					   (Props.makestring_prop A))
	)

      | Proofs.pf_nhole _ => raise VerificationError ("Proof verifier: Encountered a proof with a hole!" ^ 
						      " This should not happen\n")
	

and pfr_infer_core (decls: Declarations.decls) (S: sigma) (C: Constraints.constraint_ctx) (E: States.state_ctx)
		   (G: Proofs.ctx) (K: Terms.term) (T1: Terms.term) (T2: Terms.term) 
		   (R: Proofs.pfr) 
		   (Q: Procaps.qhcl) (SL: States.state list)
    : (Props.prop * Terms.term * Terms.term * Procaps.qhcl * States.state list) = 

    case R of
	Proofs.pf_hyp x => 
	let val (S1, HCL1) = Q
	    val r = lookup C G K T1 T2 x
	in
	    case r of 
		NONE => raise VerificationError ("Proof verifier: Unknown proof variable: " ^ 
						 (Proofs.makestring_pfvar x) ^ "\n")
	      | SOME (A, T1', T2', HCL) => (A, T1', T2', (S1, HCL @ HCL1), SL)
	end

      | Proofs.pf_impE (R', N', T1', T2') =>
	let val (A, T1'', T2'', Q0, SL0) = pfr_infer_core decls S C E G K T1 T2 R' Q SL
	in
	    case A of
		Props.p_imp (A1, A2) =>
		let val ((S1, HCL1), SL1) = pfn_check_core decls S C E G K T1 T2 N' A1 T1' T2' Q0 SL0
		    val HCL2 = (C, Constraints.c_leq (T1'', T1')) :: (C, Constraints.c_leq (T2', T2'')) :: HCL1
		in
		    (A2, T1', T2', (S1, HCL2), SL1)
		end
		
	      | _ => raise VerificationError ("Proof verifier: Expected p_imp, infered formula:\n" ^ 
					      (Props.makestring_prop A))
	end

      | Proofs.pf_conjE1 R' =>
	let val (A, T1', T2', Q0, SL0) = pfr_infer_core decls S C E G K T1 T2 R' Q SL
	in
	    case A of
		Props.p_conj (A1, A2) => (A1, T1', T2', Q0, SL0)
	      | _ => raise VerificationError ("Proof verifier: Expected p_conj, infered formula:\n" ^ 
					      (Props.makestring_prop A))
	end

      | Proofs.pf_conjE2 R' =>
	let val (A, T1', T2', Q0, SL0) = pfr_infer_core decls S C E G K T1 T2 R' Q SL
	in
	    case A of
		Props.p_conj (A1, A2) => (A2, T1', T2', Q0, SL0)
	      | _ => raise VerificationError ("Proof verifier: Expected p_conj, infered formula:\n" ^ 
					      (Props.makestring_prop A))
	end

      | Proofs.pf_forallE (R', t) =>
	let val (A, T1', T2', Q0, SL0) = pfr_infer_core decls S C E G K T1 T2 R' Q SL
	in
	    case A of
		Props.p_forall (s, i, A') => 
		let val _ = typecheck_term decls S t s
		    val At = Props.subst t i A'
		in
		    (At, T1', T2', Q0, SL0)
		end
	      | _ => raise VerificationError ("Proof verifier: Expected p_forall, infered formula:\n" ^ 
					      (Props.makestring_prop A))
	end

      | Proofs.pf_atE R' =>
	let val (A, _, _, Q0, SL0) = pfr_infer_core decls S C E G K T1 T2 R' Q SL
	in
	    case A of
		Props.p_at (A', T1', T2') => (A', T1', T2', Q0, SL0)
	      | _ => raise VerificationError ("Proof verifier: Expected p_at, infered formula:\n" ^ 
					      (Props.makestring_prop A))
	end

      | Proofs.pf_rhole _ => raise VerificationError ("Proof verifier: Encountered a proof with a hole!" ^ 
						      " This should not happen\n")




(* Check a proof with pfn_check_core and eliminate redundant parts of
   the returned qhcl via Procaps.compress_qhcl *)

fun pfn_check_core_compress (decls: Declarations.decls) (S: sigma) (C: Constraints.constraint_ctx) (E: States.state_ctx)
			    (G: Proofs.ctx) (K: Terms.term) (T1: Terms.term) (T2: Terms.term) 
			    (N: Proofs.pfn) 
			    (A: Props.prop) (T1': Terms.term) (T2': Terms.term) 
			    (Q: Procaps.qhcl) (SL: States.state list)
    : (Procaps.qhcl * States.state list) = 
    let val (Q1, SL1) = pfn_check_core decls S C E G K T1 T2 N A T1' T2' Q SL
    in
	(ProcapsProcess.compress_qhcl Q1, SL1)
    end



(* This is the top level proof-term verification code. The way it
   works is this: 

 * Type check the goal and the hypotheses.

 * Call pfn_check_core_compress to get a qhcl and a state list (Q,
   SL), in the context (loca, ninfty, pinfty) on the sequent and the
   time interval [ninfty, pinfty] for the goal.

 * Check that SL does not contain any fresh parameters that were
   introduced locally in the proof. This is done by checking that SL
   is type correct in S only. There is no way that any state predicate
   containing a universally quantified variable can hold.

 * Calculate the Sf subset of S that occurs free in (Q, SL).

 * Return the tuple (Sf, Q, SL)
 *)

fun pfn_check (decls: Declarations.decls) (S: sigma) (C: Constraints.constraint_ctx) (E: States.state_ctx)
	      (G: Proofs.ctx) (N: Proofs.pfn) (A: Props.prop) 
    : (sigma * Procaps.qhcl * States.state list) = 
    
    (let
	(* Type check the decls *)
	 val _ = Tc.tc_decls [] decls 
	 (* Type check sigma *)
	 val _ = Tc.tc_sigma decls S
         (* Type check the goal *)
         val _ = Tc.tc_prop decls S [] A
	 (* Type check the hypotheses *)
	 val _ = Tc.tc_hyp_list decls S [] G
	 (* Type check the constraint ctx *)
	 val _ = Tc.tc_constraint_ctx decls S [] C
	(* Type check the states ctx *)
	 val _ = Tc.tc_state_list decls S [] E
		
	 (* Check the proof *)
	 val ((S', HCL), SL) = pfn_check_core_compress decls S C E G (Terms.Loca) (Terms.Ninfty) (Terms.Pinfty) 
						       N A (Terms.Ninfty) (Terms.Pinfty) ([], []) []
			       
	(* Now type-check SL in S to make sure that SL has no parameters created in the proof. *)
	 val _ = (Tc.tc_state_list decls S [] SL) 
	     handle Tc.TypeCheckError s 
		    => raise VerificationError (s ^ "\n" ^ 
						"Proof verifier: a required state predicate has " ^ 
						"a universally quantified argument.\n")
			     
	 (* Calculate the subset of S that occurs free in Q or SL. *)
			     
        (* Sq' = subset of S that occurs in HCL *)
	 val Sq' = List.filter (fn (u,so) => Constraints.occurs_uvar_hypconstraint_list (u, HCL)) S
	(* Sq = Sq' - S', since S' binds variables in HCL *)
	 val Sq = List.filter (fn (u,so) => not (List.exists (fn (u',so') => Terms.eq(u',u)) S')) Sq'
	 (* Ss = subset of S that occurs in SL *)
	 val Ss = List.filter (fn (u,so) => States.occurs_uvar_state_list (u, SL)) S
	 (* Sf = Sq U Ss*)
	 val Sf = Ss @ (List.filter (fn (u, so) => not (List.exists (fn (u',so') => (Terms.eq(u',u))) Ss)) Sq)
		
	 (* All constraints in C must be in the conditions apriori since they may have been used *)
	 val HCL_init = List.map (fn c => (Constraints.empty_ctx, c)) (Constraints.ctx_to_list C)
	 val HCL' = HCL_init @ HCL
		    
	 (* All state conditions in E must also be in the conditions *)
	 val SL' = E @ SL
     in
	 (Sf, (S', HCL'), SL')
     end
    ) handle Tc.TypeCheckError s => raise VerificationError s

end (* structure Verifier *)
