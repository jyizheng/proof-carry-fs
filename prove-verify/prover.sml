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

signature PROVER = sig

val goal: Terms.term list 
	   -> Constraints.constraint_ctx
	   -> States.state_ctx
	   -> Proofs.ctx (* The first four arguments are the contexts S, C, E, G *)
	   -> Terms.term (* K on the sequent *)
	   -> Terms.term (* T1 on the sequent *)
	   -> Terms.term (* T2 on the sequent *)
	   -> Props.prop (* A: Goal proposition *)
	   -> Terms.term (* T1' on the proposition *)
	   -> Terms.term (* T2' on the proposition *)
	   -> (Proofs.pfn -> unit) (* Success continuation, s *) 
	   -> unit
end (* signature PROVER *)


structure Prover = 
struct 

infix >>= ;

fun op >>= (x: 'a option, f: 'a -> 'b option): 'b option = Option.mapPartial f x
						     

datatype subgoal_list = subgoal_nil
		      | subgoal_cons_constraint of Constraints.constraint * subgoal_list
		      (* | subgoal_cons_state of States.state * subgoal_list *)
		      | subgoal_cons_prop of Props.prop * Terms.term * Terms.term * Proofs.pfn option ref * subgoal_list
                                             (* A, T1, T2, hole for proof *)

exception BadClause
exception UngroundContext

(* Restriction operator. Note that restriction is a deterministic
process that never fails, so there is no need to pass it a
continuation *)

fun restrict [] S C K T1 T2 = []
  | restrict ((Proofs.hyp_true _) :: G) S C K T1 T2 = restrict G S C K T1 T2
  | restrict ((h as Proofs.hyp_claims (_, pfr, K', A, T1', T2')) :: G) S C K T1 T2 =
    h :: (restrict G S C K T1 T2)
(* Old code: checks for time interval restriction *)
(*
 if (Constraints.check_hc (C, Constraints.c_leq (T1', T1)) andalso
     Constraints.check_hc (C, Constraints.c_leq (T2, T2')))
 (* For DTL, add the additional condition Constraints.check_hc (C, Constraints.c_stronger (K', K)) *)
 then h :: (restrict G S C K T1 T2)
    else restrict G S C K T1 T2
 *)

(* goal directed proof search *)
fun goal S C E G K T1 T2 (Props.p_atomic (P, TL)) T1' T2' sc =
    pickone (P, TL) G
	    (fn h => residuate S C K T1 T2 h (P, TL) T1' T2' 
			       (fn (GL, pfr') => goal_list S C E G K T1 T2 GL
							   (fn () => sc (Proofs.pf_synth2check pfr'))))
    
  | goal S C E G K T1 T2 (Props.p_conj (A, B)) T1' T2' sc = 
    goal S C E G K T1 T2 A T1' T2'
	  (fn pfA => goal S C E G K T1 T2 B T1' T2' 
			   (fn pfB => sc (Proofs.pf_conjI (pfA, pfB))))
    
  | goal S C E G K T1 T2 (Props.p_disj (A, B)) T1' T2' sc = 
    (goal S C E G K T1 T2 A T1' T2' (fn pfA => sc (Proofs.pf_disjI1 pfA)) ; 
     goal S C E G K T1 T2 B T1' T2' (fn pfB => sc (Proofs.pf_disjI2 pfB)))
    
  | goal S C E G K T1 T2 (Props.p_imp (A, B)) T1' T2' sc = 
    let val hole = ref (NONE: Proofs.pfr option)
	val t1 = Terms.new_uvar (Terms.Varbind "t")
	val t2 = Terms.new_uvar (Terms.Varbind "t")
	val S' = t2 :: t1 :: S
	val C' = Constraints.add(Constraints.c_leq (T1', t1), Constraints.add (Constraints.c_leq (t2, T2'),C))
    in 
	asyncl S' C' E G [(Proofs.hyp_true ([], Proofs.pf_rhole hole, A, t1, t2))] 
	       K T1 T2 B t1 t2 (fn pfB => let val x = Proofs.new_pfvar ("pfv") 
					  in 
					      hole:= SOME (Proofs.pf_hyp x); 
					      sc (Proofs.pf_impI(t1,t2,x,pfB))
					  end)
    end 

  | goal S C E G K T1 T2 (Props.p_forall (_,x as Terms.Varbind(b), A)) T1' T2' sc = 
    let val u = Terms.new_uvar x
    in 
	goal (u :: S) C E G K T1 T2 (Props.subst u x A) T1' T2'
	     (fn pfA => sc (Proofs.pf_forallI (u, pfA)))
    end

  | goal S C E G K T1 T2 (Props.p_exists (_,x as Terms.Varbind(b), A)) T1' T2' sc =
    let val e = Terms.new_evar x S
    in 
	goal S C E G K T1 T2 (Props.subst e x A) T1' T2' 
	     (fn pfA => sc (Proofs.pf_existsI (e , pfA)))
    end 

  | goal S C E G K T1 T2 Props.p_top T1' T2' sc = sc Proofs.pf_topI 

  | goal S C E G K T1 T2 Props.p_bot T1' T2' sc = () (* backtrack *)

  | goal S C E G K T1 T2 (Props.p_is (e, x)) T1' T2' sc = Exps.simplify e x (fn () => sc (Proofs.pf_is))

  | goal S C E G K T1 T2 (Props.p_cinj c) T1' T2' sc =
    ConstraintsCheck.check_hc (C, c) (fn () => sc (Proofs.pf_cinjI))
  
  | goal S C E G K T1 T2 (Props.p_sinj s) T1' T2' sc =
    StatesCheck.check_hs (E, s) (fn () => sc (Proofs.pf_sinjI))

  | goal S C E G K T1 T2 (Props.p_says (K', A)) T1' T2' sc =
    let val K' = (Option.valOf (Terms.ground K')) handle Option.Option => raise UngroundContext
	val T1' = (Option.valOf (Terms.ground T1')) handle Option.Option => raise UngroundContext
	val T2' = (Option.valOf (Terms.ground T2')) handle Option.Option => raise UngroundContext
	val G' = restrict G S C K' T1' T2'
	in 
	    goal S C E G' K' T1' T2' A T1' T2' 
		 (fn pfn => sc (Proofs.pf_saysI pfn))
	end

  | goal S C E G K T1 T2 (Props.p_at (A, T1'', T2'')) T1' T2' sc =
    let 
	val T1'' = (Option.valOf (Terms.ground T1'')) handle Option.Option => raise UngroundContext
	val T2'' = (Option.valOf (Terms.ground T2'')) handle Option.Option => raise UngroundContext
    in
	goal S C E G K T1 T2 A T1'' T2'' (fn pfn => sc (Proofs.pf_atI pfn))
    end

(* Solve a list of subgoals *)
and goal_list S C E G K T1 T2 subgoal_nil sc = sc ()

  | goal_list S C E G K T1 T2 (subgoal_cons_prop (A, T1', T2', pf_nhole,GL)) sc = 
    goal S C E G K T1 T2 A T1' T2' (fn pfA => (pf_nhole := SOME pfA;
					       goal_list S C E G K T1 T2 GL sc))

  | goal_list S C E G K T1 T2 (subgoal_cons_constraint (c, GL)) sc = 
    ConstraintsCheck.check_hc (C, c) (fn () => goal_list S C E G K T1 T2 GL sc)

(* Pick a random hypothesis from the context. This function will set 
backtracking points *)

and pickone (P, TL) [] sc = ()
  | pickone (P, TL) (h :: G) sc = (sc h ; pickone (P, TL) G sc)





(* Residuate a hypothesis, if it is usable, else backtrack *)

and residuate S C K T1 T2 (Proofs.hyp_true (_, pfr, D, TD1, TD2)) (P, TL) T1' T2' sc =
    residuate_clause S pfr D TD1 TD2 (P, TL) T1' T2' (fn (GL, pfr', F) => sc (GL, pfr'))

  | residuate S C K T1 T2 (Proofs.hyp_claims (_, pfr, K'', D, TD1, TD2)) (P, TL) T1' T2' sc =
    ConstraintsCheck.check_hc 
	(C, Constraints.c_leq (TD1, T1)) 
    (fn () =>
	ConstraintsCheck.check_hc 
	    (C, Constraints.c_leq (T2, TD2)) 
	(fn () =>
	    ConstraintsCheck.check_hc (C, Constraints.c_stronger (K'', K))
	    (fn() => residuate_clause S pfr D TD1 TD2 (P,TL) T1' T2' (fn (GL, pfr', F) => sc (GL, pfr')))))



(* Residuate a formula (clause) with respect to a goal. Fail if the
head of the formula does not unify with the goal. Note that
conjunctions in clauses can create choice points and backtracking into
this function. This function passes three arguments to the success
continuation: a subgoal list, a proof term, and a flag to indicate
whether there was an 'at' on the residuation path or not *)

and residuate_clause S pfr (Props.p_atomic(P', TL')) TD1 TD2 (P, TL) T1' T2' sc =
    if (P <> P') then ()
    else Terms.unify_list TL TL' 
	 (fn () => let val c1 = Constraints.c_leq (TD1, T1')
		       val c2 = Constraints.c_leq (T2', TD2)
		       val GL = subgoal_cons_constraint(c1, subgoal_cons_constraint (c2, subgoal_nil))
		   in sc (GL, pfr, false) end)

  | residuate_clause S pfr (Props.p_conj(D1, D2)) TD1 TD2 (P, TL) T1' T2' sc =
    (let val h = ref (NONE: Proofs.pfr option) 
     in
	 residuate_clause S (Proofs.pf_rhole h) D1 TD1 TD2 (P, TL) T1' T2' 
			  (fn (GL, pfr', F) => (h := SOME (Proofs.pf_conjE1 pfr) ; sc(GL, pfr', F)))
     end;
     let val h = ref (NONE: Proofs.pfr option)
     in 
	 residuate_clause S (Proofs.pf_rhole h) D2 TD1 TD2 (P, TL) T1' T2' 
			  (fn (GL, pfr', F) => (h := SOME (Proofs.pf_conjE2 pfr) ; sc(GL, pfr', F)))
     end)

  | residuate_clause S pfr (Props.p_disj(D1, D2)) TD1 TD2 (P, TL) T1' T2' sc =
    raise BadClause

  | residuate_clause S pfr (Props.p_imp(G1, D2)) TD1 TD2 (P, TL) T1' T2' sc =
    let val rhole = ref (NONE: Proofs.pfr option)
    in 
	residuate_clause S (Proofs.pf_rhole rhole) D2 TD1 TD2 (P, TL) T1' T2' 
	(fn (GL, pfr', F) => 
	    let val nhole = ref (NONE: Proofs.pfn option)
	    in 
		if (F) (* There was a further 'at' on the residuation path *)
		then (rhole := SOME (Proofs.pf_impE (pfr, Proofs.pf_nhole nhole, Terms.Pinfty, Terms.Ninfty)) ; 
		      sc (subgoal_cons_prop (G1, Terms.Pinfty,Terms.Ninfty,nhole,GL), pfr', F))
		else (rhole := SOME (Proofs.pf_impE (pfr, Proofs.pf_nhole nhole, T1', T2')) ; 
		      sc (subgoal_cons_prop (G1, T1', T2', nhole, GL), pfr', F))
	    end
	)
    end

  | residuate_clause S pfr (Props.p_forall(_, x, D)) TD1 TD2 (P, TL) T1' T2' sc =
    let val e = Terms.new_evar x S
	val rhole = ref (NONE: Proofs.pfr option)
    in 
	residuate_clause S (Proofs.pf_rhole rhole) (Props.subst e x D) TD1 TD2 (P, TL) T1' T2'
	(fn (GL, pfr', F) => (rhole := SOME (Proofs.pf_forallE (pfr, e)) ; sc (GL, pfr', F)))
    end 

  | residuate_clause S pfr (Props.p_exists _) TD1 TD2 (P, TL) T1' T2' sc = raise BadClause

  | residuate_clause S pfr Props.p_top TD1 TD2 (P, TL) T1' T2' sc = ()

  | residuate_clause S pfr Props.p_bot TD1 TD2 (P, TL) T1' T2' sc = raise BadClause

  | residuate_clause S pfr (Props.p_is _) TD1 TD2 (P, TL) T1' T2' sc = raise BadClause

  | residuate_clause S pfr (Props.p_cinj _) TD1 TD2 (P, TL) T1' T2' sc = raise BadClause

  | residuate_clause S pfr (Props.p_sinj _) TD1 TD2 (P, TL) T1' T2' sc = raise BadClause

  | residuate_clause S pfr (Props.p_says _) TD1 TD2 (P, TL) T1' T2' sc = raise BadClause

  | residuate_clause S pfr (Props.p_at (D, TD1',TD2')) TD1 TD2 (P, TL) T1' T2' sc =
    let val rhole = ref (NONE: Proofs.pfr option)
    in
	(* It is okay for TD1' and TD2' to be not ground here, since some outer subgoals may unify them *)
	residuate_clause S (Proofs.pf_rhole rhole) D TD1' TD2' (P, TL) T1' T2' 
			 (fn (GL, pfr', _) => (rhole := SOME (Proofs.pf_atE pfr) ; 
					       sc (GL, pfr', true)))
    end 



(* Asynchronously decompose the list assumption list SL *)

and asyncl S C E G (SL as []) K T1 T2 A T1' T2' sc = 
    goal S C E G K T1 T2 A T1' T2' sc

  | asyncl S C E G ((h as Proofs.hyp_true (_,pfr,Props.p_atomic _,TD1,TD2)) :: SL) K T1 T2 A T1' T2' sc = 
    asyncl S C E (h :: G) SL K T1 T2 A T1' T2' sc

  | asyncl S C E G ((h as Proofs.hyp_true (_,pfr,Props.p_conj _,TD1,TD2)) :: SL) K T1 T2 A T1' T2' sc = 
    asyncl S C E (h :: G) SL K T1 T2 A T1' T2' sc

  | asyncl S C E G ((Proofs.hyp_true (_,pfr,Props.p_disj(S1, S2),TD1,TD2)) :: SL) K T1 T2 A T1' T2' sc = 
    let val x = Proofs.new_pfvar ("pfv")
	val y = Proofs.new_pfvar ("pfv")
	val SL1 = (Proofs.hyp_true ([], Proofs.pf_hyp x, S1, TD1, TD2)) :: SL
	val SL2 = (Proofs.hyp_true ([], Proofs.pf_hyp y, S2, TD1, TD2)) :: SL
    in 
	asyncl S C E G SL1 K T1 T2 A T1' T2'
	(fn pf1 => asyncl S C E G SL2 K T1 T2 A T1' T2' 
		   (fn pf2 => sc (Proofs.pf_disjE (pfr , (x, pf1), (y, pf2)))))
    end

  | asyncl S C E G ((h as Proofs.hyp_true (_,pfr,Props.p_imp _,TD1,TD2)) :: SL) K T1 T2 A T1' T2' sc = 
    asyncl S C E (h :: G) SL K T1 T2 A T1' T2' sc

  | asyncl S C E G ((h as Proofs.hyp_true (_,pfr,Props.p_forall _,TD1,TD2)) :: SL) K T1 T2 A T1' T2' sc = 
    asyncl S C E (h :: G) SL K T1 T2 A T1' T2' sc

  | asyncl S C E G ((Proofs.hyp_true (_,pfr,Props.p_exists(_, x, B),TD1,TD2)) :: SL) K T1 T2 A T1' T2' sc =
    let val u = Terms.new_uvar x
	val pfv = Proofs.new_pfvar "pfv"
	val h' = Proofs.hyp_true ([], Proofs.pf_hyp pfv, Props.subst u x B, TD1, TD2)
    in 
	asyncl (u :: S) C E G (h' :: SL) K T1 T2 A T1' T2'
	     (fn pf => sc (Proofs.pf_existsE(pfr, (u,pfv, pf))))
    end

  | asyncl S C E G ((Proofs.hyp_true (_,pfr,Props.p_top,_,_)) :: SL) K T1 T2 A T1' T2' sc = 
    asyncl S C E G SL K T1 T2 A T1' T2' sc

  | asyncl S C E G ((Proofs.hyp_true (_,pfr,Props.p_bot,_,_)) :: SL) K T1 T2 A T1' T2' sc = sc (Proofs.pf_botE pfr)

  | asyncl S C E G ((Proofs.hyp_true (_,pfr,Props.p_is _,_,_)) :: SL) K T1 T2 A T1' T2' sc = 
    raise BadClause

  | asyncl S C E G ((Proofs.hyp_true (_,pfr,Props.p_cinj c,_,_)) :: SL) K T1 T2 A T1' T2' sc =
    asyncl S (Constraints.add(c,C)) E G SL K T1 T2 A T1' T2' (fn pfn => sc (Proofs.pf_cinjE (pfr, pfn)))

  | asyncl S C E G ((Proofs.hyp_true (_,pfr,Props.p_sinj s,_,_)) :: SL) K T1 T2 A T1' T2' sc =
    asyncl S C (States.add(s,E)) G SL K T1 T2 A T1' T2' (fn pfn => sc (Proofs.pf_sinjE (pfr, pfn)))

  | asyncl S C E G ((Proofs.hyp_true (_,pfr,Props.p_says(KD, B),TD1,TD2)) :: SL) K T1 T2 A T1' T2' sc =
    let val pfv = Proofs.new_pfvar ("pfv")
	val h' = Proofs.hyp_claims ([], Proofs.pf_hyp pfv, KD, B, TD1, TD2)
    in 
	asyncl S C E (h' :: G) SL K T1 T2 A T1' T2' 
	(fn pfn => sc (Proofs.pf_saysE (pfr, (pfv, pfn))))
    end 

  | asyncl S C E G ((Proofs.hyp_true (_,pfr,Props.p_at (B, TD1', TD2'),TD1,TD2)) :: SL) K T1 T2 A T1' T2' sc =
    let (* TD1' and TD2' must be ground *)
	val TD1' = (Option.valOf (Terms.ground TD1')) handle Option.Option => raise UngroundContext
	val TD2' = (Option.valOf (Terms.ground TD2')) handle Option.Option => raise UngroundContext

	val rhole = ref (NONE: Proofs.pfr option)
	val h' = Proofs.hyp_true ([], Proofs.pf_rhole rhole, B, TD1', TD2')
    in 
	asyncl S C E G (h' :: SL) K T1 T2 A T1' T2' 
	       (fn pfn' => (rhole := SOME (Proofs.pf_atE pfr); sc pfn'))
    end 


  | asyncl S C E G ((h as (Proofs.hyp_claims _)) :: SL) K T1 T2 A T1' T2' sc = raise BadClause


(************************************************************************)

(* Split a list of hypothesis into the claims and true hypothesis *)

fun split_ctx (L: Proofs.ctx) =
    let fun split_ctx_iter [] claims trues = (claims, trues)
	  | split_ctx_iter ((h as (Proofs.hyp_claims _)) :: L) claims trues =
	    split_ctx_iter L (h :: claims) trues
	  | split_ctx_iter ((h as (Proofs.hyp_true _)) :: L) claims trues =
	    split_ctx_iter L claims (h :: trues)
    in
	(fn (L1, L2) => (List.rev L1, List.rev L2)) (split_ctx_iter L [] [])
    end

(**************************************************************************)

exception Solution of Proofs.pfn

fun solve_top (S: Terms.term list) (C: Constraints.constraint_ctx) 
	      (E: States.state_ctx) (G: Proofs.ctx) (A: Props.prop)  = 
    (
     let val (claims, trues) = split_ctx G 
     in
	 asyncl S C E claims trues (Terms.Loca) (Terms.Ninfty) (Terms.Pinfty)
	      A (Terms.Ninfty) (Terms.Pinfty) (fn pfn => raise Solution(pfn)); 
	 NONE
     end 
    ) 
    handle Solution(pfn) => SOME pfn
	 | Constraints.UngroundConstraint s => (print (s ^ "\n"); raise (Constraints.UngroundConstraint s))

end (* structure Prover *)

