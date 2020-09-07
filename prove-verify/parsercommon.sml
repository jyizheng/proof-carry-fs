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

structure ParserCommon = 
struct

datatype parserResult = 
	 Prop of Props.prop
       | Proofterm of Proofs.pfn
       | Program of Proofs.ctx
       | Term of Terms.term
       | Constraint of Constraints.constraint
       | Declarations of Declarations.decls
       | Qprocap of Procaps.qprocap_parsed
       | Substitution of Procaps.substitution

(* When a prop is first parsed, all bound variables x are parsed as
function symbols x(). This needs to be rectified by replacing such x()
with Bvar(x). The following function makes this transformation. It is
called by the parser just before it returns. Note that this function
will not change occurences of x that are function symbols with
arguments.

The argument L is the list of bound variables known so far
*)

local 

fun transform_prop_bvs_h (L: Terms.varbind list) (Props.p_atomic (p, tl)) 
  = Props.p_atomic (p, List.map (transform_term_bvs_h L) tl)
  | transform_prop_bvs_h L (Props.p_conj(A, B)) = Props.p_conj (transform_prop_bvs_h L A, transform_prop_bvs_h L B)
  | transform_prop_bvs_h L (Props.p_disj(A, B)) = Props.p_disj (transform_prop_bvs_h L A, transform_prop_bvs_h L B)
  | transform_prop_bvs_h L (Props.p_imp(A, B)) = Props.p_imp (transform_prop_bvs_h L A, transform_prop_bvs_h L B)
  | transform_prop_bvs_h L (Props.p_forall(s,x, A)) = Props.p_forall (s,x, transform_prop_bvs_h (x::L) A)
  | transform_prop_bvs_h L (Props.p_exists(s,x, A)) = Props.p_exists (s,x, transform_prop_bvs_h (x::L) A)
  | transform_prop_bvs_h L (Props.p_top) = Props.p_top
  | transform_prop_bvs_h L (Props.p_bot) = Props.p_bot
  | transform_prop_bvs_h L (Props.p_is (t1,t2)) = Props.p_is (transform_term_bvs_h L t1, transform_term_bvs_h L t2)
  | transform_prop_bvs_h L (Props.p_cinj c) = Props.p_cinj (transform_constraint_bvs_h L c)
  | transform_prop_bvs_h L (Props.p_sinj s) = Props.p_sinj (transform_state_bvs_h L s)
  | transform_prop_bvs_h L (Props.p_says(k, A)) = Props.p_says (transform_term_bvs_h L k, transform_prop_bvs_h L A)
  | transform_prop_bvs_h L (Props.p_at(A, t1, t2)) = Props.p_at (transform_prop_bvs_h L A, transform_term_bvs_h L t1,
								 transform_term_bvs_h L t2)

and transform_constraint_bvs_h L (Constraints.c_leq (t1,t2)) 
  = Constraints.c_leq (transform_term_bvs_h L t1, transform_term_bvs_h L t2)
  | transform_constraint_bvs_h L (Constraints.c_stronger (t1,t2)) 
    = Constraints.c_stronger (transform_term_bvs_h L t1, transform_term_bvs_h L t2)
  | transform_constraint_bvs_h L (Constraints.c_other (s, tl)) 
    = Constraints.c_other(s, List.map (transform_term_bvs_h L) tl)

and transform_state_bvs_h L (States.s_owner (t1, t2)) 
    = States.s_owner (transform_term_bvs_h L t1, transform_term_bvs_h L t2)
  | transform_state_bvs_h L (States.s_has_xattr (t1, t2, t3)) 
    = States.s_has_xattr (transform_term_bvs_h L t1, transform_term_bvs_h L t2, transform_term_bvs_h L t3)
  | transform_state_bvs_h L (States.s_other (s, tl))
    = States.s_other (s,  List.map (transform_term_bvs_h L) tl)

and transform_term_bvs_h L (t as Terms.Bvar _) = t
  | transform_term_bvs_h L (t as Terms.Evar _) = t
  | transform_term_bvs_h L (t as Terms.Uvar _) = t
  | transform_term_bvs_h L (t as Terms.Appl(f, [])) = if (List.exists (fn Terms.Varbind b => (b=f)) L) 
						      then Terms.Bvar f else t
  | transform_term_bvs_h L (t as Terms.Appl(f, tl)) = Terms.Appl (f, map (transform_term_bvs_h L) tl)
  | transform_term_bvs_h L (t as Terms.Prim_str _) = t
  | transform_term_bvs_h L (t as Terms.Prim_date _) = t
  | transform_term_bvs_h L (t as Terms.Prim_int _) = t
  | transform_term_bvs_h L (t as Terms.Loca) = t
  | transform_term_bvs_h L (t as Terms.Ctime) = t
  | transform_term_bvs_h L (t as Terms.Ninfty) = t
  | transform_term_bvs_h L (t as Terms.Pinfty) = t
  | transform_term_bvs_h L (t as Terms.Read) = t
  | transform_term_bvs_h L (t as Terms.Write) = t
  | transform_term_bvs_h L (t as Terms.Execute) = t
  | transform_term_bvs_h L (t as Terms.Identity) = t
  | transform_term_bvs_h L (t as Terms.Govern) = t
  | transform_term_bvs_h L (t as Terms.Prim_str2file t') = Terms.Prim_str2file (transform_term_bvs_h L t')
  | transform_term_bvs_h L (t as Terms.Prim_date2time t') = Terms.Prim_date2time (transform_term_bvs_h L t')
  | transform_term_bvs_h L (t as Terms.Prim_int2time t') = Terms.Prim_int2time (transform_term_bvs_h L t')
  | transform_term_bvs_h L (t as Terms.Prim_int2principal t') = Terms.Prim_int2principal (transform_term_bvs_h L t')
  | transform_term_bvs_h L (t as Terms.Time2exp t') =  Terms.Time2exp(transform_term_bvs_h L t')
  | transform_term_bvs_h L (t as Terms.Exp_add (t1,t2)) = Terms.Exp_add (transform_term_bvs_h L t1, 
									 transform_term_bvs_h L t2)
  | transform_term_bvs_h L (t as Terms.Exp_subtract (t1,t2)) = Terms.Exp_subtract (transform_term_bvs_h L t1, 
									 transform_term_bvs_h L t2)
  | transform_term_bvs_h L (t as Terms.Exp_max (t1,t2)) = Terms.Exp_max (transform_term_bvs_h L t1, 
									 transform_term_bvs_h L t2)
  | transform_term_bvs_h L (t as Terms.Exp_min (t1,t2)) = Terms.Exp_min (transform_term_bvs_h L t1, 
									 transform_term_bvs_h L t2)


in

fun transform_prop_bvs p = transform_prop_bvs_h [] p

end

(* When a proofterm is parsed, all bound term variables are parsed as
Terms.Bvar at binding sites and Terms.Appl at use sites. Similarly,
all bound proof variables are parsed as Proofs.Pfvar_external. These
need to be changed to Terms.Uvar and Proofs.Pfvar_internal
respectively. This is what the following functions do: they traverse a
proof recursively, making the changes.

Similar functions are needed for parsing qhcl's. In this
case, the variables bound in the qhcl are parsed as Terms.Bvar at the
binding sites and Terms.Appl at use sites. Both have to be replaced by
(fresh) Terms.uvars.

The argument tvs is a partial map from Bvars to Uvars (those Bvars
that are bound in the outer context)

Similarly, the argument pvs is a partial map from Pfvar_external to
Pfvar_internal.

*)

local

    exception Impossible_in_proof
    exception Impossible_in_qhcl

    fun transform_term_bvs_h tvs (t as Terms.Bvar s) = raise Impossible_in_proof
      | transform_term_bvs_h tvs (t as Terms.Evar _) = t
      | transform_term_bvs_h tvs (t as Terms.Uvar _) = t
      | transform_term_bvs_h tvs (t as Terms.Appl(f, [])) = 
	(case (List.find (fn (s, _) => s=f) tvs) of
	     SOME (_, u) => u
	   | NONE => Terms.Appl (f, [])
	)
      | transform_term_bvs_h tvs (t as Terms.Appl(f, tl)) = Terms.Appl (f, map (transform_term_bvs_h tvs) tl)
      | transform_term_bvs_h tvs (t as Terms.Prim_str _) = t
      | transform_term_bvs_h tvs (t as Terms.Prim_date _) = t
      | transform_term_bvs_h tvs (t as Terms.Prim_int _) = t
      | transform_term_bvs_h tvs (t as Terms.Loca) = t
      | transform_term_bvs_h tvs (t as Terms.Ctime) = t
      | transform_term_bvs_h tvs (t as Terms.Ninfty) = t
      | transform_term_bvs_h tvs (t as Terms.Pinfty) = t
      | transform_term_bvs_h tvs (t as Terms.Read) = t
      | transform_term_bvs_h tvs (t as Terms.Write) = t
      | transform_term_bvs_h tvs (t as Terms.Execute) = t
      | transform_term_bvs_h tvs (t as Terms.Identity) = t
      | transform_term_bvs_h tvs (t as Terms.Govern) = t
      | transform_term_bvs_h tvs (t as Terms.Prim_str2file t') = Terms.Prim_str2file (transform_term_bvs_h tvs t')
      | transform_term_bvs_h tvs (t as Terms.Prim_date2time t') = Terms.Prim_date2time (transform_term_bvs_h tvs t')
      | transform_term_bvs_h tvs (t as Terms.Prim_int2time t') = Terms.Prim_int2time (transform_term_bvs_h tvs t')
      | transform_term_bvs_h tvs (t as Terms.Prim_int2principal t') = Terms.Prim_int2principal (transform_term_bvs_h tvs t')
      | transform_term_bvs_h tvs (t as Terms.Time2exp t') =  Terms.Time2exp(transform_term_bvs_h tvs t')
      | transform_term_bvs_h tvs (t as Terms.Exp_add (t1,t2)) = Terms.Exp_add (transform_term_bvs_h tvs t1, 
									       transform_term_bvs_h tvs t2)
      | transform_term_bvs_h tvs (t as Terms.Exp_subtract (t1,t2)) = Terms.Exp_subtract (transform_term_bvs_h tvs t1, 
											 transform_term_bvs_h tvs t2)
      | transform_term_bvs_h tvs (t as Terms.Exp_max (t1,t2)) = Terms.Exp_max (transform_term_bvs_h tvs t1, 
									       transform_term_bvs_h tvs t2)
      | transform_term_bvs_h tvs (t as Terms.Exp_min (t1,t2)) = Terms.Exp_min (transform_term_bvs_h tvs t1, 
									       transform_term_bvs_h tvs t2)


    fun transform_pfn_bvs_h tvs pvs (Proofs.pf_conjI(p1,p2)) = Proofs.pf_conjI (transform_pfn_bvs_h tvs pvs p1, transform_pfn_bvs_h tvs pvs p2)
      | transform_pfn_bvs_h tvs pvs (Proofs.pf_impI(Terms.Bvar s1, Terms.Bvar s2, Proofs.Pfvar_external s3, p)) = 
	let val u1 = Terms.new_uvar (Terms.Varbind(s1))
	    val u2 = Terms.new_uvar (Terms.Varbind(s2))
	    val v3 = Proofs.new_pfvar s3
	in 
	   Proofs.pf_impI (u1, u2, v3, transform_pfn_bvs_h ((s2,u2)::(s1,u1)::tvs) ((s3,v3)::pvs) p)
	end
      | transform_pfn_bvs_h tvs pvs (Proofs.pf_impI(_,_,_, p)) = raise Impossible_in_proof
      | transform_pfn_bvs_h tvs pvs (Proofs.pf_disjI1 p) = Proofs.pf_disjI1 (transform_pfn_bvs_h tvs pvs p)
      | transform_pfn_bvs_h tvs pvs (Proofs.pf_disjI2 p) = Proofs.pf_disjI2 (transform_pfn_bvs_h tvs pvs p)
      | transform_pfn_bvs_h tvs pvs (Proofs.pf_disjE (r, (Proofs.Pfvar_external s1, n1), (Proofs.Pfvar_external s2, n2))) =
	let val v1 = Proofs.new_pfvar s1
	    val v2 = Proofs.new_pfvar s2
	in 
	    Proofs.pf_disjE (transform_pfr_bvs_h tvs pvs r,
			     (v1, transform_pfn_bvs_h tvs ((s1,v1) :: pvs) n1),
			     (v2, transform_pfn_bvs_h tvs ((s2,v2) :: pvs) n2))
	end
      | transform_pfn_bvs_h tvs pvs (Proofs.pf_disjE (r, (_, n1), (_, n2))) = raise Impossible_in_proof
      | transform_pfn_bvs_h tvs pvs (Proofs.pf_botE r) = Proofs.pf_botE (transform_pfr_bvs_h tvs pvs r)
      | transform_pfn_bvs_h tvs pvs (Proofs.pf_topI) = Proofs.pf_topI
      | transform_pfn_bvs_h tvs pvs (Proofs.pf_forallI (Terms.Bvar s, n)) = 
	let val u = Terms.new_uvar (Terms.Varbind(s))
	in 
	    Proofs.pf_forallI (u, transform_pfn_bvs_h ((s,u) :: tvs) pvs n)
	end
      | transform_pfn_bvs_h tvs bvs (Proofs.pf_forallI (_, n)) = raise Impossible_in_proof
      | transform_pfn_bvs_h tvs pvs (Proofs.pf_existsI (t, n)) = 
	Proofs.pf_existsI (transform_term_bvs_h tvs t, transform_pfn_bvs_h tvs pvs n)
      | transform_pfn_bvs_h tvs pvs (Proofs.pf_existsE (r, (Terms.Bvar s1, Proofs.Pfvar_external s2, n))) =
	let val u1 = Terms.new_uvar (Terms.Varbind(s1))
	    val v2 = Proofs.new_pfvar s2
	in 
	    Proofs.pf_existsE (transform_pfr_bvs_h tvs pvs r,
			       (u1, v2, transform_pfn_bvs_h ((s1,u1) :: tvs) ((s2,v2) :: pvs) n))
	end
      | transform_pfn_bvs_h tvs pvs (Proofs.pf_existsE (r, (_, _, n))) = raise Impossible_in_proof
      | transform_pfn_bvs_h tvs pvs (Proofs.pf_synth2check r) = Proofs.pf_synth2check (transform_pfr_bvs_h tvs pvs r)
      | transform_pfn_bvs_h tvs pvs (Proofs.pf_is) = Proofs.pf_is
      | transform_pfn_bvs_h tvs pvs (Proofs.pf_cinjI) = Proofs.pf_cinjI
      | transform_pfn_bvs_h tvs pvs (Proofs.pf_cinjE(r,n)) = 
	Proofs.pf_cinjE (transform_pfr_bvs_h tvs pvs r,
			 transform_pfn_bvs_h tvs pvs n)
      | transform_pfn_bvs_h tvs pvs (Proofs.pf_sinjI) = Proofs.pf_sinjI
      | transform_pfn_bvs_h tvs pvs (Proofs.pf_sinjE(r,n)) = 
	Proofs.pf_sinjE (transform_pfr_bvs_h tvs pvs r,
			 transform_pfn_bvs_h tvs pvs n)
      | transform_pfn_bvs_h tvs pvs (Proofs.pf_saysI n) = Proofs.pf_saysI (transform_pfn_bvs_h tvs pvs n)
      | transform_pfn_bvs_h tvs pvs (Proofs.pf_saysE (r, (Proofs.Pfvar_external s, n))) =
	let val v = Proofs.new_pfvar s
	in 
	    Proofs.pf_saysE (transform_pfr_bvs_h tvs pvs r,
			     (v, transform_pfn_bvs_h tvs ((s,v)::pvs) n))
	end
      | transform_pfn_bvs_h tvs pvs (Proofs.pf_saysE (r, (_, n))) = raise Impossible_in_proof
      | transform_pfn_bvs_h tvs pvs (Proofs.pf_atI n) = Proofs.pf_atI (transform_pfn_bvs_h tvs pvs n)
      | transform_pfn_bvs_h tvs pvs (Proofs.pf_nhole _) = raise Impossible_in_proof
								
and transform_pfr_bvs_h tvs pvs (Proofs.pf_hyp (Proofs.Pfvar_internal _)) = raise Impossible_in_proof
  | transform_pfr_bvs_h tvs pvs (h as Proofs.pf_hyp (Proofs.Pfvar_external s)) =
    (case (List.find (fn (s', _) => s=s') pvs) of
	 SOME (_,v') => Proofs.pf_hyp v' (* Bound variable *)
       | NONE => h (* Free variable, names a policy *)
    )
  | transform_pfr_bvs_h tvs pvs (Proofs.pf_impE (r,n,t1,t2)) = 
    Proofs.pf_impE (transform_pfr_bvs_h tvs pvs r,
		     transform_pfn_bvs_h tvs pvs n,
		     transform_term_bvs_h tvs t1,
		     transform_term_bvs_h tvs t2)
  | transform_pfr_bvs_h tvs pvs (Proofs.pf_conjE1 r) = Proofs.pf_conjE1 (transform_pfr_bvs_h tvs pvs r)
  | transform_pfr_bvs_h tvs pvs (Proofs.pf_conjE2 r) = Proofs.pf_conjE2 (transform_pfr_bvs_h tvs pvs r)
  | transform_pfr_bvs_h tvs pvs (Proofs.pf_forallE (r,t)) = 
    Proofs.pf_forallE (transform_pfr_bvs_h tvs pvs r,
		       transform_term_bvs_h tvs t)
  | transform_pfr_bvs_h tvs pvs (Proofs.pf_atE r) = Proofs.pf_atE (transform_pfr_bvs_h tvs pvs r)
  | transform_pfr_bvs_h tvs pvs (Proofs.pf_rhole _) = raise Impossible_in_proof

fun transform_constraint_bvs_h tvs (Constraints.c_leq (t1,t2)) = Constraints.c_leq (transform_term_bvs_h tvs t1,
										   transform_term_bvs_h tvs t2)
  | transform_constraint_bvs_h tvs (Constraints.c_stronger (t1,t2)) = Constraints.c_stronger (transform_term_bvs_h tvs t1,
											     transform_term_bvs_h tvs t2)
  | transform_constraint_bvs_h tvs (Constraints.c_other (f,tl)) = 
    Constraints.c_other (f, List.map (transform_term_bvs_h tvs) tl)

fun transform_constraint_list_bvs_h tvs C = List.map (transform_constraint_bvs_h tvs) C

fun transform_constraint_ctx_bvs_h tvs (C) = 
    (Constraints.make_ctx o (transform_constraint_list_bvs_h tvs) o  Constraints.ctx_to_list) C


fun transform_hypconstraint_bvs_h tvs (C,c) =
    (transform_constraint_ctx_bvs_h tvs C, transform_constraint_bvs_h tvs c)

fun transform_hypconstraint_list_bvs_h tvs hcl = 
    List.map (transform_hypconstraint_bvs_h tvs) hcl

fun transform_qhcl_bvs_h tvs ([], hcl) = ([], transform_hypconstraint_list_bvs_h tvs hcl)
  | transform_qhcl_bvs_h tvs (((Terms.Bvar v, so) :: L), hcl) = 
    let val u = Terms.new_uvar (Terms.Varbind v)
	val (sigma, hcl') = transform_qhcl_bvs_h ((v,u) :: tvs) (L, hcl)
    in
	((u,so) :: sigma, hcl')
    end
  | transform_qhcl_bvs_h tvs (_ :: L, hcl) = raise Impossible_in_qhcl

fun transform_state_bvs_h tvs (States.s_owner (f,k)) = States.s_owner (transform_term_bvs_h tvs f,
								       transform_term_bvs_h tvs k)
  | transform_state_bvs_h tvs (States.s_has_xattr (f,a,v)) = States.s_has_xattr (transform_term_bvs_h tvs f,
										 transform_term_bvs_h tvs a,
										 transform_term_bvs_h tvs v)
  | transform_state_bvs_h tvs (States.s_other (f, tl)) = States.s_other (f, List.map (transform_term_bvs_h tvs) tl)

fun transform_state_list_bvs_h tvs sl = List.map (transform_state_bvs_h tvs) sl

fun transform_qprocap_bvs_h tvs ([]: (Terms.varbind * Sorts.sort) list, (k,f,p,q,sl,S): Procaps.procap):
  Procaps.qprocap_parsed = 
    ([], 
     (transform_term_bvs_h tvs k,
      transform_term_bvs_h tvs f,
      transform_term_bvs_h tvs p,
      transform_qhcl_bvs_h tvs q,
      transform_state_list_bvs_h tvs sl,
      S
    ))
  | transform_qprocap_bvs_h tvs (((Terms.Varbind s, so)::L), procap) =
    let val u = Terms.new_uvar (Terms.Varbind s)
	val (M, procap') = transform_qprocap_bvs_h ((s,u) :: tvs) (L, procap)
    in
	((Terms.Varbind s,u,so) :: M, procap')
    end

in 

fun transform_pfn_bvs n = transform_pfn_bvs_h [] [] n
fun transform_pfr_bvs r = transform_pfr_bvs_h [] [] r

fun transform_qhcl_bvs q = transform_qhcl_bvs_h [] q
fun transform_qprocap_bvs qp = transform_qprocap_bvs_h [] qp

end

(* When a qprocap's MAC is parsed, it has the form #{ws}*{hex40},
   where hex40 is the 40 digit hex of the mac. This function takes a
   string in this form, and returns another string, containing only
   the 40 hex digits *)
exception Impossible_in_mac

fun extract_mac (s: string) = if (String.size s < 40) then raise Impossible_in_mac
			      else String.substring (s, String.size s - 40, 40)
end (* struct ParserCommon *)
