(* Taken from Elliot, Pfenning (1991) *)
(* Modified by Deepak Garg (2008) *)

(* Version of TERMS for hereditary Harrop formulas *)
(* Using destructive substitution *)


signature TERMS =
sig

datatype term = 
	 Bvar of string             (* Bound Variables *)
       | Evar of string * int * term list * (term option) ref
       (* Used in search only. Logic Variables , Stamped , Depends on , Bound to *)
       | Uvar of string * int	(* Used in search only. Parameters , Stamped *)
       | Appl of string * term list       (* Applications; a constant is a string applied to [] *)
       | Prim_str of string
       | Prim_int of int
       | Prim_date of Time.time
       | Loca 
       | Ctime
       | Ninfty
       | Pinfty
       | Read
       | Write
       | Execute
       | Identity
       | Govern
       | Prim_str2file of term
       | Prim_date2time of term
       | Prim_int2time of term
       | Prim_int2principal of term
       | Time2exp of term
       | Exp_add of term * term
       | Exp_subtract of term * term
       | Exp_max of term * term
       | Exp_min of term * term

and varbind = Varbind of string (* Variable binders *)

exception Subtype of string

val new_evar : varbind -> term list -> term
val new_uvar : varbind -> term
val shadow : varbind -> varbind -> bool
val subst : term -> varbind -> term -> term
val unify : term -> term -> (unit -> unit) -> unit
val unify_list : term list -> term list -> (unit -> unit) -> unit

val makestring_term : term -> string
val makestring_term_list: term list -> string
val print_term : term -> unit
val makestring_substitution : term list -> string
val print_substitution : term list -> unit

val eq: (term * term) -> bool
val eq_list: (term list * term list) -> bool

(* Ground a term by following references in Evars. Return NONE if any of the references is NONE *)
val ground: term -> term option
val ground_list: term list -> term list option

val occurs_uvar_term: term * term -> bool
val occurs_uvar_term_list: term * (term list) -> bool

val subst_term_for_uvar: term -> term -> term -> term
val subst_term_for_uvar_list: term -> term -> term list -> term list

end  (* signature TERMS *)

structure Terms : TERMS =
struct

datatype term = 
    Bvar of string             (* Bound Variables *)
  | Evar of string * int * term list * (term option) ref
			        (* Logic Variables , Stamped , Depends on ,Bound to *)
  | Uvar of string * int	(* Parameters , Stamped *)
  | Appl of string * term list        (* Applications, the constructors may be pre-defined or user defined *)
  | Prim_str of string
  | Prim_date of Time.time
  | Prim_int of int
  | Loca 
  | Ctime
  | Ninfty
  | Pinfty
  | Read
  | Write
  | Execute
  | Identity
  | Govern
  | Prim_str2file of term
  | Prim_date2time of term
  | Prim_int2time of term
  | Prim_int2principal of term
  | Time2exp of term
  | Exp_add of term * term
  | Exp_subtract of term * term
  | Exp_max of term * term
  | Exp_min of term * term


and varbind = Varbind of string (* Variable binders *)

exception Subtype of string

local val varcount = ref 0 in
 fun new_evar (Varbind(vname)) uvars =
   ( varcount := !varcount + 1;
     Evar(vname,!varcount,uvars,ref NONE) )

 fun new_uvar (Varbind(vname)) =
   ( varcount := !varcount + 1;
     Uvar(vname,!varcount) )
end  (* val varcount *)

fun shadow (Varbind(vname1)) (Varbind(vname2)) = (vname1 = vname2)

(* subst does not need to derefence Evar's, since an Evar can never
   contain a Bvar in its substitution term *)
fun subst s (Varbind(vname)) t =
    let fun sb (t as Bvar(bvname)) = if vname = bvname then s else t
          | sb (Appl(f,tl)) = Appl(f, map sb tl)
	  | sb (Prim_str2file t) = Prim_str2file (sb t)
	  | sb (Prim_date2time t) = Prim_date2time (sb t)
	  | sb (Prim_int2time t) = Prim_int2time (sb t)
	  | sb (Prim_int2principal t) = Prim_int2principal (sb t)
	  | sb (Time2exp t) = Time2exp (sb t)
	  | sb (Exp_add(t1,t2)) = Exp_add (sb t1, sb t2)
	  | sb (Exp_subtract(t1,t2)) = Exp_subtract (sb t1, sb t2)
	  | sb (Exp_max(t1,t2)) = Exp_max (sb t1, sb t2)
	  | sb (Exp_min(t1,t2)) = Exp_min (sb t1, sb t2)
	  | sb t = t
         in sb t end

fun same_evar (s as Evar(_,stamp1,_,_)) t = 
  let fun se (Evar(_,stamp2,_,ref NONE)) = (stamp1 = stamp2)
        | se (Evar(_,_,_,ref (SOME t0))) = se t0
        | se _ = false
       in se t end
  | same_evar s _ = raise Subtype("same_evar: argument is not an Evar.")

fun init_seg uvars1 uvars2 = length uvars1 <= length uvars2

fun extended_occurs_check (s as Evar(_,stamp1,uvars1,_)) t sc =
  let fun eoc (Evar(x,stamp2,uvars2,vslot as (ref NONE))) sc = 
	      if (stamp1 = stamp2)
		 then ()
		 else if init_seg uvars2 uvars1
			 then sc ()
			 else ( vslot := SOME (new_evar (Varbind(x)) uvars1) ;
				sc () ;
				vslot := NONE ;
				() )
        | eoc (Evar(_,_,_,ref (SOME t0))) sc = eoc t0 sc
        | eoc (Appl(f,tl)) sc = eoc_list tl sc
        | eoc (Uvar(_,stamp2)) sc =
	     if List.exists (fn (Uvar(_,stamp1)) => (stamp1 = stamp2)
			 | uv => raise Subtype("extended_occurs_check: Evar depends on a non-Uvar."))
		       uvars1
		then sc ()
		else ()
	| eoc (Prim_str2file t) sc = eoc t sc
	| eoc (Prim_date2time t) sc = eoc t sc
	| eoc (Prim_int2time t) sc = eoc t sc
	| eoc (Prim_int2principal t) sc = eoc t sc
	| eoc (Time2exp t) sc  = eoc t sc
	| eoc (Exp_add(t1,t2)) sc = eoc t1 (fn () => eoc t2 sc)
	| eoc (Exp_subtract(t1,t2)) sc = eoc t1 (fn () => eoc t2 sc)
	| eoc (Exp_max(t1,t2)) sc = eoc t1 (fn () => eoc t2 sc)
	| eoc (Exp_min(t1,t2)) sc = eoc t1 (fn () => eoc t2 sc)
	| eoc _ sc = sc ()
		     
      and eoc_list [] sc = sc ()
	| eoc_list (t :: tl) sc = eoc t (fn () => eoc_list tl sc)

  in eoc t sc end

  | extended_occurs_check s _ _ = raise Subtype("extended_occurs_check: argument is not an Evar.")

fun unify (s as Evar _) t sc = unify_evar s t sc
  | unify s (t as Evar _) sc = unify_evar t s sc
  | unify (Uvar(_,stamp1)) (Uvar(_,stamp2)) sc =
       if stamp1 = stamp2 then sc () else ()
  | unify (Appl(f,tl)) (Appl(f',tl')) sc =
    if (f <> f') then ()
    else unify_list tl tl' sc
  | unify (Prim_str s) (Prim_str s') sc = if (s <> s') then () else sc ()
  | unify (Prim_int i) (Prim_int i') sc = if (i <> i') then () else sc ()
  | unify (Prim_date d) (Prim_date d') sc = if (Time.compare (d,d') <> EQUAL) then () else sc ()
  | unify Loca Loca sc = sc ()
  | unify Ctime Ctime sc = sc ()
  | unify Ninfty Ninfty sc = sc ()
  | unify Pinfty Pinfty sc = sc ()
  | unify Read Read sc = sc ()
  | unify Write Write sc = sc ()
  | unify Execute Execute sc = sc ()
  | unify Identity Identity sc = sc ()
  | unify Govern Govern sc = sc ()
  | unify (Prim_str2file t) (Prim_str2file t') sc = unify t t' sc
  | unify (Prim_date2time t) (Prim_date2time t') sc = unify t t' sc 
  | unify (Prim_int2time t) (Prim_int2time t') sc = unify t t' sc
  | unify (Prim_int2principal t) (Prim_int2principal t') sc = unify t t' sc
  | unify (Time2exp t) (Time2exp t') sc = unify t t' sc
  | unify (Exp_add(t1,t2)) (Exp_add(t1',t2')) sc = unify t1 t2 (fn () => unify t1' t2' sc)
  | unify (Exp_subtract(t1,t2)) (Exp_subtract(t1',t2')) sc = unify t1 t2 (fn () => unify t1' t2' sc)
  | unify (Exp_max(t1,t2)) (Exp_max(t1',t2')) sc = unify t1 t2 (fn () => unify t1' t2' sc)
  | unify (Exp_min(t1,t2)) (Exp_min(t1',t2')) sc = unify t1 t2 (fn () => unify t1' t2' sc)
  | unify _ _ sc = ()

and unify_list [] [] sc = sc ()
  | unify_list [] _ sc = ()
  | unify_list _ [] sc = () 
  | unify_list (t :: tl) (t' :: tl') sc = unify t t' (fn () => unify_list tl tl' sc)

and unify_evar (s as Evar(_,_,_,(vslot as (ref NONE)))) t sc = 
       if same_evar s t
	  then sc ()
       else extended_occurs_check s t
	          (fn () => ( vslot := SOME t ; sc () ; vslot := NONE ; () ))
  | unify_evar (Evar(_,_,_,ref (SOME s0))) t sc = unify s0 t sc
  | unify_evar s _ _ = raise Subtype("unify_evar: argument is not an Evar.")

fun makestring_stamp i = Int.toString i

fun makestring_term (Bvar(vname)) = vname
  | makestring_term (Evar(vname,stamp,_,ref NONE)) =
        "?" ^ vname ^ makestring_stamp stamp
  | makestring_term (Evar(vname,stamp,_,ref (SOME t))) = makestring_term t
  | makestring_term (Uvar(vname,stamp)) = vname ^ (makestring_stamp stamp) (* "!" ^  *)
  | makestring_term (Appl(f,[])) = f 
  | makestring_term (Appl(f, tl)) = "(" ^ f ^ " " ^ (makestring_term_list(tl)) ^ ")"
  | makestring_term (Prim_str s) = "\"" ^ s ^ "\""
  | makestring_term (Prim_int i) = Int.toString i
  | makestring_term (Prim_date d) = Date.fmt "%Y:%m:%d:%H:%M:%S" (Date.fromTimeLocal d)
  | makestring_term Loca = "loca"
  | makestring_term Ctime = "ctime"
  | makestring_term Ninfty = "ninfty"
  | makestring_term Pinfty = "pinfty"
  | makestring_term Read = "read"
  | makestring_term Write = "write"
  | makestring_term Execute = "execute"
  | makestring_term Identity = "identity"
  | makestring_term Govern = "govern"
  | makestring_term (Prim_str2file t) = "(prim_str2file " ^ (makestring_term t) ^ ")"
  | makestring_term (Prim_date2time t) =  "(prim_date2time " ^ (makestring_term t) ^ ")"
  | makestring_term (Prim_int2time t) =  "(prim_int2time " ^ (makestring_term t) ^ ")"
  | makestring_term (Prim_int2principal t) =  "(prim_int2principal " ^ (makestring_term t) ^ ")"
  | makestring_term (Time2exp t) = "(time2exp " ^ (makestring_term t) ^ ")"
  | makestring_term (Exp_add(t1,t2)) = "(exp_add " ^ (makestring_term_list [t1,t2]) ^ ")"
  | makestring_term (Exp_subtract(t1,t2)) = "(exp_subtract " ^ (makestring_term_list [t1,t2]) ^ ")"
  | makestring_term (Exp_max(t1,t2)) = "(exp_max " ^ (makestring_term_list [t1,t2]) ^ ")"
  | makestring_term (Exp_min(t1,t2)) = "(exp_min " ^ (makestring_term_list [t1,t2]) ^ ")"


and makestring_term_list [] = raise Subtype ("makestring_term_list: list is empty")
  | makestring_term_list (t :: []) = makestring_term t
  | makestring_term_list (t :: tl) = (makestring_term t) ^ " " ^ (makestring_term_list tl)

val print_term = print o makestring_term

fun makestring_substitution nil = " .\n"
  | makestring_substitution (evar::rest) =
     "\n" ^ (case evar of Evar(_,_,_,ref NONE) => makestring_term evar
		        | Evar(vname,stamp,uvars,ref (SOME t)) =>
			    makestring_term(Evar(vname,stamp,uvars,ref NONE))
			| _ => raise Subtype("makestring_substitution: substitution contains a non-Evar."))
        ^ " <- " ^ makestring_term evar
        ^ (case rest of nil => "" | _ => " ,")
	^ makestring_substitution rest

val print_substitution = print o makestring_substitution

fun eq (Bvar s, Bvar s') = (s = s')
  | eq (e as Evar (_,_,_,ref NONE), e' as Evar (_,_,_,ref NONE)) = same_evar e e'
  | eq (Evar (_,_,_,ref (SOME t)), t') = eq (t, t')
  | eq (t, Evar (_,_,_,ref (SOME t'))) = eq (t, t')
  | eq (Uvar(_,stamp), Uvar(_,stamp')) = (stamp = stamp')	
  | eq (Appl(s,tl), Appl(s',tl')) = (s = s') andalso (eq_list(tl,tl'))
  | eq (Prim_str s, Prim_str s') = (s = s')
  | eq (Prim_date d, Prim_date d') = (d = d')
  | eq (Prim_int i, Prim_int i') = (i = i')
  | eq (Loca, Loca) = true 
  | eq (Ctime, Ctime) = true
  | eq (Ninfty, Ninfty) = true
  | eq (Pinfty, Pinfty) = true
  | eq (Read, Read) = true
  | eq (Write, Write) = true
  | eq (Execute, Execute) = true
  | eq (Identity, Identity) = true
  | eq (Govern, Govern) = true
  | eq (Prim_str2file t, Prim_str2file t') = eq(t,t')
  | eq (Prim_date2time t, Prim_date2time t') = eq(t,t')
  | eq (Prim_int2time t, Prim_int2time t') = eq(t,t')
  | eq (Prim_int2principal t, Prim_int2principal t') = eq(t,t')
  | eq (Time2exp t, Time2exp t') = eq(t,t')
  | eq (Exp_add(t1,t2), Exp_add(t1',t2')) = eq(t1,t2) andalso eq(t1',t2')
  | eq (Exp_subtract(t1,t2), Exp_subtract(t1',t2')) = eq(t1,t2) andalso eq(t1',t2')
  | eq (Exp_max(t1,t2), Exp_max(t1',t2')) = eq(t1,t2) andalso eq(t1',t2')
  | eq (Exp_min(t1,t2), Exp_min(t1',t2')) = eq(t1,t2) andalso eq(t1',t2')
  | eq (_,_) = false

and eq_list ([], []) = true
  | eq_list ([], _) = false
  | eq_list (_, []) = false
  | eq_list (t :: L, t' :: L') = eq(t,t') andalso eq_list(L, L')


infix >>= ;

fun op >>= (x: 'a option, f: 'a -> 'b option): 'b option = Option.mapPartial f x
						     
fun ground (t as Bvar _) = SOME t
  | ground (t as Evar (_,_,_,ref NONE)) = NONE
  | ground (t as Evar (_,_,_,ref (SOME t'))) = ground t'
  | ground (t as Uvar _) = SOME t
  | ground (t as Appl(f, tl)) = (ground_list tl) >>= (fn tl => SOME (Appl(f,tl)))
  | ground (t as Prim_str _) = SOME t
  | ground (t as Prim_int _) = SOME t
  | ground (t as Prim_date _) = SOME t
  | ground (t as Loca) = SOME t
  | ground (t as Ctime) = SOME t
  | ground (t as Ninfty) = SOME t
  | ground (t as Pinfty) = SOME t
  | ground (t as Read) = SOME t
  | ground (t as Write) = SOME t
  | ground (t as Execute) = SOME t
  | ground (t as Identity) = SOME t
  | ground (t as Govern) = SOME t
  | ground (Prim_str2file t) = (ground t) >>= SOME o Prim_str2file
  | ground (Prim_date2time t) = (ground t) >>= SOME o Prim_date2time
  | ground (Prim_int2time t) = (ground t) >>= SOME o Prim_int2time
  | ground (Prim_int2principal t) = (ground t) >>= SOME o Prim_int2principal
  | ground (Time2exp t) = (ground t) >>= SOME o Time2exp
  | ground (Exp_add(t1,t2)) = (ground t1) >>= (fn t1' => (ground t2) >>= (fn t2' => SOME (Exp_add(t1',t2'))))
  | ground (Exp_subtract(t1,t2)) = (ground t1) >>= (fn t1' => (ground t2) >>= (fn t2' => SOME (Exp_subtract(t1',t2'))))
  | ground (Exp_max(t1,t2)) = (ground t1) >>= (fn t1' => (ground t2) >>= (fn t2' => SOME (Exp_max(t1',t2'))))
  | ground (Exp_min(t1,t2)) = (ground t1) >>= (fn t1' => (ground t2) >>= (fn t2' => SOME (Exp_min(t1',t2'))))
	       
and ground_list [] = SOME []
  | ground_list (t :: tl) = (ground t) >>= (fn t' => (ground_list tl) >>= (fn tl' => SOME (t' :: tl')))



(* Return true iff the uvar u occurs in term t *)
fun occurs_uvar_term (u as Uvar _, t) = 
    (case t of
	 Bvar _ => false
       | Evar (_,_,_,ref NONE) => false
       | Evar (_,_,_,ref (SOME t')) => occurs_uvar_term (u,t')
       | u' as (Uvar _) => eq(u,u')            (* --- this is the only base case where true may be returned --- *)
       | Appl (f, tl) => occurs_uvar_term_list (u,tl)
       | Prim_str _ => false
       | Prim_date _ => false
       | Prim_int _ => false
       | Loca => false
       | Ctime => false
       | Ninfty => false
       | Pinfty => false
       | Read => false
       | Write => false
       | Execute => false
       | Identity => false
       | Govern => false
       | Prim_str2file t => occurs_uvar_term (u,t)
       | Prim_date2time t => occurs_uvar_term (u,t)
       | Prim_int2time t => occurs_uvar_term (u,t)
       | Prim_int2principal t => occurs_uvar_term (u,t)
       | Time2exp t => occurs_uvar_term (u,t)
       | Exp_add (t1,t2) => (occurs_uvar_term (u,t1)) orelse (occurs_uvar_term (u,t2))
       | Exp_subtract (t1,t2) => (occurs_uvar_term (u,t1)) orelse (occurs_uvar_term (u,t2))
       | Exp_max (t1,t2) => (occurs_uvar_term (u,t1)) orelse (occurs_uvar_term (u,t2))
       | Exp_min (t1,t2) => (occurs_uvar_term (u,t1)) orelse (occurs_uvar_term (u,t2))
    )
  | occurs_uvar_term _ = false

and occurs_uvar_term_list (u, []) = false
  | occurs_uvar_term_list (u, t :: tl) = (occurs_uvar_term (u,t)) orelse (occurs_uvar_term_list (u, tl))


(* Substitute a term u' for an existing uvar u *)
fun subst_term_for_uvar u' u (t as Bvar _) = t
  | subst_term_for_uvar u' u (t as Evar _) = t (* This should never be used *)
  | subst_term_for_uvar u' u (u'' as Uvar _) = if (eq (u,u'')) then u' else u''
  | subst_term_for_uvar u' u (Appl (f, tl)) = Appl(f, subst_term_for_uvar_list u' u tl)
  | subst_term_for_uvar u' u (t as Prim_str _) = t
  | subst_term_for_uvar u' u (t as Prim_date _) = t
  | subst_term_for_uvar u' u (t as Prim_int _) = t
  | subst_term_for_uvar u' u (t as Loca) = t
  | subst_term_for_uvar u' u (t as Ctime) = t
  | subst_term_for_uvar u' u (t as Ninfty) = t
  | subst_term_for_uvar u' u (t as Pinfty) = t
  | subst_term_for_uvar u' u (t as Read) = t
  | subst_term_for_uvar u' u (t as Write) = t
  | subst_term_for_uvar u' u (t as Execute) = t
  | subst_term_for_uvar u' u (t as Identity) = t
  | subst_term_for_uvar u' u (t as Govern) = t
  | subst_term_for_uvar u' u (Prim_str2file t) = Prim_str2file (subst_term_for_uvar u' u t)
  | subst_term_for_uvar u' u (Prim_date2time t) = Prim_date2time (subst_term_for_uvar u' u t)
  | subst_term_for_uvar u' u (Prim_int2time t) = Prim_int2time (subst_term_for_uvar u' u t)
  | subst_term_for_uvar u' u (Prim_int2principal t) = Prim_int2principal (subst_term_for_uvar u' u t)
  | subst_term_for_uvar u' u (Time2exp t) = Time2exp (subst_term_for_uvar u' u t)
  | subst_term_for_uvar u' u (Exp_add (t1,t2)) = Exp_add(subst_term_for_uvar u' u t1, subst_term_for_uvar u' u t2)
  | subst_term_for_uvar u' u (Exp_subtract (t1,t2)) = Exp_subtract(subst_term_for_uvar u' u t1, subst_term_for_uvar u' u t2)
  | subst_term_for_uvar u' u (Exp_max (t1,t2)) = Exp_max(subst_term_for_uvar u' u t1, subst_term_for_uvar u' u t2)
  | subst_term_for_uvar u' u (Exp_min (t1,t2)) = Exp_min(subst_term_for_uvar u' u t1, subst_term_for_uvar u' u t2)

and subst_term_for_uvar_list u' u [] = []
  | subst_term_for_uvar_list u' u (t :: tl) = (subst_term_for_uvar u' u t) :: (subst_term_for_uvar_list u' u tl)


end  (* structure Terms *)
