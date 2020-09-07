%% policies of admin
cred/admin/may/read/_ : hyp_claims (prim_int2principal 1001)
 (forall principal
      ([u] forall file
         ([f] forall level
    ([l] forall level
       ([l'] imp (says ciahr (atom (employee u)))
(imp (says oca (atom (level_file f l)))
   (imp (atom (level_individual u l'))
        (imp (atom (below l l'))
(atom (may u f read)))))))))) ninfty pinfty.

cred/admin/may/execute/_ : hyp_claims (prim_int2principal 1001)
 (forall principal
    ([u] forall file
       ([f] forall level
  ([l] forall level
     ([l'] imp (says ciahr (atom (employee u)))
  (imp (says oca (atom (level_file f l)))
     (imp (atom (level_individual u l'))
      (imp (atom (below l l'))
  (atom (may u f execute)))))))))) ninfty pinfty.

cred/admin/level_indi/_ : hyp_claims (prim_int2principal 1001)
     (forall principal
      ([u] forall level
         ([l] imp (says polyadmin (atom (poly u)))
            (imp (says bgadmin (atom (bg u l)))
      (atom (level_individual u l)))))) ninfty pinfty.
