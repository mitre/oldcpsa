(herald "IADH: unified model (UM)" (bound 20) (limit 8000)
  (algebra diffie-hellman))

(comment "CPSA 3.6.11")
(comment "All input read from tst/iadh_um_eq.scm")
(comment "Step count limited to 8000")
(comment "Strand count bounded at 20")

(defprotocol iadh-um diffie-hellman
  (defrole participant
    (vars (l lp e rndx) (ep expt) (self peer name))
    (trace (send (cat self peer (exp (gen) l) (exp (gen) lp)))
      (send (cat (exp (gen) l) (exp (gen) e))) (recv (exp (gen) ep)))
    (uniq-gen e)
    (fn-of ("ltx-of" (self l) (peer lp))
      ("principal-of" (l self) (lp peer)))
    (neq (ep (one)))
    (absent (e (exp (gen) l)) (e (exp (gen) lp))))
  (defrole ltx-gen
    (vars (self name) (l rndx))
    (trace (send (cat self l)))
    (uniq-orig l)
    (fn-of ("ltx-of" (self l)) ("principal-of" (l self)))))

(defskeleton iadh-um
  (vars (self peer self-0 peer-0 name) (eA lA lAp eB lB lBp rndx)
    (ep ep-0 expt))
  (defstrand participant 3 (self self) (peer peer) (l lA) (lp lAp)
    (e eA) (ep ep))
  (defstrand participant 3 (self self-0) (peer peer-0) (l lB) (lp lBp)
    (e eB) (ep ep-0))
  (absent (eB (exp (gen) lB)) (eB (exp (gen) lBp)) (eA (exp (gen) lA))
    (eA (exp (gen) lAp)))
  (fn-of ("ltx-of" (self-0 lB) (peer-0 lBp) (self lA) (peer lAp))
    ("principal-of" (lB self-0) (lBp peer-0) (lA self) (lAp peer)))
  (neq (ep-0 (one)) (eA eB) (ep (one)))
  (non-orig lA lB)
  (uniq-gen eA eB)
  (comment "Implicit authentication")
  (traces
    ((send (cat self peer (exp (gen) lA) (exp (gen) lAp)))
      (send (cat (exp (gen) lA) (exp (gen) eA))) (recv (exp (gen) ep)))
    ((send (cat self-0 peer-0 (exp (gen) lB) (exp (gen) lBp)))
      (send (cat (exp (gen) lB) (exp (gen) eB)))
      (recv (exp (gen) ep-0))))
  (label 0)
  (unrealized)
  (shape)
  (maps
    ((0 1)
      ((eA eA) (lA lA) (lAp lAp) (eB eB) (lB lB) (lBp lBp) (ep ep)
        (ep-0 ep-0) (self self) (peer peer) (self-0 self-0)
        (peer-0 peer-0))))
  (origs))

(comment "Nothing left to do")
