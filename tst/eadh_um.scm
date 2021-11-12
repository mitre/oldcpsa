(herald "Unified model (UM)" (bound 20) (limit 8000) (algebra diffie-hellman))

(defprotocol eadh-um diffie-hellman
  (defrole init
    (vars (l e rndx) (ll ee expt) (self peer name) (n data))
    (trace
     (recv (enc "cert" (exp (gen) l) self (privk self)))
     (recv (enc "cert" (exp (gen) ll) peer (privk peer)))
     (send (exp (gen) e))
     (recv (exp (gen) ee))
     (send (enc n (hash (exp (gen) (mul ll l)) (exp (gen) (mul ee e)))))
     (recv n))
    (uniq-gen e)
    (uniq-orig n)
    (neq (ee (one)))
    (fn-of ("principal-of" (self l)))
    (fn-of ("privdh-of" (l self)))
    )
  (defrole resp
    (vars (l e rndx) (ll ee expt) (self peer name) (n data))
    (trace
     (recv (enc "cert" (exp (gen) l) self (privk self)))
     (recv (enc "cert" (exp (gen) ll) peer (privk peer)))
     (send (exp (gen) e))
     (recv (exp (gen) ee))
     (recv (enc n (hash (exp (gen) (mul ll l)) (exp (gen) (mul ee e)))))
     (send n))
    (uniq-gen e)
    (neq (ee (one)))
    (fn-of ("principal-of" (self l)))
    (fn-of ("privdh-of" (l self)))
    )
  (defrole ltx-gen
    (vars (self name) (l rndx))
    (trace
     (send (enc "cert" (exp (gen) l) self (privk self)))
     (send l))
    (uniq-gen l)
    (fn-of ("principal-of" (self l)))
    (fn-of ("privdh-of" (l self)))
    )
  )

;; Explicit authentication: init's point of view
 (defskeleton eadh-um
  (vars (n data) (lA eA lB rndx) (A B name))
  (defstrand init 6 (n n) (e eA) (l lA) (self A) (peer B))
  (defstrand ltx-gen 1 (self B) (l lB))
  (non-orig lA lB)
  (non-orig (privk A) (privk B))
  (comment "Explicit authentication")
  )

;; Explicit authentication: init's point of view
 (defskeleton eadh-um
  (vars (n data) (lA eA lB rndx) (A B name))
  (defstrand init 6 (n n) (e eA) (l lA) (self A) (peer B))
  (defstrand ltx-gen 1 (self B) (l lB))
  (non-orig lB)
  (non-orig (privk A) (privk B))
  (comment "Explicit authentication, not assuming own LTK safe")
)
