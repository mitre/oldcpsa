(herald "IADH: unified model (UM), criss-cross variant" (bound 20) (limit 2000) (algebra diffie-hellman))

(defprotocol iadh-umx diffie-hellman
  (defrole participant1
    (vars (l e rndx) (lp ep expt) (self peer name) (key n data))
    (trace
     (recv (enc "cert" (exp (gen) l) self (privk self)))
     (recv (enc "cert" (exp (gen) lp) peer (privk peer)))
     (send (exp (gen) e))
     (recv (exp (gen) ep))
     (send key))
    (fn-of (foo ((hash (exp (gen) (mul lp e)) (exp (gen) (mul l ep))) key)))
    (uniq-gen e)
    (neq (ep (one))))
  (defrole participant2
    (vars (l e rndx) (lp ep expt) (self peer name) (key n data))
    (trace
     (recv (enc "cert" (exp (gen) l) self (privk self)))
     (recv (enc "cert" (exp (gen) lp) peer (privk peer)))
     (send (exp (gen) e))
     (recv (exp (gen) ep))
     (send key))
    (fn-of (foo ((hash (exp (gen) (mul l ep)) (exp (gen) (mul lp e))) key)))
    (uniq-gen e)
    (neq (ep (one))))
  (defrole ltx-gen
    (vars (self name) (l rndx))
    (trace
     (send (enc "cert" (exp (gen) l) self (privk self)))
     (send l))
    (uniq-gen l)
    (fn-of ("principal-of" (l self))))
  )

;; Implicit authentication: participants will agree on the resulting key
;; foo(key).  Therefore they should agree on the names A and B.
 (defskeleton iadh-umx
  (vars (key data) (eA lA eB lB rndx) (A B C D name))
  (defstrand participant1 5 (key key) (e eA) (l lA) (self A) (peer C))
  (defstrand participant2 5 (key key) (e eB) (l lB) (self B) (peer D))
  (non-orig lA lB)
  (neq (eA eB))
  (non-orig (privk C) (privk D) (privk A) (privk B))
)

 ;; Security:
(defskeleton iadh-umx
  (vars (e1 e2 l rndx) (lp expt) (A B name))
  (defstrand participant1 5 (e e1) (l l) (ep e2) (lp lp) (self A) (peer B))
  (defstrand participant2 3 (e e2))
  (deflistener (hash (exp (gen) (mul lp e2)) (exp (gen) (mul e1 l))))
  (non-orig (privk A) (privk B))
)
