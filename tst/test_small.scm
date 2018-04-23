(herald "small test" (algebra diffie-hellman)
;(limit 1)
)

(defprotocol test diffie-hellman
  (defrole init
    (vars (hr base) (a b name) (xi rndx))
    (trace
     (recv (enc hr (privk b)))
     (send (enc (hash (exp hr xi)) (exp (gen) xi) (privk a))))
    (uniq-gen xi)
;    (non-orig (privk b))
)
  (defrole resp
    (vars (xr rndx) (a b name) (hi base))
    (trace
     (send (enc (exp (gen) xr) (privk b)))
     (recv (enc (hash (exp hi xr)) hi (privk a))))
    (uniq-gen xr)
;    (non-orig (privk a))

;  (defrole signer
;    (vars (m mesg) (n name))
;    (trace
;      (recv m)
;      (send (enc m (privk n)))))
))

;(defskeleton test
;  (vars (a b name))
;  (defstrand init 2 (a a) (b b))
;  (non-orig (privk a) (privk b)))

(defskeleton test
  (vars (a b name))
  (defstrand resp 2 (a a) (b b))
  (non-orig (privk a) (privk b)))
