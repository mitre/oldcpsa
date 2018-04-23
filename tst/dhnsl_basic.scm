(herald "Diffie-Hellman enhanced Needham-Schroeder-Lowe Protocol"
;  (limit 1)
  (algebra diffie-hellman))

(defprotocol dhnsl diffie-hellman
  (defrole resp
    (vars (b a name) (x expt) (y rndx))
    (trace
     (recv (enc (exp (gen) x) a (pubk b)))
     (send (enc (exp (gen) x) (exp (gen) y) b (pubk a)))
     (recv (enc (exp (gen) y) (pubk b)))
     
     )
    (absent (y (exp (gen) x)))
;    (uniq-gen y)
    (comment "Y and Z should be assumed to be freshly chosen per role")
  )
  (defrole init
    (vars (a b name) (y expt) (x rndx))
    (trace
     (send (enc (exp (gen) x) a (pubk b)))
     (recv (enc (exp (gen) x) (exp (gen) y) b (pubk a)))
     (send (enc (exp (gen) y) (pubk b)))
    )
;    (uniq-gen x)
    (comment "X should be assumed to be freshly chosen per role")
  )
  (comment "Needham-Schroeder-Lowe DH challenge/responses in place of nonces")
)

;;; The initiator point-of-view
(defskeleton dhnsl
  (vars (a b name) (y expt) (x rndx))
  (defstrand init 3 (a a) (b b) (y y) (x x))
  (non-orig (privk b) (privk a))
  (uniq-gen x)
  (comment "Initiator point-of-view"))

;; The responder point-of-view
(defskeleton dhnsl
  (vars (a b name) (x expt) (y rndx))
  (defstrand resp 3 (a a) (b b) (x x) (y y))
  (non-orig (privk a))
  (uniq-gen y)
  (comment "Responder point-of-view"))
