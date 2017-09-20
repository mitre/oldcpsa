(herald "Diffie-Hellman enhanced Needham-Schroeder-Lowe Protocol"
;  (limit 1)
  (algebra diffie-hellman))

(defprotocol dhnsl diffie-hellman
  (defrole resp
    (vars (b a name) (h1 base) (y expn))
    (trace
     (recv (enc h1 a (pubk b)))
     (send (enc h1 (exp (gen) y) b (pubk a)))
     (recv (enc (exp (gen) y) (pubk b)))
     
     )
    (absent (y h1))
;    (uniq-gen y)
    (comment "Y and Z should be assumed to be freshly chosen per role")
  )
  (defrole init
    (vars (a b name) (h2 base) (x expn))
    (trace
     (send (enc (exp (gen) x) a (pubk b)))
     (recv (enc (exp (gen) x) h2 b (pubk a)))
     (send (enc h2 (pubk b)))
    )
;    (uniq-gen x)
    (comment "X should be assumed to be freshly chosen per role")
  )
  (comment "Needham-Schroeder-Lowe DH challenge/responses in place of nonces")
)

;;; The initiator point-of-view
(defskeleton dhnsl
  (vars (a b name) (h2 h3 base) (x expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (x x))
  (non-orig (privk b) (privk a))
  (uniq-gen x)
  (comment "Initiator point-of-view"))

;; The responder point-of-view
(defskeleton dhnsl
  (vars (a b name) (h1 base) (y z expn))
  (defstrand resp 3 (a a) (b b) (h1 h1) (y y))
  (non-orig (privk a))
  (uniq-gen y)
  (comment "Responder point-of-view"))
