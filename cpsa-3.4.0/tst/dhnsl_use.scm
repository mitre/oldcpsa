(herald "Diffie-Hellman enhanced Needham-Schroeder-Lowe Protocol"
  ;(limit 4)
  (algebra diffie-hellman))

(defprotocol dhnsl diffie-hellman
  (defrole resp
    (vars (b a name) (h1 base) (y expn) (n text))
    (trace
     (recv (enc h1 a (pubk b)))
     (send (enc h1 (exp (gen) y) b (pubk a)))
     (recv (enc (exp (gen) y) (pubk b)))
     (send (enc n (exp h1 y)))
     (recv n)
    )
     (uniq-orig n)
    (uniq-gen y)
    (comment "Y should be assumed to be freshly chosen per role")
  )
  (defrole init
    (vars (a b name) (h2 base) (x expn) (n text))
    (trace
     (send (enc (exp (gen) x) a (pubk b)))
     (recv (enc (exp (gen) x) h2 b (pubk a)))
     (send (enc h2 (pubk b)))
     (recv (enc n (exp h2 x)))
     (send n)
    )
    (uniq-gen x)
    (comment "X should be assumed to be freshly chosen per role")
  )
  (comment "Needham-Schroeder-Lowe DH challenge/responses in place of nonces")
)

;;; The initiator point-of-view
(defskeleton dhnsl
  (vars (a b name) (h2 h3 base) (x expn))
  (defstrand init 5 (a a) (b b) (h2 h2) (x x))
  (non-orig (privk b) (privk a))
;  (uniq-gen x)
  (comment "Initiator point-of-view"))

;; The responder point-of-view
(defskeleton dhnsl
  (vars (a b name) (h1 base) (y z expn))
  (defstrand resp 5 (a a) (b b) (h1 h1) (y y))
  (non-orig (privk a) (privk b))
;  (uniq-gen y)
  (comment "Responder point-of-view"))

(defprotocol dhns diffie-hellman
  (defrole resp
    (vars (b a name) (h1 base) (y expn) (n text))
    (trace
     (recv (enc h1 a (pubk b)))
     (send (enc h1 (exp (gen) y) (pubk a)))
     (recv (enc (exp (gen) y) (pubk b)))
     (send (enc n (exp h1 y)))
     (recv n)
    )
     (uniq-orig n)
    (uniq-gen y)
    (comment "Y should be assumed to be freshly chosen per role")
  )
  (defrole init
    (vars (a b name) (h2 base) (x expn) (n text))
    (trace
     (send (enc (exp (gen) x) a (pubk b)))
     (recv (enc (exp (gen) x) h2 (pubk a)))
     (send (enc h2 (pubk b)))
     (recv (enc n (exp h2 x)))
     (send n)
    )
    (uniq-gen x)
    (comment "X should be assumed to be freshly chosen per role")
  )
  (comment "Needham-Schroeder-Lowe DH challenge/responses in place of nonces")
)

;;; The initiator point-of-view
(defskeleton dhns
  (vars (a b name) (h2 h3 base) (x expn))
  (defstrand init 5 (a a) (b b) (h2 h2) (x x))
  (non-orig (privk b) (privk a))
;  (uniq-gen x)
  (comment "Initiator point-of-view"))

;; The responder point-of-view
(defskeleton dhns
  (vars (a b name) (h1 base) (y z expn))
  (defstrand resp 5 (a a) (b b) (h1 h1) (y y))
  (non-orig (privk a) (privk b))
;  (uniq-gen y)
  (comment "Responder point-of-view"))
