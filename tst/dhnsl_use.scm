(herald "Diffie-Hellman enhanced Needham-Schroeder-Lowe Protocol"
  ;(limit 4)
  (algebra diffie-hellman))

(defprotocol dhnsl diffie-hellman
  (defrole resp
    (vars (b a name) (x expt) (y rndx) (n text))
    (trace
     (recv (enc (exp (gen) x) a (pubk b)))
     (send (enc (exp (gen) x) (exp (gen) y) b (pubk a)))
     (recv (enc (exp (gen) y) (pubk b)))
     (send (enc n (exp (gen) (mul x y))))
     (recv n)
    )
     (uniq-orig n)
    (uniq-gen y)
    (comment "Y should be assumed to be freshly chosen per role")
  )
  (defrole init
    (vars (a b name) (y expt) (x rndx) (n text))
    (trace
     (send (enc (exp (gen) x) a (pubk b)))
     (recv (enc (exp (gen) x) (exp (gen) y) b (pubk a)))
     (send (enc (exp (gen) y) (pubk b)))
     (recv (enc n (exp (gen) (mul x y))))
     (send n)
    )
    (uniq-gen x)
    (comment "X should be assumed to be freshly chosen per role")
  )
  (comment "Needham-Schroeder-Lowe DH challenge/responses in place of nonces")
)

;;; The initiator point-of-view
(defskeleton dhnsl
  (vars (a b name) (y expt) (x rndx))
  (defstrand init 5 (a a) (b b) (y y) (x x))
  (non-orig (privk b) (privk a))
  (comment "Initiator point-of-view"))

;; The responder point-of-view
(defskeleton dhnsl
  (vars (a b name) (x expt) (y rndx))
  (defstrand resp 5 (a a) (b b) (x x) (y y))
  (non-orig (privk a) (privk b))
  (comment "Responder point-of-view"))

(defprotocol dhns diffie-hellman
  (defrole resp
    (vars (b a name) (x expt) (y rndx) (n text))
    (trace
     (recv (enc (exp (gen) x) a (pubk b)))
     (send (enc (exp (gen) x) (exp (gen) y) (pubk a)))
     (recv (enc (exp (gen) y) (pubk b)))
     (send (enc n (exp (gen) (mul x y))))
     (recv n)
    )
     (uniq-orig n)
    (uniq-gen y)
    (comment "Y should be assumed to be freshly chosen per role")
  )
  (defrole init
    (vars (a b name) (y expt) (x rndx) (n text))
    (trace
     (send (enc (exp (gen) x) a (pubk b)))
     (recv (enc (exp (gen) x) (exp (gen) y) (pubk a)))
     (send (enc (exp (gen) y) (pubk b)))
     (recv (enc n (exp (gen) (mul y x))))
     (send n)
    )
    (uniq-gen x)
    (comment "X should be assumed to be freshly chosen per role")
  )
  (comment "Needham-Schroeder-Lowe DH challenge/responses in place of nonces")
)

;;; The initiator point-of-view
(defskeleton dhns
  (vars (a b name) (y expt) (x rndx))
  (defstrand init 5 (a a) (b b) (y y) (x x))
  (non-orig (privk b) (privk a))
;  (uniq-gen x)
  (comment "Initiator point-of-view"))

;; The responder point-of-view
(defskeleton dhns
  (vars (a b name) (x expt) (y z rndx))
  (defstrand resp 5 (a a) (b b) (x x) (y y))
  (non-orig (privk a) (privk b))
;  (uniq-gen y)
  (comment "Responder point-of-view"))
