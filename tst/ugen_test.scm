(herald "Ugen test"
	(comment This protocol demonstrates a bug in CPSA: points of
origination are not always preserved for skeleton uniq-orig
assumptions) (algebra diffie-hellman))

(defprotocol uof diffie-hellman
  (defrole init
    (vars (a b name) (x1 rndx) (x2 expt))
    (trace
     (send (exp (gen) x1))
     (recv (exp (gen) x2))
     (send (enc (exp (gen) x1) (exp (gen) x2) (exp (gen) x1) (ltk a b)))))
  (defrole resp
    (vars (b a name) (x2 rndx) (x1 expt))
    (trace
     (recv (exp (gen) x1))
     (send (exp (gen) x2))
     (recv (enc (exp (gen) x2) (exp (gen) x1) (exp (gen) x1) (ltk a b)))))
)

;;; The responder point-of-view
(defskeleton uof
  (vars (a b name) (x2 rndx))
  (defstrand resp 3 (a a) (b b) (x2 x2))
  (non-orig (ltk a b))
  (uniq-gen x2)
  (comment "Responder point-of-view"))

;;; The responder point-of-view
(defskeleton uof
  (vars (a b name) (x2 rndx) (x1 expt))
  (defstrand resp 3 (a a) (b b) (x2 x2) (x1 x1))
  (non-orig (ltk a b))
  (uniq-gen x2)
  (absent (x2 (exp (gen) x1)))
  (comment "Responder point-of-view"))

(defprotocol uof2 diffie-hellman
  (defrole init
    (vars (a b name) (x1 rndx) (x2 expt))
    (trace
     (send (exp (gen) x1))
     (recv (exp (gen) x2))
     (send (enc (exp (gen) x1) (exp (gen) x2) (exp (gen) x1) (ltk a b)))))
  (defrole resp
    (vars (b a name) (x2 rndx) (x1 expt))
    (trace
     (recv (exp (gen) x1))
     (send (exp (gen) x2))
     (recv (enc (exp (gen) x2) (exp (gen) x1) (exp (gen) x1) (ltk a b))))
    (uniq-gen x2)
    )
)
;;; The responder point-of-view
(defskeleton uof2
  (vars (a b name) (x2 rndx))
  (defstrand resp 3 (a a) (b b) (x2 x2))
  (non-orig (ltk a b))
  (comment "Responder point-of-view"))
