(herald "Ugen test"
  (comment This protocol demonstrates a bug in CPSA: points of
    origination are not always preserved for skeleton uniq-orig
    assumptions) (algebra diffie-hellman))

(comment "CPSA 3.6.8")
(comment "All input read from tst/ugen_test.scm")

(defprotocol uof diffie-hellman
  (defrole init
    (vars (a b name) (x1 rndx) (x2 expt))
    (trace (send (exp (gen) x1)) (recv (exp (gen) x2))
      (send
        (enc (exp (gen) x1) (exp (gen) x2) (exp (gen) x1) (ltk a b)))))
  (defrole resp
    (vars (b a name) (x2 rndx) (x1 expt))
    (trace (recv (exp (gen) x1)) (send (exp (gen) x2))
      (recv
        (enc (exp (gen) x2) (exp (gen) x1) (exp (gen) x1) (ltk a b))))))

(defskeleton uof
  (vars (a b name) (x2 rndx) (x1 expt))
  (defstrand resp 3 (b b) (a a) (x2 x2) (x1 x1))
  (non-orig (ltk a b))
  (uniq-gen x2)
  (comment "Responder point-of-view")
  (traces
    ((recv (exp (gen) x1)) (send (exp (gen) x2))
      (recv
        (enc (exp (gen) x2) (exp (gen) x1) (exp (gen) x1) (ltk a b)))))
  (label 0)
  (unrealized (0 2))
  (origs)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton uof
  (vars (a b name) (x1 rndx))
  (defstrand resp 3 (b b) (a a) (x2 x1) (x1 x1))
  (defstrand init 3 (a a) (b b) (x1 x1) (x2 x1))
  (precedes ((1 0) (0 0)) ((1 2) (0 2)))
  (non-orig (ltk a b))
  (uniq-gen x1)
  (operation encryption-test (added-strand init 3)
    (enc (exp (gen) x1) (exp (gen) x1) (exp (gen) x1) (ltk a b)) (0 2))
  (traces
    ((recv (exp (gen) x1)) (send (exp (gen) x1))
      (recv
        (enc (exp (gen) x1) (exp (gen) x1) (exp (gen) x1) (ltk a b))))
    ((send (exp (gen) x1)) (recv (exp (gen) x1))
      (send
        (enc (exp (gen) x1) (exp (gen) x1) (exp (gen) x1) (ltk a b)))))
  (label 1)
  (parent 0)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (x2 x1) (x1 x1))))
  (origs))

(comment "Nothing left to do")

(defprotocol uof diffie-hellman
  (defrole init
    (vars (a b name) (x1 rndx) (x2 expt))
    (trace (send (exp (gen) x1)) (recv (exp (gen) x2))
      (send
        (enc (exp (gen) x1) (exp (gen) x2) (exp (gen) x1) (ltk a b)))))
  (defrole resp
    (vars (b a name) (x2 rndx) (x1 expt))
    (trace (recv (exp (gen) x1)) (send (exp (gen) x2))
      (recv
        (enc (exp (gen) x2) (exp (gen) x1) (exp (gen) x1) (ltk a b))))))

(defskeleton uof
  (vars (a b name) (x2 rndx) (x1 expt))
  (defstrand resp 3 (b b) (a a) (x2 x2) (x1 x1))
  (absent (x2 (exp (gen) x1)))
  (non-orig (ltk a b))
  (uniq-gen x2)
  (comment "Responder point-of-view")
  (traces
    ((recv (exp (gen) x1)) (send (exp (gen) x2))
      (recv
        (enc (exp (gen) x2) (exp (gen) x1) (exp (gen) x1) (ltk a b)))))
  (label 2)
  (unrealized (0 2))
  (dead)
  (origs)
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol uof2 diffie-hellman
  (defrole init
    (vars (a b name) (x1 rndx) (x2 expt))
    (trace (send (exp (gen) x1)) (recv (exp (gen) x2))
      (send
        (enc (exp (gen) x1) (exp (gen) x2) (exp (gen) x1) (ltk a b)))))
  (defrole resp
    (vars (b a name) (x2 rndx) (x1 expt))
    (trace (recv (exp (gen) x1)) (send (exp (gen) x2))
      (recv
        (enc (exp (gen) x2) (exp (gen) x1) (exp (gen) x1) (ltk a b))))
    (uniq-gen x2)
    (absent (x2 (exp (gen) x1)))))

(defskeleton uof2
  (vars (a b name) (x2 rndx) (x1 expt))
  (defstrand resp 3 (b b) (a a) (x2 x2) (x1 x1))
  (absent (x2 (exp (gen) x1)))
  (non-orig (ltk a b))
  (uniq-gen x2)
  (comment "Responder point-of-view")
  (traces
    ((recv (exp (gen) x1)) (send (exp (gen) x2))
      (recv
        (enc (exp (gen) x2) (exp (gen) x1) (exp (gen) x1) (ltk a b)))))
  (label 3)
  (unrealized (0 2))
  (dead)
  (origs)
  (comment "empty cohort"))

(comment "Nothing left to do")
