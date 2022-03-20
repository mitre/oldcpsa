(herald "small test" (algebra diffie-hellman))

(comment "CPSA 3.6.10")
(comment "All input read from tst/test_small.scm")

(defprotocol test diffie-hellman
  (defrole init
    (vars (hr base) (a b name) (xi rndx))
    (trace (recv (enc hr (privk b)))
      (send (enc (hash (exp hr xi)) (exp (gen) xi) (privk a))))
    (uniq-gen xi)
    (absent (xi hr)))
  (defrole resp
    (vars (xr rndx) (a b name) (hi base))
    (trace (send (enc (exp (gen) xr) (privk b)))
      (recv (enc (hash (exp hi xr)) hi (privk a))))
    (uniq-gen xr)))

(defskeleton test
  (vars (a b name) (hi base) (xr rndx))
  (defstrand resp 2 (a a) (b b) (hi hi) (xr xr))
  (non-orig (privk a) (privk b))
  (uniq-gen xr)
  (traces
    ((send (enc (exp (gen) xr) (privk b)))
      (recv (enc (hash (exp hi xr)) hi (privk a)))))
  (label 0)
  (unrealized (0 1))
  (origs)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton test
  (vars (a b b-0 name) (xr xi rndx))
  (defstrand resp 2 (a a) (b b) (hi (exp (gen) xi)) (xr xr))
  (defstrand init 2 (a a) (b b-0) (hr (exp (gen) xr)) (xi xi))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (absent (xi (exp (gen) xr)))
  (non-orig (privk a) (privk b))
  (uniq-gen xr xi)
  (operation encryption-test (added-strand init 2)
    (enc (hash (exp (gen) (mul xr xi))) (exp (gen) xi) (privk a)) (0 1))
  (traces
    ((send (enc (exp (gen) xr) (privk b)))
      (recv
        (enc (hash (exp (gen) (mul xr xi))) (exp (gen) xi) (privk a))))
    ((recv (enc (exp (gen) xr) (privk b-0)))
      (send
        (enc (hash (exp (gen) (mul xr xi))) (exp (gen) xi) (privk a)))))
  (label 1)
  (parent 0)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (xr xr) (hi (exp (gen) xi)))))
  (origs))

(comment "Nothing left to do")
