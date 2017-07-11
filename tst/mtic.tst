(herald "MTI C(0) protocol" (algebra diffie-hellman))

(comment "CPSA 3.2.2")
(comment "All input read from mtic.scm")

(defprotocol mtic diffie-hellman
  (defrole init
    (vars (a b name) (h2 h3 base) (x expn))
    (trace (send (enc (exp (gen) x) a (pubk b)))
      (recv (enc h2 (exp h2 x) h3 b (pubk a)))
      (send (enc (exp h3 x) (pubk b))))
    (uniq-gen x)
    (comment "X should be assumed to be freshly chosen per role"))
  (defrole resp
    (vars (b a name) (h1 base) (y z expn))
    (trace (recv (enc h1 a (pubk b)))
      (send (enc (exp (gen) y) (exp h1 y) (exp (gen) z) b (pubk a)))
      (recv (enc (exp h1 z) (pubk b))))
    (uniq-gen y z)
    (ind-zero-in (y h1) (z h1))
    (comment "Y and Z should be assumed to be freshly chosen per role"))
  (comment
    "Needham-Schroeder-Lowe DH challenge/responses in place of nonces"))

(defskeleton mtic
  (vars (a b name) (h2 h3 base) (x expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x))
  (non-orig (privk a) (privk b))
  (uniq-gen x)
  (comment "Initiator point-of-view")
  (traces
    ((send (enc (exp (gen) x) a (pubk b)))
      (recv (enc h2 (exp h2 x) h3 b (pubk a)))
      (send (enc (exp h3 x) (pubk b)))))
  (label 0)
  (unrealized (0 1))
  (origs)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton mtic
  (vars (a b name) (h3 base) (x expr) (x-0 expn))
  (defstrand init 3 (a a) (b b) (h2 (exp (gen) (rec x))) (h3 h3)
    (x x-0))
  (non-orig (privk a) (privk b))
  (uniq-gen x-0)
  (operation nonce-test (displaced 1 0 init 1)
    (exp (gen) (mul (rec x) x-1)) (0 1)
    (enc (exp (gen) x-1) a (pubk b)))
  (traces
    ((send (enc (exp (gen) x-0) a (pubk b)))
      (recv
        (enc (exp (gen) (rec x)) (exp (gen) (mul (rec x) x-0)) h3 b
          (pubk a))) (send (enc (exp h3 x-0) (pubk b)))))
  (label 1)
  (parent 0)
  (seen 1)
  (unrealized (0 1))
  (origs)
  (comment "1 in cohort - 0 not yet seen"))

(comment "Nothing left to do")

(defprotocol mtic diffie-hellman
  (defrole init
    (vars (a b name) (h2 h3 base) (x expn))
    (trace (send (enc (exp (gen) x) a (pubk b)))
      (recv (enc h2 (exp h2 x) h3 b (pubk a)))
      (send (enc (exp h3 x) (pubk b))))
    (uniq-gen x)
    (comment "X should be assumed to be freshly chosen per role"))
  (defrole resp
    (vars (b a name) (h1 base) (y z expn))
    (trace (recv (enc h1 a (pubk b)))
      (send (enc (exp (gen) y) (exp h1 y) (exp (gen) z) b (pubk a)))
      (recv (enc (exp h1 z) (pubk b))))
    (uniq-gen y z)
    (ind-zero-in (y h1) (z h1))
    (comment "Y and Z should be assumed to be freshly chosen per role"))
  (comment
    "Needham-Schroeder-Lowe DH challenge/responses in place of nonces"))

(defskeleton mtic
  (vars (a b name) (h1 base) (y z expn))
  (defstrand resp 3 (b b) (a a) (h1 h1) (y y) (z z))
  (ind-zero-in (y h1) (z h1))
  (non-orig (privk a))
  (uniq-gen y z)
  (comment "Responder point-of-view")
  (traces
    ((recv (enc h1 a (pubk b)))
      (send (enc (exp (gen) y) (exp h1 y) (exp (gen) z) b (pubk a)))
      (recv (enc (exp h1 z) (pubk b)))))
  (label 2)
  (unrealized (0 2))
  (origs)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton mtic
  (vars (a b name) (z y x expn))
  (defstrand resp 3 (b b) (a a) (h1 (exp (gen) x)) (y y) (z z))
  (defstrand init 3 (a a) (b b) (h2 (exp (gen) y)) (h3 (exp (gen) z))
    (x x))
  (precedes ((0 1) (1 1)) ((1 0) (0 0)) ((1 2) (0 2)))
  (ind-zero-in (y (exp (gen) x)) (z (exp (gen) x)))
  (non-orig (privk a))
  (uniq-gen z y x)
  (operation nonce-test (added-strand init 3) (exp (gen) (mul z x))
    (0 2)
    (enc (exp (gen) y) (exp (gen) (mul y x)) (exp (gen) z) b (pubk a)))
  (traces
    ((recv (enc (exp (gen) x) a (pubk b)))
      (send
        (enc (exp (gen) y) (exp (gen) (mul y x)) (exp (gen) z) b
          (pubk a))) (recv (enc (exp (gen) (mul z x)) (pubk b))))
    ((send (enc (exp (gen) x) a (pubk b)))
      (recv
        (enc (exp (gen) y) (exp (gen) (mul y x)) (exp (gen) z) b
          (pubk a))) (send (enc (exp (gen) (mul z x)) (pubk b)))))
  (label 3)
  (parent 2)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (h1 (exp (gen) x)) (y y) (z z))))
  (origs))

(comment "Nothing left to do")
