(herald "Diffie-Hellman enhanced Needham-Schroeder-Lowe Protocol"
  (algebra diffie-hellman))

(comment "CPSA 3.2.2")
(comment "All input read from dhnsl_basic.scm")

(defprotocol dhnsl diffie-hellman
  (defrole resp
    (vars (b a name) (h1 base) (y expn))
    (trace (recv (enc h1 a (pubk b)))
      (send (enc h1 (exp (gen) y) b (pubk a)))
      (recv (enc (exp (gen) y) (pubk b))))
    (comment "Y and Z should be assumed to be freshly chosen per role"))
  (defrole init
    (vars (a b name) (h2 base) (x expn))
    (trace (send (enc (exp (gen) x) a (pubk b)))
      (recv (enc (exp (gen) x) h2 b (pubk a))) (send (enc h2 (pubk b))))
    (comment "X should be assumed to be freshly chosen per role"))
  (comment
    "Needham-Schroeder-Lowe DH challenge/responses in place of nonces"))

(defskeleton dhnsl
  (vars (a b name) (h2 base) (x expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (x x))
  (non-orig (privk a) (privk b))
  (uniq-gen x)
  (comment "Initiator point-of-view")
  (traces
    ((send (enc (exp (gen) x) a (pubk b)))
      (recv (enc (exp (gen) x) h2 b (pubk a)))
      (send (enc h2 (pubk b)))))
  (label 0)
  (unrealized (0 1))
  (origs)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhnsl
  (vars (a b name) (h2 base) (x y expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (x x))
  (defstrand resp 2 (b b) (a a) (h1 (exp (gen) x)) (y y))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (non-orig (privk a) (privk b))
  (uniq-gen x)
  (operation nonce-test (added-strand resp 2) (exp (gen) x) (0 1)
    (enc (exp (gen) x) a (pubk b)))
  (traces
    ((send (enc (exp (gen) x) a (pubk b)))
      (recv (enc (exp (gen) x) h2 b (pubk a))) (send (enc h2 (pubk b))))
    ((recv (enc (exp (gen) x) a (pubk b)))
      (send (enc (exp (gen) x) (exp (gen) y) b (pubk a)))))
  (label 1)
  (parent 0)
  (unrealized (0 1))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton dhnsl
  (vars (a b name) (x y expn))
  (defstrand init 3 (a a) (b b) (h2 (exp (gen) y)) (x x))
  (defstrand resp 2 (b b) (a a) (h1 (exp (gen) x)) (y y))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (non-orig (privk a) (privk b))
  (uniq-gen x)
  (operation nonce-test (contracted (h2 (exp (gen) y))) (exp (gen) x)
    (0 1) (enc (exp (gen) x) a (pubk b))
    (enc (exp (gen) x) (exp (gen) y) b (pubk a)))
  (traces
    ((send (enc (exp (gen) x) a (pubk b)))
      (recv (enc (exp (gen) x) (exp (gen) y) b (pubk a)))
      (send (enc (exp (gen) y) (pubk b))))
    ((recv (enc (exp (gen) x) a (pubk b)))
      (send (enc (exp (gen) x) (exp (gen) y) b (pubk a)))))
  (label 2)
  (parent 1)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (h2 (exp (gen) y)) (x x))))
  (origs))

(defskeleton dhnsl
  (vars (a b name) (h2 base) (y expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (x y))
  (defstrand resp 2 (b b) (a a) (h1 (exp (gen) y)) (y y))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (non-orig (privk a) (privk b))
  (uniq-gen y)
  (operation nonce-test (displaced 2 1 resp 2) (exp (gen) y-0) (0 1)
    (enc (exp (gen) y-0) a (pubk b))
    (enc (exp (gen) y-0) (exp (gen) y-1) b (pubk a)))
  (traces
    ((send (enc (exp (gen) y) a (pubk b)))
      (recv (enc (exp (gen) y) h2 b (pubk a))) (send (enc h2 (pubk b))))
    ((recv (enc (exp (gen) y) a (pubk b)))
      (send (enc (exp (gen) y) (exp (gen) y) b (pubk a)))))
  (label 3)
  (parent 1)
  (unrealized (0 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhnsl
  (vars (a b name) (y expn))
  (defstrand init 3 (a a) (b b) (h2 (exp (gen) y)) (x y))
  (defstrand resp 2 (b b) (a a) (h1 (exp (gen) y)) (y y))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (non-orig (privk a) (privk b))
  (uniq-gen y)
  (operation nonce-test (contracted (h2 (exp (gen) y))) (exp (gen) y)
    (0 1) (enc (exp (gen) y) a (pubk b))
    (enc (exp (gen) y) (exp (gen) y) b (pubk a)))
  (traces
    ((send (enc (exp (gen) y) a (pubk b)))
      (recv (enc (exp (gen) y) (exp (gen) y) b (pubk a)))
      (send (enc (exp (gen) y) (pubk b))))
    ((recv (enc (exp (gen) y) a (pubk b)))
      (send (enc (exp (gen) y) (exp (gen) y) b (pubk a)))))
  (label 4)
  (parent 3)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (h2 (exp (gen) y)) (x y))))
  (origs))

(comment "Nothing left to do")

(defprotocol dhnsl diffie-hellman
  (defrole resp
    (vars (b a name) (h1 base) (y expn))
    (trace (recv (enc h1 a (pubk b)))
      (send (enc h1 (exp (gen) y) b (pubk a)))
      (recv (enc (exp (gen) y) (pubk b))))
    (comment "Y and Z should be assumed to be freshly chosen per role"))
  (defrole init
    (vars (a b name) (h2 base) (x expn))
    (trace (send (enc (exp (gen) x) a (pubk b)))
      (recv (enc (exp (gen) x) h2 b (pubk a))) (send (enc h2 (pubk b))))
    (comment "X should be assumed to be freshly chosen per role"))
  (comment
    "Needham-Schroeder-Lowe DH challenge/responses in place of nonces"))

(defskeleton dhnsl
  (vars (a b name) (h1 base) (y expn))
  (defstrand resp 3 (b b) (a a) (h1 h1) (y y))
  (non-orig (privk a))
  (uniq-gen y)
  (comment "Responder point-of-view")
  (traces
    ((recv (enc h1 a (pubk b))) (send (enc h1 (exp (gen) y) b (pubk a)))
      (recv (enc (exp (gen) y) (pubk b)))))
  (label 5)
  (unrealized (0 2))
  (origs)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhnsl
  (vars (a b name) (y x expn))
  (defstrand resp 3 (b b) (a a) (h1 (exp (gen) x)) (y y))
  (defstrand init 3 (a a) (b b) (h2 (exp (gen) y)) (x x))
  (precedes ((0 1) (1 1)) ((1 2) (0 2)))
  (non-orig (privk a))
  (uniq-gen y)
  (operation nonce-test (added-strand init 3) (exp (gen) y) (0 2)
    (enc (exp (gen) x) (exp (gen) y) b (pubk a)))
  (traces
    ((recv (enc (exp (gen) x) a (pubk b)))
      (send (enc (exp (gen) x) (exp (gen) y) b (pubk a)))
      (recv (enc (exp (gen) y) (pubk b))))
    ((send (enc (exp (gen) x) a (pubk b)))
      (recv (enc (exp (gen) x) (exp (gen) y) b (pubk a)))
      (send (enc (exp (gen) y) (pubk b)))))
  (label 6)
  (parent 5)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (h1 (exp (gen) x)) (y y))))
  (origs))

(comment "Nothing left to do")
