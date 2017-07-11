(herald "Diffie-Hellman protocol, man-in-the-middle attack"
  (algebra diffie-hellman) (bound 20))

(comment "CPSA 3.2.2")
(comment "All input read from dh_test.scm")
(comment "Strand count bounded at 20")

(defprotocol foo3 diffie-hellman
  (defrole resp (vars (h base)) (trace (recv h)))
  (defrole init (vars (x expn)) (trace (send (exp (gen) x)))))

(defskeleton foo3
  (vars (x expn))
  (defstrand resp 1 (h (exp (gen) x)))
  (non-orig x)
  (traces ((recv (exp (gen) x))))
  (label 0)
  (unrealized (0 0))
  (origs)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton foo3
  (vars (x expn))
  (defstrand resp 1 (h (exp (gen) x)))
  (defstrand init 1 (x x))
  (precedes ((1 0) (0 0)))
  (non-orig x)
  (operation nonce-test (added-strand init 1) (exp (gen) x) (0 0))
  (traces ((recv (exp (gen) x))) ((send (exp (gen) x))))
  (label 1)
  (parent 0)
  (unrealized)
  (shape)
  (maps ((0) ((x x))))
  (origs))

(comment "Nothing left to do")
