(herald precursor (algebra diffie-hellman) (bound 6))

(comment "CPSA 3.6.10")
(comment "All input read from tst/precursor.scm")
(comment "Strand count bounded at 6")

(defprotocol precursor diffie-hellman
  (defrole init
    (vars (x y rndx))
    (trace (send (cat (exp (gen) x) (exp (gen) y)))
      (recv (exp (gen) (mul x y))))
    (uniq-gen x y)))

(defskeleton precursor
  (vars (x y rndx))
  (defstrand init 2 (x x) (y y))
  (uniq-gen x y)
  (traces
    ((send (cat (exp (gen) x) (exp (gen) y)))
      (recv (exp (gen) (mul x y)))))
  (label 0)
  (unrealized (0 1))
  (origs)
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton precursor
  (vars (x y rndx))
  (defstrand init 2 (x x) (y y))
  (deflistener (cat (exp (gen) x) y))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (uniq-gen x y)
  (operation nonce-test (added-listener (cat (exp (gen) x) y))
    (exp (gen) (mul x y)) (0 1))
  (traces
    ((send (cat (exp (gen) x) (exp (gen) y)))
      (recv (exp (gen) (mul x y))))
    ((recv (cat (exp (gen) x) y)) (send (cat (exp (gen) x) y))))
  (label 1)
  (parent 0)
  (unrealized (1 0))
  (dead)
  (comment "empty cohort"))

(defskeleton precursor
  (vars (x y rndx))
  (defstrand init 2 (x x) (y y))
  (deflistener (cat (exp (gen) y) x))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (uniq-gen x y)
  (operation nonce-test (added-listener (cat (exp (gen) y) x))
    (exp (gen) (mul x y)) (0 1))
  (traces
    ((send (cat (exp (gen) x) (exp (gen) y)))
      (recv (exp (gen) (mul x y))))
    ((recv (cat (exp (gen) y) x)) (send (cat (exp (gen) y) x))))
  (label 2)
  (parent 0)
  (unrealized (1 0))
  (dead)
  (comment "empty cohort"))

(comment "Nothing left to do")
