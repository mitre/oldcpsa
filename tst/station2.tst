(herald "Station-to-station protocol" (algebra diffie-hellman))

(comment "CPSA 3.2.2")
(comment "All input read from station2.scm")

(defprotocol station-to-station diffie-hellman
  (defrole init
    (vars (x expn) (h base) (a b name))
    (trace (send (exp (gen) x)) (recv (enc h (exp (gen) x) (privk b)))
      (send (enc (enc (exp (gen) x) h (exp h x)) (privk a))))
    (uniq-gen x))
  (defrole resp
    (vars (y expn) (h base) (a b name))
    (trace (recv h) (send (enc (exp (gen) y) h (privk b)))
      (recv (enc (enc h (exp (gen) y) (exp h y)) (privk a))))
    (uniq-gen y)
    (ind-zero-in (y h))))

(defskeleton station-to-station
  (vars (a b name) (h base) (x expn))
  (defstrand init 3 (a a) (b b) (h h) (x x))
  (non-orig (privk a) (privk b))
  (uniq-gen x)
  (traces
    ((send (exp (gen) x)) (recv (enc h (exp (gen) x) (privk b)))
      (send (enc (enc (exp (gen) x) h (exp h x)) (privk a)))))
  (label 0)
  (unrealized (0 1))
  (origs)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton station-to-station
  (vars (a b name) (x y expn))
  (defstrand init 3 (a a) (b b) (h (exp (gen) y)) (x x))
  (defstrand resp 2 (b b) (h (exp (gen) x)) (y y))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (ind-zero-in (y (exp (gen) x)))
  (non-orig (privk a) (privk b))
  (uniq-gen x y)
  (operation encryption-test (added-strand resp 2)
    (enc (exp (gen) y) (exp (gen) x) (privk b)) (0 1))
  (traces
    ((send (exp (gen) x))
      (recv (enc (exp (gen) y) (exp (gen) x) (privk b)))
      (send
        (enc (enc (exp (gen) x) (exp (gen) y) (exp (gen) (mul x y)))
          (privk a))))
    ((recv (exp (gen) x))
      (send (enc (exp (gen) y) (exp (gen) x) (privk b)))))
  (label 1)
  (parent 0)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (x x) (h (exp (gen) y)))))
  (origs))

(comment "Nothing left to do")

(defprotocol station-to-station diffie-hellman
  (defrole init
    (vars (x expn) (h base) (a b name))
    (trace (send (exp (gen) x)) (recv (enc h (exp (gen) x) (privk b)))
      (send (enc (enc (exp (gen) x) h (exp h x)) (privk a))))
    (uniq-gen x))
  (defrole resp
    (vars (y expn) (h base) (a b name))
    (trace (recv h) (send (enc (exp (gen) y) h (privk b)))
      (recv (enc (enc h (exp (gen) y) (exp h y)) (privk a))))
    (uniq-gen y)
    (ind-zero-in (y h))))

(defskeleton station-to-station
  (vars (a b name) (h base) (y expn))
  (defstrand resp 3 (a a) (b b) (h h) (y y))
  (ind-zero-in (y h))
  (non-orig (privk a) (privk b))
  (uniq-gen y)
  (traces
    ((recv h) (send (enc (exp (gen) y) h (privk b)))
      (recv (enc (enc h (exp (gen) y) (exp h y)) (privk a)))))
  (label 2)
  (unrealized (0 2))
  (origs)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton station-to-station
  (vars (a b b-0 name) (y x expn))
  (defstrand resp 3 (a a) (b b) (h (exp (gen) x)) (y y))
  (defstrand init 3 (a a) (b b-0) (h (exp (gen) y)) (x x))
  (precedes ((0 1) (1 1)) ((1 0) (0 0)) ((1 2) (0 2)))
  (ind-zero-in (y (exp (gen) x)))
  (non-orig (privk a) (privk b))
  (uniq-gen y x)
  (operation encryption-test (added-strand init 3)
    (enc (enc (exp (gen) x) (exp (gen) y) (exp (gen) (mul y x)))
      (privk a)) (0 2))
  (traces
    ((recv (exp (gen) x))
      (send (enc (exp (gen) y) (exp (gen) x) (privk b)))
      (recv
        (enc (enc (exp (gen) x) (exp (gen) y) (exp (gen) (mul y x)))
          (privk a))))
    ((send (exp (gen) x))
      (recv (enc (exp (gen) y) (exp (gen) x) (privk b-0)))
      (send
        (enc (enc (exp (gen) x) (exp (gen) y) (exp (gen) (mul y x)))
          (privk a)))))
  (label 3)
  (parent 2)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (y y) (h (exp (gen) x)))))
  (origs))

(comment "Nothing left to do")
