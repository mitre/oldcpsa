(herald "Station-to-station protocol" (algebra diffie-hellman))

(comment "CPSA 3.6.1")
(comment "All input read from station2.scm")

(defprotocol station-to-station diffie-hellman
  (defrole init
    (vars (x rndx) (y expt) (a b name))
    (trace (send (exp (gen) x))
      (recv (enc (exp (gen) y) (exp (gen) x) (privk b)))
      (send
        (enc (enc (exp (gen) x) (exp (gen) y) (exp (gen) (mul x y)))
          (privk a))))
    (uniq-gen x))
  (defrole resp
    (vars (y rndx) (x expt) (a b name))
    (trace (recv (exp (gen) x))
      (send (enc (exp (gen) y) (exp (gen) x) (privk b)))
      (recv
        (enc (enc (exp (gen) x) (exp (gen) y) (exp (gen) (mul y x)))
          (privk a))))
    (uniq-gen y)
    (absent (y (exp (gen) x)))))

(defskeleton station-to-station
  (vars (a b name) (x rndx) (y expt))
  (defstrand init 3 (a a) (b b) (x x) (y y))
  (non-orig (privk a) (privk b))
  (uniq-gen x)
  (traces
    ((send (exp (gen) x))
      (recv (enc (exp (gen) y) (exp (gen) x) (privk b)))
      (send
        (enc (enc (exp (gen) x) (exp (gen) y) (exp (gen) (mul x y)))
          (privk a)))))
  (label 0)
  (unrealized (0 1))
  (origs)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton station-to-station
  (vars (a b name) (y x rndx))
  (defstrand init 3 (a a) (b b) (x x) (y y))
  (defstrand resp 2 (b b) (y y) (x x))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (absent (y (exp (gen) x)))
  (non-orig (privk a) (privk b))
  (uniq-gen y x)
  (operation encryption-test (added-strand resp 2)
    (enc (exp (gen) y) (exp (gen) x) (privk b)) (0 1))
  (traces
    ((send (exp (gen) x))
      (recv (enc (exp (gen) y) (exp (gen) x) (privk b)))
      (send
        (enc (enc (exp (gen) x) (exp (gen) y) (exp (gen) (mul y x)))
          (privk a))))
    ((recv (exp (gen) x))
      (send (enc (exp (gen) y) (exp (gen) x) (privk b)))))
  (label 1)
  (parent 0)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (x x) (y y))))
  (origs))

(comment "Nothing left to do")

(defprotocol station-to-station diffie-hellman
  (defrole init
    (vars (x rndx) (y expt) (a b name))
    (trace (send (exp (gen) x))
      (recv (enc (exp (gen) y) (exp (gen) x) (privk b)))
      (send
        (enc (enc (exp (gen) x) (exp (gen) y) (exp (gen) (mul x y)))
          (privk a))))
    (uniq-gen x))
  (defrole resp
    (vars (y rndx) (x expt) (a b name))
    (trace (recv (exp (gen) x))
      (send (enc (exp (gen) y) (exp (gen) x) (privk b)))
      (recv
        (enc (enc (exp (gen) x) (exp (gen) y) (exp (gen) (mul y x)))
          (privk a))))
    (uniq-gen y)
    (absent (y (exp (gen) x)))))

(defskeleton station-to-station
  (vars (a b name) (y rndx) (x expt))
  (defstrand resp 3 (a a) (b b) (y y) (x x))
  (absent (y (exp (gen) x)))
  (non-orig (privk a) (privk b))
  (uniq-gen y)
  (traces
    ((recv (exp (gen) x))
      (send (enc (exp (gen) y) (exp (gen) x) (privk b)))
      (recv
        (enc (enc (exp (gen) x) (exp (gen) y) (exp (gen) (mul y x)))
          (privk a)))))
  (label 2)
  (unrealized (0 2))
  (origs)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton station-to-station
  (vars (a b b-0 name) (x y rndx))
  (defstrand resp 3 (a a) (b b) (y y) (x x))
  (defstrand init 3 (a a) (b b-0) (x x) (y y))
  (precedes ((0 1) (1 1)) ((1 0) (0 0)) ((1 2) (0 2)))
  (absent (y (exp (gen) x)))
  (non-orig (privk a) (privk b))
  (uniq-gen x y)
  (operation encryption-test (added-strand init 3)
    (enc (enc (exp (gen) x) (exp (gen) y) (exp (gen) (mul x y)))
      (privk a)) (0 2))
  (traces
    ((recv (exp (gen) x))
      (send (enc (exp (gen) y) (exp (gen) x) (privk b)))
      (recv
        (enc (enc (exp (gen) x) (exp (gen) y) (exp (gen) (mul x y)))
          (privk a))))
    ((send (exp (gen) x))
      (recv (enc (exp (gen) y) (exp (gen) x) (privk b-0)))
      (send
        (enc (enc (exp (gen) x) (exp (gen) y) (exp (gen) (mul x y)))
          (privk a)))))
  (label 3)
  (parent 2)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (y y) (x x))))
  (origs))

(comment "Nothing left to do")
