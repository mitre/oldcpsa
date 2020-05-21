(herald "Station-to-station protocol" (algebra diffie-hellman))

(comment "CPSA 3.6.6")
(comment "All input read from tst/station.scm")

(defprotocol station-to-station diffie-hellman
  (defrole init
    (vars (x rndx) (y expt) (a b name))
    (trace (send (exp (gen) x))
      (recv
        (cat (exp (gen) y)
          (enc (enc (exp (gen) y) (exp (gen) x) (privk b))
            (exp (gen) (mul x y)))))
      (send
        (enc (enc (exp (gen) x) (exp (gen) y) (privk a))
          (exp (gen) (mul x y)))))
    (uniq-gen x))
  (defrole resp
    (vars (y rndx) (x expt) (a b name))
    (trace (recv (exp (gen) x))
      (send
        (cat (exp (gen) y)
          (enc (enc (exp (gen) y) (exp (gen) x) (privk b))
            (exp (gen) (mul y x)))))
      (recv
        (enc (enc (exp (gen) x) (exp (gen) y) (privk a))
          (exp (gen) (mul y x)))))
    (uniq-gen y)
    (absent (y (exp (gen) x)))))

(defskeleton station-to-station
  (vars (a b name) (x rndx) (y expt))
  (defstrand init 3 (a a) (b b) (x x) (y y))
  (non-orig (privk a) (privk b))
  (uniq-gen x)
  (traces
    ((send (exp (gen) x))
      (recv
        (cat (exp (gen) y)
          (enc (enc (exp (gen) y) (exp (gen) x) (privk b))
            (exp (gen) (mul x y)))))
      (send
        (enc (enc (exp (gen) x) (exp (gen) y) (privk a))
          (exp (gen) (mul x y))))))
  (label 0)
  (unrealized (0 1))
  (origs)
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton station-to-station
  (vars (a b b-0 name) (x x-0 rndx))
  (defstrand init 3 (a a) (b b) (x x-0) (y x))
  (defstrand init 3 (a b) (b b-0) (x x) (y x-0))
  (precedes ((0 0) (1 1)) ((1 2) (0 1)))
  (non-orig (privk a) (privk b))
  (uniq-gen x x-0)
  (operation encryption-test (added-strand init 3)
    (enc (exp (gen) x) (exp (gen) x-0) (privk b)) (0 1))
  (traces
    ((send (exp (gen) x-0))
      (recv
        (cat (exp (gen) x)
          (enc (enc (exp (gen) x) (exp (gen) x-0) (privk b))
            (exp (gen) (mul x x-0)))))
      (send
        (enc (enc (exp (gen) x-0) (exp (gen) x) (privk a))
          (exp (gen) (mul x x-0)))))
    ((send (exp (gen) x))
      (recv
        (cat (exp (gen) x-0)
          (enc (enc (exp (gen) x-0) (exp (gen) x) (privk b-0))
            (exp (gen) (mul x x-0)))))
      (send
        (enc (enc (exp (gen) x) (exp (gen) x-0) (privk b))
          (exp (gen) (mul x x-0))))))
  (label 1)
  (parent 0)
  (unrealized (1 1))
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
      (recv
        (cat (exp (gen) y)
          (enc (enc (exp (gen) y) (exp (gen) x) (privk b))
            (exp (gen) (mul y x)))))
      (send
        (enc (enc (exp (gen) x) (exp (gen) y) (privk a))
          (exp (gen) (mul y x)))))
    ((recv (exp (gen) x))
      (send
        (cat (exp (gen) y)
          (enc (enc (exp (gen) y) (exp (gen) x) (privk b))
            (exp (gen) (mul y x)))))))
  (label 2)
  (parent 0)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (x x) (y y))))
  (origs))

(defskeleton station-to-station
  (vars (a b b-0 name) (x x-0 rndx))
  (defstrand init 3 (a a) (b b) (x x-0) (y x))
  (defstrand init 3 (a b) (b b-0) (x x) (y x-0))
  (deflistener (exp (gen) (mul x x-0)))
  (precedes ((0 0) (2 0)) ((1 0) (2 0)) ((1 2) (0 1)) ((2 1) (1 1)))
  (non-orig (privk a) (privk b))
  (uniq-gen x x-0)
  (operation encryption-test (added-listener (exp (gen) (mul x x-0)))
    (enc (enc (exp (gen) x-0) (exp (gen) x) (privk b-0))
      (exp (gen) (mul x x-0))) (1 1))
  (traces
    ((send (exp (gen) x-0))
      (recv
        (cat (exp (gen) x)
          (enc (enc (exp (gen) x) (exp (gen) x-0) (privk b))
            (exp (gen) (mul x x-0)))))
      (send
        (enc (enc (exp (gen) x-0) (exp (gen) x) (privk a))
          (exp (gen) (mul x x-0)))))
    ((send (exp (gen) x))
      (recv
        (cat (exp (gen) x-0)
          (enc (enc (exp (gen) x-0) (exp (gen) x) (privk b-0))
            (exp (gen) (mul x x-0)))))
      (send
        (enc (enc (exp (gen) x) (exp (gen) x-0) (privk b))
          (exp (gen) (mul x x-0)))))
    ((recv (exp (gen) (mul x x-0))) (send (exp (gen) (mul x x-0)))))
  (label 3)
  (parent 1)
  (unrealized (2 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton station-to-station
  (vars (a b b-0 name) (x x-0 rndx) (w expt))
  (defstrand init 3 (a a) (b b) (x x-0) (y x))
  (defstrand init 3 (a b) (b b-0) (x x) (y x-0))
  (deflistener (exp (gen) (mul x x-0)))
  (deflistener (cat (exp (gen) (mul x x-0 (rec w))) w))
  (precedes ((0 0) (3 0)) ((1 0) (3 0)) ((1 2) (0 1)) ((2 1) (1 1))
    ((3 1) (2 0)))
  (non-orig (privk a) (privk b))
  (uniq-gen x x-0)
  (precur (3 0))
  (operation nonce-test
    (added-listener (cat (exp (gen) (mul x x-0 (rec w))) w))
    (exp (gen) (mul x x-0)) (2 0))
  (traces
    ((send (exp (gen) x-0))
      (recv
        (cat (exp (gen) x)
          (enc (enc (exp (gen) x) (exp (gen) x-0) (privk b))
            (exp (gen) (mul x x-0)))))
      (send
        (enc (enc (exp (gen) x-0) (exp (gen) x) (privk a))
          (exp (gen) (mul x x-0)))))
    ((send (exp (gen) x))
      (recv
        (cat (exp (gen) x-0)
          (enc (enc (exp (gen) x-0) (exp (gen) x) (privk b-0))
            (exp (gen) (mul x x-0)))))
      (send
        (enc (enc (exp (gen) x) (exp (gen) x-0) (privk b))
          (exp (gen) (mul x x-0)))))
    ((recv (exp (gen) (mul x x-0))) (send (exp (gen) (mul x x-0))))
    ((recv (cat (exp (gen) (mul x x-0 (rec w))) w))
      (send (cat (exp (gen) (mul x x-0 (rec w))) w))))
  (label 4)
  (parent 3)
  (unrealized (3 0))
  (comment "5 in cohort - 5 not yet seen"))

(defskeleton station-to-station
  (vars (a b b-0 name) (x x-0 rndx))
  (defstrand init 3 (a a) (b b) (x x-0) (y x))
  (defstrand init 3 (a b) (b b-0) (x x) (y x-0))
  (deflistener (exp (gen) (mul x x-0)))
  (deflistener (cat (gen) (mul x x-0)))
  (precedes ((0 0) (3 0)) ((1 0) (3 0)) ((1 2) (0 1)) ((2 1) (1 1))
    ((3 1) (2 0)))
  (non-orig (privk a) (privk b))
  (uniq-gen x x-0)
  (precur (3 0))
  (operation nonce-test (contracted (x-1 x) (x-2 x-0) (w (mul x x-0)))
    (gen) (3 0))
  (traces
    ((send (exp (gen) x-0))
      (recv
        (cat (exp (gen) x)
          (enc (enc (exp (gen) x) (exp (gen) x-0) (privk b))
            (exp (gen) (mul x x-0)))))
      (send
        (enc (enc (exp (gen) x-0) (exp (gen) x) (privk a))
          (exp (gen) (mul x x-0)))))
    ((send (exp (gen) x))
      (recv
        (cat (exp (gen) x-0)
          (enc (enc (exp (gen) x-0) (exp (gen) x) (privk b-0))
            (exp (gen) (mul x x-0)))))
      (send
        (enc (enc (exp (gen) x) (exp (gen) x-0) (privk b))
          (exp (gen) (mul x x-0)))))
    ((recv (exp (gen) (mul x x-0))) (send (exp (gen) (mul x x-0))))
    ((recv (cat (gen) (mul x x-0))) (send (cat (gen) (mul x x-0)))))
  (label 5)
  (parent 4)
  (unrealized (2 0) (3 0))
  (dead)
  (comment "empty cohort"))

(defskeleton station-to-station
  (vars (a b b-0 name) (x x-0 rndx))
  (defstrand init 3 (a a) (b b) (x x-0) (y x))
  (defstrand init 3 (a b) (b b-0) (x x) (y x-0))
  (deflistener (exp (gen) (mul x x-0)))
  (deflistener (cat (exp (gen) x-0) x))
  (precedes ((0 0) (3 0)) ((1 0) (3 0)) ((1 2) (0 1)) ((2 1) (1 1))
    ((3 1) (2 0)))
  (non-orig (privk a) (privk b))
  (precur (3 0))
  (uniq-gen x x-0)
  (operation nonce-test (displaced 4 0 init 1) (exp (gen) x-1) (3 0))
  (traces
    ((send (exp (gen) x-0))
      (recv
        (cat (exp (gen) x)
          (enc (enc (exp (gen) x) (exp (gen) x-0) (privk b))
            (exp (gen) (mul x x-0)))))
      (send
        (enc (enc (exp (gen) x-0) (exp (gen) x) (privk a))
          (exp (gen) (mul x x-0)))))
    ((send (exp (gen) x))
      (recv
        (cat (exp (gen) x-0)
          (enc (enc (exp (gen) x-0) (exp (gen) x) (privk b-0))
            (exp (gen) (mul x x-0)))))
      (send
        (enc (enc (exp (gen) x) (exp (gen) x-0) (privk b))
          (exp (gen) (mul x x-0)))))
    ((recv (exp (gen) (mul x x-0))) (send (exp (gen) (mul x x-0))))
    ((recv (cat (exp (gen) x-0) x)) (send (cat (exp (gen) x-0) x))))
  (label 6)
  (parent 4)
  (unrealized (3 0))
  (dead)
  (comment "empty cohort"))

(defskeleton station-to-station
  (vars (a b b-0 name) (x x-0 rndx))
  (defstrand init 3 (a a) (b b) (x x) (y x-0))
  (defstrand init 3 (a b) (b b-0) (x x-0) (y x))
  (deflistener (exp (gen) (mul x x-0)))
  (deflistener (cat (exp (gen) x-0) x))
  (precedes ((0 0) (3 0)) ((1 0) (3 0)) ((1 2) (0 1)) ((2 1) (1 1))
    ((3 1) (2 0)))
  (non-orig (privk a) (privk b))
  (precur (3 0))
  (uniq-gen x x-0)
  (operation nonce-test (displaced 4 1 init 1) (exp (gen) x-1) (3 0))
  (traces
    ((send (exp (gen) x))
      (recv
        (cat (exp (gen) x-0)
          (enc (enc (exp (gen) x-0) (exp (gen) x) (privk b))
            (exp (gen) (mul x x-0)))))
      (send
        (enc (enc (exp (gen) x) (exp (gen) x-0) (privk a))
          (exp (gen) (mul x x-0)))))
    ((send (exp (gen) x-0))
      (recv
        (cat (exp (gen) x)
          (enc (enc (exp (gen) x) (exp (gen) x-0) (privk b-0))
            (exp (gen) (mul x x-0)))))
      (send
        (enc (enc (exp (gen) x-0) (exp (gen) x) (privk b))
          (exp (gen) (mul x x-0)))))
    ((recv (exp (gen) (mul x x-0))) (send (exp (gen) (mul x x-0))))
    ((recv (cat (exp (gen) x-0) x)) (send (cat (exp (gen) x-0) x))))
  (label 7)
  (parent 4)
  (unrealized (3 0))
  (dead)
  (comment "empty cohort"))

(defskeleton station-to-station
  (vars (a b b-0 name) (x x-0 x-1 rndx))
  (defstrand init 3 (a a) (b b) (x x-0) (y x))
  (defstrand init 3 (a b) (b b-0) (x x) (y x-0))
  (deflistener (exp (gen) (mul x x-0)))
  (deflistener (cat (exp (gen) x-1) (mul x x-0 (rec x-1))))
  (defstrand init 1 (x x-1))
  (precedes ((0 0) (3 0)) ((1 0) (3 0)) ((1 2) (0 1)) ((2 1) (1 1))
    ((3 1) (2 0)) ((4 0) (3 0)))
  (non-orig (privk a) (privk b))
  (precur (3 0))
  (uniq-gen x x-0 x-1)
  (operation nonce-test (added-strand init 1) (exp (gen) x-1) (3 0))
  (traces
    ((send (exp (gen) x-0))
      (recv
        (cat (exp (gen) x)
          (enc (enc (exp (gen) x) (exp (gen) x-0) (privk b))
            (exp (gen) (mul x x-0)))))
      (send
        (enc (enc (exp (gen) x-0) (exp (gen) x) (privk a))
          (exp (gen) (mul x x-0)))))
    ((send (exp (gen) x))
      (recv
        (cat (exp (gen) x-0)
          (enc (enc (exp (gen) x-0) (exp (gen) x) (privk b-0))
            (exp (gen) (mul x x-0)))))
      (send
        (enc (enc (exp (gen) x) (exp (gen) x-0) (privk b))
          (exp (gen) (mul x x-0)))))
    ((recv (exp (gen) (mul x x-0))) (send (exp (gen) (mul x x-0))))
    ((recv (cat (exp (gen) x-1) (mul x x-0 (rec x-1))))
      (send (cat (exp (gen) x-1) (mul x x-0 (rec x-1)))))
    ((send (exp (gen) x-1))))
  (label 8)
  (parent 4)
  (unrealized (2 0) (3 0))
  (dead)
  (comment "empty cohort"))

(defskeleton station-to-station
  (vars (a b b-0 b-1 name) (x expt) (x-0 x-1 y rndx))
  (defstrand init 3 (a a) (b b) (x x-1) (y x-0))
  (defstrand init 3 (a b) (b b-0) (x x-0) (y x-1))
  (deflistener (exp (gen) (mul x-0 x-1)))
  (deflistener (cat (exp (gen) y) (mul x-0 x-1 (rec y))))
  (defstrand resp 2 (b b-1) (y y) (x x))
  (precedes ((0 0) (3 0)) ((1 0) (3 0)) ((1 2) (0 1)) ((2 1) (1 1))
    ((3 1) (2 0)) ((4 1) (3 0)))
  (absent (y (exp (gen) x)))
  (non-orig (privk a) (privk b))
  (precur (3 0))
  (uniq-gen x-0 x-1 y)
  (operation nonce-test (added-strand resp 2) (exp (gen) y) (3 0))
  (traces
    ((send (exp (gen) x-1))
      (recv
        (cat (exp (gen) x-0)
          (enc (enc (exp (gen) x-0) (exp (gen) x-1) (privk b))
            (exp (gen) (mul x-0 x-1)))))
      (send
        (enc (enc (exp (gen) x-1) (exp (gen) x-0) (privk a))
          (exp (gen) (mul x-0 x-1)))))
    ((send (exp (gen) x-0))
      (recv
        (cat (exp (gen) x-1)
          (enc (enc (exp (gen) x-1) (exp (gen) x-0) (privk b-0))
            (exp (gen) (mul x-0 x-1)))))
      (send
        (enc (enc (exp (gen) x-0) (exp (gen) x-1) (privk b))
          (exp (gen) (mul x-0 x-1)))))
    ((recv (exp (gen) (mul x-0 x-1))) (send (exp (gen) (mul x-0 x-1))))
    ((recv (cat (exp (gen) y) (mul x-0 x-1 (rec y))))
      (send (cat (exp (gen) y) (mul x-0 x-1 (rec y)))))
    ((recv (exp (gen) x))
      (send
        (cat (exp (gen) y)
          (enc (enc (exp (gen) y) (exp (gen) x) (privk b-1))
            (exp (gen) (mul x y)))))))
  (label 9)
  (parent 4)
  (unrealized (2 0) (3 0))
  (dead)
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol station-to-station diffie-hellman
  (defrole init
    (vars (x rndx) (y expt) (a b name))
    (trace (send (exp (gen) x))
      (recv
        (cat (exp (gen) y)
          (enc (enc (exp (gen) y) (exp (gen) x) (privk b))
            (exp (gen) (mul x y)))))
      (send
        (enc (enc (exp (gen) x) (exp (gen) y) (privk a))
          (exp (gen) (mul x y)))))
    (uniq-gen x))
  (defrole resp
    (vars (y rndx) (x expt) (a b name))
    (trace (recv (exp (gen) x))
      (send
        (cat (exp (gen) y)
          (enc (enc (exp (gen) y) (exp (gen) x) (privk b))
            (exp (gen) (mul y x)))))
      (recv
        (enc (enc (exp (gen) x) (exp (gen) y) (privk a))
          (exp (gen) (mul y x)))))
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
      (send
        (cat (exp (gen) y)
          (enc (enc (exp (gen) y) (exp (gen) x) (privk b))
            (exp (gen) (mul y x)))))
      (recv
        (enc (enc (exp (gen) x) (exp (gen) y) (privk a))
          (exp (gen) (mul y x))))))
  (label 10)
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
    (enc (exp (gen) x) (exp (gen) y) (privk a)) (0 2))
  (traces
    ((recv (exp (gen) x))
      (send
        (cat (exp (gen) y)
          (enc (enc (exp (gen) y) (exp (gen) x) (privk b))
            (exp (gen) (mul x y)))))
      (recv
        (enc (enc (exp (gen) x) (exp (gen) y) (privk a))
          (exp (gen) (mul x y)))))
    ((send (exp (gen) x))
      (recv
        (cat (exp (gen) y)
          (enc (enc (exp (gen) y) (exp (gen) x) (privk b-0))
            (exp (gen) (mul x y)))))
      (send
        (enc (enc (exp (gen) x) (exp (gen) y) (privk a))
          (exp (gen) (mul x y))))))
  (label 11)
  (parent 10)
  (unrealized (1 1))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton station-to-station
  (vars (a b name) (y x rndx))
  (defstrand resp 3 (a a) (b b) (y y) (x x))
  (defstrand init 3 (a a) (b b) (x x) (y y))
  (precedes ((0 1) (1 1)) ((1 0) (0 0)) ((1 2) (0 2)))
  (absent (y (exp (gen) x)))
  (non-orig (privk a) (privk b))
  (uniq-gen y x)
  (operation encryption-test (displaced 2 0 resp 2)
    (enc (enc (exp (gen) y) (exp (gen) x) (privk b-0))
      (exp (gen) (mul y x))) (1 1))
  (traces
    ((recv (exp (gen) x))
      (send
        (cat (exp (gen) y)
          (enc (enc (exp (gen) y) (exp (gen) x) (privk b))
            (exp (gen) (mul y x)))))
      (recv
        (enc (enc (exp (gen) x) (exp (gen) y) (privk a))
          (exp (gen) (mul y x)))))
    ((send (exp (gen) x))
      (recv
        (cat (exp (gen) y)
          (enc (enc (exp (gen) y) (exp (gen) x) (privk b))
            (exp (gen) (mul y x)))))
      (send
        (enc (enc (exp (gen) x) (exp (gen) y) (privk a))
          (exp (gen) (mul y x))))))
  (label 12)
  (parent 11)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (y y) (x x))))
  (origs))

(defskeleton station-to-station
  (vars (a b b-0 name) (x y rndx))
  (defstrand resp 3 (a a) (b b) (y y) (x x))
  (defstrand init 3 (a a) (b b-0) (x x) (y y))
  (deflistener (exp (gen) (mul x y)))
  (precedes ((0 1) (2 0)) ((1 0) (0 0)) ((1 2) (0 2)) ((2 1) (1 1)))
  (absent (y (exp (gen) x)))
  (non-orig (privk a) (privk b))
  (uniq-gen x y)
  (operation encryption-test (added-listener (exp (gen) (mul x y)))
    (enc (enc (exp (gen) y) (exp (gen) x) (privk b-0))
      (exp (gen) (mul x y))) (1 1))
  (traces
    ((recv (exp (gen) x))
      (send
        (cat (exp (gen) y)
          (enc (enc (exp (gen) y) (exp (gen) x) (privk b))
            (exp (gen) (mul x y)))))
      (recv
        (enc (enc (exp (gen) x) (exp (gen) y) (privk a))
          (exp (gen) (mul x y)))))
    ((send (exp (gen) x))
      (recv
        (cat (exp (gen) y)
          (enc (enc (exp (gen) y) (exp (gen) x) (privk b-0))
            (exp (gen) (mul x y)))))
      (send
        (enc (enc (exp (gen) x) (exp (gen) y) (privk a))
          (exp (gen) (mul x y)))))
    ((recv (exp (gen) (mul x y))) (send (exp (gen) (mul x y)))))
  (label 13)
  (parent 11)
  (unrealized (2 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton station-to-station
  (vars (a b b-0 name) (x y rndx) (w expt))
  (defstrand resp 3 (a a) (b b) (y y) (x x))
  (defstrand init 3 (a a) (b b-0) (x x) (y y))
  (deflistener (exp (gen) (mul x y)))
  (deflistener (cat (exp (gen) (mul x y (rec w))) w))
  (precedes ((0 1) (3 0)) ((1 0) (0 0)) ((1 2) (0 2)) ((2 1) (1 1))
    ((3 1) (2 0)))
  (absent (y (exp (gen) x)))
  (non-orig (privk a) (privk b))
  (uniq-gen x y)
  (precur (3 0))
  (operation nonce-test
    (added-listener (cat (exp (gen) (mul x y (rec w))) w))
    (exp (gen) (mul x y)) (2 0))
  (traces
    ((recv (exp (gen) x))
      (send
        (cat (exp (gen) y)
          (enc (enc (exp (gen) y) (exp (gen) x) (privk b))
            (exp (gen) (mul x y)))))
      (recv
        (enc (enc (exp (gen) x) (exp (gen) y) (privk a))
          (exp (gen) (mul x y)))))
    ((send (exp (gen) x))
      (recv
        (cat (exp (gen) y)
          (enc (enc (exp (gen) y) (exp (gen) x) (privk b-0))
            (exp (gen) (mul x y)))))
      (send
        (enc (enc (exp (gen) x) (exp (gen) y) (privk a))
          (exp (gen) (mul x y)))))
    ((recv (exp (gen) (mul x y))) (send (exp (gen) (mul x y))))
    ((recv (cat (exp (gen) (mul x y (rec w))) w))
      (send (cat (exp (gen) (mul x y (rec w))) w))))
  (label 14)
  (parent 13)
  (unrealized (3 0))
  (comment "5 in cohort - 5 not yet seen"))

(defskeleton station-to-station
  (vars (a b b-0 name) (x y rndx))
  (defstrand resp 3 (a a) (b b) (y y) (x x))
  (defstrand init 3 (a a) (b b-0) (x x) (y y))
  (deflistener (exp (gen) (mul x y)))
  (deflistener (cat (gen) (mul x y)))
  (precedes ((0 1) (3 0)) ((1 0) (0 0)) ((1 2) (0 2)) ((2 1) (1 1))
    ((3 1) (2 0)))
  (absent (y (exp (gen) x)))
  (non-orig (privk a) (privk b))
  (uniq-gen x y)
  (precur (3 0))
  (operation nonce-test (contracted (x-0 x) (y-0 y) (w (mul x y))) (gen)
    (3 0))
  (traces
    ((recv (exp (gen) x))
      (send
        (cat (exp (gen) y)
          (enc (enc (exp (gen) y) (exp (gen) x) (privk b))
            (exp (gen) (mul x y)))))
      (recv
        (enc (enc (exp (gen) x) (exp (gen) y) (privk a))
          (exp (gen) (mul x y)))))
    ((send (exp (gen) x))
      (recv
        (cat (exp (gen) y)
          (enc (enc (exp (gen) y) (exp (gen) x) (privk b-0))
            (exp (gen) (mul x y)))))
      (send
        (enc (enc (exp (gen) x) (exp (gen) y) (privk a))
          (exp (gen) (mul x y)))))
    ((recv (exp (gen) (mul x y))) (send (exp (gen) (mul x y))))
    ((recv (cat (gen) (mul x y))) (send (cat (gen) (mul x y)))))
  (label 15)
  (parent 14)
  (unrealized (2 0) (3 0))
  (dead)
  (comment "empty cohort"))

(defskeleton station-to-station
  (vars (a b b-0 name) (y x rndx))
  (defstrand resp 3 (a a) (b b) (y y) (x x))
  (defstrand init 3 (a a) (b b-0) (x x) (y y))
  (deflistener (exp (gen) (mul y x)))
  (deflistener (cat (exp (gen) x) y))
  (precedes ((0 1) (3 0)) ((1 0) (0 0)) ((1 2) (0 2)) ((2 1) (1 1))
    ((3 1) (2 0)))
  (absent (y (exp (gen) x)))
  (non-orig (privk a) (privk b))
  (precur (3 0))
  (uniq-gen y x)
  (operation nonce-test (displaced 4 1 init 1) (exp (gen) x-0) (3 0))
  (traces
    ((recv (exp (gen) x))
      (send
        (cat (exp (gen) y)
          (enc (enc (exp (gen) y) (exp (gen) x) (privk b))
            (exp (gen) (mul y x)))))
      (recv
        (enc (enc (exp (gen) x) (exp (gen) y) (privk a))
          (exp (gen) (mul y x)))))
    ((send (exp (gen) x))
      (recv
        (cat (exp (gen) y)
          (enc (enc (exp (gen) y) (exp (gen) x) (privk b-0))
            (exp (gen) (mul y x)))))
      (send
        (enc (enc (exp (gen) x) (exp (gen) y) (privk a))
          (exp (gen) (mul y x)))))
    ((recv (exp (gen) (mul y x))) (send (exp (gen) (mul y x))))
    ((recv (cat (exp (gen) x) y)) (send (cat (exp (gen) x) y))))
  (label 16)
  (parent 14)
  (unrealized (3 0))
  (dead)
  (comment "empty cohort"))

(defskeleton station-to-station
  (vars (a b b-0 name) (x y x-0 rndx))
  (defstrand resp 3 (a a) (b b) (y y) (x x))
  (defstrand init 3 (a a) (b b-0) (x x) (y y))
  (deflistener (exp (gen) (mul x y)))
  (deflistener (cat (exp (gen) x-0) (mul x y (rec x-0))))
  (defstrand init 1 (x x-0))
  (precedes ((0 1) (3 0)) ((1 0) (0 0)) ((1 2) (0 2)) ((2 1) (1 1))
    ((3 1) (2 0)) ((4 0) (3 0)))
  (absent (y (exp (gen) x)))
  (non-orig (privk a) (privk b))
  (precur (3 0))
  (uniq-gen x y x-0)
  (operation nonce-test (added-strand init 1) (exp (gen) x-0) (3 0))
  (traces
    ((recv (exp (gen) x))
      (send
        (cat (exp (gen) y)
          (enc (enc (exp (gen) y) (exp (gen) x) (privk b))
            (exp (gen) (mul x y)))))
      (recv
        (enc (enc (exp (gen) x) (exp (gen) y) (privk a))
          (exp (gen) (mul x y)))))
    ((send (exp (gen) x))
      (recv
        (cat (exp (gen) y)
          (enc (enc (exp (gen) y) (exp (gen) x) (privk b-0))
            (exp (gen) (mul x y)))))
      (send
        (enc (enc (exp (gen) x) (exp (gen) y) (privk a))
          (exp (gen) (mul x y)))))
    ((recv (exp (gen) (mul x y))) (send (exp (gen) (mul x y))))
    ((recv (cat (exp (gen) x-0) (mul x y (rec x-0))))
      (send (cat (exp (gen) x-0) (mul x y (rec x-0)))))
    ((send (exp (gen) x-0))))
  (label 17)
  (parent 14)
  (unrealized (2 0) (3 0))
  (dead)
  (comment "empty cohort"))

(defskeleton station-to-station
  (vars (a b b-0 name) (x y rndx))
  (defstrand resp 3 (a a) (b b) (y y) (x x))
  (defstrand init 3 (a a) (b b-0) (x x) (y y))
  (deflistener (exp (gen) (mul x y)))
  (deflistener (cat (exp (gen) y) x))
  (precedes ((0 1) (3 0)) ((1 0) (0 0)) ((1 2) (0 2)) ((2 1) (1 1))
    ((3 1) (2 0)))
  (absent (y (exp (gen) x)))
  (non-orig (privk a) (privk b))
  (precur (3 0))
  (uniq-gen x y)
  (operation nonce-test (displaced 4 0 resp 2) (exp (gen) y-0) (3 0))
  (traces
    ((recv (exp (gen) x))
      (send
        (cat (exp (gen) y)
          (enc (enc (exp (gen) y) (exp (gen) x) (privk b))
            (exp (gen) (mul x y)))))
      (recv
        (enc (enc (exp (gen) x) (exp (gen) y) (privk a))
          (exp (gen) (mul x y)))))
    ((send (exp (gen) x))
      (recv
        (cat (exp (gen) y)
          (enc (enc (exp (gen) y) (exp (gen) x) (privk b-0))
            (exp (gen) (mul x y)))))
      (send
        (enc (enc (exp (gen) x) (exp (gen) y) (privk a))
          (exp (gen) (mul x y)))))
    ((recv (exp (gen) (mul x y))) (send (exp (gen) (mul x y))))
    ((recv (cat (exp (gen) y) x)) (send (cat (exp (gen) y) x))))
  (label 18)
  (parent 14)
  (unrealized (3 0))
  (dead)
  (comment "empty cohort"))

(defskeleton station-to-station
  (vars (a b b-0 b-1 name) (x expt) (x-0 y y-0 rndx))
  (defstrand resp 3 (a a) (b b) (y y) (x x-0))
  (defstrand init 3 (a a) (b b-0) (x x-0) (y y))
  (deflistener (exp (gen) (mul x-0 y)))
  (deflistener (cat (exp (gen) y-0) (mul x-0 y (rec y-0))))
  (defstrand resp 2 (b b-1) (y y-0) (x x))
  (precedes ((0 1) (3 0)) ((1 0) (0 0)) ((1 2) (0 2)) ((2 1) (1 1))
    ((3 1) (2 0)) ((4 1) (3 0)))
  (absent (y-0 (exp (gen) x)) (y (exp (gen) x-0)))
  (non-orig (privk a) (privk b))
  (precur (3 0))
  (uniq-gen x-0 y y-0)
  (operation nonce-test (added-strand resp 2) (exp (gen) y-0) (3 0))
  (traces
    ((recv (exp (gen) x-0))
      (send
        (cat (exp (gen) y)
          (enc (enc (exp (gen) y) (exp (gen) x-0) (privk b))
            (exp (gen) (mul x-0 y)))))
      (recv
        (enc (enc (exp (gen) x-0) (exp (gen) y) (privk a))
          (exp (gen) (mul x-0 y)))))
    ((send (exp (gen) x-0))
      (recv
        (cat (exp (gen) y)
          (enc (enc (exp (gen) y) (exp (gen) x-0) (privk b-0))
            (exp (gen) (mul x-0 y)))))
      (send
        (enc (enc (exp (gen) x-0) (exp (gen) y) (privk a))
          (exp (gen) (mul x-0 y)))))
    ((recv (exp (gen) (mul x-0 y))) (send (exp (gen) (mul x-0 y))))
    ((recv (cat (exp (gen) y-0) (mul x-0 y (rec y-0))))
      (send (cat (exp (gen) y-0) (mul x-0 y (rec y-0)))))
    ((recv (exp (gen) x))
      (send
        (cat (exp (gen) y-0)
          (enc (enc (exp (gen) y-0) (exp (gen) x) (privk b-1))
            (exp (gen) (mul x y-0)))))))
  (label 19)
  (parent 14)
  (unrealized (2 0) (3 0))
  (dead)
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol station-weak diffie-hellman
  (defrole weak-init
    (vars (x rndx) (y expt) (a b name))
    (trace (send (exp (gen) x))
      (recv
        (cat (exp (gen) y)
          (enc (enc (exp (gen) y) (exp (gen) x) (privk b))
            (exp (gen) (mul x y)))))
      (send
        (enc (enc (exp (gen) x) (exp (gen) y) (privk a))
          (exp (gen) (mul x y))))))
  (defrole weak-resp
    (vars (y rndx) (x expt) (a b name))
    (trace (recv (exp (gen) x))
      (send
        (cat (exp (gen) y)
          (enc (enc (exp (gen) y) (exp (gen) x) (privk b))
            (exp (gen) (mul y x)))))
      (recv
        (enc (enc (exp (gen) x) (exp (gen) y) (privk a))
          (exp (gen) (mul y x)))))
    (absent (y (exp (gen) x)))))

(defskeleton station-weak
  (vars (a b name) (x rndx) (y expt))
  (defstrand weak-init 3 (a a) (b b) (x x) (y y))
  (non-orig (privk a) (privk b))
  (uniq-gen x)
  (traces
    ((send (exp (gen) x))
      (recv
        (cat (exp (gen) y)
          (enc (enc (exp (gen) y) (exp (gen) x) (privk b))
            (exp (gen) (mul x y)))))
      (send
        (enc (enc (exp (gen) x) (exp (gen) y) (privk a))
          (exp (gen) (mul x y))))))
  (label 20)
  (unrealized (0 1))
  (origs)
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton station-weak
  (vars (a b b-0 name) (x x-0 rndx))
  (defstrand weak-init 3 (a a) (b b) (x x-0) (y x))
  (defstrand weak-init 3 (a b) (b b-0) (x x) (y x-0))
  (precedes ((0 0) (1 1)) ((1 2) (0 1)))
  (non-orig (privk a) (privk b))
  (uniq-gen x-0)
  (operation encryption-test (added-strand weak-init 3)
    (enc (exp (gen) x) (exp (gen) x-0) (privk b)) (0 1))
  (traces
    ((send (exp (gen) x-0))
      (recv
        (cat (exp (gen) x)
          (enc (enc (exp (gen) x) (exp (gen) x-0) (privk b))
            (exp (gen) (mul x x-0)))))
      (send
        (enc (enc (exp (gen) x-0) (exp (gen) x) (privk a))
          (exp (gen) (mul x x-0)))))
    ((send (exp (gen) x))
      (recv
        (cat (exp (gen) x-0)
          (enc (enc (exp (gen) x-0) (exp (gen) x) (privk b-0))
            (exp (gen) (mul x x-0)))))
      (send
        (enc (enc (exp (gen) x) (exp (gen) x-0) (privk b))
          (exp (gen) (mul x x-0))))))
  (label 21)
  (parent 20)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (x x-0) (y x))))
  (origs))

(defskeleton station-weak
  (vars (a b name) (y x rndx))
  (defstrand weak-init 3 (a a) (b b) (x x) (y y))
  (defstrand weak-resp 2 (b b) (y y) (x x))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (absent (y (exp (gen) x)))
  (non-orig (privk a) (privk b))
  (uniq-gen x)
  (operation encryption-test (added-strand weak-resp 2)
    (enc (exp (gen) y) (exp (gen) x) (privk b)) (0 1))
  (traces
    ((send (exp (gen) x))
      (recv
        (cat (exp (gen) y)
          (enc (enc (exp (gen) y) (exp (gen) x) (privk b))
            (exp (gen) (mul y x)))))
      (send
        (enc (enc (exp (gen) x) (exp (gen) y) (privk a))
          (exp (gen) (mul y x)))))
    ((recv (exp (gen) x))
      (send
        (cat (exp (gen) y)
          (enc (enc (exp (gen) y) (exp (gen) x) (privk b))
            (exp (gen) (mul y x)))))))
  (label 22)
  (parent 20)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (x x) (y y))))
  (origs))

(comment "Nothing left to do")

(defprotocol station-weak diffie-hellman
  (defrole weak-init
    (vars (x rndx) (y expt) (a b name))
    (trace (send (exp (gen) x))
      (recv
        (cat (exp (gen) y)
          (enc (enc (exp (gen) y) (exp (gen) x) (privk b))
            (exp (gen) (mul x y)))))
      (send
        (enc (enc (exp (gen) x) (exp (gen) y) (privk a))
          (exp (gen) (mul x y))))))
  (defrole weak-resp
    (vars (y rndx) (x expt) (a b name))
    (trace (recv (exp (gen) x))
      (send
        (cat (exp (gen) y)
          (enc (enc (exp (gen) y) (exp (gen) x) (privk b))
            (exp (gen) (mul y x)))))
      (recv
        (enc (enc (exp (gen) x) (exp (gen) y) (privk a))
          (exp (gen) (mul y x)))))
    (absent (y (exp (gen) x)))))

(defskeleton station-weak
  (vars (a b name) (y rndx) (x expt))
  (defstrand weak-resp 3 (a a) (b b) (y y) (x x))
  (absent (y (exp (gen) x)))
  (non-orig (privk a) (privk b))
  (uniq-gen y)
  (traces
    ((recv (exp (gen) x))
      (send
        (cat (exp (gen) y)
          (enc (enc (exp (gen) y) (exp (gen) x) (privk b))
            (exp (gen) (mul y x)))))
      (recv
        (enc (enc (exp (gen) x) (exp (gen) y) (privk a))
          (exp (gen) (mul y x))))))
  (label 23)
  (unrealized (0 2))
  (origs)
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton station-weak
  (vars (a b b-0 name) (x y rndx))
  (defstrand weak-resp 3 (a a) (b b) (y y) (x x))
  (defstrand weak-init 3 (a a) (b b-0) (x x) (y y))
  (precedes ((0 1) (1 1)) ((1 2) (0 2)))
  (absent (y (exp (gen) x)))
  (non-orig (privk a) (privk b))
  (uniq-gen y)
  (operation encryption-test (added-strand weak-init 3)
    (enc (exp (gen) x) (exp (gen) y) (privk a)) (0 2))
  (traces
    ((recv (exp (gen) x))
      (send
        (cat (exp (gen) y)
          (enc (enc (exp (gen) y) (exp (gen) x) (privk b))
            (exp (gen) (mul x y)))))
      (recv
        (enc (enc (exp (gen) x) (exp (gen) y) (privk a))
          (exp (gen) (mul x y)))))
    ((send (exp (gen) x))
      (recv
        (cat (exp (gen) y)
          (enc (enc (exp (gen) y) (exp (gen) x) (privk b-0))
            (exp (gen) (mul x y)))))
      (send
        (enc (enc (exp (gen) x) (exp (gen) y) (privk a))
          (exp (gen) (mul x y))))))
  (label 24)
  (parent 23)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (y y) (x x))))
  (origs))

(defskeleton station-weak
  (vars (a b name) (y y-0 rndx))
  (defstrand weak-resp 3 (a a) (b b) (y y-0) (x y))
  (defstrand weak-resp 2 (b a) (y y) (x y-0))
  (precedes ((0 1) (1 0)) ((1 1) (0 2)))
  (absent (y (exp (gen) y-0)) (y-0 (exp (gen) y)))
  (non-orig (privk a) (privk b))
  (uniq-gen y-0)
  (operation encryption-test (added-strand weak-resp 2)
    (enc (exp (gen) y) (exp (gen) y-0) (privk a)) (0 2))
  (traces
    ((recv (exp (gen) y))
      (send
        (cat (exp (gen) y-0)
          (enc (enc (exp (gen) y-0) (exp (gen) y) (privk b))
            (exp (gen) (mul y y-0)))))
      (recv
        (enc (enc (exp (gen) y) (exp (gen) y-0) (privk a))
          (exp (gen) (mul y y-0)))))
    ((recv (exp (gen) y-0))
      (send
        (cat (exp (gen) y)
          (enc (enc (exp (gen) y) (exp (gen) y-0) (privk a))
            (exp (gen) (mul y y-0)))))))
  (label 25)
  (parent 23)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (y y-0) (x y))))
  (origs))

(comment "Nothing left to do")
