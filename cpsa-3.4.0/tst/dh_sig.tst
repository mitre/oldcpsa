(herald "Signed DH exchange" (algebra diffie-hellman))

(comment "CPSA 3.4.0")
(comment "All input read from dh_sig.scm")

(defprotocol dh_sig diffie-hellman
  (defrole init
    (vars (x expn) (h base) (a b name))
    (trace (send (exp (gen) x)) (recv (enc h (exp (gen) x) (privk b)))
      (send (enc h (exp (gen) x) (privk a))))
    (uniq-gen x))
  (defrole resp
    (vars (y expn) (h base) (a b name))
    (trace (recv h) (send (enc (exp (gen) y) h (privk b)))
      (recv (enc (exp (gen) y) h (privk a))))
    (uniq-gen y)
    (absent (y h))))

(defskeleton dh_sig
  (vars (a b name) (h base) (x expn))
  (defstrand init 3 (a a) (b b) (h h) (x x))
  (non-orig (privk a) (privk b))
  (uniq-gen x)
  (traces
    ((send (exp (gen) x)) (recv (enc h (exp (gen) x) (privk b)))
      (send (enc h (exp (gen) x) (privk a)))))
  (label 0)
  (unrealized (0 1))
  (origs)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dh_sig
  (vars (a b name) (x y expn))
  (defstrand init 3 (a a) (b b) (h (exp (gen) y)) (x x))
  (defstrand resp 2 (b b) (h (exp (gen) x)) (y y))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (absent (y (exp (gen) x)))
  (non-orig (privk a) (privk b))
  (uniq-gen x y)
  (operation encryption-test (added-strand resp 2)
    (enc (exp (gen) y) (exp (gen) x) (privk b)) (0 1))
  (traces
    ((send (exp (gen) x))
      (recv (enc (exp (gen) y) (exp (gen) x) (privk b)))
      (send (enc (exp (gen) y) (exp (gen) x) (privk a))))
    ((recv (exp (gen) x))
      (send (enc (exp (gen) y) (exp (gen) x) (privk b)))))
  (label 1)
  (parent 0)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (x x) (h (exp (gen) y)))))
  (origs))

(comment "Nothing left to do")

(defprotocol dh_sig diffie-hellman
  (defrole init
    (vars (x expn) (h base) (a b name))
    (trace (send (exp (gen) x)) (recv (enc h (exp (gen) x) (privk b)))
      (send (enc h (exp (gen) x) (privk a))))
    (uniq-gen x))
  (defrole resp
    (vars (y expn) (h base) (a b name))
    (trace (recv h) (send (enc (exp (gen) y) h (privk b)))
      (recv (enc (exp (gen) y) h (privk a))))
    (uniq-gen y)
    (absent (y h))))

(defskeleton dh_sig
  (vars (a b name) (h base) (y expn))
  (defstrand resp 3 (a a) (b b) (h h) (y y))
  (absent (y h))
  (non-orig (privk a) (privk b))
  (uniq-gen y)
  (traces
    ((recv h) (send (enc (exp (gen) y) h (privk b)))
      (recv (enc (exp (gen) y) h (privk a)))))
  (label 2)
  (unrealized (0 2))
  (origs)
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton dh_sig
  (vars (a b b-0 name) (y x expn))
  (defstrand resp 3 (a a) (b b) (h (exp (gen) x)) (y y))
  (defstrand init 3 (a a) (b b-0) (h (exp (gen) y)) (x x))
  (precedes ((0 1) (1 1)) ((1 0) (0 0)) ((1 2) (0 2)))
  (absent (y (exp (gen) x)))
  (non-orig (privk a) (privk b))
  (uniq-gen y x)
  (operation encryption-test (added-strand init 3)
    (enc (exp (gen) y) (exp (gen) x) (privk a)) (0 2))
  (traces
    ((recv (exp (gen) x))
      (send (enc (exp (gen) y) (exp (gen) x) (privk b)))
      (recv (enc (exp (gen) y) (exp (gen) x) (privk a))))
    ((send (exp (gen) x))
      (recv (enc (exp (gen) y) (exp (gen) x) (privk b-0)))
      (send (enc (exp (gen) y) (exp (gen) x) (privk a)))))
  (label 3)
  (parent 2)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (y y) (h (exp (gen) x)))))
  (origs))

(defskeleton dh_sig
  (vars (b name) (h base) (y expn))
  (defstrand resp 3 (a b) (b b) (h h) (y y))
  (absent (y h))
  (non-orig (privk b))
  (uniq-gen y)
  (operation encryption-test (displaced 1 0 resp 2)
    (enc (exp (gen) y) h (privk a)) (0 2))
  (traces
    ((recv h) (send (enc (exp (gen) y) h (privk b)))
      (recv (enc (exp (gen) y) h (privk b)))))
  (label 4)
  (parent 2)
  (unrealized)
  (shape)
  (maps ((0) ((a b) (b b) (y y) (h h))))
  (origs))

(comment "Nothing left to do")

(defprotocol dh_sig2 diffie-hellman
  (defrole init
    (vars (x expn) (h base) (a b name))
    (trace (send (exp (gen) x)) (recv (enc h (exp (gen) x) (privk b)))
      (send (enc (exp (gen) x) h b (privk a))))
    (uniq-gen x))
  (defrole resp
    (vars (y expn) (h base) (a b name))
    (trace (recv h) (send (enc (exp (gen) y) h (privk b)))
      (recv (enc h (exp (gen) y) b (privk a))))
    (uniq-gen y)
    (absent (y h))))

(defskeleton dh_sig2
  (vars (a b name) (h base) (x expn))
  (defstrand init 3 (a a) (b b) (h h) (x x))
  (non-orig (privk a) (privk b))
  (uniq-gen x)
  (traces
    ((send (exp (gen) x)) (recv (enc h (exp (gen) x) (privk b)))
      (send (enc (exp (gen) x) h b (privk a)))))
  (label 5)
  (unrealized (0 1))
  (origs)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dh_sig2
  (vars (a b name) (x y expn))
  (defstrand init 3 (a a) (b b) (h (exp (gen) y)) (x x))
  (defstrand resp 2 (b b) (h (exp (gen) x)) (y y))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (absent (y (exp (gen) x)))
  (non-orig (privk a) (privk b))
  (uniq-gen x y)
  (operation encryption-test (added-strand resp 2)
    (enc (exp (gen) y) (exp (gen) x) (privk b)) (0 1))
  (traces
    ((send (exp (gen) x))
      (recv (enc (exp (gen) y) (exp (gen) x) (privk b)))
      (send (enc (exp (gen) x) (exp (gen) y) b (privk a))))
    ((recv (exp (gen) x))
      (send (enc (exp (gen) y) (exp (gen) x) (privk b)))))
  (label 6)
  (parent 5)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (x x) (h (exp (gen) y)))))
  (origs))

(comment "Nothing left to do")

(defprotocol dh_sig2 diffie-hellman
  (defrole init
    (vars (x expn) (h base) (a b name))
    (trace (send (exp (gen) x)) (recv (enc h (exp (gen) x) (privk b)))
      (send (enc (exp (gen) x) h b (privk a))))
    (uniq-gen x))
  (defrole resp
    (vars (y expn) (h base) (a b name))
    (trace (recv h) (send (enc (exp (gen) y) h (privk b)))
      (recv (enc h (exp (gen) y) b (privk a))))
    (uniq-gen y)
    (absent (y h))))

(defskeleton dh_sig2
  (vars (a b name) (h base) (y expn))
  (defstrand resp 3 (a a) (b b) (h h) (y y))
  (absent (y h))
  (non-orig (privk a) (privk b))
  (uniq-gen y)
  (traces
    ((recv h) (send (enc (exp (gen) y) h (privk b)))
      (recv (enc h (exp (gen) y) b (privk a)))))
  (label 7)
  (unrealized (0 2))
  (origs)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dh_sig2
  (vars (a b name) (y x expn))
  (defstrand resp 3 (a a) (b b) (h (exp (gen) x)) (y y))
  (defstrand init 3 (a a) (b b) (h (exp (gen) y)) (x x))
  (precedes ((0 1) (1 1)) ((1 0) (0 0)) ((1 2) (0 2)))
  (absent (y (exp (gen) x)))
  (non-orig (privk a) (privk b))
  (uniq-gen y x)
  (operation encryption-test (added-strand init 3)
    (enc (exp (gen) x) (exp (gen) y) b (privk a)) (0 2))
  (traces
    ((recv (exp (gen) x))
      (send (enc (exp (gen) y) (exp (gen) x) (privk b)))
      (recv (enc (exp (gen) x) (exp (gen) y) b (privk a))))
    ((send (exp (gen) x))
      (recv (enc (exp (gen) y) (exp (gen) x) (privk b)))
      (send (enc (exp (gen) x) (exp (gen) y) b (privk a)))))
  (label 8)
  (parent 7)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (y y) (h (exp (gen) x)))))
  (origs))

(comment "Nothing left to do")
