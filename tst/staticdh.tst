(herald "Static DH key exchange" (algebra diffie-hellman))

(comment "CPSA 3.6.0")
(comment "All input read from staticdh.scm")

(defprotocol staticdh1 diffie-hellman
  (defrole init
    (vars (a b ca name) (h base) (x rndx) (n text))
    (trace (recv (enc "cert" (exp (gen) x) a (privk ca)))
      (recv (enc "cert" h b (privk ca))) (send (enc n (exp h x)))
      (recv n))
    (non-orig (privk ca))
    (uniq-orig n)
    (neq (a b)))
  (defrole resp
    (vars (a b ca name) (h base) (y rndx) (n text))
    (trace (recv (enc "cert" h a (privk ca)))
      (recv (enc "cert" (exp (gen) y) b (privk ca)))
      (recv (enc n (exp h y))) (send n))
    (non-orig (privk ca))
    (neq (a b)))
  (defrole ca
    (vars (p ca name) (x rndx))
    (trace (send (enc "cert" (exp (gen) x) p (privk ca))))
    (non-orig x)
    (fn-of (owner-of (p x)))))

(defskeleton staticdh1
  (vars (n text) (a b ca name) (h base) (x rndx))
  (defstrand init 4 (n n) (a a) (b b) (ca ca) (h h) (x x))
  (neq (a b))
  (non-orig (privk ca))
  (uniq-orig n)
  (traces
    ((recv (enc "cert" (exp (gen) x) a (privk ca)))
      (recv (enc "cert" h b (privk ca))) (send (enc n (exp h x)))
      (recv n)))
  (label 0)
  (unrealized (0 0) (0 1))
  (origs (n (0 2)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton staticdh1
  (vars (n text) (a b ca name) (h base) (x rndx))
  (defstrand init 4 (n n) (a a) (b b) (ca ca) (h h) (x x))
  (defstrand ca 1 (p a) (ca ca) (x x))
  (precedes ((1 0) (0 0)))
  (fn-of (owner-of (a x)))
  (neq (a b))
  (non-orig (privk ca) x)
  (uniq-orig n)
  (operation encryption-test (added-strand ca 1)
    (enc "cert" (exp (gen) x) a (privk ca)) (0 0))
  (traces
    ((recv (enc "cert" (exp (gen) x) a (privk ca)))
      (recv (enc "cert" h b (privk ca))) (send (enc n (exp h x)))
      (recv n)) ((send (enc "cert" (exp (gen) x) a (privk ca)))))
  (label 1)
  (parent 0)
  (unrealized (0 1) (0 3))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton staticdh1
  (vars (n text) (a b ca name) (x x-0 rndx))
  (defstrand init 4 (n n) (a a) (b b) (ca ca) (h (exp (gen) x-0)) (x x))
  (defstrand ca 1 (p a) (ca ca) (x x))
  (defstrand ca 1 (p b) (ca ca) (x x-0))
  (precedes ((1 0) (0 0)) ((2 0) (0 1)))
  (fn-of (owner-of (b x-0) (a x)))
  (neq (a b))
  (non-orig (privk ca) x x-0)
  (uniq-orig n)
  (operation encryption-test (added-strand ca 1)
    (enc "cert" (exp (gen) x-0) b (privk ca)) (0 1))
  (traces
    ((recv (enc "cert" (exp (gen) x) a (privk ca)))
      (recv (enc "cert" (exp (gen) x-0) b (privk ca)))
      (send (enc n (exp (gen) (mul x x-0)))) (recv n))
    ((send (enc "cert" (exp (gen) x) a (privk ca))))
    ((send (enc "cert" (exp (gen) x-0) b (privk ca)))))
  (label 2)
  (parent 1)
  (unrealized (0 3))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton staticdh1
  (vars (n text) (a b ca a-0 b-0 ca-0 name) (x x-0 y rndx))
  (defstrand init 4 (n n) (a a) (b b) (ca ca) (h (exp (gen) x-0)) (x x))
  (defstrand ca 1 (p a) (ca ca) (x x))
  (defstrand ca 1 (p b) (ca ca) (x x-0))
  (defstrand resp 4 (n n) (a a-0) (b b-0) (ca ca-0)
    (h (exp (gen) (mul x x-0 (rec y)))) (y y))
  (precedes ((0 2) (3 2)) ((1 0) (0 0)) ((2 0) (0 1)) ((3 3) (0 3)))
  (fn-of (owner-of (b x-0) (a x)))
  (neq (a-0 b-0) (a b))
  (non-orig (privk ca) (privk ca-0) x x-0)
  (uniq-orig n)
  (operation nonce-test (added-strand resp 4) n (0 3)
    (enc n (exp (gen) (mul x x-0))))
  (traces
    ((recv (enc "cert" (exp (gen) x) a (privk ca)))
      (recv (enc "cert" (exp (gen) x-0) b (privk ca)))
      (send (enc n (exp (gen) (mul x x-0)))) (recv n))
    ((send (enc "cert" (exp (gen) x) a (privk ca))))
    ((send (enc "cert" (exp (gen) x-0) b (privk ca))))
    ((recv
       (enc "cert" (exp (gen) (mul x x-0 (rec y))) a-0 (privk ca-0)))
      (recv (enc "cert" (exp (gen) y) b-0 (privk ca-0)))
      (recv (enc n (exp (gen) (mul x x-0)))) (send n)))
  (label 3)
  (parent 2)
  (unrealized (3 0) (3 1))
  (comment "4 in cohort - 4 not yet seen"))

(defskeleton staticdh1
  (vars (n text) (a b ca name) (x x-0 rndx))
  (defstrand init 4 (n n) (a a) (b b) (ca ca) (h (exp (gen) x-0)) (x x))
  (defstrand ca 1 (p a) (ca ca) (x x))
  (defstrand ca 1 (p b) (ca ca) (x x-0))
  (deflistener (exp (gen) (mul x x-0)))
  (precedes ((1 0) (0 0)) ((2 0) (0 1)) ((3 1) (0 3)))
  (fn-of (owner-of (b x-0) (a x)))
  (neq (a b))
  (non-orig (privk ca) x x-0)
  (uniq-orig n)
  (operation nonce-test (added-listener (exp (gen) (mul x x-0))) n (0 3)
    (enc n (exp (gen) (mul x x-0))))
  (traces
    ((recv (enc "cert" (exp (gen) x) a (privk ca)))
      (recv (enc "cert" (exp (gen) x-0) b (privk ca)))
      (send (enc n (exp (gen) (mul x x-0)))) (recv n))
    ((send (enc "cert" (exp (gen) x) a (privk ca))))
    ((send (enc "cert" (exp (gen) x-0) b (privk ca))))
    ((recv (exp (gen) (mul x x-0))) (send (exp (gen) (mul x x-0)))))
  (label 4)
  (parent 2)
  (unrealized (3 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton staticdh1
  (vars (n text) (a b ca b-0 name) (y x rndx))
  (defstrand init 4 (n n) (a a) (b b) (ca ca) (h (exp (gen) x)) (x y))
  (defstrand ca 1 (p a) (ca ca) (x y))
  (defstrand ca 1 (p b) (ca ca) (x x))
  (defstrand resp 4 (n n) (a b) (b b-0) (ca ca) (h (exp (gen) x)) (y y))
  (precedes ((0 2) (3 2)) ((1 0) (0 0)) ((2 0) (0 1)) ((2 0) (3 0))
    ((3 3) (0 3)))
  (fn-of (owner-of (b x) (a y)))
  (neq (b b-0) (a b))
  (non-orig (privk ca) y x)
  (uniq-orig n)
  (operation encryption-test (displaced 4 2 ca 1)
    (enc "cert" (exp (gen) x) a-0 (privk ca-0)) (3 0))
  (traces
    ((recv (enc "cert" (exp (gen) y) a (privk ca)))
      (recv (enc "cert" (exp (gen) x) b (privk ca)))
      (send (enc n (exp (gen) (mul y x)))) (recv n))
    ((send (enc "cert" (exp (gen) y) a (privk ca))))
    ((send (enc "cert" (exp (gen) x) b (privk ca))))
    ((recv (enc "cert" (exp (gen) x) b (privk ca)))
      (recv (enc "cert" (exp (gen) y) b-0 (privk ca)))
      (recv (enc n (exp (gen) (mul y x)))) (send n)))
  (label 5)
  (parent 3)
  (unrealized (3 1))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton staticdh1
  (vars (n text) (a b ca b-0 ca-0 name) (y x rndx))
  (defstrand init 4 (n n) (a a) (b b) (ca ca) (h (exp (gen) x)) (x y))
  (defstrand ca 1 (p a) (ca ca) (x y))
  (defstrand ca 1 (p b) (ca ca) (x x))
  (defstrand resp 4 (n n) (a b) (b b-0) (ca ca-0) (h (exp (gen) x))
    (y y))
  (defstrand ca 1 (p b) (ca ca-0) (x x))
  (precedes ((0 2) (3 2)) ((1 0) (0 0)) ((2 0) (0 1)) ((3 3) (0 3))
    ((4 0) (3 0)))
  (fn-of (owner-of (b x) (a y)))
  (neq (b b-0) (a b))
  (non-orig (privk ca) (privk ca-0) y x)
  (uniq-orig n)
  (operation encryption-test (added-strand ca 1)
    (enc "cert" (exp (gen) x) b (privk ca-0)) (3 0))
  (traces
    ((recv (enc "cert" (exp (gen) y) a (privk ca)))
      (recv (enc "cert" (exp (gen) x) b (privk ca)))
      (send (enc n (exp (gen) (mul y x)))) (recv n))
    ((send (enc "cert" (exp (gen) y) a (privk ca))))
    ((send (enc "cert" (exp (gen) x) b (privk ca))))
    ((recv (enc "cert" (exp (gen) x) b (privk ca-0)))
      (recv (enc "cert" (exp (gen) y) b-0 (privk ca-0)))
      (recv (enc n (exp (gen) (mul y x)))) (send n))
    ((send (enc "cert" (exp (gen) x) b (privk ca-0)))))
  (label 6)
  (parent 3)
  (unrealized (3 1))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton staticdh1
  (vars (n text) (a b ca b-0 name) (y x rndx))
  (defstrand init 4 (n n) (a a) (b b) (ca ca) (h (exp (gen) y)) (x x))
  (defstrand ca 1 (p a) (ca ca) (x x))
  (defstrand ca 1 (p b) (ca ca) (x y))
  (defstrand resp 4 (n n) (a a) (b b-0) (ca ca) (h (exp (gen) x)) (y y))
  (precedes ((0 2) (3 2)) ((1 0) (0 0)) ((1 0) (3 0)) ((2 0) (0 1))
    ((3 3) (0 3)))
  (fn-of (owner-of (a x) (b y)))
  (neq (a b-0) (a b))
  (non-orig (privk ca) y x)
  (uniq-orig n)
  (operation encryption-test (displaced 4 1 ca 1)
    (enc "cert" (exp (gen) x) a-0 (privk ca-0)) (3 0))
  (traces
    ((recv (enc "cert" (exp (gen) x) a (privk ca)))
      (recv (enc "cert" (exp (gen) y) b (privk ca)))
      (send (enc n (exp (gen) (mul y x)))) (recv n))
    ((send (enc "cert" (exp (gen) x) a (privk ca))))
    ((send (enc "cert" (exp (gen) y) b (privk ca))))
    ((recv (enc "cert" (exp (gen) x) a (privk ca)))
      (recv (enc "cert" (exp (gen) y) b-0 (privk ca)))
      (recv (enc n (exp (gen) (mul y x)))) (send n)))
  (label 7)
  (parent 3)
  (unrealized (3 1))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton staticdh1
  (vars (n text) (a b ca b-0 ca-0 name) (y x rndx))
  (defstrand init 4 (n n) (a a) (b b) (ca ca) (h (exp (gen) y)) (x x))
  (defstrand ca 1 (p a) (ca ca) (x x))
  (defstrand ca 1 (p b) (ca ca) (x y))
  (defstrand resp 4 (n n) (a a) (b b-0) (ca ca-0) (h (exp (gen) x))
    (y y))
  (defstrand ca 1 (p a) (ca ca-0) (x x))
  (precedes ((0 2) (3 2)) ((1 0) (0 0)) ((2 0) (0 1)) ((3 3) (0 3))
    ((4 0) (3 0)))
  (fn-of (owner-of (a x) (b y)))
  (neq (a b-0) (a b))
  (non-orig (privk ca) (privk ca-0) y x)
  (uniq-orig n)
  (operation encryption-test (added-strand ca 1)
    (enc "cert" (exp (gen) x) a (privk ca-0)) (3 0))
  (traces
    ((recv (enc "cert" (exp (gen) x) a (privk ca)))
      (recv (enc "cert" (exp (gen) y) b (privk ca)))
      (send (enc n (exp (gen) (mul y x)))) (recv n))
    ((send (enc "cert" (exp (gen) x) a (privk ca))))
    ((send (enc "cert" (exp (gen) y) b (privk ca))))
    ((recv (enc "cert" (exp (gen) x) a (privk ca-0)))
      (recv (enc "cert" (exp (gen) y) b-0 (privk ca-0)))
      (recv (enc n (exp (gen) (mul y x)))) (send n))
    ((send (enc "cert" (exp (gen) x) a (privk ca-0)))))
  (label 8)
  (parent 3)
  (unrealized (3 1))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton staticdh1
  (vars (n text) (a b ca name) (x x-0 rndx) (w expt))
  (defstrand init 4 (n n) (a a) (b b) (ca ca) (h (exp (gen) x-0)) (x x))
  (defstrand ca 1 (p a) (ca ca) (x x))
  (defstrand ca 1 (p b) (ca ca) (x x-0))
  (deflistener (exp (gen) (mul x x-0)))
  (deflistener (cat (exp (gen) (mul x x-0 (rec w))) w))
  (precedes ((1 0) (0 0)) ((2 0) (0 1)) ((3 1) (0 3)) ((4 1) (3 0)))
  (fn-of (owner-of (b x-0) (a x)))
  (neq (a b))
  (non-orig (privk ca) x x-0)
  (uniq-orig n)
  (precur (4 0))
  (operation nonce-test
    (added-listener (cat (exp (gen) (mul x x-0 (rec w))) w))
    (exp (gen) (mul x x-0)) (3 0))
  (traces
    ((recv (enc "cert" (exp (gen) x) a (privk ca)))
      (recv (enc "cert" (exp (gen) x-0) b (privk ca)))
      (send (enc n (exp (gen) (mul x x-0)))) (recv n))
    ((send (enc "cert" (exp (gen) x) a (privk ca))))
    ((send (enc "cert" (exp (gen) x-0) b (privk ca))))
    ((recv (exp (gen) (mul x x-0))) (send (exp (gen) (mul x x-0))))
    ((recv (cat (exp (gen) (mul x x-0 (rec w))) w))
      (send (cat (exp (gen) (mul x x-0 (rec w))) w))))
  (label 9)
  (parent 4)
  (unrealized (4 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton staticdh1
  (vars (n text) (a b ca name) (x x-0 rndx))
  (defstrand init 4 (n n) (a a) (b b) (ca ca) (h (exp (gen) x)) (x x-0))
  (defstrand ca 1 (p a) (ca ca) (x x-0))
  (defstrand ca 1 (p b) (ca ca) (x x))
  (defstrand resp 4 (n n) (a b) (b a) (ca ca) (h (exp (gen) x)) (y x-0))
  (precedes ((0 2) (3 2)) ((1 0) (0 0)) ((1 0) (3 1)) ((2 0) (0 1))
    ((2 0) (3 0)) ((3 3) (0 3)))
  (fn-of (owner-of (a x-0) (b x)))
  (neq (b a) (a b))
  (non-orig (privk ca) x x-0)
  (uniq-orig n)
  (operation encryption-test (displaced 4 1 ca 1)
    (enc "cert" (exp (gen) x-0) b-0 (privk ca)) (3 1))
  (traces
    ((recv (enc "cert" (exp (gen) x-0) a (privk ca)))
      (recv (enc "cert" (exp (gen) x) b (privk ca)))
      (send (enc n (exp (gen) (mul x x-0)))) (recv n))
    ((send (enc "cert" (exp (gen) x-0) a (privk ca))))
    ((send (enc "cert" (exp (gen) x) b (privk ca))))
    ((recv (enc "cert" (exp (gen) x) b (privk ca)))
      (recv (enc "cert" (exp (gen) x-0) a (privk ca)))
      (recv (enc n (exp (gen) (mul x x-0)))) (send n)))
  (label 10)
  (parent 5)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (ca ca) (h (exp (gen) x)) (x x-0) (n n))))
  (origs (n (0 2))))

(defskeleton staticdh1
  (vars (n text) (a b ca name) (x x-0 rndx))
  (defstrand init 4 (n n) (a a) (b b) (ca ca) (h (exp (gen) x)) (x x-0))
  (defstrand ca 1 (p a) (ca ca) (x x-0))
  (defstrand ca 1 (p b) (ca ca) (x x))
  (defstrand resp 4 (n n) (a b) (b a) (ca ca) (h (exp (gen) x)) (y x-0))
  (defstrand ca 1 (p a) (ca ca) (x x-0))
  (precedes ((0 2) (3 2)) ((1 0) (0 0)) ((2 0) (0 1)) ((2 0) (3 0))
    ((3 3) (0 3)) ((4 0) (3 1)))
  (fn-of (owner-of (a x-0) (b x)))
  (neq (b a) (a b))
  (non-orig (privk ca) x x-0)
  (uniq-orig n)
  (operation encryption-test (added-strand ca 1)
    (enc "cert" (exp (gen) x-0) a (privk ca)) (3 1))
  (traces
    ((recv (enc "cert" (exp (gen) x-0) a (privk ca)))
      (recv (enc "cert" (exp (gen) x) b (privk ca)))
      (send (enc n (exp (gen) (mul x x-0)))) (recv n))
    ((send (enc "cert" (exp (gen) x-0) a (privk ca))))
    ((send (enc "cert" (exp (gen) x) b (privk ca))))
    ((recv (enc "cert" (exp (gen) x) b (privk ca)))
      (recv (enc "cert" (exp (gen) x-0) a (privk ca)))
      (recv (enc n (exp (gen) (mul x x-0)))) (send n))
    ((send (enc "cert" (exp (gen) x-0) a (privk ca)))))
  (label 11)
  (parent 5)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (ca ca) (h (exp (gen) x)) (x x-0) (n n))))
  (origs (n (0 2))))

(defskeleton staticdh1
  (vars (n text) (a b ca name) (x x-0 rndx))
  (defstrand init 4 (n n) (a a) (b b) (ca ca) (h (exp (gen) x)) (x x-0))
  (defstrand ca 1 (p a) (ca ca) (x x-0))
  (defstrand ca 1 (p b) (ca ca) (x x))
  (defstrand resp 4 (n n) (a b) (b a) (ca ca) (h (exp (gen) x)) (y x-0))
  (defstrand ca 1 (p b) (ca ca) (x x))
  (precedes ((0 2) (3 2)) ((1 0) (0 0)) ((1 0) (3 1)) ((2 0) (0 1))
    ((3 3) (0 3)) ((4 0) (3 0)))
  (fn-of (owner-of (a x-0) (b x)))
  (neq (b a) (a b))
  (non-orig (privk ca) x x-0)
  (uniq-orig n)
  (operation encryption-test (displaced 5 1 ca 1)
    (enc "cert" (exp (gen) x-0) b-0 (privk ca-0)) (3 1))
  (traces
    ((recv (enc "cert" (exp (gen) x-0) a (privk ca)))
      (recv (enc "cert" (exp (gen) x) b (privk ca)))
      (send (enc n (exp (gen) (mul x x-0)))) (recv n))
    ((send (enc "cert" (exp (gen) x-0) a (privk ca))))
    ((send (enc "cert" (exp (gen) x) b (privk ca))))
    ((recv (enc "cert" (exp (gen) x) b (privk ca)))
      (recv (enc "cert" (exp (gen) x-0) a (privk ca)))
      (recv (enc n (exp (gen) (mul x x-0)))) (send n))
    ((send (enc "cert" (exp (gen) x) b (privk ca)))))
  (label 12)
  (parent 6)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (ca ca) (h (exp (gen) x)) (x x-0) (n n))))
  (origs (n (0 2))))

(defskeleton staticdh1
  (vars (n text) (a b ca ca-0 name) (x x-0 rndx))
  (defstrand init 4 (n n) (a a) (b b) (ca ca) (h (exp (gen) x)) (x x-0))
  (defstrand ca 1 (p a) (ca ca) (x x-0))
  (defstrand ca 1 (p b) (ca ca) (x x))
  (defstrand resp 4 (n n) (a b) (b a) (ca ca-0) (h (exp (gen) x))
    (y x-0))
  (defstrand ca 1 (p b) (ca ca-0) (x x))
  (defstrand ca 1 (p a) (ca ca-0) (x x-0))
  (precedes ((0 2) (3 2)) ((1 0) (0 0)) ((2 0) (0 1)) ((3 3) (0 3))
    ((4 0) (3 0)) ((5 0) (3 1)))
  (fn-of (owner-of (a x-0) (b x)))
  (neq (b a) (a b))
  (non-orig (privk ca) (privk ca-0) x x-0)
  (uniq-orig n)
  (operation encryption-test (added-strand ca 1)
    (enc "cert" (exp (gen) x-0) a (privk ca-0)) (3 1))
  (traces
    ((recv (enc "cert" (exp (gen) x-0) a (privk ca)))
      (recv (enc "cert" (exp (gen) x) b (privk ca)))
      (send (enc n (exp (gen) (mul x x-0)))) (recv n))
    ((send (enc "cert" (exp (gen) x-0) a (privk ca))))
    ((send (enc "cert" (exp (gen) x) b (privk ca))))
    ((recv (enc "cert" (exp (gen) x) b (privk ca-0)))
      (recv (enc "cert" (exp (gen) x-0) a (privk ca-0)))
      (recv (enc n (exp (gen) (mul x x-0)))) (send n))
    ((send (enc "cert" (exp (gen) x) b (privk ca-0))))
    ((send (enc "cert" (exp (gen) x-0) a (privk ca-0)))))
  (label 13)
  (parent 6)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (ca ca) (h (exp (gen) x)) (x x-0) (n n))))
  (origs (n (0 2))))

(defskeleton staticdh1
  (vars (n text) (a b ca name) (x x-0 rndx))
  (defstrand init 4 (n n) (a a) (b b) (ca ca) (h (exp (gen) x-0)) (x x))
  (defstrand ca 1 (p a) (ca ca) (x x))
  (defstrand ca 1 (p b) (ca ca) (x x-0))
  (defstrand resp 4 (n n) (a a) (b b) (ca ca) (h (exp (gen) x)) (y x-0))
  (precedes ((0 2) (3 2)) ((1 0) (0 0)) ((1 0) (3 0)) ((2 0) (0 1))
    ((2 0) (3 1)) ((3 3) (0 3)))
  (fn-of (owner-of (b x-0) (a x)))
  (neq (a b))
  (non-orig (privk ca) x x-0)
  (uniq-orig n)
  (operation encryption-test (displaced 4 2 ca 1)
    (enc "cert" (exp (gen) x-0) b-0 (privk ca)) (3 1))
  (traces
    ((recv (enc "cert" (exp (gen) x) a (privk ca)))
      (recv (enc "cert" (exp (gen) x-0) b (privk ca)))
      (send (enc n (exp (gen) (mul x x-0)))) (recv n))
    ((send (enc "cert" (exp (gen) x) a (privk ca))))
    ((send (enc "cert" (exp (gen) x-0) b (privk ca))))
    ((recv (enc "cert" (exp (gen) x) a (privk ca)))
      (recv (enc "cert" (exp (gen) x-0) b (privk ca)))
      (recv (enc n (exp (gen) (mul x x-0)))) (send n)))
  (label 14)
  (parent 7)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (ca ca) (h (exp (gen) x-0)) (x x) (n n))))
  (origs (n (0 2))))

(defskeleton staticdh1
  (vars (n text) (a b ca name) (x x-0 rndx))
  (defstrand init 4 (n n) (a a) (b b) (ca ca) (h (exp (gen) x-0)) (x x))
  (defstrand ca 1 (p a) (ca ca) (x x))
  (defstrand ca 1 (p b) (ca ca) (x x-0))
  (defstrand resp 4 (n n) (a a) (b b) (ca ca) (h (exp (gen) x)) (y x-0))
  (defstrand ca 1 (p b) (ca ca) (x x-0))
  (precedes ((0 2) (3 2)) ((1 0) (0 0)) ((1 0) (3 0)) ((2 0) (0 1))
    ((3 3) (0 3)) ((4 0) (3 1)))
  (fn-of (owner-of (b x-0) (a x)))
  (neq (a b))
  (non-orig (privk ca) x x-0)
  (uniq-orig n)
  (operation encryption-test (added-strand ca 1)
    (enc "cert" (exp (gen) x-0) b (privk ca)) (3 1))
  (traces
    ((recv (enc "cert" (exp (gen) x) a (privk ca)))
      (recv (enc "cert" (exp (gen) x-0) b (privk ca)))
      (send (enc n (exp (gen) (mul x x-0)))) (recv n))
    ((send (enc "cert" (exp (gen) x) a (privk ca))))
    ((send (enc "cert" (exp (gen) x-0) b (privk ca))))
    ((recv (enc "cert" (exp (gen) x) a (privk ca)))
      (recv (enc "cert" (exp (gen) x-0) b (privk ca)))
      (recv (enc n (exp (gen) (mul x x-0)))) (send n))
    ((send (enc "cert" (exp (gen) x-0) b (privk ca)))))
  (label 15)
  (parent 7)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (ca ca) (h (exp (gen) x-0)) (x x) (n n))))
  (origs (n (0 2))))

(defskeleton staticdh1
  (vars (n text) (a b ca name) (x x-0 rndx))
  (defstrand init 4 (n n) (a a) (b b) (ca ca) (h (exp (gen) x-0)) (x x))
  (defstrand ca 1 (p a) (ca ca) (x x))
  (defstrand ca 1 (p b) (ca ca) (x x-0))
  (defstrand resp 4 (n n) (a a) (b b) (ca ca) (h (exp (gen) x)) (y x-0))
  (defstrand ca 1 (p a) (ca ca) (x x))
  (precedes ((0 2) (3 2)) ((1 0) (0 0)) ((2 0) (0 1)) ((2 0) (3 1))
    ((3 3) (0 3)) ((4 0) (3 0)))
  (fn-of (owner-of (b x-0) (a x)))
  (neq (a b))
  (non-orig (privk ca) x x-0)
  (uniq-orig n)
  (operation encryption-test (displaced 5 2 ca 1)
    (enc "cert" (exp (gen) x-0) b-0 (privk ca-0)) (3 1))
  (traces
    ((recv (enc "cert" (exp (gen) x) a (privk ca)))
      (recv (enc "cert" (exp (gen) x-0) b (privk ca)))
      (send (enc n (exp (gen) (mul x x-0)))) (recv n))
    ((send (enc "cert" (exp (gen) x) a (privk ca))))
    ((send (enc "cert" (exp (gen) x-0) b (privk ca))))
    ((recv (enc "cert" (exp (gen) x) a (privk ca)))
      (recv (enc "cert" (exp (gen) x-0) b (privk ca)))
      (recv (enc n (exp (gen) (mul x x-0)))) (send n))
    ((send (enc "cert" (exp (gen) x) a (privk ca)))))
  (label 16)
  (parent 8)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (ca ca) (h (exp (gen) x-0)) (x x) (n n))))
  (origs (n (0 2))))

(defskeleton staticdh1
  (vars (n text) (a b ca ca-0 name) (x x-0 rndx))
  (defstrand init 4 (n n) (a a) (b b) (ca ca) (h (exp (gen) x-0)) (x x))
  (defstrand ca 1 (p a) (ca ca) (x x))
  (defstrand ca 1 (p b) (ca ca) (x x-0))
  (defstrand resp 4 (n n) (a a) (b b) (ca ca-0) (h (exp (gen) x))
    (y x-0))
  (defstrand ca 1 (p a) (ca ca-0) (x x))
  (defstrand ca 1 (p b) (ca ca-0) (x x-0))
  (precedes ((0 2) (3 2)) ((1 0) (0 0)) ((2 0) (0 1)) ((3 3) (0 3))
    ((4 0) (3 0)) ((5 0) (3 1)))
  (fn-of (owner-of (b x-0) (a x)))
  (neq (a b))
  (non-orig (privk ca) (privk ca-0) x x-0)
  (uniq-orig n)
  (operation encryption-test (added-strand ca 1)
    (enc "cert" (exp (gen) x-0) b (privk ca-0)) (3 1))
  (traces
    ((recv (enc "cert" (exp (gen) x) a (privk ca)))
      (recv (enc "cert" (exp (gen) x-0) b (privk ca)))
      (send (enc n (exp (gen) (mul x x-0)))) (recv n))
    ((send (enc "cert" (exp (gen) x) a (privk ca))))
    ((send (enc "cert" (exp (gen) x-0) b (privk ca))))
    ((recv (enc "cert" (exp (gen) x) a (privk ca-0)))
      (recv (enc "cert" (exp (gen) x-0) b (privk ca-0)))
      (recv (enc n (exp (gen) (mul x x-0)))) (send n))
    ((send (enc "cert" (exp (gen) x) a (privk ca-0))))
    ((send (enc "cert" (exp (gen) x-0) b (privk ca-0)))))
  (label 17)
  (parent 8)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (ca ca) (h (exp (gen) x-0)) (x x) (n n))))
  (origs (n (0 2))))

(defskeleton staticdh1
  (vars (n text) (a b ca name) (x x-0 rndx))
  (defstrand init 4 (n n) (a a) (b b) (ca ca) (h (exp (gen) x-0)) (x x))
  (defstrand ca 1 (p a) (ca ca) (x x))
  (defstrand ca 1 (p b) (ca ca) (x x-0))
  (deflistener (exp (gen) (mul x x-0)))
  (deflistener (cat (gen) (mul x x-0)))
  (precedes ((1 0) (0 0)) ((2 0) (0 1)) ((3 1) (0 3)) ((4 1) (3 0)))
  (fn-of (owner-of (b x-0) (a x)))
  (neq (a b))
  (non-orig (privk ca) x x-0)
  (uniq-orig n)
  (precur (4 0))
  (operation nonce-test (contracted (x-1 x) (x-2 x-0) (w (mul x x-0)))
    (gen) (4 0))
  (traces
    ((recv (enc "cert" (exp (gen) x) a (privk ca)))
      (recv (enc "cert" (exp (gen) x-0) b (privk ca)))
      (send (enc n (exp (gen) (mul x x-0)))) (recv n))
    ((send (enc "cert" (exp (gen) x) a (privk ca))))
    ((send (enc "cert" (exp (gen) x-0) b (privk ca))))
    ((recv (exp (gen) (mul x x-0))) (send (exp (gen) (mul x x-0))))
    ((recv (cat (gen) (mul x x-0))) (send (cat (gen) (mul x x-0)))))
  (label 18)
  (parent 9)
  (unrealized (3 0) (4 0))
  (comment "empty cohort"))

(defskeleton staticdh1
  (vars (n text) (a b ca p ca-0 name) (x x-0 x-1 rndx))
  (defstrand init 4 (n n) (a a) (b b) (ca ca) (h (exp (gen) x-0)) (x x))
  (defstrand ca 1 (p a) (ca ca) (x x))
  (defstrand ca 1 (p b) (ca ca) (x x-0))
  (deflistener (exp (gen) (mul x x-0)))
  (deflistener (cat (exp (gen) x-1) (mul x x-0 (rec x-1))))
  (defstrand ca 1 (p p) (ca ca-0) (x x-1))
  (precedes ((1 0) (0 0)) ((2 0) (0 1)) ((3 1) (0 3)) ((4 1) (3 0))
    ((5 0) (4 0)))
  (fn-of (owner-of (p x-1) (b x-0) (a x)))
  (neq (a b))
  (non-orig (privk ca) x x-0 x-1)
  (precur (4 0))
  (uniq-orig n)
  (operation nonce-test (added-strand ca 1) (exp (gen) x-1) (4 0))
  (traces
    ((recv (enc "cert" (exp (gen) x) a (privk ca)))
      (recv (enc "cert" (exp (gen) x-0) b (privk ca)))
      (send (enc n (exp (gen) (mul x x-0)))) (recv n))
    ((send (enc "cert" (exp (gen) x) a (privk ca))))
    ((send (enc "cert" (exp (gen) x-0) b (privk ca))))
    ((recv (exp (gen) (mul x x-0))) (send (exp (gen) (mul x x-0))))
    ((recv (cat (exp (gen) x-1) (mul x x-0 (rec x-1))))
      (send (cat (exp (gen) x-1) (mul x x-0 (rec x-1)))))
    ((send (enc "cert" (exp (gen) x-1) p (privk ca-0)))))
  (label 19)
  (parent 9)
  (unrealized (3 0) (4 0))
  (comment "empty cohort"))

(comment "Nothing left to do")
