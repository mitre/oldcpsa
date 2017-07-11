(herald "Diffie-Hellman enhanced Needham-Schroeder-Lowe Protocol"
  (algebra diffie-hellman))

(comment "CPSA 3.0.2")
(comment "All input read from dhnsl.lsp")

(defprotocol dhnsl diffie-hellman
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

(defskeleton dhnsl
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
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 name) (h2 h3 h2-0 base) (x x-0 x-1 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x x-0 (rec x-1)))) (x x-1))
  (precedes ((0 0) (1 1)) ((1 2) (0 1)))
  (non-orig (privk a) (privk b))
  (uniq-gen x x-1)
  (operation nonce-test (added-strand init 3) (exp h2 x) (0 1)
    (enc (exp (gen) x) a (pubk b)))
  (traces
    ((send (enc (exp (gen) x) a (pubk b)))
      (recv (enc h2 (exp h2 x) h3 b (pubk a)))
      (send (enc (exp h3 x) (pubk b))))
    ((send (enc (exp (gen) x-1) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-1) (exp h2 (mul x x-0 (rec x-1))) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-0)) (pubk b-0)))))
  (label 1)
  (parent 0)
  (unrealized (1 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b name) (h3 base) (x x-0 expn))
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
  (label 2)
  (parent 0)
  (seen 2)
  (unrealized (0 1))
  (origs)
  (comment "1 in cohort - 0 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 name) (h2 h3 h2-0 h2-1 base)
    (x x-0 x-1 x-2 x-3 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-1))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x (rec x-0) x-1))) (x x-0))
  (defstrand init 3 (a a-1) (b b-1) (h2 h2-1)
    (h3 (exp h2-0 (mul x-0 x-2 (rec x-3)))) (x x-3))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 2) (1 1)))
  (non-orig (privk a) (privk b))
  (uniq-gen x-0 x-1 x-3)
  (operation nonce-test (added-strand init 3) (exp h2-0 x-0) (1 1)
    (exp (gen) x-0))
  (traces
    ((send (enc (exp (gen) x-1) a (pubk b)))
      (recv (enc h2 (exp h2 x-1) h3 b (pubk a)))
      (send (enc (exp h3 x-1) (pubk b))))
    ((send (enc (exp (gen) x-0) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-0) (exp h2 (mul x (rec x-0) x-1)) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-1)) (pubk b-0))))
    ((send (enc (exp (gen) x-3) a-1 (pubk b-1)))
      (recv
        (enc h2-1 (exp h2-1 x-3) (exp h2-0 (mul x-0 x-2 (rec x-3))) b-1
          (pubk a-1)))
      (send (enc (exp h2-0 (mul x-0 x-2)) (pubk b-1)))))
  (label 3)
  (parent 1)
  (unrealized (1 1) (2 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 b-1 a-1 name) (h2 h3 h2-0 base)
    (x z x-0 x-1 x-2 x-3 x-4 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-1))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp h2-0 x-4))
    (h3 (exp h2 (mul x (rec x-0) x-1))) (x x-0))
  (defstrand resp 2 (b b-1) (a a-1) (h1 (exp h2-0 (mul x-0 x-2)))
    (y x-3) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (2 0)) ((1 2) (0 1)) ((2 1) (1 1)))
  (ind-zero-in (x-3 (exp h2-0 (mul x-0 x-2)))
    (z (exp h2-0 (mul x-0 x-2))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-0 x-1 x-3)
  (operation nonce-test (added-strand resp 2) (exp h2-0 (mul x-0 x-4))
    (1 1) (exp (gen) x-0))
  (traces
    ((send (enc (exp (gen) x-1) a (pubk b)))
      (recv (enc h2 (exp h2 x-1) h3 b (pubk a)))
      (send (enc (exp h3 x-1) (pubk b))))
    ((send (enc (exp (gen) x-0) a-0 (pubk b-0)))
      (recv
        (enc (exp h2-0 x-4) (exp h2-0 (mul x-0 x-4))
          (exp h2 (mul x (rec x-0) x-1)) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x x-1)) (pubk b-0))))
    ((recv (enc (exp h2-0 (mul x-0 x-2)) a-1 (pubk b-1)))
      (send
        (enc (exp (gen) x-3) (exp h2-0 (mul x-0 x-2 x-3)) (exp (gen) z)
          b-1 (pubk a-1)))))
  (label 4)
  (parent 1)
  (unrealized (1 1) (2 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 name) (h2 h3 base) (x x-0 x-1 x-2 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp (gen) x-2))
    (h3 (exp h2 (mul x x-0 (rec x-1)))) (x x-1))
  (precedes ((0 0) (1 1)) ((1 2) (0 1)))
  (non-orig (privk a) (privk b))
  (uniq-gen x x-1)
  (operation nonce-test (algebra-contracted (h2-0 (exp (gen) x-2)))
    (exp (gen) (mul x-1 x-2)) (1 1) (exp (gen) x-1))
  (traces
    ((send (enc (exp (gen) x) a (pubk b)))
      (recv (enc h2 (exp h2 x) h3 b (pubk a)))
      (send (enc (exp h3 x) (pubk b))))
    ((send (enc (exp (gen) x-1) a-0 (pubk b-0)))
      (recv
        (enc (exp (gen) x-2) (exp (gen) (mul x-1 x-2))
          (exp h2 (mul x x-0 (rec x-1))) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x x-0)) (pubk b-0)))))
  (label 5)
  (parent 1)
  (unrealized (1 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 name) (h2 h3 h2-0 h2-1 h2-2 base)
    (x x-0 x-1 x-2 x-3 x-4 x-5 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-2))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x x-2 (rec x-3)))) (x x-3))
  (defstrand init 3 (a a-1) (b b-1) (h2 h2-1)
    (h3 (exp h2-0 (mul x-0 (rec x-1) x-3))) (x x-1))
  (defstrand init 3 (a a-2) (b b-2) (h2 h2-2)
    (h3 (exp h2-1 (mul x-1 x-4 (rec x-5)))) (x x-5))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (3 1))
    ((2 2) (1 1)) ((3 2) (2 1)))
  (non-orig (privk a) (privk b))
  (uniq-gen x-1 x-2 x-3 x-5)
  (operation nonce-test (added-strand init 3) (exp h2-1 x-1) (2 1)
    (exp (gen) x-1))
  (traces
    ((send (enc (exp (gen) x-2) a (pubk b)))
      (recv (enc h2 (exp h2 x-2) h3 b (pubk a)))
      (send (enc (exp h3 x-2) (pubk b))))
    ((send (enc (exp (gen) x-3) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-3) (exp h2 (mul x x-2 (rec x-3))) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-2)) (pubk b-0))))
    ((send (enc (exp (gen) x-1) a-1 (pubk b-1)))
      (recv
        (enc h2-1 (exp h2-1 x-1) (exp h2-0 (mul x-0 (rec x-1) x-3)) b-1
          (pubk a-1))) (send (enc (exp h2-0 (mul x-0 x-3)) (pubk b-1))))
    ((send (enc (exp (gen) x-5) a-2 (pubk b-2)))
      (recv
        (enc h2-2 (exp h2-2 x-5) (exp h2-1 (mul x-1 x-4 (rec x-5))) b-2
          (pubk a-2)))
      (send (enc (exp h2-1 (mul x-1 x-4)) (pubk b-2)))))
  (label 6)
  (parent 3)
  (unrealized (1 1) (2 1) (3 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 b-2 a-2 name) (h2 h3 h2-0 h2-1 base)
    (x x-0 z x-1 x-2 x-3 x-4 x-5 x-6 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-2))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x x-2 (rec x-3)))) (x x-3))
  (defstrand init 3 (a a-1) (b b-1) (h2 (exp h2-1 x-6))
    (h3 (exp h2-0 (mul x-0 (rec x-1) x-3))) (x x-1))
  (defstrand resp 2 (b b-2) (a a-2) (h1 (exp h2-1 (mul x-1 x-4)))
    (y x-5) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (3 0))
    ((2 2) (1 1)) ((3 1) (2 1)))
  (ind-zero-in (x-5 (exp h2-1 (mul x-1 x-4)))
    (z (exp h2-1 (mul x-1 x-4))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-1 x-2 x-3 x-5)
  (operation nonce-test (added-strand resp 2) (exp h2-1 (mul x-1 x-6))
    (2 1) (exp (gen) x-1))
  (traces
    ((send (enc (exp (gen) x-2) a (pubk b)))
      (recv (enc h2 (exp h2 x-2) h3 b (pubk a)))
      (send (enc (exp h3 x-2) (pubk b))))
    ((send (enc (exp (gen) x-3) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-3) (exp h2 (mul x x-2 (rec x-3))) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-2)) (pubk b-0))))
    ((send (enc (exp (gen) x-1) a-1 (pubk b-1)))
      (recv
        (enc (exp h2-1 x-6) (exp h2-1 (mul x-1 x-6))
          (exp h2-0 (mul x-0 (rec x-1) x-3)) b-1 (pubk a-1)))
      (send (enc (exp h2-0 (mul x-0 x-3)) (pubk b-1))))
    ((recv (enc (exp h2-1 (mul x-1 x-4)) a-2 (pubk b-2)))
      (send
        (enc (exp (gen) x-5) (exp h2-1 (mul x-1 x-4 x-5)) (exp (gen) z)
          b-2 (pubk a-2)))))
  (label 7)
  (parent 3)
  (unrealized (1 1) (2 1) (3 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 name) (h2 h3 h2-0 base)
    (x x-0 x-1 x-2 x-3 x-4 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-1))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x (rec x-0) x-1))) (x x-0))
  (defstrand init 3 (a a-1) (b b-1) (h2 (exp (gen) x-4))
    (h3 (exp h2-0 (mul x-0 x-2 (rec x-3)))) (x x-3))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 2) (1 1)))
  (non-orig (privk a) (privk b))
  (uniq-gen x-0 x-1 x-3)
  (operation nonce-test (algebra-contracted (h2-1 (exp (gen) x-4)))
    (exp (gen) (mul x-3 x-4)) (2 1) (exp (gen) x-3))
  (traces
    ((send (enc (exp (gen) x-1) a (pubk b)))
      (recv (enc h2 (exp h2 x-1) h3 b (pubk a)))
      (send (enc (exp h3 x-1) (pubk b))))
    ((send (enc (exp (gen) x-0) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-0) (exp h2 (mul x (rec x-0) x-1)) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-1)) (pubk b-0))))
    ((send (enc (exp (gen) x-3) a-1 (pubk b-1)))
      (recv
        (enc (exp (gen) x-4) (exp (gen) (mul x-3 x-4))
          (exp h2-0 (mul x-0 x-2 (rec x-3))) b-1 (pubk a-1)))
      (send (enc (exp h2-0 (mul x-0 x-2)) (pubk b-1)))))
  (label 8)
  (parent 3)
  (unrealized (1 1) (2 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 b-1 a-1 a-2 b-2 name) (h2 h3 h2-0 h2-1 base)
    (x x-0 z x-1 x-2 x-3 x-4 x-5 x-6 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-1))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp h2-0 x-0))
    (h3 (exp h2 (mul x x-1 (rec x-2)))) (x x-2))
  (defstrand resp 2 (b b-1) (a a-1) (h1 (exp h2-0 (mul x-2 x-4))) (y z)
    (z x-3))
  (defstrand init 3 (a a-2) (b b-2) (h2 h2-1)
    (h3 (exp h2-0 (mul x-2 x-4 x-5 (rec x-6)))) (x x-6))
  (precedes ((0 0) (1 1)) ((1 0) (3 1)) ((1 2) (0 1)) ((2 1) (1 1))
    ((3 2) (2 0)))
  (ind-zero-in (z (exp h2-0 (mul x-2 x-4)))
    (x-3 (exp h2-0 (mul x-2 x-4))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-1 x-2 x-3 x-6)
  (operation nonce-test (added-strand init 3) (exp h2-0 (mul x-2 x-4))
    (2 0) (exp (gen) x-2))
  (traces
    ((send (enc (exp (gen) x-1) a (pubk b)))
      (recv (enc h2 (exp h2 x-1) h3 b (pubk a)))
      (send (enc (exp h3 x-1) (pubk b))))
    ((send (enc (exp (gen) x-2) a-0 (pubk b-0)))
      (recv
        (enc (exp h2-0 x-0) (exp h2-0 (mul x-0 x-2))
          (exp h2 (mul x x-1 (rec x-2))) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x x-1)) (pubk b-0))))
    ((recv (enc (exp h2-0 (mul x-2 x-4)) a-1 (pubk b-1)))
      (send
        (enc (exp (gen) z) (exp h2-0 (mul z x-2 x-4)) (exp (gen) x-3)
          b-1 (pubk a-1))))
    ((send (enc (exp (gen) x-6) a-2 (pubk b-2)))
      (recv
        (enc h2-1 (exp h2-1 x-6) (exp h2-0 (mul x-2 x-4 x-5 (rec x-6)))
          b-2 (pubk a-2)))
      (send (enc (exp h2-0 (mul x-2 x-4 x-5)) (pubk b-2)))))
  (label 9)
  (parent 4)
  (unrealized (1 1) (3 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 b-1 a-1 b-2 a-2 name) (h2 h3 h2-0 base)
    (x x-0 z z-0 x-1 x-2 x-3 x-4 x-5 x-6 x-7 x-8 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-1))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp h2-0 (mul x-0 x-7)))
    (h3 (exp h2 (mul x x-1 (rec x-2)))) (x x-2))
  (defstrand resp 2 (b b-1) (a a-1)
    (h1 (exp h2-0 (mul x-2 x-4 x-5 x-6 (rec x-8)))) (y z-0) (z x-3))
  (defstrand resp 2 (b b-2) (a a-2) (h1 (exp h2-0 (mul x-2 x-4 x-5)))
    (y x-6) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (3 0)) ((1 2) (0 1)) ((2 1) (1 1))
    ((3 1) (2 0)))
  (ind-zero-in (x-6 (exp h2-0 (mul x-2 x-4 x-5)))
    (z (exp h2-0 (mul x-2 x-4 x-5)))
    (z-0 (exp h2-0 (mul x-2 x-4 x-5 x-6 (rec x-8))))
    (x-3 (exp h2-0 (mul x-2 x-4 x-5 x-6 (rec x-8)))))
  (non-orig (privk a) (privk b))
  (uniq-gen z z-0 x-1 x-2 x-3 x-6)
  (operation nonce-test (added-strand resp 2)
    (exp h2-0 (mul x-2 x-4 x-5 x-6 (rec x-8))) (2 0) (exp (gen) x-2))
  (traces
    ((send (enc (exp (gen) x-1) a (pubk b)))
      (recv (enc h2 (exp h2 x-1) h3 b (pubk a)))
      (send (enc (exp h3 x-1) (pubk b))))
    ((send (enc (exp (gen) x-2) a-0 (pubk b-0)))
      (recv
        (enc (exp h2-0 (mul x-0 x-7)) (exp h2-0 (mul x-0 x-2 x-7))
          (exp h2 (mul x x-1 (rec x-2))) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x x-1)) (pubk b-0))))
    ((recv
       (enc (exp h2-0 (mul x-2 x-4 x-5 x-6 (rec x-8))) a-1 (pubk b-1)))
      (send
        (enc (exp (gen) z-0)
          (exp h2-0 (mul z-0 x-2 x-4 x-5 x-6 (rec x-8))) (exp (gen) x-3)
          b-1 (pubk a-1))))
    ((recv (enc (exp h2-0 (mul x-2 x-4 x-5)) a-2 (pubk b-2)))
      (send
        (enc (exp (gen) x-6) (exp h2-0 (mul x-2 x-4 x-5 x-6))
          (exp (gen) z) b-2 (pubk a-2)))))
  (label 10)
  (parent 4)
  (unrealized (1 1) (3 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 b-1 a-1 name) (h2 h3 base)
    (x z x-0 x-1 x-2 x-3 x-4 x-5 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-1))
  (defstrand init 3 (a a-0) (b b-0)
    (h2 (exp (gen) (mul (rec x-2) x-4 x-5)))
    (h3 (exp h2 (mul x (rec x-0) x-1))) (x x-0))
  (defstrand resp 2 (b b-1) (a a-1) (h1 (exp (gen) (mul x-0 x-5)))
    (y x-3) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (2 0)) ((1 2) (0 1)) ((2 1) (1 1)))
  (ind-zero-in (x-3 (exp (gen) (mul x-0 x-5)))
    (z (exp (gen) (mul x-0 x-5))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-0 x-1 x-3)
  (operation nonce-test
    (algebra-contracted (h2-0 (exp (gen) (mul (rec x-2) x-5))))
    (exp (gen) (mul x-0 x-5)) (2 0) (exp (gen) x-0))
  (traces
    ((send (enc (exp (gen) x-1) a (pubk b)))
      (recv (enc h2 (exp h2 x-1) h3 b (pubk a)))
      (send (enc (exp h3 x-1) (pubk b))))
    ((send (enc (exp (gen) x-0) a-0 (pubk b-0)))
      (recv
        (enc (exp (gen) (mul (rec x-2) x-4 x-5))
          (exp (gen) (mul x-0 (rec x-2) x-4 x-5))
          (exp h2 (mul x (rec x-0) x-1)) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x x-1)) (pubk b-0))))
    ((recv (enc (exp (gen) (mul x-0 x-5)) a-1 (pubk b-1)))
      (send
        (enc (exp (gen) x-3) (exp (gen) (mul x-0 x-3 x-5)) (exp (gen) z)
          b-1 (pubk a-1)))))
  (label 11)
  (parent 4)
  (unrealized (1 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 name) (h2 h3 h2-0 base)
    (x x-0 x-1 x-2 x-3 x-4 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-1))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp (gen) x))
    (h3 (exp h2 (mul (rec x-0) x-1 x-2))) (x x-0))
  (defstrand init 3 (a a-1) (b b-1) (h2 h2-0)
    (h3 (exp h2 (mul (rec x-0) x-1 x-2 x-3 (rec x-4)))) (x x-4))
  (precedes ((0 0) (2 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 2) (1 1)))
  (non-orig (privk a) (privk b))
  (uniq-gen x-0 x-1 x-4)
  (operation nonce-test (added-strand init 3)
    (exp h2 (mul (rec x-0) x-1 x-2)) (1 1))
  (traces
    ((send (enc (exp (gen) x-1) a (pubk b)))
      (recv (enc h2 (exp h2 x-1) h3 b (pubk a)))
      (send (enc (exp h3 x-1) (pubk b))))
    ((send (enc (exp (gen) x-0) a-0 (pubk b-0)))
      (recv
        (enc (exp (gen) x) (exp (gen) (mul x x-0))
          (exp h2 (mul (rec x-0) x-1 x-2)) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x-1 x-2)) (pubk b-0))))
    ((send (enc (exp (gen) x-4) a-1 (pubk b-1)))
      (recv
        (enc h2-0 (exp h2-0 x-4)
          (exp h2 (mul (rec x-0) x-1 x-2 x-3 (rec x-4))) b-1
          (pubk a-1)))
      (send (enc (exp h2 (mul (rec x-0) x-1 x-2 x-3)) (pubk b-1)))))
  (label 12)
  (parent 5)
  (unrealized (2 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 b-1 a-1 name) (h2 h3 base)
    (x z x-0 x-1 x-2 x-3 x-4 x-5 x-6 expn))
  (defstrand init 3 (a a) (b b) (h2 (exp h2 x-5)) (h3 h3) (x x-1))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp (gen) x))
    (h3 (exp h2 (mul (rec x-0) x-1 x-2 x-3 x-4 (rec x-6)))) (x x-0))
  (defstrand resp 2 (b b-1) (a a-1)
    (h1 (exp h2 (mul (rec x-0) x-1 x-2 x-3))) (y x-4) (z z))
  (precedes ((0 0) (2 0)) ((1 0) (2 0)) ((1 2) (0 1)) ((2 1) (1 1)))
  (ind-zero-in (x-4 (exp h2 (mul (rec x-0) x-1 x-2 x-3)))
    (z (exp h2 (mul (rec x-0) x-1 x-2 x-3))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-0 x-1 x-4)
  (operation nonce-test (added-strand resp 2)
    (exp h2 (mul (rec x-0) x-1 x-2 x-3 x-4 (rec x-6))) (1 1))
  (traces
    ((send (enc (exp (gen) x-1) a (pubk b)))
      (recv (enc (exp h2 x-5) (exp h2 (mul x-1 x-5)) h3 b (pubk a)))
      (send (enc (exp h3 x-1) (pubk b))))
    ((send (enc (exp (gen) x-0) a-0 (pubk b-0)))
      (recv
        (enc (exp (gen) x) (exp (gen) (mul x x-0))
          (exp h2 (mul (rec x-0) x-1 x-2 x-3 x-4 (rec x-6))) b-0
          (pubk a-0)))
      (send (enc (exp h2 (mul x-1 x-2 x-3 x-4 (rec x-6))) (pubk b-0))))
    ((recv (enc (exp h2 (mul (rec x-0) x-1 x-2 x-3)) a-1 (pubk b-1)))
      (send
        (enc (exp (gen) x-4) (exp h2 (mul (rec x-0) x-1 x-2 x-3 x-4))
          (exp (gen) z) b-1 (pubk a-1)))))
  (label 13)
  (parent 5)
  (unrealized (0 1) (2 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 name) (h3 base) (x x-0 x-1 x-2 x-3 expn))
  (defstrand init 3 (a a) (b b)
    (h2 (exp (gen) (mul (rec x) (rec x-0) x-1 x-1 x-3))) (h3 h3) (x x))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp (gen) x-2))
    (h3 (exp (gen) (mul x-1 x-3))) (x x-1))
  (precedes ((0 0) (1 1)) ((1 2) (0 1)))
  (non-orig (privk a) (privk b))
  (uniq-gen x x-1)
  (operation nonce-test
    (algebra-contracted
      (h2 (exp (gen) (mul (rec x) (rec x-0) x-1 x-1 x-3))))
    (exp (gen) (mul x-1 x-3)) (1 1))
  (traces
    ((send (enc (exp (gen) x) a (pubk b)))
      (recv
        (enc (exp (gen) (mul (rec x) (rec x-0) x-1 x-1 x-3))
          (exp (gen) (mul (rec x-0) x-1 x-1 x-3)) h3 b (pubk a)))
      (send (enc (exp h3 x) (pubk b))))
    ((send (enc (exp (gen) x-1) a-0 (pubk b-0)))
      (recv
        (enc (exp (gen) x-2) (exp (gen) (mul x-1 x-2))
          (exp (gen) (mul x-1 x-3)) b-0 (pubk a-0)))
      (send (enc (exp (gen) (mul x-1 x-1 x-3)) (pubk b-0)))))
  (label 14)
  (parent 5)
  (seen 14)
  (unrealized (0 1))
  (comment "1 in cohort - 0 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 a-3 b-3 name)
    (h2 h3 h2-0 h2-1 h2-2 h2-3 base)
    (x x-0 x-1 x-2 x-3 x-4 x-5 x-6 x-7 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-4))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x (rec x-3) x-4))) (x x-3))
  (defstrand init 3 (a a-1) (b b-1) (h2 h2-1)
    (h3 (exp h2-0 (mul x-0 x-3 (rec x-5)))) (x x-5))
  (defstrand init 3 (a a-2) (b b-2) (h2 h2-2)
    (h3 (exp h2-1 (mul x-1 (rec x-2) x-5))) (x x-2))
  (defstrand init 3 (a a-3) (b b-3) (h2 h2-3)
    (h3 (exp h2-2 (mul x-2 x-6 (rec x-7)))) (x x-7))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (3 1))
    ((2 2) (1 1)) ((3 0) (4 1)) ((3 2) (2 1)) ((4 2) (3 1)))
  (non-orig (privk a) (privk b))
  (uniq-gen x-2 x-3 x-4 x-5 x-7)
  (operation nonce-test (added-strand init 3) (exp h2-2 x-2) (3 1)
    (exp (gen) x-2))
  (traces
    ((send (enc (exp (gen) x-4) a (pubk b)))
      (recv (enc h2 (exp h2 x-4) h3 b (pubk a)))
      (send (enc (exp h3 x-4) (pubk b))))
    ((send (enc (exp (gen) x-3) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-3) (exp h2 (mul x (rec x-3) x-4)) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-4)) (pubk b-0))))
    ((send (enc (exp (gen) x-5) a-1 (pubk b-1)))
      (recv
        (enc h2-1 (exp h2-1 x-5) (exp h2-0 (mul x-0 x-3 (rec x-5))) b-1
          (pubk a-1))) (send (enc (exp h2-0 (mul x-0 x-3)) (pubk b-1))))
    ((send (enc (exp (gen) x-2) a-2 (pubk b-2)))
      (recv
        (enc h2-2 (exp h2-2 x-2) (exp h2-1 (mul x-1 (rec x-2) x-5)) b-2
          (pubk a-2))) (send (enc (exp h2-1 (mul x-1 x-5)) (pubk b-2))))
    ((send (enc (exp (gen) x-7) a-3 (pubk b-3)))
      (recv
        (enc h2-3 (exp h2-3 x-7) (exp h2-2 (mul x-2 x-6 (rec x-7))) b-3
          (pubk a-3)))
      (send (enc (exp h2-2 (mul x-2 x-6)) (pubk b-3)))))
  (label 15)
  (parent 6)
  (unrealized (1 1) (2 1) (3 1) (4 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 b-3 a-3 name)
    (h2 h3 h2-0 h2-1 h2-2 base)
    (x x-0 x-1 z x-2 x-3 x-4 x-5 x-6 x-7 x-8 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-4))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x (rec x-3) x-4))) (x x-3))
  (defstrand init 3 (a a-1) (b b-1) (h2 h2-1)
    (h3 (exp h2-0 (mul x-0 x-3 (rec x-5)))) (x x-5))
  (defstrand init 3 (a a-2) (b b-2) (h2 (exp h2-2 x-8))
    (h3 (exp h2-1 (mul x-1 (rec x-2) x-5))) (x x-2))
  (defstrand resp 2 (b b-3) (a a-3) (h1 (exp h2-2 (mul x-2 x-6)))
    (y x-7) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (3 1))
    ((2 2) (1 1)) ((3 0) (4 0)) ((3 2) (2 1)) ((4 1) (3 1)))
  (ind-zero-in (x-7 (exp h2-2 (mul x-2 x-6)))
    (z (exp h2-2 (mul x-2 x-6))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-2 x-3 x-4 x-5 x-7)
  (operation nonce-test (added-strand resp 2) (exp h2-2 (mul x-2 x-8))
    (3 1) (exp (gen) x-2))
  (traces
    ((send (enc (exp (gen) x-4) a (pubk b)))
      (recv (enc h2 (exp h2 x-4) h3 b (pubk a)))
      (send (enc (exp h3 x-4) (pubk b))))
    ((send (enc (exp (gen) x-3) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-3) (exp h2 (mul x (rec x-3) x-4)) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-4)) (pubk b-0))))
    ((send (enc (exp (gen) x-5) a-1 (pubk b-1)))
      (recv
        (enc h2-1 (exp h2-1 x-5) (exp h2-0 (mul x-0 x-3 (rec x-5))) b-1
          (pubk a-1))) (send (enc (exp h2-0 (mul x-0 x-3)) (pubk b-1))))
    ((send (enc (exp (gen) x-2) a-2 (pubk b-2)))
      (recv
        (enc (exp h2-2 x-8) (exp h2-2 (mul x-2 x-8))
          (exp h2-1 (mul x-1 (rec x-2) x-5)) b-2 (pubk a-2)))
      (send (enc (exp h2-1 (mul x-1 x-5)) (pubk b-2))))
    ((recv (enc (exp h2-2 (mul x-2 x-6)) a-3 (pubk b-3)))
      (send
        (enc (exp (gen) x-7) (exp h2-2 (mul x-2 x-6 x-7)) (exp (gen) z)
          b-3 (pubk a-3)))))
  (label 16)
  (parent 6)
  (unrealized (1 1) (2 1) (3 1) (4 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 name) (h2 h3 h2-0 h2-1 base)
    (x x-0 x-1 x-2 x-3 x-4 x-5 x-6 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-2))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x x-2 (rec x-3)))) (x x-3))
  (defstrand init 3 (a a-1) (b b-1) (h2 h2-1)
    (h3 (exp h2-0 (mul x-0 (rec x-1) x-3))) (x x-1))
  (defstrand init 3 (a a-2) (b b-2) (h2 (exp (gen) x-6))
    (h3 (exp h2-1 (mul x-1 x-4 (rec x-5)))) (x x-5))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (3 1))
    ((2 2) (1 1)) ((3 2) (2 1)))
  (non-orig (privk a) (privk b))
  (uniq-gen x-1 x-2 x-3 x-5)
  (operation nonce-test (algebra-contracted (h2-2 (exp (gen) x-6)))
    (exp (gen) (mul x-5 x-6)) (3 1) (exp (gen) x-5))
  (traces
    ((send (enc (exp (gen) x-2) a (pubk b)))
      (recv (enc h2 (exp h2 x-2) h3 b (pubk a)))
      (send (enc (exp h3 x-2) (pubk b))))
    ((send (enc (exp (gen) x-3) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-3) (exp h2 (mul x x-2 (rec x-3))) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-2)) (pubk b-0))))
    ((send (enc (exp (gen) x-1) a-1 (pubk b-1)))
      (recv
        (enc h2-1 (exp h2-1 x-1) (exp h2-0 (mul x-0 (rec x-1) x-3)) b-1
          (pubk a-1))) (send (enc (exp h2-0 (mul x-0 x-3)) (pubk b-1))))
    ((send (enc (exp (gen) x-5) a-2 (pubk b-2)))
      (recv
        (enc (exp (gen) x-6) (exp (gen) (mul x-5 x-6))
          (exp h2-1 (mul x-1 x-4 (rec x-5))) b-2 (pubk a-2)))
      (send (enc (exp h2-1 (mul x-1 x-4)) (pubk b-2)))))
  (label 17)
  (parent 6)
  (unrealized (1 1) (2 1) (3 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 b-2 a-2 a-3 b-3 name)
    (h2 h3 h2-0 h2-1 h2-2 base)
    (x x-0 x-1 z x-2 x-3 x-4 x-5 x-6 x-7 x-8 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-3))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x (rec x-2) x-3))) (x x-2))
  (defstrand init 3 (a a-1) (b b-1) (h2 (exp h2-1 x-1))
    (h3 (exp h2-0 (mul x-0 x-2 (rec x-4)))) (x x-4))
  (defstrand resp 2 (b b-2) (a a-2) (h1 (exp h2-1 (mul x-4 x-6))) (y z)
    (z x-5))
  (defstrand init 3 (a a-3) (b b-3) (h2 h2-2)
    (h3 (exp h2-1 (mul x-4 x-6 x-7 (rec x-8)))) (x x-8))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (4 1))
    ((2 2) (1 1)) ((3 1) (2 1)) ((4 2) (3 0)))
  (ind-zero-in (z (exp h2-1 (mul x-4 x-6)))
    (x-5 (exp h2-1 (mul x-4 x-6))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-2 x-3 x-4 x-5 x-8)
  (operation nonce-test (added-strand init 3) (exp h2-1 (mul x-4 x-6))
    (3 0) (exp (gen) x-4))
  (traces
    ((send (enc (exp (gen) x-3) a (pubk b)))
      (recv (enc h2 (exp h2 x-3) h3 b (pubk a)))
      (send (enc (exp h3 x-3) (pubk b))))
    ((send (enc (exp (gen) x-2) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-2) (exp h2 (mul x (rec x-2) x-3)) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-3)) (pubk b-0))))
    ((send (enc (exp (gen) x-4) a-1 (pubk b-1)))
      (recv
        (enc (exp h2-1 x-1) (exp h2-1 (mul x-1 x-4))
          (exp h2-0 (mul x-0 x-2 (rec x-4))) b-1 (pubk a-1)))
      (send (enc (exp h2-0 (mul x-0 x-2)) (pubk b-1))))
    ((recv (enc (exp h2-1 (mul x-4 x-6)) a-2 (pubk b-2)))
      (send
        (enc (exp (gen) z) (exp h2-1 (mul z x-4 x-6)) (exp (gen) x-5)
          b-2 (pubk a-2))))
    ((send (enc (exp (gen) x-8) a-3 (pubk b-3)))
      (recv
        (enc h2-2 (exp h2-2 x-8) (exp h2-1 (mul x-4 x-6 x-7 (rec x-8)))
          b-3 (pubk a-3)))
      (send (enc (exp h2-1 (mul x-4 x-6 x-7)) (pubk b-3)))))
  (label 18)
  (parent 7)
  (unrealized (1 1) (2 1) (4 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 b-2 a-2 b-3 a-3 name)
    (h2 h3 h2-0 h2-1 base)
    (x x-0 x-1 z z-0 x-2 x-3 x-4 x-5 x-6 x-7 x-8 x-9 x-10 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-3))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x (rec x-2) x-3))) (x x-2))
  (defstrand init 3 (a a-1) (b b-1) (h2 (exp h2-1 (mul x-1 x-9)))
    (h3 (exp h2-0 (mul x-0 x-2 (rec x-4)))) (x x-4))
  (defstrand resp 2 (b b-2) (a a-2)
    (h1 (exp h2-1 (mul x-4 x-6 x-7 x-8 (rec x-10)))) (y z-0) (z x-5))
  (defstrand resp 2 (b b-3) (a a-3) (h1 (exp h2-1 (mul x-4 x-6 x-7)))
    (y x-8) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (4 0))
    ((2 2) (1 1)) ((3 1) (2 1)) ((4 1) (3 0)))
  (ind-zero-in (x-8 (exp h2-1 (mul x-4 x-6 x-7)))
    (z (exp h2-1 (mul x-4 x-6 x-7)))
    (z-0 (exp h2-1 (mul x-4 x-6 x-7 x-8 (rec x-10))))
    (x-5 (exp h2-1 (mul x-4 x-6 x-7 x-8 (rec x-10)))))
  (non-orig (privk a) (privk b))
  (uniq-gen z z-0 x-2 x-3 x-4 x-5 x-8)
  (operation nonce-test (added-strand resp 2)
    (exp h2-1 (mul x-4 x-6 x-7 x-8 (rec x-10))) (3 0) (exp (gen) x-4))
  (traces
    ((send (enc (exp (gen) x-3) a (pubk b)))
      (recv (enc h2 (exp h2 x-3) h3 b (pubk a)))
      (send (enc (exp h3 x-3) (pubk b))))
    ((send (enc (exp (gen) x-2) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-2) (exp h2 (mul x (rec x-2) x-3)) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-3)) (pubk b-0))))
    ((send (enc (exp (gen) x-4) a-1 (pubk b-1)))
      (recv
        (enc (exp h2-1 (mul x-1 x-9)) (exp h2-1 (mul x-1 x-4 x-9))
          (exp h2-0 (mul x-0 x-2 (rec x-4))) b-1 (pubk a-1)))
      (send (enc (exp h2-0 (mul x-0 x-2)) (pubk b-1))))
    ((recv
       (enc (exp h2-1 (mul x-4 x-6 x-7 x-8 (rec x-10))) a-2 (pubk b-2)))
      (send
        (enc (exp (gen) z-0)
          (exp h2-1 (mul z-0 x-4 x-6 x-7 x-8 (rec x-10)))
          (exp (gen) x-5) b-2 (pubk a-2))))
    ((recv (enc (exp h2-1 (mul x-4 x-6 x-7)) a-3 (pubk b-3)))
      (send
        (enc (exp (gen) x-8) (exp h2-1 (mul x-4 x-6 x-7 x-8))
          (exp (gen) z) b-3 (pubk a-3)))))
  (label 19)
  (parent 7)
  (unrealized (1 1) (2 1) (4 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 b-2 a-2 name) (h2 h3 h2-0 base)
    (x x-0 z x-1 x-2 x-3 x-4 x-5 x-6 x-7 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-2))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x x-2 (rec x-3)))) (x x-3))
  (defstrand init 3 (a a-1) (b b-1)
    (h2 (exp (gen) (mul (rec x-4) x-6 x-7)))
    (h3 (exp h2-0 (mul x-0 (rec x-1) x-3))) (x x-1))
  (defstrand resp 2 (b b-2) (a a-2) (h1 (exp (gen) (mul x-1 x-7)))
    (y x-5) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (3 0))
    ((2 2) (1 1)) ((3 1) (2 1)))
  (ind-zero-in (x-5 (exp (gen) (mul x-1 x-7)))
    (z (exp (gen) (mul x-1 x-7))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-1 x-2 x-3 x-5)
  (operation nonce-test
    (algebra-contracted (h2-1 (exp (gen) (mul (rec x-4) x-7))))
    (exp (gen) (mul x-1 x-7)) (3 0) (exp (gen) x-1))
  (traces
    ((send (enc (exp (gen) x-2) a (pubk b)))
      (recv (enc h2 (exp h2 x-2) h3 b (pubk a)))
      (send (enc (exp h3 x-2) (pubk b))))
    ((send (enc (exp (gen) x-3) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-3) (exp h2 (mul x x-2 (rec x-3))) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-2)) (pubk b-0))))
    ((send (enc (exp (gen) x-1) a-1 (pubk b-1)))
      (recv
        (enc (exp (gen) (mul (rec x-4) x-6 x-7))
          (exp (gen) (mul x-1 (rec x-4) x-6 x-7))
          (exp h2-0 (mul x-0 (rec x-1) x-3)) b-1 (pubk a-1)))
      (send (enc (exp h2-0 (mul x-0 x-3)) (pubk b-1))))
    ((recv (enc (exp (gen) (mul x-1 x-7)) a-2 (pubk b-2)))
      (send
        (enc (exp (gen) x-5) (exp (gen) (mul x-1 x-5 x-7)) (exp (gen) z)
          b-2 (pubk a-2)))))
  (label 20)
  (parent 7)
  (unrealized (1 1) (2 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 name) (h2 h3 h2-0 h2-1 base)
    (x x-0 x-1 x-2 x-3 x-4 x-5 x-6 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-2))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x x-2 (rec x-3)))) (x x-3))
  (defstrand init 3 (a a-1) (b b-1) (h2 (exp (gen) x-0))
    (h3 (exp h2-0 (mul (rec x-1) x-3 x-4))) (x x-1))
  (defstrand init 3 (a a-2) (b b-2) (h2 h2-1)
    (h3 (exp h2-0 (mul (rec x-1) x-3 x-4 x-5 (rec x-6)))) (x x-6))
  (precedes ((0 0) (1 1)) ((1 0) (3 1)) ((1 2) (0 1)) ((2 0) (3 1))
    ((2 2) (1 1)) ((3 2) (2 1)))
  (non-orig (privk a) (privk b))
  (uniq-gen x-1 x-2 x-3 x-6)
  (operation nonce-test (added-strand init 3)
    (exp h2-0 (mul (rec x-1) x-3 x-4)) (2 1))
  (traces
    ((send (enc (exp (gen) x-2) a (pubk b)))
      (recv (enc h2 (exp h2 x-2) h3 b (pubk a)))
      (send (enc (exp h3 x-2) (pubk b))))
    ((send (enc (exp (gen) x-3) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-3) (exp h2 (mul x x-2 (rec x-3))) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-2)) (pubk b-0))))
    ((send (enc (exp (gen) x-1) a-1 (pubk b-1)))
      (recv
        (enc (exp (gen) x-0) (exp (gen) (mul x-0 x-1))
          (exp h2-0 (mul (rec x-1) x-3 x-4)) b-1 (pubk a-1)))
      (send (enc (exp h2-0 (mul x-3 x-4)) (pubk b-1))))
    ((send (enc (exp (gen) x-6) a-2 (pubk b-2)))
      (recv
        (enc h2-1 (exp h2-1 x-6)
          (exp h2-0 (mul (rec x-1) x-3 x-4 x-5 (rec x-6))) b-2
          (pubk a-2)))
      (send (enc (exp h2-0 (mul (rec x-1) x-3 x-4 x-5)) (pubk b-2)))))
  (label 21)
  (parent 8)
  (unrealized (1 1) (3 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 b-2 a-2 name) (h2 h3 h2-0 base)
    (x x-0 z x-1 x-2 x-3 x-4 x-5 x-6 x-7 x-8 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-2))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp h2-0 x-7))
    (h3 (exp h2 (mul x x-2 (rec x-3)))) (x x-3))
  (defstrand init 3 (a a-1) (b b-1) (h2 (exp (gen) x-0))
    (h3 (exp h2-0 (mul (rec x-1) x-3 x-4 x-5 x-6 (rec x-8)))) (x x-1))
  (defstrand resp 2 (b b-2) (a a-2)
    (h1 (exp h2-0 (mul (rec x-1) x-3 x-4 x-5))) (y x-6) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (3 0)) ((1 2) (0 1)) ((2 0) (3 0))
    ((2 2) (1 1)) ((3 1) (2 1)))
  (ind-zero-in (x-6 (exp h2-0 (mul (rec x-1) x-3 x-4 x-5)))
    (z (exp h2-0 (mul (rec x-1) x-3 x-4 x-5))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-1 x-2 x-3 x-6)
  (operation nonce-test (added-strand resp 2)
    (exp h2-0 (mul (rec x-1) x-3 x-4 x-5 x-6 (rec x-8))) (2 1))
  (traces
    ((send (enc (exp (gen) x-2) a (pubk b)))
      (recv (enc h2 (exp h2 x-2) h3 b (pubk a)))
      (send (enc (exp h3 x-2) (pubk b))))
    ((send (enc (exp (gen) x-3) a-0 (pubk b-0)))
      (recv
        (enc (exp h2-0 x-7) (exp h2-0 (mul x-3 x-7))
          (exp h2 (mul x x-2 (rec x-3))) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x x-2)) (pubk b-0))))
    ((send (enc (exp (gen) x-1) a-1 (pubk b-1)))
      (recv
        (enc (exp (gen) x-0) (exp (gen) (mul x-0 x-1))
          (exp h2-0 (mul (rec x-1) x-3 x-4 x-5 x-6 (rec x-8))) b-1
          (pubk a-1)))
      (send
        (enc (exp h2-0 (mul x-3 x-4 x-5 x-6 (rec x-8))) (pubk b-1))))
    ((recv (enc (exp h2-0 (mul (rec x-1) x-3 x-4 x-5)) a-2 (pubk b-2)))
      (send
        (enc (exp (gen) x-6) (exp h2-0 (mul (rec x-1) x-3 x-4 x-5 x-6))
          (exp (gen) z) b-2 (pubk a-2)))))
  (label 22)
  (parent 8)
  (unrealized (1 1) (3 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 name) (h2 h3 base)
    (x x-0 x-1 x-2 x-3 x-4 x-5 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-1))
  (defstrand init 3 (a a-0) (b b-0)
    (h2 (exp (gen) (mul (rec x-0) (rec x-2) x-3 x-3 x-5)))
    (h3 (exp h2 (mul x (rec x-0) x-1))) (x x-0))
  (defstrand init 3 (a a-1) (b b-1) (h2 (exp (gen) x-4))
    (h3 (exp (gen) (mul x-3 x-5))) (x x-3))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 2) (1 1)))
  (non-orig (privk a) (privk b))
  (uniq-gen x-0 x-1 x-3)
  (operation nonce-test
    (algebra-contracted
      (h2-0 (exp (gen) (mul (rec x-0) (rec x-2) x-3 x-3 x-5))))
    (exp (gen) (mul x-3 x-5)) (2 1))
  (traces
    ((send (enc (exp (gen) x-1) a (pubk b)))
      (recv (enc h2 (exp h2 x-1) h3 b (pubk a)))
      (send (enc (exp h3 x-1) (pubk b))))
    ((send (enc (exp (gen) x-0) a-0 (pubk b-0)))
      (recv
        (enc (exp (gen) (mul (rec x-0) (rec x-2) x-3 x-3 x-5))
          (exp (gen) (mul (rec x-2) x-3 x-3 x-5))
          (exp h2 (mul x (rec x-0) x-1)) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x x-1)) (pubk b-0))))
    ((send (enc (exp (gen) x-3) a-1 (pubk b-1)))
      (recv
        (enc (exp (gen) x-4) (exp (gen) (mul x-3 x-4))
          (exp (gen) (mul x-3 x-5)) b-1 (pubk a-1)))
      (send (enc (exp (gen) (mul x-3 x-3 x-5)) (pubk b-1)))))
  (label 23)
  (parent 8)
  (unrealized (1 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 b-1 a-1 a-2 b-2 a-3 b-3 name)
    (h2 h3 h2-0 h2-1 h2-2 base)
    (x x-0 x-1 x-2 z x-3 x-4 x-5 x-6 x-7 x-8 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-5))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp h2-0 x-0))
    (h3 (exp h2 (mul x (rec x-4) x-5))) (x x-4))
  (defstrand resp 2 (b b-1) (a a-1) (h1 (exp h2-0 (mul x-1 x-4)))
    (y x-6) (z x-3))
  (defstrand init 3 (a a-2) (b b-2) (h2 h2-1)
    (h3 (exp h2-0 (mul x-1 x-2 (rec z) x-4))) (x z))
  (defstrand init 3 (a a-3) (b b-3) (h2 h2-2)
    (h3 (exp h2-1 (mul z x-7 (rec x-8)))) (x x-8))
  (precedes ((0 0) (1 1)) ((1 0) (3 1)) ((1 2) (0 1)) ((2 1) (1 1))
    ((3 0) (4 1)) ((3 2) (2 0)) ((4 2) (3 1)))
  (ind-zero-in (x-6 (exp h2-0 (mul x-1 x-4)))
    (x-3 (exp h2-0 (mul x-1 x-4))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-3 x-4 x-5 x-6 x-8)
  (operation nonce-test (added-strand init 3) (exp h2-1 z) (3 1)
    (exp (gen) z))
  (traces
    ((send (enc (exp (gen) x-5) a (pubk b)))
      (recv (enc h2 (exp h2 x-5) h3 b (pubk a)))
      (send (enc (exp h3 x-5) (pubk b))))
    ((send (enc (exp (gen) x-4) a-0 (pubk b-0)))
      (recv
        (enc (exp h2-0 x-0) (exp h2-0 (mul x-0 x-4))
          (exp h2 (mul x (rec x-4) x-5)) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x x-5)) (pubk b-0))))
    ((recv (enc (exp h2-0 (mul x-1 x-4)) a-1 (pubk b-1)))
      (send
        (enc (exp (gen) x-6) (exp h2-0 (mul x-1 x-4 x-6))
          (exp (gen) x-3) b-1 (pubk a-1))))
    ((send (enc (exp (gen) z) a-2 (pubk b-2)))
      (recv
        (enc h2-1 (exp h2-1 z) (exp h2-0 (mul x-1 x-2 (rec z) x-4)) b-2
          (pubk a-2)))
      (send (enc (exp h2-0 (mul x-1 x-2 x-4)) (pubk b-2))))
    ((send (enc (exp (gen) x-8) a-3 (pubk b-3)))
      (recv
        (enc h2-2 (exp h2-2 x-8) (exp h2-1 (mul z x-7 (rec x-8))) b-3
          (pubk a-3))) (send (enc (exp h2-1 (mul z x-7)) (pubk b-3)))))
  (label 24)
  (parent 9)
  (unrealized (1 1) (3 1) (4 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 b-1 a-1 a-2 b-2 b-3 a-3 name)
    (h2 h3 h2-0 h2-1 base)
    (x x-0 x-1 x-2 z z-0 x-3 x-4 x-5 x-6 x-7 x-8 x-9 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-5))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp h2-0 x-0))
    (h3 (exp h2 (mul x (rec x-4) x-5))) (x x-4))
  (defstrand resp 2 (b b-1) (a a-1) (h1 (exp h2-0 (mul x-1 x-4)))
    (y x-6) (z x-3))
  (defstrand init 3 (a a-2) (b b-2) (h2 (exp h2-1 x-9))
    (h3 (exp h2-0 (mul x-1 x-2 (rec z-0) x-4))) (x z-0))
  (defstrand resp 2 (b b-3) (a a-3) (h1 (exp h2-1 (mul z-0 x-7)))
    (y x-8) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (3 1)) ((1 2) (0 1)) ((2 1) (1 1))
    ((3 0) (4 0)) ((3 2) (2 0)) ((4 1) (3 1)))
  (ind-zero-in (x-8 (exp h2-1 (mul z-0 x-7)))
    (z (exp h2-1 (mul z-0 x-7))) (x-6 (exp h2-0 (mul x-1 x-4)))
    (x-3 (exp h2-0 (mul x-1 x-4))))
  (non-orig (privk a) (privk b))
  (uniq-gen z z-0 x-3 x-4 x-5 x-6 x-8)
  (operation nonce-test (added-strand resp 2) (exp h2-1 (mul z-0 x-9))
    (3 1) (exp (gen) z-0))
  (traces
    ((send (enc (exp (gen) x-5) a (pubk b)))
      (recv (enc h2 (exp h2 x-5) h3 b (pubk a)))
      (send (enc (exp h3 x-5) (pubk b))))
    ((send (enc (exp (gen) x-4) a-0 (pubk b-0)))
      (recv
        (enc (exp h2-0 x-0) (exp h2-0 (mul x-0 x-4))
          (exp h2 (mul x (rec x-4) x-5)) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x x-5)) (pubk b-0))))
    ((recv (enc (exp h2-0 (mul x-1 x-4)) a-1 (pubk b-1)))
      (send
        (enc (exp (gen) x-6) (exp h2-0 (mul x-1 x-4 x-6))
          (exp (gen) x-3) b-1 (pubk a-1))))
    ((send (enc (exp (gen) z-0) a-2 (pubk b-2)))
      (recv
        (enc (exp h2-1 x-9) (exp h2-1 (mul z-0 x-9))
          (exp h2-0 (mul x-1 x-2 (rec z-0) x-4)) b-2 (pubk a-2)))
      (send (enc (exp h2-0 (mul x-1 x-2 x-4)) (pubk b-2))))
    ((recv (enc (exp h2-1 (mul z-0 x-7)) a-3 (pubk b-3)))
      (send
        (enc (exp (gen) x-8) (exp h2-1 (mul z-0 x-7 x-8)) (exp (gen) z)
          b-3 (pubk a-3)))))
  (label 25)
  (parent 9)
  (unrealized (1 1) (3 1) (4 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 b-1 a-1 a-2 b-2 name) (h2 h3 h2-0 base)
    (x x-0 z x-1 x-2 x-3 x-4 x-5 x-6 x-7 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-1))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp h2-0 x-0))
    (h3 (exp h2 (mul x x-1 (rec x-2)))) (x x-2))
  (defstrand resp 2 (b b-1) (a a-1) (h1 (exp h2-0 (mul x-2 x-4))) (y z)
    (z x-3))
  (defstrand init 3 (a a-2) (b b-2) (h2 (exp (gen) x-7))
    (h3 (exp h2-0 (mul x-2 x-4 x-5 (rec x-6)))) (x x-6))
  (precedes ((0 0) (1 1)) ((1 0) (3 1)) ((1 2) (0 1)) ((2 1) (1 1))
    ((3 2) (2 0)))
  (ind-zero-in (z (exp h2-0 (mul x-2 x-4)))
    (x-3 (exp h2-0 (mul x-2 x-4))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-1 x-2 x-3 x-6)
  (operation nonce-test (algebra-contracted (h2-1 (exp (gen) x-7)))
    (exp (gen) (mul x-6 x-7)) (3 1) (exp (gen) x-6))
  (traces
    ((send (enc (exp (gen) x-1) a (pubk b)))
      (recv (enc h2 (exp h2 x-1) h3 b (pubk a)))
      (send (enc (exp h3 x-1) (pubk b))))
    ((send (enc (exp (gen) x-2) a-0 (pubk b-0)))
      (recv
        (enc (exp h2-0 x-0) (exp h2-0 (mul x-0 x-2))
          (exp h2 (mul x x-1 (rec x-2))) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x x-1)) (pubk b-0))))
    ((recv (enc (exp h2-0 (mul x-2 x-4)) a-1 (pubk b-1)))
      (send
        (enc (exp (gen) z) (exp h2-0 (mul z x-2 x-4)) (exp (gen) x-3)
          b-1 (pubk a-1))))
    ((send (enc (exp (gen) x-6) a-2 (pubk b-2)))
      (recv
        (enc (exp (gen) x-7) (exp (gen) (mul x-6 x-7))
          (exp h2-0 (mul x-2 x-4 x-5 (rec x-6))) b-2 (pubk a-2)))
      (send (enc (exp h2-0 (mul x-2 x-4 x-5)) (pubk b-2)))))
  (label 26)
  (parent 9)
  (unrealized (1 1) (3 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 b-1 a-1 b-2 a-2 a-3 b-3 name)
    (h2 h3 h2-0 h2-1 base)
    (x x-0 x-1 x-2 z z-0 x-3 x-4 x-5 x-6 x-7 x-8 x-9 x-10 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-4))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp h2-0 (mul x-0 x-1)))
    (h3 (exp h2 (mul x (rec x-3) x-4))) (x x-3))
  (defstrand resp 2 (b b-1) (a a-1)
    (h1 (exp h2-0 (mul (rec x-2) z x-3 x-7 x-8))) (y x-5) (z z-0))
  (defstrand resp 2 (b b-2) (a a-2) (h1 (exp h2-0 (mul x-3 x-7 x-8)))
    (y z) (z x-6))
  (defstrand init 3 (a a-3) (b b-3) (h2 h2-1)
    (h3 (exp h2-0 (mul x-3 x-7 x-8 x-9 (rec x-10)))) (x x-10))
  (precedes ((0 0) (1 1)) ((1 0) (4 1)) ((1 2) (0 1)) ((2 1) (1 1))
    ((3 1) (2 0)) ((4 2) (3 0)))
  (ind-zero-in (z (exp h2-0 (mul x-3 x-7 x-8)))
    (x-6 (exp h2-0 (mul x-3 x-7 x-8)))
    (x-5 (exp h2-0 (mul (rec x-2) z x-3 x-7 x-8)))
    (z-0 (exp h2-0 (mul (rec x-2) z x-3 x-7 x-8))))
  (non-orig (privk a) (privk b))
  (uniq-gen z z-0 x-3 x-4 x-5 x-6 x-10)
  (operation nonce-test (added-strand init 3)
    (exp h2-0 (mul x-3 x-7 x-8)) (3 0) (exp (gen) x-3))
  (traces
    ((send (enc (exp (gen) x-4) a (pubk b)))
      (recv (enc h2 (exp h2 x-4) h3 b (pubk a)))
      (send (enc (exp h3 x-4) (pubk b))))
    ((send (enc (exp (gen) x-3) a-0 (pubk b-0)))
      (recv
        (enc (exp h2-0 (mul x-0 x-1)) (exp h2-0 (mul x-0 x-1 x-3))
          (exp h2 (mul x (rec x-3) x-4)) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x x-4)) (pubk b-0))))
    ((recv
       (enc (exp h2-0 (mul (rec x-2) z x-3 x-7 x-8)) a-1 (pubk b-1)))
      (send
        (enc (exp (gen) x-5)
          (exp h2-0 (mul (rec x-2) z x-3 x-5 x-7 x-8)) (exp (gen) z-0)
          b-1 (pubk a-1))))
    ((recv (enc (exp h2-0 (mul x-3 x-7 x-8)) a-2 (pubk b-2)))
      (send
        (enc (exp (gen) z) (exp h2-0 (mul z x-3 x-7 x-8))
          (exp (gen) x-6) b-2 (pubk a-2))))
    ((send (enc (exp (gen) x-10) a-3 (pubk b-3)))
      (recv
        (enc h2-1 (exp h2-1 x-10)
          (exp h2-0 (mul x-3 x-7 x-8 x-9 (rec x-10))) b-3 (pubk a-3)))
      (send (enc (exp h2-0 (mul x-3 x-7 x-8 x-9)) (pubk b-3)))))
  (label 27)
  (parent 10)
  (unrealized (1 1) (4 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 b-1 a-1 b-2 a-2 b-3 a-3 name) (h2 h3 h2-0 base)
    (x x-0 x-1 x-2 z z-0 z-1 x-3 x-4 x-5 x-6 x-7 x-8 x-9 x-10 x-11 x-12
      expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-4))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp h2-0 (mul x-0 x-1 x-11)))
    (h3 (exp h2 (mul x (rec x-3) x-4))) (x x-3))
  (defstrand resp 2 (b b-1) (a a-1)
    (h1 (exp h2-0 (mul (rec x-2) z-0 x-3 x-7 x-8 x-9 x-10 (rec x-12))))
    (y x-5) (z z-1))
  (defstrand resp 2 (b b-2) (a a-2)
    (h1 (exp h2-0 (mul x-3 x-7 x-8 x-9 x-10 (rec x-12)))) (y z-0)
    (z x-6))
  (defstrand resp 2 (b b-3) (a a-3)
    (h1 (exp h2-0 (mul x-3 x-7 x-8 x-9))) (y x-10) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (4 0)) ((1 2) (0 1)) ((2 1) (1 1))
    ((3 1) (2 0)) ((4 1) (3 0)))
  (ind-zero-in (x-10 (exp h2-0 (mul x-3 x-7 x-8 x-9)))
    (z (exp h2-0 (mul x-3 x-7 x-8 x-9)))
    (z-0 (exp h2-0 (mul x-3 x-7 x-8 x-9 x-10 (rec x-12))))
    (x-6 (exp h2-0 (mul x-3 x-7 x-8 x-9 x-10 (rec x-12))))
    (x-5 (exp h2-0 (mul (rec x-2) z-0 x-3 x-7 x-8 x-9 x-10 (rec x-12))))
    (z-1
      (exp h2-0 (mul (rec x-2) z-0 x-3 x-7 x-8 x-9 x-10 (rec x-12)))))
  (non-orig (privk a) (privk b))
  (uniq-gen z z-0 z-1 x-3 x-4 x-5 x-6 x-10)
  (operation nonce-test (added-strand resp 2)
    (exp h2-0 (mul x-3 x-7 x-8 x-9 x-10 (rec x-12))) (3 0)
    (exp (gen) x-3))
  (traces
    ((send (enc (exp (gen) x-4) a (pubk b)))
      (recv (enc h2 (exp h2 x-4) h3 b (pubk a)))
      (send (enc (exp h3 x-4) (pubk b))))
    ((send (enc (exp (gen) x-3) a-0 (pubk b-0)))
      (recv
        (enc (exp h2-0 (mul x-0 x-1 x-11))
          (exp h2-0 (mul x-0 x-1 x-3 x-11))
          (exp h2 (mul x (rec x-3) x-4)) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x x-4)) (pubk b-0))))
    ((recv
       (enc
         (exp h2-0 (mul (rec x-2) z-0 x-3 x-7 x-8 x-9 x-10 (rec x-12)))
         a-1 (pubk b-1)))
      (send
        (enc (exp (gen) x-5)
          (exp h2-0
            (mul (rec x-2) z-0 x-3 x-5 x-7 x-8 x-9 x-10 (rec x-12)))
          (exp (gen) z-1) b-1 (pubk a-1))))
    ((recv
       (enc (exp h2-0 (mul x-3 x-7 x-8 x-9 x-10 (rec x-12))) a-2
         (pubk b-2)))
      (send
        (enc (exp (gen) z-0)
          (exp h2-0 (mul z-0 x-3 x-7 x-8 x-9 x-10 (rec x-12)))
          (exp (gen) x-6) b-2 (pubk a-2))))
    ((recv (enc (exp h2-0 (mul x-3 x-7 x-8 x-9)) a-3 (pubk b-3)))
      (send
        (enc (exp (gen) x-10) (exp h2-0 (mul x-3 x-7 x-8 x-9 x-10))
          (exp (gen) z) b-3 (pubk a-3)))))
  (label 28)
  (parent 10)
  (unrealized (1 1) (4 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 b-1 a-1 b-2 a-2 name) (h2 h3 base)
    (x x-0 z z-0 x-1 x-2 x-3 x-4 x-5 x-6 x-7 x-8 x-9 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-1))
  (defstrand init 3 (a a-0) (b b-0)
    (h2 (exp (gen) (mul x-0 (rec x-4) (rec x-5) x-7 x-9)))
    (h3 (exp h2 (mul x x-1 (rec x-2)))) (x x-2))
  (defstrand resp 2 (b b-1) (a a-1)
    (h1 (exp (gen) (mul x-2 x-6 (rec x-8) x-9))) (y z-0) (z x-3))
  (defstrand resp 2 (b b-2) (a a-2) (h1 (exp (gen) (mul x-2 x-9)))
    (y x-6) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (3 0)) ((1 2) (0 1)) ((2 1) (1 1))
    ((3 1) (2 0)))
  (ind-zero-in (x-6 (exp (gen) (mul x-2 x-9)))
    (z (exp (gen) (mul x-2 x-9)))
    (z-0 (exp (gen) (mul x-2 x-6 (rec x-8) x-9)))
    (x-3 (exp (gen) (mul x-2 x-6 (rec x-8) x-9))))
  (non-orig (privk a) (privk b))
  (uniq-gen z z-0 x-1 x-2 x-3 x-6)
  (operation nonce-test
    (algebra-contracted
      (h2-0 (exp (gen) (mul (rec x-4) (rec x-5) x-9))))
    (exp (gen) (mul x-2 x-9)) (3 0) (exp (gen) x-2))
  (traces
    ((send (enc (exp (gen) x-1) a (pubk b)))
      (recv (enc h2 (exp h2 x-1) h3 b (pubk a)))
      (send (enc (exp h3 x-1) (pubk b))))
    ((send (enc (exp (gen) x-2) a-0 (pubk b-0)))
      (recv
        (enc (exp (gen) (mul x-0 (rec x-4) (rec x-5) x-7 x-9))
          (exp (gen) (mul x-0 x-2 (rec x-4) (rec x-5) x-7 x-9))
          (exp h2 (mul x x-1 (rec x-2))) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x x-1)) (pubk b-0))))
    ((recv (enc (exp (gen) (mul x-2 x-6 (rec x-8) x-9)) a-1 (pubk b-1)))
      (send
        (enc (exp (gen) z-0) (exp (gen) (mul z-0 x-2 x-6 (rec x-8) x-9))
          (exp (gen) x-3) b-1 (pubk a-1))))
    ((recv (enc (exp (gen) (mul x-2 x-9)) a-2 (pubk b-2)))
      (send
        (enc (exp (gen) x-6) (exp (gen) (mul x-2 x-6 x-9)) (exp (gen) z)
          b-2 (pubk a-2)))))
  (label 29)
  (parent 10)
  (unrealized (1 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 b-1 a-1 name) (h3 base)
    (x z x-0 x-1 x-2 x-3 x-4 x-5 x-6 expn))
  (defstrand init 3 (a a) (b b)
    (h2 (exp (gen) (mul (rec x) x-0 (rec x-1) x-3 x-6))) (h3 h3)
    (x x-1))
  (defstrand init 3 (a a-0) (b b-0)
    (h2 (exp (gen) (mul (rec x-2) x-4 x-5)))
    (h3 (exp (gen) (mul x-3 x-6))) (x x-0))
  (defstrand resp 2 (b b-1) (a a-1) (h1 (exp (gen) (mul x-0 x-5)))
    (y x-3) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (2 0)) ((1 2) (0 1)) ((2 1) (1 1)))
  (ind-zero-in (x-3 (exp (gen) (mul x-0 x-5)))
    (z (exp (gen) (mul x-0 x-5))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-0 x-1 x-3)
  (operation nonce-test
    (algebra-contracted
      (h2 (exp (gen) (mul (rec x) x-0 (rec x-1) x-3 x-6))))
    (exp (gen) (mul x-3 x-6)) (1 1))
  (traces
    ((send (enc (exp (gen) x-1) a (pubk b)))
      (recv
        (enc (exp (gen) (mul (rec x) x-0 (rec x-1) x-3 x-6))
          (exp (gen) (mul (rec x) x-0 x-3 x-6)) h3 b (pubk a)))
      (send (enc (exp h3 x-1) (pubk b))))
    ((send (enc (exp (gen) x-0) a-0 (pubk b-0)))
      (recv
        (enc (exp (gen) (mul (rec x-2) x-4 x-5))
          (exp (gen) (mul x-0 (rec x-2) x-4 x-5))
          (exp (gen) (mul x-3 x-6)) b-0 (pubk a-0)))
      (send (enc (exp (gen) (mul x-0 x-3 x-6)) (pubk b-0))))
    ((recv (enc (exp (gen) (mul x-0 x-5)) a-1 (pubk b-1)))
      (send
        (enc (exp (gen) x-3) (exp (gen) (mul x-0 x-3 x-5)) (exp (gen) z)
          b-1 (pubk a-1)))))
  (label 30)
  (parent 11)
  (seen 30)
  (unrealized (0 1))
  (comment "1 in cohort - 0 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 name) (h2 h3 h2-0 h2-1 base)
    (x x-0 x-1 x-2 x-3 x-4 x-5 x-6 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-3))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp (gen) x))
    (h3 (exp h2 (mul x-0 x-3 (rec x-4)))) (x x-4))
  (defstrand init 3 (a a-1) (b b-1) (h2 h2-0)
    (h3 (exp h2 (mul x-0 x-1 (rec x-2) x-3 (rec x-4)))) (x x-2))
  (defstrand init 3 (a a-2) (b b-2) (h2 h2-1)
    (h3 (exp h2-0 (mul x-2 x-5 (rec x-6)))) (x x-6))
  (precedes ((0 0) (2 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (3 1))
    ((2 2) (1 1)) ((3 2) (2 1)))
  (non-orig (privk a) (privk b))
  (uniq-gen x-2 x-3 x-4 x-6)
  (operation nonce-test (added-strand init 3) (exp h2-0 x-2) (2 1)
    (exp (gen) x-2))
  (traces
    ((send (enc (exp (gen) x-3) a (pubk b)))
      (recv (enc h2 (exp h2 x-3) h3 b (pubk a)))
      (send (enc (exp h3 x-3) (pubk b))))
    ((send (enc (exp (gen) x-4) a-0 (pubk b-0)))
      (recv
        (enc (exp (gen) x) (exp (gen) (mul x x-4))
          (exp h2 (mul x-0 x-3 (rec x-4))) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x-0 x-3)) (pubk b-0))))
    ((send (enc (exp (gen) x-2) a-1 (pubk b-1)))
      (recv
        (enc h2-0 (exp h2-0 x-2)
          (exp h2 (mul x-0 x-1 (rec x-2) x-3 (rec x-4))) b-1
          (pubk a-1)))
      (send (enc (exp h2 (mul x-0 x-1 x-3 (rec x-4))) (pubk b-1))))
    ((send (enc (exp (gen) x-6) a-2 (pubk b-2)))
      (recv
        (enc h2-1 (exp h2-1 x-6) (exp h2-0 (mul x-2 x-5 (rec x-6))) b-2
          (pubk a-2)))
      (send (enc (exp h2-0 (mul x-2 x-5)) (pubk b-2)))))
  (label 31)
  (parent 12)
  (unrealized (2 1) (3 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 b-2 a-2 name) (h2 h3 h2-0 base)
    (x x-0 x-1 z x-2 x-3 x-4 x-5 x-6 x-7 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-3))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp (gen) x))
    (h3 (exp h2 (mul x-0 x-3 (rec x-4)))) (x x-4))
  (defstrand init 3 (a a-1) (b b-1) (h2 (exp h2-0 x-7))
    (h3 (exp h2 (mul x-0 x-1 (rec x-2) x-3 (rec x-4)))) (x x-2))
  (defstrand resp 2 (b b-2) (a a-2) (h1 (exp h2-0 (mul x-2 x-5)))
    (y x-6) (z z))
  (precedes ((0 0) (2 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (3 0))
    ((2 2) (1 1)) ((3 1) (2 1)))
  (ind-zero-in (x-6 (exp h2-0 (mul x-2 x-5)))
    (z (exp h2-0 (mul x-2 x-5))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-2 x-3 x-4 x-6)
  (operation nonce-test (added-strand resp 2) (exp h2-0 (mul x-2 x-7))
    (2 1) (exp (gen) x-2))
  (traces
    ((send (enc (exp (gen) x-3) a (pubk b)))
      (recv (enc h2 (exp h2 x-3) h3 b (pubk a)))
      (send (enc (exp h3 x-3) (pubk b))))
    ((send (enc (exp (gen) x-4) a-0 (pubk b-0)))
      (recv
        (enc (exp (gen) x) (exp (gen) (mul x x-4))
          (exp h2 (mul x-0 x-3 (rec x-4))) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x-0 x-3)) (pubk b-0))))
    ((send (enc (exp (gen) x-2) a-1 (pubk b-1)))
      (recv
        (enc (exp h2-0 x-7) (exp h2-0 (mul x-2 x-7))
          (exp h2 (mul x-0 x-1 (rec x-2) x-3 (rec x-4))) b-1
          (pubk a-1)))
      (send (enc (exp h2 (mul x-0 x-1 x-3 (rec x-4))) (pubk b-1))))
    ((recv (enc (exp h2-0 (mul x-2 x-5)) a-2 (pubk b-2)))
      (send
        (enc (exp (gen) x-6) (exp h2-0 (mul x-2 x-5 x-6)) (exp (gen) z)
          b-2 (pubk a-2)))))
  (label 32)
  (parent 12)
  (unrealized (2 1) (3 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 name) (h2 h3 base)
    (x x-0 x-1 x-2 x-3 x-4 x-5 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-1))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp (gen) x))
    (h3 (exp h2 (mul (rec x-0) x-1 x-2))) (x x-0))
  (defstrand init 3 (a a-1) (b b-1) (h2 (exp (gen) x-5))
    (h3 (exp h2 (mul (rec x-0) x-1 x-2 x-3 (rec x-4)))) (x x-4))
  (precedes ((0 0) (2 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 2) (1 1)))
  (non-orig (privk a) (privk b))
  (uniq-gen x-0 x-1 x-4)
  (operation nonce-test (algebra-contracted (h2-0 (exp (gen) x-5)))
    (exp (gen) (mul x-4 x-5)) (2 1) (exp (gen) x-4))
  (traces
    ((send (enc (exp (gen) x-1) a (pubk b)))
      (recv (enc h2 (exp h2 x-1) h3 b (pubk a)))
      (send (enc (exp h3 x-1) (pubk b))))
    ((send (enc (exp (gen) x-0) a-0 (pubk b-0)))
      (recv
        (enc (exp (gen) x) (exp (gen) (mul x x-0))
          (exp h2 (mul (rec x-0) x-1 x-2)) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x-1 x-2)) (pubk b-0))))
    ((send (enc (exp (gen) x-4) a-1 (pubk b-1)))
      (recv
        (enc (exp (gen) x-5) (exp (gen) (mul x-4 x-5))
          (exp h2 (mul (rec x-0) x-1 x-2 x-3 (rec x-4))) b-1
          (pubk a-1)))
      (send (enc (exp h2 (mul (rec x-0) x-1 x-2 x-3)) (pubk b-1)))))
  (label 33)
  (parent 12)
  (unrealized (2 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 b-1 a-1 a-2 b-2 name) (h2 h3 h2-0 base)
    (x x-0 x-1 z x-2 x-3 x-4 x-5 x-6 x-7 x-8 expn))
  (defstrand init 3 (a a) (b b) (h2 (exp h2 x-0)) (h3 h3) (x x-2))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp (gen) x))
    (h3 (exp h2 (mul (rec x-1) z x-2 (rec x-3) x-5 x-6))) (x x-3))
  (defstrand resp 2 (b b-1) (a a-1)
    (h1 (exp h2 (mul x-2 (rec x-3) x-5 x-6))) (y z) (z x-4))
  (defstrand init 3 (a a-2) (b b-2) (h2 h2-0)
    (h3 (exp h2 (mul x-2 (rec x-3) x-5 x-6 (rec x-7) (rec x-8))))
    (x x-8))
  (precedes ((0 0) (3 1)) ((1 0) (3 1)) ((1 2) (0 1)) ((2 1) (1 1))
    ((3 2) (2 0)))
  (ind-zero-in (z (exp h2 (mul x-2 (rec x-3) x-5 x-6)))
    (x-4 (exp h2 (mul x-2 (rec x-3) x-5 x-6))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-2 x-3 x-4 x-8)
  (operation nonce-test (added-strand init 3)
    (exp h2 (mul x-2 (rec x-3) x-5 x-6)) (2 0))
  (traces
    ((send (enc (exp (gen) x-2) a (pubk b)))
      (recv (enc (exp h2 x-0) (exp h2 (mul x-0 x-2)) h3 b (pubk a)))
      (send (enc (exp h3 x-2) (pubk b))))
    ((send (enc (exp (gen) x-3) a-0 (pubk b-0)))
      (recv
        (enc (exp (gen) x) (exp (gen) (mul x x-3))
          (exp h2 (mul (rec x-1) z x-2 (rec x-3) x-5 x-6)) b-0
          (pubk a-0)))
      (send (enc (exp h2 (mul (rec x-1) z x-2 x-5 x-6)) (pubk b-0))))
    ((recv (enc (exp h2 (mul x-2 (rec x-3) x-5 x-6)) a-1 (pubk b-1)))
      (send
        (enc (exp (gen) z) (exp h2 (mul z x-2 (rec x-3) x-5 x-6))
          (exp (gen) x-4) b-1 (pubk a-1))))
    ((send (enc (exp (gen) x-8) a-2 (pubk b-2)))
      (recv
        (enc h2-0 (exp h2-0 x-8)
          (exp h2 (mul x-2 (rec x-3) x-5 x-6 (rec x-7) (rec x-8))) b-2
          (pubk a-2)))
      (send
        (enc (exp h2 (mul x-2 (rec x-3) x-5 x-6 (rec x-7)))
          (pubk b-2)))))
  (label 34)
  (parent 13)
  (unrealized (0 1) (3 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 b-1 a-1 b-2 a-2 name) (h2 h3 base)
    (x x-0 x-1 z z-0 x-2 y x-3 x-4 x-5 x-6 x-7 x-8 x-9 expn))
  (defstrand init 3 (a a) (b b) (h2 (exp h2 (mul x-0 x-8))) (h3 h3)
    (x x-2))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp (gen) x))
    (h3
      (exp h2
        (mul (rec x-1) z-0 x-2 (rec y) x-4 x-5 (rec x-6) x-7 x-9)))
    (x y))
  (defstrand resp 2 (b b-1) (a a-1)
    (h1 (exp h2 (mul x-2 (rec y) x-4 x-5 (rec x-6) x-7 x-9))) (y z-0)
    (z x-3))
  (defstrand resp 2 (b b-2) (a a-2)
    (h1 (exp h2 (mul x-2 (rec y) x-4 x-5 (rec x-6)))) (y x-7) (z z))
  (precedes ((0 0) (3 0)) ((1 0) (3 0)) ((1 2) (0 1)) ((2 1) (1 1))
    ((3 1) (2 0)))
  (ind-zero-in (x-7 (exp h2 (mul x-2 (rec y) x-4 x-5 (rec x-6))))
    (z (exp h2 (mul x-2 (rec y) x-4 x-5 (rec x-6))))
    (z-0 (exp h2 (mul x-2 (rec y) x-4 x-5 (rec x-6) x-7 x-9)))
    (x-3 (exp h2 (mul x-2 (rec y) x-4 x-5 (rec x-6) x-7 x-9))))
  (non-orig (privk a) (privk b))
  (uniq-gen z z-0 x-2 y x-3 x-7)
  (operation nonce-test (added-strand resp 2)
    (exp h2 (mul x-2 (rec y) x-4 x-5 (rec x-6) x-7 x-9)) (2 0))
  (traces
    ((send (enc (exp (gen) x-2) a (pubk b)))
      (recv
        (enc (exp h2 (mul x-0 x-8)) (exp h2 (mul x-0 x-2 x-8)) h3 b
          (pubk a))) (send (enc (exp h3 x-2) (pubk b))))
    ((send (enc (exp (gen) y) a-0 (pubk b-0)))
      (recv
        (enc (exp (gen) x) (exp (gen) (mul x y))
          (exp h2
            (mul (rec x-1) z-0 x-2 (rec y) x-4 x-5 (rec x-6) x-7 x-9))
          b-0 (pubk a-0)))
      (send
        (enc (exp h2 (mul (rec x-1) z-0 x-2 x-4 x-5 (rec x-6) x-7 x-9))
          (pubk b-0))))
    ((recv
       (enc (exp h2 (mul x-2 (rec y) x-4 x-5 (rec x-6) x-7 x-9)) a-1
         (pubk b-1)))
      (send
        (enc (exp (gen) z-0)
          (exp h2 (mul z-0 x-2 (rec y) x-4 x-5 (rec x-6) x-7 x-9))
          (exp (gen) x-3) b-1 (pubk a-1))))
    ((recv
       (enc (exp h2 (mul x-2 (rec y) x-4 x-5 (rec x-6))) a-2
         (pubk b-2)))
      (send
        (enc (exp (gen) x-7)
          (exp h2 (mul x-2 (rec y) x-4 x-5 (rec x-6) x-7)) (exp (gen) z)
          b-2 (pubk a-2)))))
  (label 35)
  (parent 13)
  (unrealized (0 1) (3 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 b-1 a-1 name) (h3 base)
    (x z x-0 x-1 x-2 x-3 x-4 x-5 x-6 x-7 expn))
  (defstrand init 3 (a a) (b b)
    (h2 (exp (gen) (mul x-0 x-0 (rec x-1) (rec x-2) (rec x-3) x-5 x-7)))
    (h3 h3) (x x-1))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp (gen) x))
    (h3 (exp (gen) (mul x-0 x-4 (rec x-6) x-7))) (x x-0))
  (defstrand resp 2 (b b-1) (a a-1) (h1 (exp (gen) (mul x-0 x-7)))
    (y x-4) (z z))
  (precedes ((0 0) (2 0)) ((1 0) (2 0)) ((1 2) (0 1)) ((2 1) (1 1)))
  (ind-zero-in (x-4 (exp (gen) (mul x-0 x-7)))
    (z (exp (gen) (mul x-0 x-7))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-0 x-1 x-4)
  (operation nonce-test
    (algebra-contracted
      (h2 (exp (gen) (mul x-0 x-0 (rec x-1) (rec x-2) (rec x-3) x-7))))
    (exp (gen) (mul x-0 x-7)) (2 0))
  (traces
    ((send (enc (exp (gen) x-1) a (pubk b)))
      (recv
        (enc
          (exp (gen)
            (mul x-0 x-0 (rec x-1) (rec x-2) (rec x-3) x-5 x-7))
          (exp (gen) (mul x-0 x-0 (rec x-2) (rec x-3) x-5 x-7)) h3 b
          (pubk a))) (send (enc (exp h3 x-1) (pubk b))))
    ((send (enc (exp (gen) x-0) a-0 (pubk b-0)))
      (recv
        (enc (exp (gen) x) (exp (gen) (mul x x-0))
          (exp (gen) (mul x-0 x-4 (rec x-6) x-7)) b-0 (pubk a-0)))
      (send
        (enc (exp (gen) (mul x-0 x-0 x-4 (rec x-6) x-7)) (pubk b-0))))
    ((recv (enc (exp (gen) (mul x-0 x-7)) a-1 (pubk b-1)))
      (send
        (enc (exp (gen) x-4) (exp (gen) (mul x-0 x-4 x-7)) (exp (gen) z)
          b-1 (pubk a-1)))))
  (label 36)
  (parent 13)
  (seen 36)
  (unrealized (0 1))
  (comment "1 in cohort - 0 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 a-3 b-3 a-4 b-4 name)
    (h2 h3 h2-0 h2-1 h2-2 h2-3 h2-4 base)
    (x x-0 x-1 x-2 x-3 x-4 x-5 x-6 x-7 x-8 x-9 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-5))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x x-5 (rec x-6)))) (x x-6))
  (defstrand init 3 (a a-1) (b b-1) (h2 h2-1)
    (h3 (exp h2-0 (mul x-0 (rec x-4) x-6))) (x x-4))
  (defstrand init 3 (a a-2) (b b-2) (h2 h2-2)
    (h3 (exp h2-1 (mul x-1 x-4 (rec x-7)))) (x x-7))
  (defstrand init 3 (a a-3) (b b-3) (h2 h2-3)
    (h3 (exp h2-2 (mul x-2 (rec x-3) x-7))) (x x-3))
  (defstrand init 3 (a a-4) (b b-4) (h2 h2-4)
    (h3 (exp h2-3 (mul x-3 x-8 (rec x-9)))) (x x-9))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (3 1))
    ((2 2) (1 1)) ((3 0) (4 1)) ((3 2) (2 1)) ((4 0) (5 1))
    ((4 2) (3 1)) ((5 2) (4 1)))
  (non-orig (privk a) (privk b))
  (uniq-gen x-3 x-4 x-5 x-6 x-7 x-9)
  (operation nonce-test (added-strand init 3) (exp h2-3 x-3) (4 1)
    (exp (gen) x-3))
  (traces
    ((send (enc (exp (gen) x-5) a (pubk b)))
      (recv (enc h2 (exp h2 x-5) h3 b (pubk a)))
      (send (enc (exp h3 x-5) (pubk b))))
    ((send (enc (exp (gen) x-6) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-6) (exp h2 (mul x x-5 (rec x-6))) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-5)) (pubk b-0))))
    ((send (enc (exp (gen) x-4) a-1 (pubk b-1)))
      (recv
        (enc h2-1 (exp h2-1 x-4) (exp h2-0 (mul x-0 (rec x-4) x-6)) b-1
          (pubk a-1))) (send (enc (exp h2-0 (mul x-0 x-6)) (pubk b-1))))
    ((send (enc (exp (gen) x-7) a-2 (pubk b-2)))
      (recv
        (enc h2-2 (exp h2-2 x-7) (exp h2-1 (mul x-1 x-4 (rec x-7))) b-2
          (pubk a-2))) (send (enc (exp h2-1 (mul x-1 x-4)) (pubk b-2))))
    ((send (enc (exp (gen) x-3) a-3 (pubk b-3)))
      (recv
        (enc h2-3 (exp h2-3 x-3) (exp h2-2 (mul x-2 (rec x-3) x-7)) b-3
          (pubk a-3))) (send (enc (exp h2-2 (mul x-2 x-7)) (pubk b-3))))
    ((send (enc (exp (gen) x-9) a-4 (pubk b-4)))
      (recv
        (enc h2-4 (exp h2-4 x-9) (exp h2-3 (mul x-3 x-8 (rec x-9))) b-4
          (pubk a-4)))
      (send (enc (exp h2-3 (mul x-3 x-8)) (pubk b-4)))))
  (label 37)
  (parent 15)
  (unrealized (1 1) (2 1) (3 1) (4 1) (5 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 a-3 b-3 b-4 a-4 name)
    (h2 h3 h2-0 h2-1 h2-2 h2-3 base)
    (x x-0 x-1 x-2 z x-3 x-4 x-5 x-6 x-7 x-8 x-9 x-10 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-5))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x x-5 (rec x-6)))) (x x-6))
  (defstrand init 3 (a a-1) (b b-1) (h2 h2-1)
    (h3 (exp h2-0 (mul x-0 (rec x-4) x-6))) (x x-4))
  (defstrand init 3 (a a-2) (b b-2) (h2 h2-2)
    (h3 (exp h2-1 (mul x-1 x-4 (rec x-7)))) (x x-7))
  (defstrand init 3 (a a-3) (b b-3) (h2 (exp h2-3 x-10))
    (h3 (exp h2-2 (mul x-2 (rec x-3) x-7))) (x x-3))
  (defstrand resp 2 (b b-4) (a a-4) (h1 (exp h2-3 (mul x-3 x-8)))
    (y x-9) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (3 1))
    ((2 2) (1 1)) ((3 0) (4 1)) ((3 2) (2 1)) ((4 0) (5 0))
    ((4 2) (3 1)) ((5 1) (4 1)))
  (ind-zero-in (x-9 (exp h2-3 (mul x-3 x-8)))
    (z (exp h2-3 (mul x-3 x-8))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-3 x-4 x-5 x-6 x-7 x-9)
  (operation nonce-test (added-strand resp 2) (exp h2-3 (mul x-3 x-10))
    (4 1) (exp (gen) x-3))
  (traces
    ((send (enc (exp (gen) x-5) a (pubk b)))
      (recv (enc h2 (exp h2 x-5) h3 b (pubk a)))
      (send (enc (exp h3 x-5) (pubk b))))
    ((send (enc (exp (gen) x-6) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-6) (exp h2 (mul x x-5 (rec x-6))) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-5)) (pubk b-0))))
    ((send (enc (exp (gen) x-4) a-1 (pubk b-1)))
      (recv
        (enc h2-1 (exp h2-1 x-4) (exp h2-0 (mul x-0 (rec x-4) x-6)) b-1
          (pubk a-1))) (send (enc (exp h2-0 (mul x-0 x-6)) (pubk b-1))))
    ((send (enc (exp (gen) x-7) a-2 (pubk b-2)))
      (recv
        (enc h2-2 (exp h2-2 x-7) (exp h2-1 (mul x-1 x-4 (rec x-7))) b-2
          (pubk a-2))) (send (enc (exp h2-1 (mul x-1 x-4)) (pubk b-2))))
    ((send (enc (exp (gen) x-3) a-3 (pubk b-3)))
      (recv
        (enc (exp h2-3 x-10) (exp h2-3 (mul x-3 x-10))
          (exp h2-2 (mul x-2 (rec x-3) x-7)) b-3 (pubk a-3)))
      (send (enc (exp h2-2 (mul x-2 x-7)) (pubk b-3))))
    ((recv (enc (exp h2-3 (mul x-3 x-8)) a-4 (pubk b-4)))
      (send
        (enc (exp (gen) x-9) (exp h2-3 (mul x-3 x-8 x-9)) (exp (gen) z)
          b-4 (pubk a-4)))))
  (label 38)
  (parent 15)
  (unrealized (1 1) (2 1) (3 1) (4 1) (5 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 a-3 b-3 name)
    (h2 h3 h2-0 h2-1 h2-2 base)
    (x x-0 x-1 x-2 x-3 x-4 x-5 x-6 x-7 x-8 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-4))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x (rec x-3) x-4))) (x x-3))
  (defstrand init 3 (a a-1) (b b-1) (h2 h2-1)
    (h3 (exp h2-0 (mul x-0 x-3 (rec x-5)))) (x x-5))
  (defstrand init 3 (a a-2) (b b-2) (h2 h2-2)
    (h3 (exp h2-1 (mul x-1 (rec x-2) x-5))) (x x-2))
  (defstrand init 3 (a a-3) (b b-3) (h2 (exp (gen) x-8))
    (h3 (exp h2-2 (mul x-2 x-6 (rec x-7)))) (x x-7))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (3 1))
    ((2 2) (1 1)) ((3 0) (4 1)) ((3 2) (2 1)) ((4 2) (3 1)))
  (non-orig (privk a) (privk b))
  (uniq-gen x-2 x-3 x-4 x-5 x-7)
  (operation nonce-test (algebra-contracted (h2-3 (exp (gen) x-8)))
    (exp (gen) (mul x-7 x-8)) (4 1) (exp (gen) x-7))
  (traces
    ((send (enc (exp (gen) x-4) a (pubk b)))
      (recv (enc h2 (exp h2 x-4) h3 b (pubk a)))
      (send (enc (exp h3 x-4) (pubk b))))
    ((send (enc (exp (gen) x-3) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-3) (exp h2 (mul x (rec x-3) x-4)) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-4)) (pubk b-0))))
    ((send (enc (exp (gen) x-5) a-1 (pubk b-1)))
      (recv
        (enc h2-1 (exp h2-1 x-5) (exp h2-0 (mul x-0 x-3 (rec x-5))) b-1
          (pubk a-1))) (send (enc (exp h2-0 (mul x-0 x-3)) (pubk b-1))))
    ((send (enc (exp (gen) x-2) a-2 (pubk b-2)))
      (recv
        (enc h2-2 (exp h2-2 x-2) (exp h2-1 (mul x-1 (rec x-2) x-5)) b-2
          (pubk a-2))) (send (enc (exp h2-1 (mul x-1 x-5)) (pubk b-2))))
    ((send (enc (exp (gen) x-7) a-3 (pubk b-3)))
      (recv
        (enc (exp (gen) x-8) (exp (gen) (mul x-7 x-8))
          (exp h2-2 (mul x-2 x-6 (rec x-7))) b-3 (pubk a-3)))
      (send (enc (exp h2-2 (mul x-2 x-6)) (pubk b-3)))))
  (label 39)
  (parent 15)
  (unrealized (1 1) (2 1) (3 1) (4 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 b-3 a-3 a-4 b-4 name)
    (h2 h3 h2-0 h2-1 h2-2 h2-3 base)
    (x x-0 x-1 x-2 z x-3 x-4 x-5 x-6 x-7 x-8 x-9 x-10 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-4))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x x-4 (rec x-5)))) (x x-5))
  (defstrand init 3 (a a-1) (b b-1) (h2 h2-1)
    (h3 (exp h2-0 (mul x-0 (rec x-3) x-5))) (x x-3))
  (defstrand init 3 (a a-2) (b b-2) (h2 (exp h2-2 x-2))
    (h3 (exp h2-1 (mul x-1 x-3 (rec x-6)))) (x x-6))
  (defstrand resp 2 (b b-3) (a a-3) (h1 (exp h2-2 (mul x-6 x-8))) (y z)
    (z x-7))
  (defstrand init 3 (a a-4) (b b-4) (h2 h2-3)
    (h3 (exp h2-2 (mul x-6 x-8 x-9 (rec x-10)))) (x x-10))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (3 1))
    ((2 2) (1 1)) ((3 0) (5 1)) ((3 2) (2 1)) ((4 1) (3 1))
    ((5 2) (4 0)))
  (ind-zero-in (z (exp h2-2 (mul x-6 x-8)))
    (x-7 (exp h2-2 (mul x-6 x-8))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-3 x-4 x-5 x-6 x-7 x-10)
  (operation nonce-test (added-strand init 3) (exp h2-2 (mul x-6 x-8))
    (4 0) (exp (gen) x-6))
  (traces
    ((send (enc (exp (gen) x-4) a (pubk b)))
      (recv (enc h2 (exp h2 x-4) h3 b (pubk a)))
      (send (enc (exp h3 x-4) (pubk b))))
    ((send (enc (exp (gen) x-5) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-5) (exp h2 (mul x x-4 (rec x-5))) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-4)) (pubk b-0))))
    ((send (enc (exp (gen) x-3) a-1 (pubk b-1)))
      (recv
        (enc h2-1 (exp h2-1 x-3) (exp h2-0 (mul x-0 (rec x-3) x-5)) b-1
          (pubk a-1))) (send (enc (exp h2-0 (mul x-0 x-5)) (pubk b-1))))
    ((send (enc (exp (gen) x-6) a-2 (pubk b-2)))
      (recv
        (enc (exp h2-2 x-2) (exp h2-2 (mul x-2 x-6))
          (exp h2-1 (mul x-1 x-3 (rec x-6))) b-2 (pubk a-2)))
      (send (enc (exp h2-1 (mul x-1 x-3)) (pubk b-2))))
    ((recv (enc (exp h2-2 (mul x-6 x-8)) a-3 (pubk b-3)))
      (send
        (enc (exp (gen) z) (exp h2-2 (mul z x-6 x-8)) (exp (gen) x-7)
          b-3 (pubk a-3))))
    ((send (enc (exp (gen) x-10) a-4 (pubk b-4)))
      (recv
        (enc h2-3 (exp h2-3 x-10)
          (exp h2-2 (mul x-6 x-8 x-9 (rec x-10))) b-4 (pubk a-4)))
      (send (enc (exp h2-2 (mul x-6 x-8 x-9)) (pubk b-4)))))
  (label 40)
  (parent 16)
  (unrealized (1 1) (2 1) (3 1) (5 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 b-3 a-3 b-4 a-4 name)
    (h2 h3 h2-0 h2-1 h2-2 base)
    (x x-0 x-1 x-2 z z-0 x-3 x-4 x-5 x-6 x-7 x-8 x-9 x-10 x-11 x-12
      expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-4))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x x-4 (rec x-5)))) (x x-5))
  (defstrand init 3 (a a-1) (b b-1) (h2 h2-1)
    (h3 (exp h2-0 (mul x-0 (rec x-3) x-5))) (x x-3))
  (defstrand init 3 (a a-2) (b b-2) (h2 (exp h2-2 (mul x-2 x-11)))
    (h3 (exp h2-1 (mul x-1 x-3 (rec x-6)))) (x x-6))
  (defstrand resp 2 (b b-3) (a a-3)
    (h1 (exp h2-2 (mul x-6 x-8 x-9 x-10 (rec x-12)))) (y z-0) (z x-7))
  (defstrand resp 2 (b b-4) (a a-4) (h1 (exp h2-2 (mul x-6 x-8 x-9)))
    (y x-10) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (3 1))
    ((2 2) (1 1)) ((3 0) (5 0)) ((3 2) (2 1)) ((4 1) (3 1))
    ((5 1) (4 0)))
  (ind-zero-in (x-10 (exp h2-2 (mul x-6 x-8 x-9)))
    (z (exp h2-2 (mul x-6 x-8 x-9)))
    (z-0 (exp h2-2 (mul x-6 x-8 x-9 x-10 (rec x-12))))
    (x-7 (exp h2-2 (mul x-6 x-8 x-9 x-10 (rec x-12)))))
  (non-orig (privk a) (privk b))
  (uniq-gen z z-0 x-3 x-4 x-5 x-6 x-7 x-10)
  (operation nonce-test (added-strand resp 2)
    (exp h2-2 (mul x-6 x-8 x-9 x-10 (rec x-12))) (4 0) (exp (gen) x-6))
  (traces
    ((send (enc (exp (gen) x-4) a (pubk b)))
      (recv (enc h2 (exp h2 x-4) h3 b (pubk a)))
      (send (enc (exp h3 x-4) (pubk b))))
    ((send (enc (exp (gen) x-5) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-5) (exp h2 (mul x x-4 (rec x-5))) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-4)) (pubk b-0))))
    ((send (enc (exp (gen) x-3) a-1 (pubk b-1)))
      (recv
        (enc h2-1 (exp h2-1 x-3) (exp h2-0 (mul x-0 (rec x-3) x-5)) b-1
          (pubk a-1))) (send (enc (exp h2-0 (mul x-0 x-5)) (pubk b-1))))
    ((send (enc (exp (gen) x-6) a-2 (pubk b-2)))
      (recv
        (enc (exp h2-2 (mul x-2 x-11)) (exp h2-2 (mul x-2 x-6 x-11))
          (exp h2-1 (mul x-1 x-3 (rec x-6))) b-2 (pubk a-2)))
      (send (enc (exp h2-1 (mul x-1 x-3)) (pubk b-2))))
    ((recv
       (enc (exp h2-2 (mul x-6 x-8 x-9 x-10 (rec x-12))) a-3
         (pubk b-3)))
      (send
        (enc (exp (gen) z-0)
          (exp h2-2 (mul z-0 x-6 x-8 x-9 x-10 (rec x-12)))
          (exp (gen) x-7) b-3 (pubk a-3))))
    ((recv (enc (exp h2-2 (mul x-6 x-8 x-9)) a-4 (pubk b-4)))
      (send
        (enc (exp (gen) x-10) (exp h2-2 (mul x-6 x-8 x-9 x-10))
          (exp (gen) z) b-4 (pubk a-4)))))
  (label 41)
  (parent 16)
  (unrealized (1 1) (2 1) (3 1) (5 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 b-3 a-3 name)
    (h2 h3 h2-0 h2-1 base)
    (x x-0 x-1 z x-2 x-3 x-4 x-5 x-6 x-7 x-8 x-9 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-4))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x (rec x-3) x-4))) (x x-3))
  (defstrand init 3 (a a-1) (b b-1) (h2 h2-1)
    (h3 (exp h2-0 (mul x-0 x-3 (rec x-5)))) (x x-5))
  (defstrand init 3 (a a-2) (b b-2)
    (h2 (exp (gen) (mul (rec x-6) x-8 x-9)))
    (h3 (exp h2-1 (mul x-1 (rec x-2) x-5))) (x x-2))
  (defstrand resp 2 (b b-3) (a a-3) (h1 (exp (gen) (mul x-2 x-9)))
    (y x-7) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (3 1))
    ((2 2) (1 1)) ((3 0) (4 0)) ((3 2) (2 1)) ((4 1) (3 1)))
  (ind-zero-in (x-7 (exp (gen) (mul x-2 x-9)))
    (z (exp (gen) (mul x-2 x-9))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-2 x-3 x-4 x-5 x-7)
  (operation nonce-test
    (algebra-contracted (h2-2 (exp (gen) (mul (rec x-6) x-9))))
    (exp (gen) (mul x-2 x-9)) (4 0) (exp (gen) x-2))
  (traces
    ((send (enc (exp (gen) x-4) a (pubk b)))
      (recv (enc h2 (exp h2 x-4) h3 b (pubk a)))
      (send (enc (exp h3 x-4) (pubk b))))
    ((send (enc (exp (gen) x-3) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-3) (exp h2 (mul x (rec x-3) x-4)) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-4)) (pubk b-0))))
    ((send (enc (exp (gen) x-5) a-1 (pubk b-1)))
      (recv
        (enc h2-1 (exp h2-1 x-5) (exp h2-0 (mul x-0 x-3 (rec x-5))) b-1
          (pubk a-1))) (send (enc (exp h2-0 (mul x-0 x-3)) (pubk b-1))))
    ((send (enc (exp (gen) x-2) a-2 (pubk b-2)))
      (recv
        (enc (exp (gen) (mul (rec x-6) x-8 x-9))
          (exp (gen) (mul x-2 (rec x-6) x-8 x-9))
          (exp h2-1 (mul x-1 (rec x-2) x-5)) b-2 (pubk a-2)))
      (send (enc (exp h2-1 (mul x-1 x-5)) (pubk b-2))))
    ((recv (enc (exp (gen) (mul x-2 x-9)) a-3 (pubk b-3)))
      (send
        (enc (exp (gen) x-7) (exp (gen) (mul x-2 x-7 x-9)) (exp (gen) z)
          b-3 (pubk a-3)))))
  (label 42)
  (parent 16)
  (unrealized (1 1) (2 1) (3 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 a-3 b-3 name)
    (h2 h3 h2-0 h2-1 h2-2 base)
    (x x-0 x-1 x-2 x-3 x-4 x-5 x-6 x-7 x-8 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-4))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x (rec x-3) x-4))) (x x-3))
  (defstrand init 3 (a a-1) (b b-1) (h2 h2-1)
    (h3 (exp h2-0 (mul x-0 x-3 (rec x-5)))) (x x-5))
  (defstrand init 3 (a a-2) (b b-2) (h2 (exp (gen) x-1))
    (h3 (exp h2-1 (mul (rec x-2) x-5 x-6))) (x x-2))
  (defstrand init 3 (a a-3) (b b-3) (h2 h2-2)
    (h3 (exp h2-1 (mul (rec x-2) x-5 x-6 x-7 (rec x-8)))) (x x-8))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (4 1))
    ((2 2) (1 1)) ((3 0) (4 1)) ((3 2) (2 1)) ((4 2) (3 1)))
  (non-orig (privk a) (privk b))
  (uniq-gen x-2 x-3 x-4 x-5 x-8)
  (operation nonce-test (added-strand init 3)
    (exp h2-1 (mul (rec x-2) x-5 x-6)) (3 1))
  (traces
    ((send (enc (exp (gen) x-4) a (pubk b)))
      (recv (enc h2 (exp h2 x-4) h3 b (pubk a)))
      (send (enc (exp h3 x-4) (pubk b))))
    ((send (enc (exp (gen) x-3) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-3) (exp h2 (mul x (rec x-3) x-4)) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-4)) (pubk b-0))))
    ((send (enc (exp (gen) x-5) a-1 (pubk b-1)))
      (recv
        (enc h2-1 (exp h2-1 x-5) (exp h2-0 (mul x-0 x-3 (rec x-5))) b-1
          (pubk a-1))) (send (enc (exp h2-0 (mul x-0 x-3)) (pubk b-1))))
    ((send (enc (exp (gen) x-2) a-2 (pubk b-2)))
      (recv
        (enc (exp (gen) x-1) (exp (gen) (mul x-1 x-2))
          (exp h2-1 (mul (rec x-2) x-5 x-6)) b-2 (pubk a-2)))
      (send (enc (exp h2-1 (mul x-5 x-6)) (pubk b-2))))
    ((send (enc (exp (gen) x-8) a-3 (pubk b-3)))
      (recv
        (enc h2-2 (exp h2-2 x-8)
          (exp h2-1 (mul (rec x-2) x-5 x-6 x-7 (rec x-8))) b-3
          (pubk a-3)))
      (send (enc (exp h2-1 (mul (rec x-2) x-5 x-6 x-7)) (pubk b-3)))))
  (label 43)
  (parent 17)
  (unrealized (1 1) (2 1) (4 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 b-3 a-3 name)
    (h2 h3 h2-0 h2-1 base)
    (x x-0 x-1 z x-2 x-3 x-4 x-5 x-6 x-7 x-8 x-9 x-10 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-4))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x (rec x-3) x-4))) (x x-3))
  (defstrand init 3 (a a-1) (b b-1) (h2 (exp h2-1 x-9))
    (h3 (exp h2-0 (mul x-0 x-3 (rec x-5)))) (x x-5))
  (defstrand init 3 (a a-2) (b b-2) (h2 (exp (gen) x-1))
    (h3 (exp h2-1 (mul (rec x-2) x-5 x-6 x-7 x-8 (rec x-10)))) (x x-2))
  (defstrand resp 2 (b b-3) (a a-3)
    (h1 (exp h2-1 (mul (rec x-2) x-5 x-6 x-7))) (y x-8) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (4 0))
    ((2 2) (1 1)) ((3 0) (4 0)) ((3 2) (2 1)) ((4 1) (3 1)))
  (ind-zero-in (x-8 (exp h2-1 (mul (rec x-2) x-5 x-6 x-7)))
    (z (exp h2-1 (mul (rec x-2) x-5 x-6 x-7))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-2 x-3 x-4 x-5 x-8)
  (operation nonce-test (added-strand resp 2)
    (exp h2-1 (mul (rec x-2) x-5 x-6 x-7 x-8 (rec x-10))) (3 1))
  (traces
    ((send (enc (exp (gen) x-4) a (pubk b)))
      (recv (enc h2 (exp h2 x-4) h3 b (pubk a)))
      (send (enc (exp h3 x-4) (pubk b))))
    ((send (enc (exp (gen) x-3) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-3) (exp h2 (mul x (rec x-3) x-4)) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-4)) (pubk b-0))))
    ((send (enc (exp (gen) x-5) a-1 (pubk b-1)))
      (recv
        (enc (exp h2-1 x-9) (exp h2-1 (mul x-5 x-9))
          (exp h2-0 (mul x-0 x-3 (rec x-5))) b-1 (pubk a-1)))
      (send (enc (exp h2-0 (mul x-0 x-3)) (pubk b-1))))
    ((send (enc (exp (gen) x-2) a-2 (pubk b-2)))
      (recv
        (enc (exp (gen) x-1) (exp (gen) (mul x-1 x-2))
          (exp h2-1 (mul (rec x-2) x-5 x-6 x-7 x-8 (rec x-10))) b-2
          (pubk a-2)))
      (send
        (enc (exp h2-1 (mul x-5 x-6 x-7 x-8 (rec x-10))) (pubk b-2))))
    ((recv (enc (exp h2-1 (mul (rec x-2) x-5 x-6 x-7)) a-3 (pubk b-3)))
      (send
        (enc (exp (gen) x-8) (exp h2-1 (mul (rec x-2) x-5 x-6 x-7 x-8))
          (exp (gen) z) b-3 (pubk a-3)))))
  (label 44)
  (parent 17)
  (unrealized (1 1) (2 1) (4 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 name) (h2 h3 h2-0 base)
    (x x-0 x-1 x-2 x-3 x-4 x-5 x-6 x-7 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-2))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x x-2 (rec x-3)))) (x x-3))
  (defstrand init 3 (a a-1) (b b-1)
    (h2 (exp (gen) (mul (rec x-1) (rec x-4) x-5 x-5 x-7)))
    (h3 (exp h2-0 (mul x-0 (rec x-1) x-3))) (x x-1))
  (defstrand init 3 (a a-2) (b b-2) (h2 (exp (gen) x-6))
    (h3 (exp (gen) (mul x-5 x-7))) (x x-5))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (3 1))
    ((2 2) (1 1)) ((3 2) (2 1)))
  (non-orig (privk a) (privk b))
  (uniq-gen x-1 x-2 x-3 x-5)
  (operation nonce-test
    (algebra-contracted
      (h2-1 (exp (gen) (mul (rec x-1) (rec x-4) x-5 x-5 x-7))))
    (exp (gen) (mul x-5 x-7)) (3 1))
  (traces
    ((send (enc (exp (gen) x-2) a (pubk b)))
      (recv (enc h2 (exp h2 x-2) h3 b (pubk a)))
      (send (enc (exp h3 x-2) (pubk b))))
    ((send (enc (exp (gen) x-3) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-3) (exp h2 (mul x x-2 (rec x-3))) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-2)) (pubk b-0))))
    ((send (enc (exp (gen) x-1) a-1 (pubk b-1)))
      (recv
        (enc (exp (gen) (mul (rec x-1) (rec x-4) x-5 x-5 x-7))
          (exp (gen) (mul (rec x-4) x-5 x-5 x-7))
          (exp h2-0 (mul x-0 (rec x-1) x-3)) b-1 (pubk a-1)))
      (send (enc (exp h2-0 (mul x-0 x-3)) (pubk b-1))))
    ((send (enc (exp (gen) x-5) a-2 (pubk b-2)))
      (recv
        (enc (exp (gen) x-6) (exp (gen) (mul x-5 x-6))
          (exp (gen) (mul x-5 x-7)) b-2 (pubk a-2)))
      (send (enc (exp (gen) (mul x-5 x-5 x-7)) (pubk b-2)))))
  (label 45)
  (parent 17)
  (unrealized (1 1) (2 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 b-2 a-2 a-3 b-3 a-4 b-4 name)
    (h2 h3 h2-0 h2-1 h2-2 h2-3 base)
    (x x-0 x-1 x-2 x-3 z x-4 x-5 x-6 x-7 x-8 x-9 x-10 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-6))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x x-6 (rec x-7)))) (x x-7))
  (defstrand init 3 (a a-1) (b b-1) (h2 (exp h2-1 x-1))
    (h3 (exp h2-0 (mul x-0 (rec x-5) x-7))) (x x-5))
  (defstrand resp 2 (b b-2) (a a-2) (h1 (exp h2-1 (mul x-2 x-5)))
    (y x-8) (z x-4))
  (defstrand init 3 (a a-3) (b b-3) (h2 h2-2)
    (h3 (exp h2-1 (mul x-2 x-3 (rec z) x-5))) (x z))
  (defstrand init 3 (a a-4) (b b-4) (h2 h2-3)
    (h3 (exp h2-2 (mul z x-9 (rec x-10)))) (x x-10))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (4 1))
    ((2 2) (1 1)) ((3 1) (2 1)) ((4 0) (5 1)) ((4 2) (3 0))
    ((5 2) (4 1)))
  (ind-zero-in (x-8 (exp h2-1 (mul x-2 x-5)))
    (x-4 (exp h2-1 (mul x-2 x-5))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-4 x-5 x-6 x-7 x-8 x-10)
  (operation nonce-test (added-strand init 3) (exp h2-2 z) (4 1)
    (exp (gen) z))
  (traces
    ((send (enc (exp (gen) x-6) a (pubk b)))
      (recv (enc h2 (exp h2 x-6) h3 b (pubk a)))
      (send (enc (exp h3 x-6) (pubk b))))
    ((send (enc (exp (gen) x-7) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-7) (exp h2 (mul x x-6 (rec x-7))) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-6)) (pubk b-0))))
    ((send (enc (exp (gen) x-5) a-1 (pubk b-1)))
      (recv
        (enc (exp h2-1 x-1) (exp h2-1 (mul x-1 x-5))
          (exp h2-0 (mul x-0 (rec x-5) x-7)) b-1 (pubk a-1)))
      (send (enc (exp h2-0 (mul x-0 x-7)) (pubk b-1))))
    ((recv (enc (exp h2-1 (mul x-2 x-5)) a-2 (pubk b-2)))
      (send
        (enc (exp (gen) x-8) (exp h2-1 (mul x-2 x-5 x-8))
          (exp (gen) x-4) b-2 (pubk a-2))))
    ((send (enc (exp (gen) z) a-3 (pubk b-3)))
      (recv
        (enc h2-2 (exp h2-2 z) (exp h2-1 (mul x-2 x-3 (rec z) x-5)) b-3
          (pubk a-3)))
      (send (enc (exp h2-1 (mul x-2 x-3 x-5)) (pubk b-3))))
    ((send (enc (exp (gen) x-10) a-4 (pubk b-4)))
      (recv
        (enc h2-3 (exp h2-3 x-10) (exp h2-2 (mul z x-9 (rec x-10))) b-4
          (pubk a-4))) (send (enc (exp h2-2 (mul z x-9)) (pubk b-4)))))
  (label 46)
  (parent 18)
  (unrealized (1 1) (2 1) (4 1) (5 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 b-2 a-2 a-3 b-3 b-4 a-4 name)
    (h2 h3 h2-0 h2-1 h2-2 base)
    (x x-0 x-1 x-2 x-3 z z-0 x-4 x-5 x-6 x-7 x-8 x-9 x-10 x-11 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-6))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x x-6 (rec x-7)))) (x x-7))
  (defstrand init 3 (a a-1) (b b-1) (h2 (exp h2-1 x-1))
    (h3 (exp h2-0 (mul x-0 (rec x-5) x-7))) (x x-5))
  (defstrand resp 2 (b b-2) (a a-2) (h1 (exp h2-1 (mul x-2 x-5)))
    (y x-8) (z x-4))
  (defstrand init 3 (a a-3) (b b-3) (h2 (exp h2-2 x-11))
    (h3 (exp h2-1 (mul x-2 x-3 (rec z-0) x-5))) (x z-0))
  (defstrand resp 2 (b b-4) (a a-4) (h1 (exp h2-2 (mul z-0 x-9)))
    (y x-10) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (4 1))
    ((2 2) (1 1)) ((3 1) (2 1)) ((4 0) (5 0)) ((4 2) (3 0))
    ((5 1) (4 1)))
  (ind-zero-in (x-10 (exp h2-2 (mul z-0 x-9)))
    (z (exp h2-2 (mul z-0 x-9))) (x-8 (exp h2-1 (mul x-2 x-5)))
    (x-4 (exp h2-1 (mul x-2 x-5))))
  (non-orig (privk a) (privk b))
  (uniq-gen z z-0 x-4 x-5 x-6 x-7 x-8 x-10)
  (operation nonce-test (added-strand resp 2) (exp h2-2 (mul z-0 x-11))
    (4 1) (exp (gen) z-0))
  (traces
    ((send (enc (exp (gen) x-6) a (pubk b)))
      (recv (enc h2 (exp h2 x-6) h3 b (pubk a)))
      (send (enc (exp h3 x-6) (pubk b))))
    ((send (enc (exp (gen) x-7) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-7) (exp h2 (mul x x-6 (rec x-7))) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-6)) (pubk b-0))))
    ((send (enc (exp (gen) x-5) a-1 (pubk b-1)))
      (recv
        (enc (exp h2-1 x-1) (exp h2-1 (mul x-1 x-5))
          (exp h2-0 (mul x-0 (rec x-5) x-7)) b-1 (pubk a-1)))
      (send (enc (exp h2-0 (mul x-0 x-7)) (pubk b-1))))
    ((recv (enc (exp h2-1 (mul x-2 x-5)) a-2 (pubk b-2)))
      (send
        (enc (exp (gen) x-8) (exp h2-1 (mul x-2 x-5 x-8))
          (exp (gen) x-4) b-2 (pubk a-2))))
    ((send (enc (exp (gen) z-0) a-3 (pubk b-3)))
      (recv
        (enc (exp h2-2 x-11) (exp h2-2 (mul z-0 x-11))
          (exp h2-1 (mul x-2 x-3 (rec z-0) x-5)) b-3 (pubk a-3)))
      (send (enc (exp h2-1 (mul x-2 x-3 x-5)) (pubk b-3))))
    ((recv (enc (exp h2-2 (mul z-0 x-9)) a-4 (pubk b-4)))
      (send
        (enc (exp (gen) x-10) (exp h2-2 (mul z-0 x-9 x-10))
          (exp (gen) z) b-4 (pubk a-4)))))
  (label 47)
  (parent 18)
  (unrealized (1 1) (2 1) (4 1) (5 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 b-2 a-2 a-3 b-3 name)
    (h2 h3 h2-0 h2-1 base)
    (x x-0 x-1 z x-2 x-3 x-4 x-5 x-6 x-7 x-8 x-9 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-3))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x (rec x-2) x-3))) (x x-2))
  (defstrand init 3 (a a-1) (b b-1) (h2 (exp h2-1 x-1))
    (h3 (exp h2-0 (mul x-0 x-2 (rec x-4)))) (x x-4))
  (defstrand resp 2 (b b-2) (a a-2) (h1 (exp h2-1 (mul x-4 x-6))) (y z)
    (z x-5))
  (defstrand init 3 (a a-3) (b b-3) (h2 (exp (gen) x-9))
    (h3 (exp h2-1 (mul x-4 x-6 x-7 (rec x-8)))) (x x-8))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (4 1))
    ((2 2) (1 1)) ((3 1) (2 1)) ((4 2) (3 0)))
  (ind-zero-in (z (exp h2-1 (mul x-4 x-6)))
    (x-5 (exp h2-1 (mul x-4 x-6))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-2 x-3 x-4 x-5 x-8)
  (operation nonce-test (algebra-contracted (h2-2 (exp (gen) x-9)))
    (exp (gen) (mul x-8 x-9)) (4 1) (exp (gen) x-8))
  (traces
    ((send (enc (exp (gen) x-3) a (pubk b)))
      (recv (enc h2 (exp h2 x-3) h3 b (pubk a)))
      (send (enc (exp h3 x-3) (pubk b))))
    ((send (enc (exp (gen) x-2) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-2) (exp h2 (mul x (rec x-2) x-3)) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-3)) (pubk b-0))))
    ((send (enc (exp (gen) x-4) a-1 (pubk b-1)))
      (recv
        (enc (exp h2-1 x-1) (exp h2-1 (mul x-1 x-4))
          (exp h2-0 (mul x-0 x-2 (rec x-4))) b-1 (pubk a-1)))
      (send (enc (exp h2-0 (mul x-0 x-2)) (pubk b-1))))
    ((recv (enc (exp h2-1 (mul x-4 x-6)) a-2 (pubk b-2)))
      (send
        (enc (exp (gen) z) (exp h2-1 (mul z x-4 x-6)) (exp (gen) x-5)
          b-2 (pubk a-2))))
    ((send (enc (exp (gen) x-8) a-3 (pubk b-3)))
      (recv
        (enc (exp (gen) x-9) (exp (gen) (mul x-8 x-9))
          (exp h2-1 (mul x-4 x-6 x-7 (rec x-8))) b-3 (pubk a-3)))
      (send (enc (exp h2-1 (mul x-4 x-6 x-7)) (pubk b-3)))))
  (label 48)
  (parent 18)
  (unrealized (1 1) (2 1) (4 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 b-2 a-2 b-3 a-3 a-4 b-4 name)
    (h2 h3 h2-0 h2-1 h2-2 base)
    (x x-0 x-1 x-2 x-3 z z-0 x-4 x-5 x-6 x-7 x-8 x-9 x-10 x-11 x-12
      expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-5))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x x-5 (rec x-6)))) (x x-6))
  (defstrand init 3 (a a-1) (b b-1) (h2 (exp h2-1 (mul x-1 x-2)))
    (h3 (exp h2-0 (mul x-0 (rec x-4) x-6))) (x x-4))
  (defstrand resp 2 (b b-2) (a a-2)
    (h1 (exp h2-1 (mul (rec x-3) z x-4 x-9 x-10))) (y x-7) (z z-0))
  (defstrand resp 2 (b b-3) (a a-3) (h1 (exp h2-1 (mul x-4 x-9 x-10)))
    (y z) (z x-8))
  (defstrand init 3 (a a-4) (b b-4) (h2 h2-2)
    (h3 (exp h2-1 (mul x-4 x-9 x-10 x-11 (rec x-12)))) (x x-12))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (5 1))
    ((2 2) (1 1)) ((3 1) (2 1)) ((4 1) (3 0)) ((5 2) (4 0)))
  (ind-zero-in (z (exp h2-1 (mul x-4 x-9 x-10)))
    (x-8 (exp h2-1 (mul x-4 x-9 x-10)))
    (x-7 (exp h2-1 (mul (rec x-3) z x-4 x-9 x-10)))
    (z-0 (exp h2-1 (mul (rec x-3) z x-4 x-9 x-10))))
  (non-orig (privk a) (privk b))
  (uniq-gen z z-0 x-4 x-5 x-6 x-7 x-8 x-12)
  (operation nonce-test (added-strand init 3)
    (exp h2-1 (mul x-4 x-9 x-10)) (4 0) (exp (gen) x-4))
  (traces
    ((send (enc (exp (gen) x-5) a (pubk b)))
      (recv (enc h2 (exp h2 x-5) h3 b (pubk a)))
      (send (enc (exp h3 x-5) (pubk b))))
    ((send (enc (exp (gen) x-6) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-6) (exp h2 (mul x x-5 (rec x-6))) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-5)) (pubk b-0))))
    ((send (enc (exp (gen) x-4) a-1 (pubk b-1)))
      (recv
        (enc (exp h2-1 (mul x-1 x-2)) (exp h2-1 (mul x-1 x-2 x-4))
          (exp h2-0 (mul x-0 (rec x-4) x-6)) b-1 (pubk a-1)))
      (send (enc (exp h2-0 (mul x-0 x-6)) (pubk b-1))))
    ((recv
       (enc (exp h2-1 (mul (rec x-3) z x-4 x-9 x-10)) a-2 (pubk b-2)))
      (send
        (enc (exp (gen) x-7)
          (exp h2-1 (mul (rec x-3) z x-4 x-7 x-9 x-10)) (exp (gen) z-0)
          b-2 (pubk a-2))))
    ((recv (enc (exp h2-1 (mul x-4 x-9 x-10)) a-3 (pubk b-3)))
      (send
        (enc (exp (gen) z) (exp h2-1 (mul z x-4 x-9 x-10))
          (exp (gen) x-8) b-3 (pubk a-3))))
    ((send (enc (exp (gen) x-12) a-4 (pubk b-4)))
      (recv
        (enc h2-2 (exp h2-2 x-12)
          (exp h2-1 (mul x-4 x-9 x-10 x-11 (rec x-12))) b-4 (pubk a-4)))
      (send (enc (exp h2-1 (mul x-4 x-9 x-10 x-11)) (pubk b-4)))))
  (label 49)
  (parent 19)
  (unrealized (1 1) (2 1) (5 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 b-2 a-2 b-3 a-3 b-4 a-4 name)
    (h2 h3 h2-0 h2-1 base)
    (x x-0 x-1 x-2 x-3 z z-0 z-1 x-4 x-5 x-6 x-7 x-8 x-9 x-10 x-11 x-12
      x-13 x-14 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-5))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x x-5 (rec x-6)))) (x x-6))
  (defstrand init 3 (a a-1) (b b-1) (h2 (exp h2-1 (mul x-1 x-2 x-13)))
    (h3 (exp h2-0 (mul x-0 (rec x-4) x-6))) (x x-4))
  (defstrand resp 2 (b b-2) (a a-2)
    (h1
      (exp h2-1 (mul (rec x-3) z-0 x-4 x-9 x-10 x-11 x-12 (rec x-14))))
    (y x-7) (z z-1))
  (defstrand resp 2 (b b-3) (a a-3)
    (h1 (exp h2-1 (mul x-4 x-9 x-10 x-11 x-12 (rec x-14)))) (y z-0)
    (z x-8))
  (defstrand resp 2 (b b-4) (a a-4)
    (h1 (exp h2-1 (mul x-4 x-9 x-10 x-11))) (y x-12) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (5 0))
    ((2 2) (1 1)) ((3 1) (2 1)) ((4 1) (3 0)) ((5 1) (4 0)))
  (ind-zero-in (x-12 (exp h2-1 (mul x-4 x-9 x-10 x-11)))
    (z (exp h2-1 (mul x-4 x-9 x-10 x-11)))
    (z-0 (exp h2-1 (mul x-4 x-9 x-10 x-11 x-12 (rec x-14))))
    (x-8 (exp h2-1 (mul x-4 x-9 x-10 x-11 x-12 (rec x-14))))
    (x-7
      (exp h2-1 (mul (rec x-3) z-0 x-4 x-9 x-10 x-11 x-12 (rec x-14))))
    (z-1
      (exp h2-1 (mul (rec x-3) z-0 x-4 x-9 x-10 x-11 x-12 (rec x-14)))))
  (non-orig (privk a) (privk b))
  (uniq-gen z z-0 z-1 x-4 x-5 x-6 x-7 x-8 x-12)
  (operation nonce-test (added-strand resp 2)
    (exp h2-1 (mul x-4 x-9 x-10 x-11 x-12 (rec x-14))) (4 0)
    (exp (gen) x-4))
  (traces
    ((send (enc (exp (gen) x-5) a (pubk b)))
      (recv (enc h2 (exp h2 x-5) h3 b (pubk a)))
      (send (enc (exp h3 x-5) (pubk b))))
    ((send (enc (exp (gen) x-6) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-6) (exp h2 (mul x x-5 (rec x-6))) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-5)) (pubk b-0))))
    ((send (enc (exp (gen) x-4) a-1 (pubk b-1)))
      (recv
        (enc (exp h2-1 (mul x-1 x-2 x-13))
          (exp h2-1 (mul x-1 x-2 x-4 x-13))
          (exp h2-0 (mul x-0 (rec x-4) x-6)) b-1 (pubk a-1)))
      (send (enc (exp h2-0 (mul x-0 x-6)) (pubk b-1))))
    ((recv
       (enc
         (exp h2-1
           (mul (rec x-3) z-0 x-4 x-9 x-10 x-11 x-12 (rec x-14))) a-2
         (pubk b-2)))
      (send
        (enc (exp (gen) x-7)
          (exp h2-1
            (mul (rec x-3) z-0 x-4 x-7 x-9 x-10 x-11 x-12 (rec x-14)))
          (exp (gen) z-1) b-2 (pubk a-2))))
    ((recv
       (enc (exp h2-1 (mul x-4 x-9 x-10 x-11 x-12 (rec x-14))) a-3
         (pubk b-3)))
      (send
        (enc (exp (gen) z-0)
          (exp h2-1 (mul z-0 x-4 x-9 x-10 x-11 x-12 (rec x-14)))
          (exp (gen) x-8) b-3 (pubk a-3))))
    ((recv (enc (exp h2-1 (mul x-4 x-9 x-10 x-11)) a-4 (pubk b-4)))
      (send
        (enc (exp (gen) x-12) (exp h2-1 (mul x-4 x-9 x-10 x-11 x-12))
          (exp (gen) z) b-4 (pubk a-4)))))
  (label 50)
  (parent 19)
  (unrealized (1 1) (2 1) (5 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 b-2 a-2 b-3 a-3 name) (h2 h3 h2-0 base)
    (x x-0 x-1 z z-0 x-2 x-3 x-4 x-5 x-6 x-7 x-8 x-9 x-10 x-11 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-3))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x (rec x-2) x-3))) (x x-2))
  (defstrand init 3 (a a-1) (b b-1)
    (h2 (exp (gen) (mul x-1 (rec x-6) (rec x-7) x-9 x-11)))
    (h3 (exp h2-0 (mul x-0 x-2 (rec x-4)))) (x x-4))
  (defstrand resp 2 (b b-2) (a a-2)
    (h1 (exp (gen) (mul x-4 x-8 (rec x-10) x-11))) (y z-0) (z x-5))
  (defstrand resp 2 (b b-3) (a a-3) (h1 (exp (gen) (mul x-4 x-11)))
    (y x-8) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (4 0))
    ((2 2) (1 1)) ((3 1) (2 1)) ((4 1) (3 0)))
  (ind-zero-in (x-8 (exp (gen) (mul x-4 x-11)))
    (z (exp (gen) (mul x-4 x-11)))
    (z-0 (exp (gen) (mul x-4 x-8 (rec x-10) x-11)))
    (x-5 (exp (gen) (mul x-4 x-8 (rec x-10) x-11))))
  (non-orig (privk a) (privk b))
  (uniq-gen z z-0 x-2 x-3 x-4 x-5 x-8)
  (operation nonce-test
    (algebra-contracted
      (h2-1 (exp (gen) (mul (rec x-6) (rec x-7) x-11))))
    (exp (gen) (mul x-4 x-11)) (4 0) (exp (gen) x-4))
  (traces
    ((send (enc (exp (gen) x-3) a (pubk b)))
      (recv (enc h2 (exp h2 x-3) h3 b (pubk a)))
      (send (enc (exp h3 x-3) (pubk b))))
    ((send (enc (exp (gen) x-2) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-2) (exp h2 (mul x (rec x-2) x-3)) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-3)) (pubk b-0))))
    ((send (enc (exp (gen) x-4) a-1 (pubk b-1)))
      (recv
        (enc (exp (gen) (mul x-1 (rec x-6) (rec x-7) x-9 x-11))
          (exp (gen) (mul x-1 x-4 (rec x-6) (rec x-7) x-9 x-11))
          (exp h2-0 (mul x-0 x-2 (rec x-4))) b-1 (pubk a-1)))
      (send (enc (exp h2-0 (mul x-0 x-2)) (pubk b-1))))
    ((recv
       (enc (exp (gen) (mul x-4 x-8 (rec x-10) x-11)) a-2 (pubk b-2)))
      (send
        (enc (exp (gen) z-0)
          (exp (gen) (mul z-0 x-4 x-8 (rec x-10) x-11)) (exp (gen) x-5)
          b-2 (pubk a-2))))
    ((recv (enc (exp (gen) (mul x-4 x-11)) a-3 (pubk b-3)))
      (send
        (enc (exp (gen) x-8) (exp (gen) (mul x-4 x-8 x-11))
          (exp (gen) z) b-3 (pubk a-3)))))
  (label 51)
  (parent 19)
  (unrealized (1 1) (2 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 b-2 a-2 name) (h2 h3 base)
    (x x-0 z x-1 x-2 x-3 x-4 x-5 x-6 x-7 x-8 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-2))
  (defstrand init 3 (a a-0) (b b-0)
    (h2 (exp (gen) (mul (rec x-0) x-1 (rec x-3) x-5 x-8)))
    (h3 (exp h2 (mul x x-2 (rec x-3)))) (x x-3))
  (defstrand init 3 (a a-1) (b b-1)
    (h2 (exp (gen) (mul (rec x-4) x-6 x-7)))
    (h3 (exp (gen) (mul x-5 x-8))) (x x-1))
  (defstrand resp 2 (b b-2) (a a-2) (h1 (exp (gen) (mul x-1 x-7)))
    (y x-5) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (3 0))
    ((2 2) (1 1)) ((3 1) (2 1)))
  (ind-zero-in (x-5 (exp (gen) (mul x-1 x-7)))
    (z (exp (gen) (mul x-1 x-7))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-1 x-2 x-3 x-5)
  (operation nonce-test
    (algebra-contracted
      (h2-0 (exp (gen) (mul (rec x-0) x-1 (rec x-3) x-5 x-8))))
    (exp (gen) (mul x-5 x-8)) (2 1))
  (traces
    ((send (enc (exp (gen) x-2) a (pubk b)))
      (recv (enc h2 (exp h2 x-2) h3 b (pubk a)))
      (send (enc (exp h3 x-2) (pubk b))))
    ((send (enc (exp (gen) x-3) a-0 (pubk b-0)))
      (recv
        (enc (exp (gen) (mul (rec x-0) x-1 (rec x-3) x-5 x-8))
          (exp (gen) (mul (rec x-0) x-1 x-5 x-8))
          (exp h2 (mul x x-2 (rec x-3))) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x x-2)) (pubk b-0))))
    ((send (enc (exp (gen) x-1) a-1 (pubk b-1)))
      (recv
        (enc (exp (gen) (mul (rec x-4) x-6 x-7))
          (exp (gen) (mul x-1 (rec x-4) x-6 x-7))
          (exp (gen) (mul x-5 x-8)) b-1 (pubk a-1)))
      (send (enc (exp (gen) (mul x-1 x-5 x-8)) (pubk b-1))))
    ((recv (enc (exp (gen) (mul x-1 x-7)) a-2 (pubk b-2)))
      (send
        (enc (exp (gen) x-5) (exp (gen) (mul x-1 x-5 x-7)) (exp (gen) z)
          b-2 (pubk a-2)))))
  (label 52)
  (parent 20)
  (seen 52)
  (unrealized (1 1))
  (comment "1 in cohort - 0 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 a-3 b-3 name)
    (h2 h3 h2-0 h2-1 h2-2 base)
    (x x-0 x-1 x-2 x-3 x-4 x-5 x-6 x-7 x-8 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-5))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x (rec x-4) x-5))) (x x-4))
  (defstrand init 3 (a a-1) (b b-1) (h2 (exp (gen) x-0))
    (h3 (exp h2-0 (mul x-1 x-4 (rec x-6)))) (x x-6))
  (defstrand init 3 (a a-2) (b b-2) (h2 h2-1)
    (h3 (exp h2-0 (mul x-1 x-2 (rec x-3) x-4 (rec x-6)))) (x x-3))
  (defstrand init 3 (a a-3) (b b-3) (h2 h2-2)
    (h3 (exp h2-1 (mul x-3 x-7 (rec x-8)))) (x x-8))
  (precedes ((0 0) (1 1)) ((1 0) (3 1)) ((1 2) (0 1)) ((2 0) (3 1))
    ((2 2) (1 1)) ((3 0) (4 1)) ((3 2) (2 1)) ((4 2) (3 1)))
  (non-orig (privk a) (privk b))
  (uniq-gen x-3 x-4 x-5 x-6 x-8)
  (operation nonce-test (added-strand init 3) (exp h2-1 x-3) (3 1)
    (exp (gen) x-3))
  (traces
    ((send (enc (exp (gen) x-5) a (pubk b)))
      (recv (enc h2 (exp h2 x-5) h3 b (pubk a)))
      (send (enc (exp h3 x-5) (pubk b))))
    ((send (enc (exp (gen) x-4) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-4) (exp h2 (mul x (rec x-4) x-5)) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-5)) (pubk b-0))))
    ((send (enc (exp (gen) x-6) a-1 (pubk b-1)))
      (recv
        (enc (exp (gen) x-0) (exp (gen) (mul x-0 x-6))
          (exp h2-0 (mul x-1 x-4 (rec x-6))) b-1 (pubk a-1)))
      (send (enc (exp h2-0 (mul x-1 x-4)) (pubk b-1))))
    ((send (enc (exp (gen) x-3) a-2 (pubk b-2)))
      (recv
        (enc h2-1 (exp h2-1 x-3)
          (exp h2-0 (mul x-1 x-2 (rec x-3) x-4 (rec x-6))) b-2
          (pubk a-2)))
      (send (enc (exp h2-0 (mul x-1 x-2 x-4 (rec x-6))) (pubk b-2))))
    ((send (enc (exp (gen) x-8) a-3 (pubk b-3)))
      (recv
        (enc h2-2 (exp h2-2 x-8) (exp h2-1 (mul x-3 x-7 (rec x-8))) b-3
          (pubk a-3)))
      (send (enc (exp h2-1 (mul x-3 x-7)) (pubk b-3)))))
  (label 53)
  (parent 21)
  (unrealized (1 1) (3 1) (4 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 b-3 a-3 name)
    (h2 h3 h2-0 h2-1 base)
    (x x-0 x-1 x-2 z x-3 x-4 x-5 x-6 x-7 x-8 x-9 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-5))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x (rec x-4) x-5))) (x x-4))
  (defstrand init 3 (a a-1) (b b-1) (h2 (exp (gen) x-0))
    (h3 (exp h2-0 (mul x-1 x-4 (rec x-6)))) (x x-6))
  (defstrand init 3 (a a-2) (b b-2) (h2 (exp h2-1 x-9))
    (h3 (exp h2-0 (mul x-1 x-2 (rec x-3) x-4 (rec x-6)))) (x x-3))
  (defstrand resp 2 (b b-3) (a a-3) (h1 (exp h2-1 (mul x-3 x-7)))
    (y x-8) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (3 1)) ((1 2) (0 1)) ((2 0) (3 1))
    ((2 2) (1 1)) ((3 0) (4 0)) ((3 2) (2 1)) ((4 1) (3 1)))
  (ind-zero-in (x-8 (exp h2-1 (mul x-3 x-7)))
    (z (exp h2-1 (mul x-3 x-7))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-3 x-4 x-5 x-6 x-8)
  (operation nonce-test (added-strand resp 2) (exp h2-1 (mul x-3 x-9))
    (3 1) (exp (gen) x-3))
  (traces
    ((send (enc (exp (gen) x-5) a (pubk b)))
      (recv (enc h2 (exp h2 x-5) h3 b (pubk a)))
      (send (enc (exp h3 x-5) (pubk b))))
    ((send (enc (exp (gen) x-4) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-4) (exp h2 (mul x (rec x-4) x-5)) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-5)) (pubk b-0))))
    ((send (enc (exp (gen) x-6) a-1 (pubk b-1)))
      (recv
        (enc (exp (gen) x-0) (exp (gen) (mul x-0 x-6))
          (exp h2-0 (mul x-1 x-4 (rec x-6))) b-1 (pubk a-1)))
      (send (enc (exp h2-0 (mul x-1 x-4)) (pubk b-1))))
    ((send (enc (exp (gen) x-3) a-2 (pubk b-2)))
      (recv
        (enc (exp h2-1 x-9) (exp h2-1 (mul x-3 x-9))
          (exp h2-0 (mul x-1 x-2 (rec x-3) x-4 (rec x-6))) b-2
          (pubk a-2)))
      (send (enc (exp h2-0 (mul x-1 x-2 x-4 (rec x-6))) (pubk b-2))))
    ((recv (enc (exp h2-1 (mul x-3 x-7)) a-3 (pubk b-3)))
      (send
        (enc (exp (gen) x-8) (exp h2-1 (mul x-3 x-7 x-8)) (exp (gen) z)
          b-3 (pubk a-3)))))
  (label 54)
  (parent 21)
  (unrealized (1 1) (3 1) (4 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 name) (h2 h3 h2-0 base)
    (x x-0 x-1 x-2 x-3 x-4 x-5 x-6 x-7 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-2))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x x-2 (rec x-3)))) (x x-3))
  (defstrand init 3 (a a-1) (b b-1) (h2 (exp (gen) x-0))
    (h3 (exp h2-0 (mul (rec x-1) x-3 x-4))) (x x-1))
  (defstrand init 3 (a a-2) (b b-2) (h2 (exp (gen) x-7))
    (h3 (exp h2-0 (mul (rec x-1) x-3 x-4 x-5 (rec x-6)))) (x x-6))
  (precedes ((0 0) (1 1)) ((1 0) (3 1)) ((1 2) (0 1)) ((2 0) (3 1))
    ((2 2) (1 1)) ((3 2) (2 1)))
  (non-orig (privk a) (privk b))
  (uniq-gen x-1 x-2 x-3 x-6)
  (operation nonce-test (algebra-contracted (h2-1 (exp (gen) x-7)))
    (exp (gen) (mul x-6 x-7)) (3 1) (exp (gen) x-6))
  (traces
    ((send (enc (exp (gen) x-2) a (pubk b)))
      (recv (enc h2 (exp h2 x-2) h3 b (pubk a)))
      (send (enc (exp h3 x-2) (pubk b))))
    ((send (enc (exp (gen) x-3) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-3) (exp h2 (mul x x-2 (rec x-3))) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-2)) (pubk b-0))))
    ((send (enc (exp (gen) x-1) a-1 (pubk b-1)))
      (recv
        (enc (exp (gen) x-0) (exp (gen) (mul x-0 x-1))
          (exp h2-0 (mul (rec x-1) x-3 x-4)) b-1 (pubk a-1)))
      (send (enc (exp h2-0 (mul x-3 x-4)) (pubk b-1))))
    ((send (enc (exp (gen) x-6) a-2 (pubk b-2)))
      (recv
        (enc (exp (gen) x-7) (exp (gen) (mul x-6 x-7))
          (exp h2-0 (mul (rec x-1) x-3 x-4 x-5 (rec x-6))) b-2
          (pubk a-2)))
      (send (enc (exp h2-0 (mul (rec x-1) x-3 x-4 x-5)) (pubk b-2)))))
  (label 55)
  (parent 21)
  (unrealized (1 1) (3 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 b-2 a-2 a-3 b-3 name)
    (h2 h3 h2-0 h2-1 base)
    (x x-0 x-1 x-2 z x-3 x-4 x-5 x-6 x-7 x-8 x-9 x-10 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-4))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp h2-0 x-1))
    (h3 (exp h2 (mul x (rec x-3) x-4))) (x x-3))
  (defstrand init 3 (a a-1) (b b-1) (h2 (exp (gen) x-0))
    (h3 (exp h2-0 (mul (rec x-2) z x-3 (rec x-5) x-7 x-8))) (x x-5))
  (defstrand resp 2 (b b-2) (a a-2)
    (h1 (exp h2-0 (mul x-3 (rec x-5) x-7 x-8))) (y z) (z x-6))
  (defstrand init 3 (a a-3) (b b-3) (h2 h2-1)
    (h3 (exp h2-0 (mul x-3 (rec x-5) x-7 x-8 (rec x-9) (rec x-10))))
    (x x-10))
  (precedes ((0 0) (1 1)) ((1 0) (4 1)) ((1 2) (0 1)) ((2 0) (4 1))
    ((2 2) (1 1)) ((3 1) (2 1)) ((4 2) (3 0)))
  (ind-zero-in (z (exp h2-0 (mul x-3 (rec x-5) x-7 x-8)))
    (x-6 (exp h2-0 (mul x-3 (rec x-5) x-7 x-8))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-3 x-4 x-5 x-6 x-10)
  (operation nonce-test (added-strand init 3)
    (exp h2-0 (mul x-3 (rec x-5) x-7 x-8)) (3 0))
  (traces
    ((send (enc (exp (gen) x-4) a (pubk b)))
      (recv (enc h2 (exp h2 x-4) h3 b (pubk a)))
      (send (enc (exp h3 x-4) (pubk b))))
    ((send (enc (exp (gen) x-3) a-0 (pubk b-0)))
      (recv
        (enc (exp h2-0 x-1) (exp h2-0 (mul x-1 x-3))
          (exp h2 (mul x (rec x-3) x-4)) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x x-4)) (pubk b-0))))
    ((send (enc (exp (gen) x-5) a-1 (pubk b-1)))
      (recv
        (enc (exp (gen) x-0) (exp (gen) (mul x-0 x-5))
          (exp h2-0 (mul (rec x-2) z x-3 (rec x-5) x-7 x-8)) b-1
          (pubk a-1)))
      (send (enc (exp h2-0 (mul (rec x-2) z x-3 x-7 x-8)) (pubk b-1))))
    ((recv (enc (exp h2-0 (mul x-3 (rec x-5) x-7 x-8)) a-2 (pubk b-2)))
      (send
        (enc (exp (gen) z) (exp h2-0 (mul z x-3 (rec x-5) x-7 x-8))
          (exp (gen) x-6) b-2 (pubk a-2))))
    ((send (enc (exp (gen) x-10) a-3 (pubk b-3)))
      (recv
        (enc h2-1 (exp h2-1 x-10)
          (exp h2-0 (mul x-3 (rec x-5) x-7 x-8 (rec x-9) (rec x-10)))
          b-3 (pubk a-3)))
      (send
        (enc (exp h2-0 (mul x-3 (rec x-5) x-7 x-8 (rec x-9)))
          (pubk b-3)))))
  (label 56)
  (parent 22)
  (unrealized (1 1) (4 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 b-2 a-2 b-3 a-3 name) (h2 h3 h2-0 base)
    (x x-0 x-1 x-2 z z-0 x-3 x-4 y x-5 x-6 x-7 x-8 x-9 x-10 x-11 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-4))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp h2-0 (mul x-1 x-10)))
    (h3 (exp h2 (mul x (rec x-3) x-4))) (x x-3))
  (defstrand init 3 (a a-1) (b b-1) (h2 (exp (gen) x-0))
    (h3
      (exp h2-0
        (mul (rec x-2) z-0 x-3 (rec y) x-6 x-7 (rec x-8) x-9 x-11)))
    (x y))
  (defstrand resp 2 (b b-2) (a a-2)
    (h1 (exp h2-0 (mul x-3 (rec y) x-6 x-7 (rec x-8) x-9 x-11))) (y z-0)
    (z x-5))
  (defstrand resp 2 (b b-3) (a a-3)
    (h1 (exp h2-0 (mul x-3 (rec y) x-6 x-7 (rec x-8)))) (y x-9) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (4 0)) ((1 2) (0 1)) ((2 0) (4 0))
    ((2 2) (1 1)) ((3 1) (2 1)) ((4 1) (3 0)))
  (ind-zero-in (x-9 (exp h2-0 (mul x-3 (rec y) x-6 x-7 (rec x-8))))
    (z (exp h2-0 (mul x-3 (rec y) x-6 x-7 (rec x-8))))
    (z-0 (exp h2-0 (mul x-3 (rec y) x-6 x-7 (rec x-8) x-9 x-11)))
    (x-5 (exp h2-0 (mul x-3 (rec y) x-6 x-7 (rec x-8) x-9 x-11))))
  (non-orig (privk a) (privk b))
  (uniq-gen z z-0 x-3 x-4 y x-5 x-9)
  (operation nonce-test (added-strand resp 2)
    (exp h2-0 (mul x-3 (rec y) x-6 x-7 (rec x-8) x-9 x-11)) (3 0))
  (traces
    ((send (enc (exp (gen) x-4) a (pubk b)))
      (recv (enc h2 (exp h2 x-4) h3 b (pubk a)))
      (send (enc (exp h3 x-4) (pubk b))))
    ((send (enc (exp (gen) x-3) a-0 (pubk b-0)))
      (recv
        (enc (exp h2-0 (mul x-1 x-10)) (exp h2-0 (mul x-1 x-3 x-10))
          (exp h2 (mul x (rec x-3) x-4)) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x x-4)) (pubk b-0))))
    ((send (enc (exp (gen) y) a-1 (pubk b-1)))
      (recv
        (enc (exp (gen) x-0) (exp (gen) (mul x-0 y))
          (exp h2-0
            (mul (rec x-2) z-0 x-3 (rec y) x-6 x-7 (rec x-8) x-9 x-11))
          b-1 (pubk a-1)))
      (send
        (enc
          (exp h2-0 (mul (rec x-2) z-0 x-3 x-6 x-7 (rec x-8) x-9 x-11))
          (pubk b-1))))
    ((recv
       (enc (exp h2-0 (mul x-3 (rec y) x-6 x-7 (rec x-8) x-9 x-11)) a-2
         (pubk b-2)))
      (send
        (enc (exp (gen) z-0)
          (exp h2-0 (mul z-0 x-3 (rec y) x-6 x-7 (rec x-8) x-9 x-11))
          (exp (gen) x-5) b-2 (pubk a-2))))
    ((recv
       (enc (exp h2-0 (mul x-3 (rec y) x-6 x-7 (rec x-8))) a-3
         (pubk b-3)))
      (send
        (enc (exp (gen) x-9)
          (exp h2-0 (mul x-3 (rec y) x-6 x-7 (rec x-8) x-9))
          (exp (gen) z) b-3 (pubk a-3)))))
  (label 57)
  (parent 22)
  (unrealized (1 1) (4 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 b-2 a-2 name) (h2 h3 base)
    (x x-0 z x-1 x-2 x-3 x-4 x-5 x-6 x-7 x-8 x-9 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-2))
  (defstrand init 3 (a a-0) (b b-0)
    (h2 (exp (gen) (mul x-1 (rec x-4) (rec x-5) x-7 x-9)))
    (h3 (exp h2 (mul x x-2 (rec x-3)))) (x x-3))
  (defstrand init 3 (a a-1) (b b-1) (h2 (exp (gen) x-0))
    (h3 (exp (gen) (mul x-3 x-6 (rec x-8) x-9))) (x x-1))
  (defstrand resp 2 (b b-2) (a a-2) (h1 (exp (gen) (mul x-3 x-9)))
    (y x-6) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (3 0)) ((1 2) (0 1)) ((2 0) (3 0))
    ((2 2) (1 1)) ((3 1) (2 1)))
  (ind-zero-in (x-6 (exp (gen) (mul x-3 x-9)))
    (z (exp (gen) (mul x-3 x-9))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-1 x-2 x-3 x-6)
  (operation nonce-test
    (algebra-contracted
      (h2-0 (exp (gen) (mul x-1 (rec x-4) (rec x-5) x-9))))
    (exp (gen) (mul x-3 x-9)) (3 0))
  (traces
    ((send (enc (exp (gen) x-2) a (pubk b)))
      (recv (enc h2 (exp h2 x-2) h3 b (pubk a)))
      (send (enc (exp h3 x-2) (pubk b))))
    ((send (enc (exp (gen) x-3) a-0 (pubk b-0)))
      (recv
        (enc (exp (gen) (mul x-1 (rec x-4) (rec x-5) x-7 x-9))
          (exp (gen) (mul x-1 x-3 (rec x-4) (rec x-5) x-7 x-9))
          (exp h2 (mul x x-2 (rec x-3))) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x x-2)) (pubk b-0))))
    ((send (enc (exp (gen) x-1) a-1 (pubk b-1)))
      (recv
        (enc (exp (gen) x-0) (exp (gen) (mul x-0 x-1))
          (exp (gen) (mul x-3 x-6 (rec x-8) x-9)) b-1 (pubk a-1)))
      (send
        (enc (exp (gen) (mul x-1 x-3 x-6 (rec x-8) x-9)) (pubk b-1))))
    ((recv (enc (exp (gen) (mul x-3 x-9)) a-2 (pubk b-2)))
      (send
        (enc (exp (gen) x-6) (exp (gen) (mul x-3 x-6 x-9)) (exp (gen) z)
          b-2 (pubk a-2)))))
  (label 58)
  (parent 22)
  (unrealized (1 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 name) (h2 h3 base)
    (x x-0 x-1 x-2 x-3 x-4 x-5 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-0))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp (gen) (mul x-2 x-5)))
    (h3 (exp h2 (mul x x-0 (rec x-5)))) (x x-5))
  (defstrand init 3 (a a-1) (b b-1) (h2 (exp (gen) x-1))
    (h3 (exp (gen) (mul x-3 x-4))) (x x-4))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 2) (1 1)))
  (non-orig (privk a) (privk b))
  (uniq-gen x-0 x-4 x-5)
  (operation nonce-test
    (algebra-contracted (x-6 x-5)
      (x-7 (mul (rec x-2) x-3 x-4 x-4 (rec x-5) (rec x-5))) (x-8 x-4)
      (x-9 x-3) (x-10 x-2)) (exp (gen) (mul x-2 x-5)) (1 1))
  (traces
    ((send (enc (exp (gen) x-0) a (pubk b)))
      (recv (enc h2 (exp h2 x-0) h3 b (pubk a)))
      (send (enc (exp h3 x-0) (pubk b))))
    ((send (enc (exp (gen) x-5) a-0 (pubk b-0)))
      (recv
        (enc (exp (gen) (mul x-2 x-5)) (exp (gen) (mul x-2 x-5 x-5))
          (exp h2 (mul x x-0 (rec x-5))) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x x-0)) (pubk b-0))))
    ((send (enc (exp (gen) x-4) a-1 (pubk b-1)))
      (recv
        (enc (exp (gen) x-1) (exp (gen) (mul x-1 x-4))
          (exp (gen) (mul x-3 x-4)) b-1 (pubk a-1)))
      (send (enc (exp (gen) (mul x-3 x-4 x-4)) (pubk b-1)))))
  (label 59)
  (parent 23)
  (seen 59)
  (unrealized (1 1))
  (comment "1 in cohort - 0 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 b-1 a-1 a-2 b-2 a-3 b-3 a-4 b-4 name)
    (h2 h3 h2-0 h2-1 h2-2 h2-3 base)
    (x x-0 x-1 x-2 x-3 z x-4 x-5 x-6 x-7 x-8 x-9 x-10 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-5))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp h2-0 x-0))
    (h3 (exp h2 (mul x x-5 (rec x-6)))) (x x-6))
  (defstrand resp 2 (b b-1) (a a-1) (h1 (exp h2-0 (mul x-1 x-6)))
    (y x-4) (z x-7))
  (defstrand init 3 (a a-2) (b b-2) (h2 h2-1)
    (h3 (exp h2-0 (mul x-1 x-2 x-6 (rec x-8)))) (x x-8))
  (defstrand init 3 (a a-3) (b b-3) (h2 h2-2)
    (h3 (exp h2-1 (mul x-3 (rec z) x-8))) (x z))
  (defstrand init 3 (a a-4) (b b-4) (h2 h2-3)
    (h3 (exp h2-2 (mul z x-9 (rec x-10)))) (x x-10))
  (precedes ((0 0) (1 1)) ((1 0) (3 1)) ((1 2) (0 1)) ((2 1) (1 1))
    ((3 0) (4 1)) ((3 2) (2 0)) ((4 0) (5 1)) ((4 2) (3 1))
    ((5 2) (4 1)))
  (ind-zero-in (x-4 (exp h2-0 (mul x-1 x-6)))
    (x-7 (exp h2-0 (mul x-1 x-6))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-4 x-5 x-6 x-7 x-8 x-10)
  (operation nonce-test (added-strand init 3) (exp h2-2 z) (4 1)
    (exp (gen) z))
  (traces
    ((send (enc (exp (gen) x-5) a (pubk b)))
      (recv (enc h2 (exp h2 x-5) h3 b (pubk a)))
      (send (enc (exp h3 x-5) (pubk b))))
    ((send (enc (exp (gen) x-6) a-0 (pubk b-0)))
      (recv
        (enc (exp h2-0 x-0) (exp h2-0 (mul x-0 x-6))
          (exp h2 (mul x x-5 (rec x-6))) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x x-5)) (pubk b-0))))
    ((recv (enc (exp h2-0 (mul x-1 x-6)) a-1 (pubk b-1)))
      (send
        (enc (exp (gen) x-4) (exp h2-0 (mul x-1 x-4 x-6))
          (exp (gen) x-7) b-1 (pubk a-1))))
    ((send (enc (exp (gen) x-8) a-2 (pubk b-2)))
      (recv
        (enc h2-1 (exp h2-1 x-8) (exp h2-0 (mul x-1 x-2 x-6 (rec x-8)))
          b-2 (pubk a-2)))
      (send (enc (exp h2-0 (mul x-1 x-2 x-6)) (pubk b-2))))
    ((send (enc (exp (gen) z) a-3 (pubk b-3)))
      (recv
        (enc h2-2 (exp h2-2 z) (exp h2-1 (mul x-3 (rec z) x-8)) b-3
          (pubk a-3))) (send (enc (exp h2-1 (mul x-3 x-8)) (pubk b-3))))
    ((send (enc (exp (gen) x-10) a-4 (pubk b-4)))
      (recv
        (enc h2-3 (exp h2-3 x-10) (exp h2-2 (mul z x-9 (rec x-10))) b-4
          (pubk a-4))) (send (enc (exp h2-2 (mul z x-9)) (pubk b-4)))))
  (label 60)
  (parent 24)
  (unrealized (1 1) (3 1) (4 1) (5 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 b-1 a-1 a-2 b-2 a-3 b-3 b-4 a-4 name)
    (h2 h3 h2-0 h2-1 h2-2 base)
    (x x-0 x-1 x-2 x-3 z z-0 x-4 x-5 x-6 x-7 x-8 x-9 x-10 x-11 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-5))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp h2-0 x-0))
    (h3 (exp h2 (mul x x-5 (rec x-6)))) (x x-6))
  (defstrand resp 2 (b b-1) (a a-1) (h1 (exp h2-0 (mul x-1 x-6)))
    (y x-4) (z x-7))
  (defstrand init 3 (a a-2) (b b-2) (h2 h2-1)
    (h3 (exp h2-0 (mul x-1 x-2 x-6 (rec x-8)))) (x x-8))
  (defstrand init 3 (a a-3) (b b-3) (h2 (exp h2-2 x-11))
    (h3 (exp h2-1 (mul x-3 (rec z-0) x-8))) (x z-0))
  (defstrand resp 2 (b b-4) (a a-4) (h1 (exp h2-2 (mul z-0 x-9)))
    (y x-10) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (3 1)) ((1 2) (0 1)) ((2 1) (1 1))
    ((3 0) (4 1)) ((3 2) (2 0)) ((4 0) (5 0)) ((4 2) (3 1))
    ((5 1) (4 1)))
  (ind-zero-in (x-10 (exp h2-2 (mul z-0 x-9)))
    (z (exp h2-2 (mul z-0 x-9))) (x-4 (exp h2-0 (mul x-1 x-6)))
    (x-7 (exp h2-0 (mul x-1 x-6))))
  (non-orig (privk a) (privk b))
  (uniq-gen z z-0 x-4 x-5 x-6 x-7 x-8 x-10)
  (operation nonce-test (added-strand resp 2) (exp h2-2 (mul z-0 x-11))
    (4 1) (exp (gen) z-0))
  (traces
    ((send (enc (exp (gen) x-5) a (pubk b)))
      (recv (enc h2 (exp h2 x-5) h3 b (pubk a)))
      (send (enc (exp h3 x-5) (pubk b))))
    ((send (enc (exp (gen) x-6) a-0 (pubk b-0)))
      (recv
        (enc (exp h2-0 x-0) (exp h2-0 (mul x-0 x-6))
          (exp h2 (mul x x-5 (rec x-6))) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x x-5)) (pubk b-0))))
    ((recv (enc (exp h2-0 (mul x-1 x-6)) a-1 (pubk b-1)))
      (send
        (enc (exp (gen) x-4) (exp h2-0 (mul x-1 x-4 x-6))
          (exp (gen) x-7) b-1 (pubk a-1))))
    ((send (enc (exp (gen) x-8) a-2 (pubk b-2)))
      (recv
        (enc h2-1 (exp h2-1 x-8) (exp h2-0 (mul x-1 x-2 x-6 (rec x-8)))
          b-2 (pubk a-2)))
      (send (enc (exp h2-0 (mul x-1 x-2 x-6)) (pubk b-2))))
    ((send (enc (exp (gen) z-0) a-3 (pubk b-3)))
      (recv
        (enc (exp h2-2 x-11) (exp h2-2 (mul z-0 x-11))
          (exp h2-1 (mul x-3 (rec z-0) x-8)) b-3 (pubk a-3)))
      (send (enc (exp h2-1 (mul x-3 x-8)) (pubk b-3))))
    ((recv (enc (exp h2-2 (mul z-0 x-9)) a-4 (pubk b-4)))
      (send
        (enc (exp (gen) x-10) (exp h2-2 (mul z-0 x-9 x-10))
          (exp (gen) z) b-4 (pubk a-4)))))
  (label 61)
  (parent 24)
  (unrealized (1 1) (3 1) (4 1) (5 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 b-1 a-1 a-2 b-2 a-3 b-3 name)
    (h2 h3 h2-0 h2-1 base)
    (x x-0 x-1 x-2 z x-3 x-4 x-5 x-6 x-7 x-8 x-9 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-5))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp h2-0 x-0))
    (h3 (exp h2 (mul x (rec x-4) x-5))) (x x-4))
  (defstrand resp 2 (b b-1) (a a-1) (h1 (exp h2-0 (mul x-1 x-4)))
    (y x-6) (z x-3))
  (defstrand init 3 (a a-2) (b b-2) (h2 h2-1)
    (h3 (exp h2-0 (mul x-1 x-2 (rec z) x-4))) (x z))
  (defstrand init 3 (a a-3) (b b-3) (h2 (exp (gen) x-9))
    (h3 (exp h2-1 (mul z x-7 (rec x-8)))) (x x-8))
  (precedes ((0 0) (1 1)) ((1 0) (3 1)) ((1 2) (0 1)) ((2 1) (1 1))
    ((3 0) (4 1)) ((3 2) (2 0)) ((4 2) (3 1)))
  (ind-zero-in (x-6 (exp h2-0 (mul x-1 x-4)))
    (x-3 (exp h2-0 (mul x-1 x-4))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-3 x-4 x-5 x-6 x-8)
  (operation nonce-test (algebra-contracted (h2-2 (exp (gen) x-9)))
    (exp (gen) (mul x-8 x-9)) (4 1) (exp (gen) x-8))
  (traces
    ((send (enc (exp (gen) x-5) a (pubk b)))
      (recv (enc h2 (exp h2 x-5) h3 b (pubk a)))
      (send (enc (exp h3 x-5) (pubk b))))
    ((send (enc (exp (gen) x-4) a-0 (pubk b-0)))
      (recv
        (enc (exp h2-0 x-0) (exp h2-0 (mul x-0 x-4))
          (exp h2 (mul x (rec x-4) x-5)) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x x-5)) (pubk b-0))))
    ((recv (enc (exp h2-0 (mul x-1 x-4)) a-1 (pubk b-1)))
      (send
        (enc (exp (gen) x-6) (exp h2-0 (mul x-1 x-4 x-6))
          (exp (gen) x-3) b-1 (pubk a-1))))
    ((send (enc (exp (gen) z) a-2 (pubk b-2)))
      (recv
        (enc h2-1 (exp h2-1 z) (exp h2-0 (mul x-1 x-2 (rec z) x-4)) b-2
          (pubk a-2)))
      (send (enc (exp h2-0 (mul x-1 x-2 x-4)) (pubk b-2))))
    ((send (enc (exp (gen) x-8) a-3 (pubk b-3)))
      (recv
        (enc (exp (gen) x-9) (exp (gen) (mul x-8 x-9))
          (exp h2-1 (mul z x-7 (rec x-8))) b-3 (pubk a-3)))
      (send (enc (exp h2-1 (mul z x-7)) (pubk b-3)))))
  (label 62)
  (parent 24)
  (unrealized (1 1) (3 1) (4 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 b-1 a-1 a-2 b-2 b-3 a-3 a-4 b-4 name)
    (h2 h3 h2-0 h2-1 h2-2 base)
    (x x-0 x-1 x-2 x-3 z z-0 x-4 x-5 x-6 x-7 x-8 z-1 x-9 z-2 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-4))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp h2-0 x-0))
    (h3 (exp h2 (mul x x-4 (rec x-5)))) (x x-5))
  (defstrand resp 2 (b b-1) (a a-1) (h1 (exp h2-0 (mul x-1 x-5)))
    (y z-0) (z x-6))
  (defstrand init 3 (a a-2) (b b-2) (h2 (exp h2-1 x-3))
    (h3 (exp h2-0 (mul x-1 x-2 x-5 (rec x-7)))) (x x-7))
  (defstrand resp 2 (b b-3) (a a-3) (h1 (exp h2-1 (mul x-7 z-1))) (y z)
    (z x-8))
  (defstrand init 3 (a a-4) (b b-4) (h2 h2-2)
    (h3 (exp h2-1 (mul x-7 z-1 x-9 (rec z-2)))) (x z-2))
  (precedes ((0 0) (1 1)) ((1 0) (3 1)) ((1 2) (0 1)) ((2 1) (1 1))
    ((3 0) (5 1)) ((3 2) (2 0)) ((4 1) (3 1)) ((5 2) (4 0)))
  (ind-zero-in (z (exp h2-1 (mul x-7 z-1)))
    (x-8 (exp h2-1 (mul x-7 z-1))) (z-0 (exp h2-0 (mul x-1 x-5)))
    (x-6 (exp h2-0 (mul x-1 x-5))))
  (non-orig (privk a) (privk b))
  (uniq-gen z z-0 x-4 x-5 x-6 x-7 x-8 z-2)
  (operation nonce-test (added-strand init 3) (exp h2-1 (mul x-7 z-1))
    (4 0) (exp (gen) x-7))
  (traces
    ((send (enc (exp (gen) x-4) a (pubk b)))
      (recv (enc h2 (exp h2 x-4) h3 b (pubk a)))
      (send (enc (exp h3 x-4) (pubk b))))
    ((send (enc (exp (gen) x-5) a-0 (pubk b-0)))
      (recv
        (enc (exp h2-0 x-0) (exp h2-0 (mul x-0 x-5))
          (exp h2 (mul x x-4 (rec x-5))) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x x-4)) (pubk b-0))))
    ((recv (enc (exp h2-0 (mul x-1 x-5)) a-1 (pubk b-1)))
      (send
        (enc (exp (gen) z-0) (exp h2-0 (mul x-1 z-0 x-5))
          (exp (gen) x-6) b-1 (pubk a-1))))
    ((send (enc (exp (gen) x-7) a-2 (pubk b-2)))
      (recv
        (enc (exp h2-1 x-3) (exp h2-1 (mul x-3 x-7))
          (exp h2-0 (mul x-1 x-2 x-5 (rec x-7))) b-2 (pubk a-2)))
      (send (enc (exp h2-0 (mul x-1 x-2 x-5)) (pubk b-2))))
    ((recv (enc (exp h2-1 (mul x-7 z-1)) a-3 (pubk b-3)))
      (send
        (enc (exp (gen) z) (exp h2-1 (mul z x-7 z-1)) (exp (gen) x-8)
          b-3 (pubk a-3))))
    ((send (enc (exp (gen) z-2) a-4 (pubk b-4)))
      (recv
        (enc h2-2 (exp h2-2 z-2) (exp h2-1 (mul x-7 z-1 x-9 (rec z-2)))
          b-4 (pubk a-4)))
      (send (enc (exp h2-1 (mul x-7 z-1 x-9)) (pubk b-4)))))
  (label 63)
  (parent 25)
  (unrealized (1 1) (3 1) (5 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 b-1 a-1 a-2 b-2 b-3 a-3 b-4 a-4 name)
    (h2 h3 h2-0 h2-1 base)
    (x x-0 x-1 x-2 x-3 z z-0 z-1 x-4 x-5 x-6 x-7 x-8 z-2 x-9 z-3 z-4
      x-10 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-4))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp h2-0 x-0))
    (h3 (exp h2 (mul x x-4 (rec x-5)))) (x x-5))
  (defstrand resp 2 (b b-1) (a a-1) (h1 (exp h2-0 (mul x-1 x-5)))
    (y z-1) (z x-6))
  (defstrand init 3 (a a-2) (b b-2) (h2 (exp h2-1 (mul x-3 z-4)))
    (h3 (exp h2-0 (mul x-1 x-2 x-5 (rec x-7)))) (x x-7))
  (defstrand resp 2 (b b-3) (a a-3)
    (h1 (exp h2-1 (mul x-7 z-2 x-9 z-3 (rec x-10)))) (y z-0) (z x-8))
  (defstrand resp 2 (b b-4) (a a-4) (h1 (exp h2-1 (mul x-7 z-2 x-9)))
    (y z-3) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (3 1)) ((1 2) (0 1)) ((2 1) (1 1))
    ((3 0) (5 0)) ((3 2) (2 0)) ((4 1) (3 1)) ((5 1) (4 0)))
  (ind-zero-in (z-3 (exp h2-1 (mul x-7 z-2 x-9)))
    (z (exp h2-1 (mul x-7 z-2 x-9)))
    (z-0 (exp h2-1 (mul x-7 z-2 x-9 z-3 (rec x-10))))
    (x-8 (exp h2-1 (mul x-7 z-2 x-9 z-3 (rec x-10))))
    (z-1 (exp h2-0 (mul x-1 x-5))) (x-6 (exp h2-0 (mul x-1 x-5))))
  (non-orig (privk a) (privk b))
  (uniq-gen z z-0 z-1 x-4 x-5 x-6 x-7 x-8 z-3)
  (operation nonce-test (added-strand resp 2)
    (exp h2-1 (mul x-7 z-2 x-9 z-3 (rec x-10))) (4 0) (exp (gen) x-7))
  (traces
    ((send (enc (exp (gen) x-4) a (pubk b)))
      (recv (enc h2 (exp h2 x-4) h3 b (pubk a)))
      (send (enc (exp h3 x-4) (pubk b))))
    ((send (enc (exp (gen) x-5) a-0 (pubk b-0)))
      (recv
        (enc (exp h2-0 x-0) (exp h2-0 (mul x-0 x-5))
          (exp h2 (mul x x-4 (rec x-5))) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x x-4)) (pubk b-0))))
    ((recv (enc (exp h2-0 (mul x-1 x-5)) a-1 (pubk b-1)))
      (send
        (enc (exp (gen) z-1) (exp h2-0 (mul x-1 z-1 x-5))
          (exp (gen) x-6) b-1 (pubk a-1))))
    ((send (enc (exp (gen) x-7) a-2 (pubk b-2)))
      (recv
        (enc (exp h2-1 (mul x-3 z-4)) (exp h2-1 (mul x-3 x-7 z-4))
          (exp h2-0 (mul x-1 x-2 x-5 (rec x-7))) b-2 (pubk a-2)))
      (send (enc (exp h2-0 (mul x-1 x-2 x-5)) (pubk b-2))))
    ((recv
       (enc (exp h2-1 (mul x-7 z-2 x-9 z-3 (rec x-10))) a-3 (pubk b-3)))
      (send
        (enc (exp (gen) z-0)
          (exp h2-1 (mul z-0 x-7 z-2 x-9 z-3 (rec x-10)))
          (exp (gen) x-8) b-3 (pubk a-3))))
    ((recv (enc (exp h2-1 (mul x-7 z-2 x-9)) a-4 (pubk b-4)))
      (send
        (enc (exp (gen) z-3) (exp h2-1 (mul x-7 z-2 x-9 z-3))
          (exp (gen) z) b-4 (pubk a-4)))))
  (label 64)
  (parent 25)
  (unrealized (1 1) (3 1) (5 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 b-1 a-1 a-2 b-2 b-3 a-3 name) (h2 h3 h2-0 base)
    (x x-0 x-1 x-2 z z-0 x-3 x-4 x-5 x-6 x-7 x-8 x-9 z-1 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-5))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp h2-0 x-0))
    (h3 (exp h2 (mul x (rec x-4) x-5))) (x x-4))
  (defstrand resp 2 (b b-1) (a a-1) (h1 (exp h2-0 (mul x-1 x-4)))
    (y x-6) (z x-3))
  (defstrand init 3 (a a-2) (b b-2)
    (h2 (exp (gen) (mul (rec x-7) x-9 z-1)))
    (h3 (exp h2-0 (mul x-1 x-2 (rec z-0) x-4))) (x z-0))
  (defstrand resp 2 (b b-3) (a a-3) (h1 (exp (gen) (mul z-0 z-1)))
    (y x-8) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (3 1)) ((1 2) (0 1)) ((2 1) (1 1))
    ((3 0) (4 0)) ((3 2) (2 0)) ((4 1) (3 1)))
  (ind-zero-in (x-8 (exp (gen) (mul z-0 z-1)))
    (z (exp (gen) (mul z-0 z-1))) (x-6 (exp h2-0 (mul x-1 x-4)))
    (x-3 (exp h2-0 (mul x-1 x-4))))
  (non-orig (privk a) (privk b))
  (uniq-gen z z-0 x-3 x-4 x-5 x-6 x-8)
  (operation nonce-test
    (algebra-contracted (h2-1 (exp (gen) (mul (rec x-7) z-1))))
    (exp (gen) (mul z-0 z-1)) (4 0) (exp (gen) z-0))
  (traces
    ((send (enc (exp (gen) x-5) a (pubk b)))
      (recv (enc h2 (exp h2 x-5) h3 b (pubk a)))
      (send (enc (exp h3 x-5) (pubk b))))
    ((send (enc (exp (gen) x-4) a-0 (pubk b-0)))
      (recv
        (enc (exp h2-0 x-0) (exp h2-0 (mul x-0 x-4))
          (exp h2 (mul x (rec x-4) x-5)) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x x-5)) (pubk b-0))))
    ((recv (enc (exp h2-0 (mul x-1 x-4)) a-1 (pubk b-1)))
      (send
        (enc (exp (gen) x-6) (exp h2-0 (mul x-1 x-4 x-6))
          (exp (gen) x-3) b-1 (pubk a-1))))
    ((send (enc (exp (gen) z-0) a-2 (pubk b-2)))
      (recv
        (enc (exp (gen) (mul (rec x-7) x-9 z-1))
          (exp (gen) (mul z-0 (rec x-7) x-9 z-1))
          (exp h2-0 (mul x-1 x-2 (rec z-0) x-4)) b-2 (pubk a-2)))
      (send (enc (exp h2-0 (mul x-1 x-2 x-4)) (pubk b-2))))
    ((recv (enc (exp (gen) (mul z-0 z-1)) a-3 (pubk b-3)))
      (send
        (enc (exp (gen) x-8) (exp (gen) (mul z-0 x-8 z-1)) (exp (gen) z)
          b-3 (pubk a-3)))))
  (label 65)
  (parent 25)
  (unrealized (1 1) (3 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 b-1 a-1 a-2 b-2 a-3 b-3 name)
    (h2 h3 h2-0 h2-1 base)
    (x x-0 x-1 z x-2 x-3 x-4 x-5 x-6 x-7 x-8 x-9 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-4))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp h2-0 x-0))
    (h3 (exp h2 (mul x (rec x-3) x-4))) (x x-3))
  (defstrand resp 2 (b b-1) (a a-1) (h1 (exp h2-0 (mul x-3 x-6)))
    (y x-5) (z x-2))
  (defstrand init 3 (a a-2) (b b-2) (h2 (exp (gen) x-1))
    (h3 (exp h2-0 (mul (rec z) x-3 x-6 x-7))) (x z))
  (defstrand init 3 (a a-3) (b b-3) (h2 h2-1)
    (h3 (exp h2-0 (mul (rec z) x-3 x-6 x-7 x-8 (rec x-9)))) (x x-9))
  (precedes ((0 0) (1 1)) ((1 0) (4 1)) ((1 2) (0 1)) ((2 1) (1 1))
    ((3 0) (4 1)) ((3 2) (2 0)) ((4 2) (3 1)))
  (ind-zero-in (x-5 (exp h2-0 (mul x-3 x-6)))
    (x-2 (exp h2-0 (mul x-3 x-6))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-2 x-3 x-4 x-5 x-9)
  (operation nonce-test (added-strand init 3)
    (exp h2-0 (mul (rec z) x-3 x-6 x-7)) (3 1))
  (traces
    ((send (enc (exp (gen) x-4) a (pubk b)))
      (recv (enc h2 (exp h2 x-4) h3 b (pubk a)))
      (send (enc (exp h3 x-4) (pubk b))))
    ((send (enc (exp (gen) x-3) a-0 (pubk b-0)))
      (recv
        (enc (exp h2-0 x-0) (exp h2-0 (mul x-0 x-3))
          (exp h2 (mul x (rec x-3) x-4)) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x x-4)) (pubk b-0))))
    ((recv (enc (exp h2-0 (mul x-3 x-6)) a-1 (pubk b-1)))
      (send
        (enc (exp (gen) x-5) (exp h2-0 (mul x-3 x-5 x-6))
          (exp (gen) x-2) b-1 (pubk a-1))))
    ((send (enc (exp (gen) z) a-2 (pubk b-2)))
      (recv
        (enc (exp (gen) x-1) (exp (gen) (mul x-1 z))
          (exp h2-0 (mul (rec z) x-3 x-6 x-7)) b-2 (pubk a-2)))
      (send (enc (exp h2-0 (mul x-3 x-6 x-7)) (pubk b-2))))
    ((send (enc (exp (gen) x-9) a-3 (pubk b-3)))
      (recv
        (enc h2-1 (exp h2-1 x-9)
          (exp h2-0 (mul (rec z) x-3 x-6 x-7 x-8 (rec x-9))) b-3
          (pubk a-3)))
      (send (enc (exp h2-0 (mul (rec z) x-3 x-6 x-7 x-8)) (pubk b-3)))))
  (label 66)
  (parent 26)
  (unrealized (1 1) (4 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 b-1 a-1 a-2 b-2 b-3 a-3 name) (h2 h3 h2-0 base)
    (x x-0 x-1 z z-0 x-2 x-3 x-4 x-5 x-6 x-7 x-8 x-9 x-10 x-11 x-12
      expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-4))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp h2-0 (mul x-0 x-10)))
    (h3 (exp h2 (mul x (rec x-3) x-4))) (x x-3))
  (defstrand resp 2 (b b-1) (a a-1)
    (h1 (exp h2-0 (mul x-3 x-6 x-7 x-8 x-9 (rec x-11) (rec x-12))))
    (y x-5) (z x-2))
  (defstrand init 3 (a a-2) (b b-2) (h2 (exp (gen) x-1))
    (h3 (exp h2-0 (mul (rec z-0) x-3 x-6 x-7 x-8 x-9 (rec x-11))))
    (x z-0))
  (defstrand resp 2 (b b-3) (a a-3)
    (h1 (exp h2-0 (mul (rec z-0) x-3 x-6 x-7 x-8))) (y x-9) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (4 0)) ((1 2) (0 1)) ((2 1) (1 1))
    ((3 0) (4 0)) ((3 2) (2 0)) ((4 1) (3 1)))
  (ind-zero-in (x-9 (exp h2-0 (mul (rec z-0) x-3 x-6 x-7 x-8)))
    (z (exp h2-0 (mul (rec z-0) x-3 x-6 x-7 x-8)))
    (x-5 (exp h2-0 (mul x-3 x-6 x-7 x-8 x-9 (rec x-11) (rec x-12))))
    (x-2 (exp h2-0 (mul x-3 x-6 x-7 x-8 x-9 (rec x-11) (rec x-12)))))
  (non-orig (privk a) (privk b))
  (uniq-gen z z-0 x-2 x-3 x-4 x-5 x-9)
  (operation nonce-test (added-strand resp 2)
    (exp h2-0 (mul (rec z-0) x-3 x-6 x-7 x-8 x-9 (rec x-11))) (3 1))
  (traces
    ((send (enc (exp (gen) x-4) a (pubk b)))
      (recv (enc h2 (exp h2 x-4) h3 b (pubk a)))
      (send (enc (exp h3 x-4) (pubk b))))
    ((send (enc (exp (gen) x-3) a-0 (pubk b-0)))
      (recv
        (enc (exp h2-0 (mul x-0 x-10)) (exp h2-0 (mul x-0 x-3 x-10))
          (exp h2 (mul x (rec x-3) x-4)) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x x-4)) (pubk b-0))))
    ((recv
       (enc (exp h2-0 (mul x-3 x-6 x-7 x-8 x-9 (rec x-11) (rec x-12)))
         a-1 (pubk b-1)))
      (send
        (enc (exp (gen) x-5)
          (exp h2-0 (mul x-3 x-5 x-6 x-7 x-8 x-9 (rec x-11) (rec x-12)))
          (exp (gen) x-2) b-1 (pubk a-1))))
    ((send (enc (exp (gen) z-0) a-2 (pubk b-2)))
      (recv
        (enc (exp (gen) x-1) (exp (gen) (mul x-1 z-0))
          (exp h2-0 (mul (rec z-0) x-3 x-6 x-7 x-8 x-9 (rec x-11))) b-2
          (pubk a-2)))
      (send
        (enc (exp h2-0 (mul x-3 x-6 x-7 x-8 x-9 (rec x-11)))
          (pubk b-2))))
    ((recv
       (enc (exp h2-0 (mul (rec z-0) x-3 x-6 x-7 x-8)) a-3 (pubk b-3)))
      (send
        (enc (exp (gen) x-9)
          (exp h2-0 (mul (rec z-0) x-3 x-6 x-7 x-8 x-9)) (exp (gen) z)
          b-3 (pubk a-3)))))
  (label 67)
  (parent 26)
  (unrealized (1 1) (4 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 b-1 a-1 a-2 b-2 name) (h2 h3 base)
    (x x-0 z x-1 x-2 x-3 x-4 x-5 x-6 x-7 x-8 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-1))
  (defstrand init 3 (a a-0) (b b-0)
    (h2 (exp (gen) (mul x-0 (rec x-2) (rec x-4) (rec x-5) x-6 x-6 x-8)))
    (h3 (exp h2 (mul x x-1 (rec x-2)))) (x x-2))
  (defstrand resp 2 (b b-1) (a a-1)
    (h1 (exp (gen) (mul (rec x-5) x-6 x-6 x-8))) (y z) (z x-3))
  (defstrand init 3 (a a-2) (b b-2) (h2 (exp (gen) x-7))
    (h3 (exp (gen) (mul x-6 x-8))) (x x-6))
  (precedes ((0 0) (1 1)) ((1 0) (3 1)) ((1 2) (0 1)) ((2 1) (1 1))
    ((3 2) (2 0)))
  (ind-zero-in (z (exp (gen) (mul (rec x-5) x-6 x-6 x-8)))
    (x-3 (exp (gen) (mul (rec x-5) x-6 x-6 x-8))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-1 x-2 x-3 x-6)
  (operation nonce-test
    (algebra-contracted
      (h2-0
        (exp (gen) (mul (rec x-2) (rec x-4) (rec x-5) x-6 x-6 x-8))))
    (exp (gen) (mul x-6 x-8)) (3 1))
  (traces
    ((send (enc (exp (gen) x-1) a (pubk b)))
      (recv (enc h2 (exp h2 x-1) h3 b (pubk a)))
      (send (enc (exp h3 x-1) (pubk b))))
    ((send (enc (exp (gen) x-2) a-0 (pubk b-0)))
      (recv
        (enc
          (exp (gen)
            (mul x-0 (rec x-2) (rec x-4) (rec x-5) x-6 x-6 x-8))
          (exp (gen) (mul x-0 (rec x-4) (rec x-5) x-6 x-6 x-8))
          (exp h2 (mul x x-1 (rec x-2))) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x x-1)) (pubk b-0))))
    ((recv (enc (exp (gen) (mul (rec x-5) x-6 x-6 x-8)) a-1 (pubk b-1)))
      (send
        (enc (exp (gen) z) (exp (gen) (mul z (rec x-5) x-6 x-6 x-8))
          (exp (gen) x-3) b-1 (pubk a-1))))
    ((send (enc (exp (gen) x-6) a-2 (pubk b-2)))
      (recv
        (enc (exp (gen) x-7) (exp (gen) (mul x-6 x-7))
          (exp (gen) (mul x-6 x-8)) b-2 (pubk a-2)))
      (send (enc (exp (gen) (mul x-6 x-6 x-8)) (pubk b-2)))))
  (label 68)
  (parent 26)
  (unrealized (1 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 b-1 a-1 b-2 a-2 a-3 b-3 a-4 b-4 name)
    (h2 h3 h2-0 h2-1 h2-2 base)
    (x x-0 x-1 x-2 x-3 x-4 x-5 z z-0 x-6 x-7 x-8 x-9 x-10 x-11 x-12
      expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-7))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp h2-0 (mul x-0 x-1)))
    (h3 (exp h2 (mul x x-7 (rec x-8)))) (x x-8))
  (defstrand resp 2 (b b-1) (a a-1)
    (h1 (exp h2-0 (mul (rec x-2) x-3 x-4 x-8 x-10))) (y x-6) (z x-9))
  (defstrand resp 2 (b b-2) (a a-2) (h1 (exp h2-0 (mul x-3 x-4 x-8)))
    (y x-10) (z z-0))
  (defstrand init 3 (a a-3) (b b-3) (h2 h2-1)
    (h3 (exp h2-0 (mul x-3 x-4 x-5 (rec z) x-8))) (x z))
  (defstrand init 3 (a a-4) (b b-4) (h2 h2-2)
    (h3 (exp h2-1 (mul z x-11 (rec x-12)))) (x x-12))
  (precedes ((0 0) (1 1)) ((1 0) (4 1)) ((1 2) (0 1)) ((2 1) (1 1))
    ((3 1) (2 0)) ((4 0) (5 1)) ((4 2) (3 0)) ((5 2) (4 1)))
  (ind-zero-in (x-10 (exp h2-0 (mul x-3 x-4 x-8)))
    (z-0 (exp h2-0 (mul x-3 x-4 x-8)))
    (x-6 (exp h2-0 (mul (rec x-2) x-3 x-4 x-8 x-10)))
    (x-9 (exp h2-0 (mul (rec x-2) x-3 x-4 x-8 x-10))))
  (non-orig (privk a) (privk b))
  (uniq-gen z z-0 x-6 x-7 x-8 x-9 x-10 x-12)
  (operation nonce-test (added-strand init 3) (exp h2-1 z) (4 1)
    (exp (gen) z))
  (traces
    ((send (enc (exp (gen) x-7) a (pubk b)))
      (recv (enc h2 (exp h2 x-7) h3 b (pubk a)))
      (send (enc (exp h3 x-7) (pubk b))))
    ((send (enc (exp (gen) x-8) a-0 (pubk b-0)))
      (recv
        (enc (exp h2-0 (mul x-0 x-1)) (exp h2-0 (mul x-0 x-1 x-8))
          (exp h2 (mul x x-7 (rec x-8))) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x x-7)) (pubk b-0))))
    ((recv
       (enc (exp h2-0 (mul (rec x-2) x-3 x-4 x-8 x-10)) a-1 (pubk b-1)))
      (send
        (enc (exp (gen) x-6)
          (exp h2-0 (mul (rec x-2) x-3 x-4 x-6 x-8 x-10))
          (exp (gen) x-9) b-1 (pubk a-1))))
    ((recv (enc (exp h2-0 (mul x-3 x-4 x-8)) a-2 (pubk b-2)))
      (send
        (enc (exp (gen) x-10) (exp h2-0 (mul x-3 x-4 x-8 x-10))
          (exp (gen) z-0) b-2 (pubk a-2))))
    ((send (enc (exp (gen) z) a-3 (pubk b-3)))
      (recv
        (enc h2-1 (exp h2-1 z) (exp h2-0 (mul x-3 x-4 x-5 (rec z) x-8))
          b-3 (pubk a-3)))
      (send (enc (exp h2-0 (mul x-3 x-4 x-5 x-8)) (pubk b-3))))
    ((send (enc (exp (gen) x-12) a-4 (pubk b-4)))
      (recv
        (enc h2-2 (exp h2-2 x-12) (exp h2-1 (mul z x-11 (rec x-12))) b-4
          (pubk a-4))) (send (enc (exp h2-1 (mul z x-11)) (pubk b-4)))))
  (label 69)
  (parent 27)
  (unrealized (1 1) (4 1) (5 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 b-1 a-1 b-2 a-2 a-3 b-3 b-4 a-4 name)
    (h2 h3 h2-0 h2-1 base)
    (x x-0 x-1 x-2 x-3 x-4 x-5 z z-0 z-1 x-6 x-7 x-8 x-9 x-10 x-11 x-12
      x-13 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-7))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp h2-0 (mul x-0 x-1)))
    (h3 (exp h2 (mul x x-7 (rec x-8)))) (x x-8))
  (defstrand resp 2 (b b-1) (a a-1)
    (h1 (exp h2-0 (mul (rec x-2) x-3 x-4 x-8 x-10))) (y x-6) (z x-9))
  (defstrand resp 2 (b b-2) (a a-2) (h1 (exp h2-0 (mul x-3 x-4 x-8)))
    (y x-10) (z z-1))
  (defstrand init 3 (a a-3) (b b-3) (h2 (exp h2-1 x-13))
    (h3 (exp h2-0 (mul x-3 x-4 x-5 (rec z-0) x-8))) (x z-0))
  (defstrand resp 2 (b b-4) (a a-4) (h1 (exp h2-1 (mul z-0 x-11)))
    (y x-12) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (4 1)) ((1 2) (0 1)) ((2 1) (1 1))
    ((3 1) (2 0)) ((4 0) (5 0)) ((4 2) (3 0)) ((5 1) (4 1)))
  (ind-zero-in (x-12 (exp h2-1 (mul z-0 x-11)))
    (z (exp h2-1 (mul z-0 x-11))) (x-10 (exp h2-0 (mul x-3 x-4 x-8)))
    (z-1 (exp h2-0 (mul x-3 x-4 x-8)))
    (x-6 (exp h2-0 (mul (rec x-2) x-3 x-4 x-8 x-10)))
    (x-9 (exp h2-0 (mul (rec x-2) x-3 x-4 x-8 x-10))))
  (non-orig (privk a) (privk b))
  (uniq-gen z z-0 z-1 x-6 x-7 x-8 x-9 x-10 x-12)
  (operation nonce-test (added-strand resp 2) (exp h2-1 (mul z-0 x-13))
    (4 1) (exp (gen) z-0))
  (traces
    ((send (enc (exp (gen) x-7) a (pubk b)))
      (recv (enc h2 (exp h2 x-7) h3 b (pubk a)))
      (send (enc (exp h3 x-7) (pubk b))))
    ((send (enc (exp (gen) x-8) a-0 (pubk b-0)))
      (recv
        (enc (exp h2-0 (mul x-0 x-1)) (exp h2-0 (mul x-0 x-1 x-8))
          (exp h2 (mul x x-7 (rec x-8))) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x x-7)) (pubk b-0))))
    ((recv
       (enc (exp h2-0 (mul (rec x-2) x-3 x-4 x-8 x-10)) a-1 (pubk b-1)))
      (send
        (enc (exp (gen) x-6)
          (exp h2-0 (mul (rec x-2) x-3 x-4 x-6 x-8 x-10))
          (exp (gen) x-9) b-1 (pubk a-1))))
    ((recv (enc (exp h2-0 (mul x-3 x-4 x-8)) a-2 (pubk b-2)))
      (send
        (enc (exp (gen) x-10) (exp h2-0 (mul x-3 x-4 x-8 x-10))
          (exp (gen) z-1) b-2 (pubk a-2))))
    ((send (enc (exp (gen) z-0) a-3 (pubk b-3)))
      (recv
        (enc (exp h2-1 x-13) (exp h2-1 (mul z-0 x-13))
          (exp h2-0 (mul x-3 x-4 x-5 (rec z-0) x-8)) b-3 (pubk a-3)))
      (send (enc (exp h2-0 (mul x-3 x-4 x-5 x-8)) (pubk b-3))))
    ((recv (enc (exp h2-1 (mul z-0 x-11)) a-4 (pubk b-4)))
      (send
        (enc (exp (gen) x-12) (exp h2-1 (mul z-0 x-11 x-12))
          (exp (gen) z) b-4 (pubk a-4)))))
  (label 70)
  (parent 27)
  (unrealized (1 1) (4 1) (5 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 b-1 a-1 b-2 a-2 a-3 b-3 name) (h2 h3 h2-0 base)
    (x x-0 x-1 x-2 z z-0 x-3 x-4 x-5 x-6 x-7 x-8 x-9 x-10 x-11 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-4))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp h2-0 (mul x-0 x-1)))
    (h3 (exp h2 (mul x (rec x-3) x-4))) (x x-3))
  (defstrand resp 2 (b b-1) (a a-1)
    (h1 (exp h2-0 (mul (rec x-2) z x-3 x-7 x-8))) (y x-5) (z z-0))
  (defstrand resp 2 (b b-2) (a a-2) (h1 (exp h2-0 (mul x-3 x-7 x-8)))
    (y z) (z x-6))
  (defstrand init 3 (a a-3) (b b-3) (h2 (exp (gen) x-11))
    (h3 (exp h2-0 (mul x-3 x-7 x-8 x-9 (rec x-10)))) (x x-10))
  (precedes ((0 0) (1 1)) ((1 0) (4 1)) ((1 2) (0 1)) ((2 1) (1 1))
    ((3 1) (2 0)) ((4 2) (3 0)))
  (ind-zero-in (z (exp h2-0 (mul x-3 x-7 x-8)))
    (x-6 (exp h2-0 (mul x-3 x-7 x-8)))
    (x-5 (exp h2-0 (mul (rec x-2) z x-3 x-7 x-8)))
    (z-0 (exp h2-0 (mul (rec x-2) z x-3 x-7 x-8))))
  (non-orig (privk a) (privk b))
  (uniq-gen z z-0 x-3 x-4 x-5 x-6 x-10)
  (operation nonce-test (algebra-contracted (h2-1 (exp (gen) x-11)))
    (exp (gen) (mul x-10 x-11)) (4 1) (exp (gen) x-10))
  (traces
    ((send (enc (exp (gen) x-4) a (pubk b)))
      (recv (enc h2 (exp h2 x-4) h3 b (pubk a)))
      (send (enc (exp h3 x-4) (pubk b))))
    ((send (enc (exp (gen) x-3) a-0 (pubk b-0)))
      (recv
        (enc (exp h2-0 (mul x-0 x-1)) (exp h2-0 (mul x-0 x-1 x-3))
          (exp h2 (mul x (rec x-3) x-4)) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x x-4)) (pubk b-0))))
    ((recv
       (enc (exp h2-0 (mul (rec x-2) z x-3 x-7 x-8)) a-1 (pubk b-1)))
      (send
        (enc (exp (gen) x-5)
          (exp h2-0 (mul (rec x-2) z x-3 x-5 x-7 x-8)) (exp (gen) z-0)
          b-1 (pubk a-1))))
    ((recv (enc (exp h2-0 (mul x-3 x-7 x-8)) a-2 (pubk b-2)))
      (send
        (enc (exp (gen) z) (exp h2-0 (mul z x-3 x-7 x-8))
          (exp (gen) x-6) b-2 (pubk a-2))))
    ((send (enc (exp (gen) x-10) a-3 (pubk b-3)))
      (recv
        (enc (exp (gen) x-11) (exp (gen) (mul x-10 x-11))
          (exp h2-0 (mul x-3 x-7 x-8 x-9 (rec x-10))) b-3 (pubk a-3)))
      (send (enc (exp h2-0 (mul x-3 x-7 x-8 x-9)) (pubk b-3)))))
  (label 71)
  (parent 27)
  (unrealized (1 1) (4 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 b-1 a-1 b-2 a-2 b-3 a-3 a-4 b-4 name)
    (h2 h3 h2-0 h2-1 base)
    (x x-0 x-1 x-2 x-3 x-4 z z-0 z-1 x-5 x-6 x-7 x-8 x-9 x-10 x-11 x-12
      x-13 x-14 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-5))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp h2-0 (mul x-0 x-1 x-3)))
    (h3 (exp h2 (mul x x-5 (rec x-6)))) (x x-6))
  (defstrand resp 2 (b b-1) (a a-1)
    (h1 (exp h2-0 (mul (rec x-2) (rec x-4) z x-6 x-8 x-10 x-11 x-12)))
    (y z-1) (z x-7))
  (defstrand resp 2 (b b-2) (a a-2)
    (h1 (exp h2-0 (mul (rec x-4) z x-6 x-10 x-11 x-12))) (y x-8)
    (z z-0))
  (defstrand resp 2 (b b-3) (a a-3)
    (h1 (exp h2-0 (mul x-6 x-10 x-11 x-12))) (y z) (z x-9))
  (defstrand init 3 (a a-4) (b b-4) (h2 h2-1)
    (h3 (exp h2-0 (mul x-6 x-10 x-11 x-12 x-13 (rec x-14)))) (x x-14))
  (precedes ((0 0) (1 1)) ((1 0) (5 1)) ((1 2) (0 1)) ((2 1) (1 1))
    ((3 1) (2 0)) ((4 1) (3 0)) ((5 2) (4 0)))
  (ind-zero-in (z (exp h2-0 (mul x-6 x-10 x-11 x-12)))
    (x-9 (exp h2-0 (mul x-6 x-10 x-11 x-12)))
    (x-8 (exp h2-0 (mul (rec x-4) z x-6 x-10 x-11 x-12)))
    (z-0 (exp h2-0 (mul (rec x-4) z x-6 x-10 x-11 x-12)))
    (z-1 (exp h2-0 (mul (rec x-2) (rec x-4) z x-6 x-8 x-10 x-11 x-12)))
    (x-7 (exp h2-0 (mul (rec x-2) (rec x-4) z x-6 x-8 x-10 x-11 x-12))))
  (non-orig (privk a) (privk b))
  (uniq-gen z z-0 z-1 x-5 x-6 x-7 x-8 x-9 x-14)
  (operation nonce-test (added-strand init 3)
    (exp h2-0 (mul x-6 x-10 x-11 x-12)) (4 0) (exp (gen) x-6))
  (traces
    ((send (enc (exp (gen) x-5) a (pubk b)))
      (recv (enc h2 (exp h2 x-5) h3 b (pubk a)))
      (send (enc (exp h3 x-5) (pubk b))))
    ((send (enc (exp (gen) x-6) a-0 (pubk b-0)))
      (recv
        (enc (exp h2-0 (mul x-0 x-1 x-3))
          (exp h2-0 (mul x-0 x-1 x-3 x-6))
          (exp h2 (mul x x-5 (rec x-6))) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x x-5)) (pubk b-0))))
    ((recv
       (enc
         (exp h2-0 (mul (rec x-2) (rec x-4) z x-6 x-8 x-10 x-11 x-12))
         a-1 (pubk b-1)))
      (send
        (enc (exp (gen) z-1)
          (exp h2-0
            (mul (rec x-2) (rec x-4) z z-1 x-6 x-8 x-10 x-11 x-12))
          (exp (gen) x-7) b-1 (pubk a-1))))
    ((recv
       (enc (exp h2-0 (mul (rec x-4) z x-6 x-10 x-11 x-12)) a-2
         (pubk b-2)))
      (send
        (enc (exp (gen) x-8)
          (exp h2-0 (mul (rec x-4) z x-6 x-8 x-10 x-11 x-12))
          (exp (gen) z-0) b-2 (pubk a-2))))
    ((recv (enc (exp h2-0 (mul x-6 x-10 x-11 x-12)) a-3 (pubk b-3)))
      (send
        (enc (exp (gen) z) (exp h2-0 (mul z x-6 x-10 x-11 x-12))
          (exp (gen) x-9) b-3 (pubk a-3))))
    ((send (enc (exp (gen) x-14) a-4 (pubk b-4)))
      (recv
        (enc h2-1 (exp h2-1 x-14)
          (exp h2-0 (mul x-6 x-10 x-11 x-12 x-13 (rec x-14))) b-4
          (pubk a-4)))
      (send (enc (exp h2-0 (mul x-6 x-10 x-11 x-12 x-13)) (pubk b-4)))))
  (label 72)
  (parent 28)
  (unrealized (1 1) (5 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 b-1 a-1 b-2 a-2 b-3 a-3 b-4 a-4 name)
    (h2 h3 h2-0 base)
    (x x-0 x-1 x-2 x-3 x-4 z z-0 z-1 z-2 x-5 x-6 x-7 x-8 x-9 x-10 x-11
      x-12 x-13 x-14 x-15 x-16 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-5))
  (defstrand init 3 (a a-0) (b b-0)
    (h2 (exp h2-0 (mul x-0 x-1 x-3 x-15)))
    (h3 (exp h2 (mul x x-5 (rec x-6)))) (x x-6))
  (defstrand resp 2 (b b-1) (a a-1)
    (h1
      (exp h2-0
        (mul (rec x-2) (rec x-4) z-0 x-6 x-8 x-10 x-11 x-12 x-13 x-14
          (rec x-16)))) (y z-2) (z x-7))
  (defstrand resp 2 (b b-2) (a a-2)
    (h1
      (exp h2-0
        (mul (rec x-4) z-0 x-6 x-10 x-11 x-12 x-13 x-14 (rec x-16))))
    (y x-8) (z z-1))
  (defstrand resp 2 (b b-3) (a a-3)
    (h1 (exp h2-0 (mul x-6 x-10 x-11 x-12 x-13 x-14 (rec x-16))))
    (y z-0) (z x-9))
  (defstrand resp 2 (b b-4) (a a-4)
    (h1 (exp h2-0 (mul x-6 x-10 x-11 x-12 x-13))) (y x-14) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (5 0)) ((1 2) (0 1)) ((2 1) (1 1))
    ((3 1) (2 0)) ((4 1) (3 0)) ((5 1) (4 0)))
  (ind-zero-in (x-14 (exp h2-0 (mul x-6 x-10 x-11 x-12 x-13)))
    (z (exp h2-0 (mul x-6 x-10 x-11 x-12 x-13)))
    (z-0 (exp h2-0 (mul x-6 x-10 x-11 x-12 x-13 x-14 (rec x-16))))
    (x-9 (exp h2-0 (mul x-6 x-10 x-11 x-12 x-13 x-14 (rec x-16))))
    (x-8
      (exp h2-0
        (mul (rec x-4) z-0 x-6 x-10 x-11 x-12 x-13 x-14 (rec x-16))))
    (z-1
      (exp h2-0
        (mul (rec x-4) z-0 x-6 x-10 x-11 x-12 x-13 x-14 (rec x-16))))
    (z-2
      (exp h2-0
        (mul (rec x-2) (rec x-4) z-0 x-6 x-8 x-10 x-11 x-12 x-13 x-14
          (rec x-16))))
    (x-7
      (exp h2-0
        (mul (rec x-2) (rec x-4) z-0 x-6 x-8 x-10 x-11 x-12 x-13 x-14
          (rec x-16)))))
  (non-orig (privk a) (privk b))
  (uniq-gen z z-0 z-1 z-2 x-5 x-6 x-7 x-8 x-9 x-14)
  (operation nonce-test (added-strand resp 2)
    (exp h2-0 (mul x-6 x-10 x-11 x-12 x-13 x-14 (rec x-16))) (4 0)
    (exp (gen) x-6))
  (traces
    ((send (enc (exp (gen) x-5) a (pubk b)))
      (recv (enc h2 (exp h2 x-5) h3 b (pubk a)))
      (send (enc (exp h3 x-5) (pubk b))))
    ((send (enc (exp (gen) x-6) a-0 (pubk b-0)))
      (recv
        (enc (exp h2-0 (mul x-0 x-1 x-3 x-15))
          (exp h2-0 (mul x-0 x-1 x-3 x-6 x-15))
          (exp h2 (mul x x-5 (rec x-6))) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x x-5)) (pubk b-0))))
    ((recv
       (enc
         (exp h2-0
           (mul (rec x-2) (rec x-4) z-0 x-6 x-8 x-10 x-11 x-12 x-13 x-14
             (rec x-16))) a-1 (pubk b-1)))
      (send
        (enc (exp (gen) z-2)
          (exp h2-0
            (mul (rec x-2) (rec x-4) z-0 z-2 x-6 x-8 x-10 x-11 x-12 x-13
              x-14 (rec x-16))) (exp (gen) x-7) b-1 (pubk a-1))))
    ((recv
       (enc
         (exp h2-0
           (mul (rec x-4) z-0 x-6 x-10 x-11 x-12 x-13 x-14 (rec x-16)))
         a-2 (pubk b-2)))
      (send
        (enc (exp (gen) x-8)
          (exp h2-0
            (mul (rec x-4) z-0 x-6 x-8 x-10 x-11 x-12 x-13 x-14
              (rec x-16))) (exp (gen) z-1) b-2 (pubk a-2))))
    ((recv
       (enc (exp h2-0 (mul x-6 x-10 x-11 x-12 x-13 x-14 (rec x-16))) a-3
         (pubk b-3)))
      (send
        (enc (exp (gen) z-0)
          (exp h2-0 (mul z-0 x-6 x-10 x-11 x-12 x-13 x-14 (rec x-16)))
          (exp (gen) x-9) b-3 (pubk a-3))))
    ((recv
       (enc (exp h2-0 (mul x-6 x-10 x-11 x-12 x-13)) a-4 (pubk b-4)))
      (send
        (enc (exp (gen) x-14)
          (exp h2-0 (mul x-6 x-10 x-11 x-12 x-13 x-14)) (exp (gen) z)
          b-4 (pubk a-4)))))
  (label 73)
  (parent 28)
  (unrealized (1 1) (5 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 b-1 a-1 b-2 a-2 b-3 a-3 name) (h2 h3 base)
    (x x-0 x-1 x-2 z z-0 z-1 x-3 x-4 x-5 x-6 x-7 x-8 x-9 x-10 x-11 x-12
      x-13 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-4))
  (defstrand init 3 (a a-0) (b b-0)
    (h2
      (exp (gen) (mul x-0 x-1 (rec x-7) (rec x-8) (rec x-9) x-11 x-13)))
    (h3 (exp h2 (mul x (rec x-3) x-4))) (x x-3))
  (defstrand resp 2 (b b-1) (a a-1)
    (h1 (exp (gen) (mul (rec x-2) z-0 x-3 x-10 (rec x-12) x-13)))
    (y x-5) (z z-1))
  (defstrand resp 2 (b b-2) (a a-2)
    (h1 (exp (gen) (mul x-3 x-10 (rec x-12) x-13))) (y z-0) (z x-6))
  (defstrand resp 2 (b b-3) (a a-3) (h1 (exp (gen) (mul x-3 x-13)))
    (y x-10) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (4 0)) ((1 2) (0 1)) ((2 1) (1 1))
    ((3 1) (2 0)) ((4 1) (3 0)))
  (ind-zero-in (x-10 (exp (gen) (mul x-3 x-13)))
    (z (exp (gen) (mul x-3 x-13)))
    (z-0 (exp (gen) (mul x-3 x-10 (rec x-12) x-13)))
    (x-6 (exp (gen) (mul x-3 x-10 (rec x-12) x-13)))
    (x-5 (exp (gen) (mul (rec x-2) z-0 x-3 x-10 (rec x-12) x-13)))
    (z-1 (exp (gen) (mul (rec x-2) z-0 x-3 x-10 (rec x-12) x-13))))
  (non-orig (privk a) (privk b))
  (uniq-gen z z-0 z-1 x-3 x-4 x-5 x-6 x-10)
  (operation nonce-test
    (algebra-contracted
      (h2-0 (exp (gen) (mul (rec x-7) (rec x-8) (rec x-9) x-13))))
    (exp (gen) (mul x-3 x-13)) (4 0) (exp (gen) x-3))
  (traces
    ((send (enc (exp (gen) x-4) a (pubk b)))
      (recv (enc h2 (exp h2 x-4) h3 b (pubk a)))
      (send (enc (exp h3 x-4) (pubk b))))
    ((send (enc (exp (gen) x-3) a-0 (pubk b-0)))
      (recv
        (enc
          (exp (gen)
            (mul x-0 x-1 (rec x-7) (rec x-8) (rec x-9) x-11 x-13))
          (exp (gen)
            (mul x-0 x-1 x-3 (rec x-7) (rec x-8) (rec x-9) x-11 x-13))
          (exp h2 (mul x (rec x-3) x-4)) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x x-4)) (pubk b-0))))
    ((recv
       (enc (exp (gen) (mul (rec x-2) z-0 x-3 x-10 (rec x-12) x-13)) a-1
         (pubk b-1)))
      (send
        (enc (exp (gen) x-5)
          (exp (gen) (mul (rec x-2) z-0 x-3 x-5 x-10 (rec x-12) x-13))
          (exp (gen) z-1) b-1 (pubk a-1))))
    ((recv
       (enc (exp (gen) (mul x-3 x-10 (rec x-12) x-13)) a-2 (pubk b-2)))
      (send
        (enc (exp (gen) z-0)
          (exp (gen) (mul z-0 x-3 x-10 (rec x-12) x-13)) (exp (gen) x-6)
          b-2 (pubk a-2))))
    ((recv (enc (exp (gen) (mul x-3 x-13)) a-3 (pubk b-3)))
      (send
        (enc (exp (gen) x-10) (exp (gen) (mul x-3 x-10 x-13))
          (exp (gen) z) b-3 (pubk a-3)))))
  (label 74)
  (parent 28)
  (unrealized (1 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 b-1 a-1 b-2 a-2 name) (h3 base)
    (x x-0 z z-0 x-1 x-2 x-3 x-4 x-5 x-6 x-7 x-8 x-9 x-10 expn))
  (defstrand init 3 (a a) (b b)
    (h2 (exp (gen) (mul (rec x) (rec x-1) x-2 x-6 x-10))) (h3 h3)
    (x x-1))
  (defstrand init 3 (a a-0) (b b-0)
    (h2 (exp (gen) (mul x-0 (rec x-4) (rec x-5) x-7 x-9)))
    (h3 (exp (gen) (mul x-6 x-10))) (x x-2))
  (defstrand resp 2 (b b-1) (a a-1)
    (h1 (exp (gen) (mul x-2 x-6 (rec x-8) x-9))) (y z-0) (z x-3))
  (defstrand resp 2 (b b-2) (a a-2) (h1 (exp (gen) (mul x-2 x-9)))
    (y x-6) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (3 0)) ((1 2) (0 1)) ((2 1) (1 1))
    ((3 1) (2 0)))
  (ind-zero-in (x-6 (exp (gen) (mul x-2 x-9)))
    (z (exp (gen) (mul x-2 x-9)))
    (z-0 (exp (gen) (mul x-2 x-6 (rec x-8) x-9)))
    (x-3 (exp (gen) (mul x-2 x-6 (rec x-8) x-9))))
  (non-orig (privk a) (privk b))
  (uniq-gen z z-0 x-1 x-2 x-3 x-6)
  (operation nonce-test
    (algebra-contracted
      (h2 (exp (gen) (mul (rec x) (rec x-1) x-2 x-6 x-10))))
    (exp (gen) (mul x-6 x-10)) (1 1))
  (traces
    ((send (enc (exp (gen) x-1) a (pubk b)))
      (recv
        (enc (exp (gen) (mul (rec x) (rec x-1) x-2 x-6 x-10))
          (exp (gen) (mul (rec x) x-2 x-6 x-10)) h3 b (pubk a)))
      (send (enc (exp h3 x-1) (pubk b))))
    ((send (enc (exp (gen) x-2) a-0 (pubk b-0)))
      (recv
        (enc (exp (gen) (mul x-0 (rec x-4) (rec x-5) x-7 x-9))
          (exp (gen) (mul x-0 x-2 (rec x-4) (rec x-5) x-7 x-9))
          (exp (gen) (mul x-6 x-10)) b-0 (pubk a-0)))
      (send (enc (exp (gen) (mul x-2 x-6 x-10)) (pubk b-0))))
    ((recv (enc (exp (gen) (mul x-2 x-6 (rec x-8) x-9)) a-1 (pubk b-1)))
      (send
        (enc (exp (gen) z-0) (exp (gen) (mul z-0 x-2 x-6 (rec x-8) x-9))
          (exp (gen) x-3) b-1 (pubk a-1))))
    ((recv (enc (exp (gen) (mul x-2 x-9)) a-2 (pubk b-2)))
      (send
        (enc (exp (gen) x-6) (exp (gen) (mul x-2 x-6 x-9)) (exp (gen) z)
          b-2 (pubk a-2)))))
  (label 75)
  (parent 29)
  (seen 75)
  (unrealized (0 1))
  (comment "1 in cohort - 0 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 a-3 b-3 name)
    (h2 h3 h2-0 h2-1 h2-2 base)
    (x x-0 x-1 x-2 x-3 x-4 x-5 x-6 x-7 x-8 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-5))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp (gen) x))
    (h3 (exp h2 (mul x-0 (rec x-4) x-5))) (x x-4))
  (defstrand init 3 (a a-1) (b b-1) (h2 h2-0)
    (h3 (exp h2 (mul x-0 x-1 (rec x-4) x-5 (rec x-6)))) (x x-6))
  (defstrand init 3 (a a-2) (b b-2) (h2 h2-1)
    (h3 (exp h2-0 (mul x-2 (rec x-3) x-6))) (x x-3))
  (defstrand init 3 (a a-3) (b b-3) (h2 h2-2)
    (h3 (exp h2-1 (mul x-3 x-7 (rec x-8)))) (x x-8))
  (precedes ((0 0) (2 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (3 1))
    ((2 2) (1 1)) ((3 0) (4 1)) ((3 2) (2 1)) ((4 2) (3 1)))
  (non-orig (privk a) (privk b))
  (uniq-gen x-3 x-4 x-5 x-6 x-8)
  (operation nonce-test (added-strand init 3) (exp h2-1 x-3) (3 1)
    (exp (gen) x-3))
  (traces
    ((send (enc (exp (gen) x-5) a (pubk b)))
      (recv (enc h2 (exp h2 x-5) h3 b (pubk a)))
      (send (enc (exp h3 x-5) (pubk b))))
    ((send (enc (exp (gen) x-4) a-0 (pubk b-0)))
      (recv
        (enc (exp (gen) x) (exp (gen) (mul x x-4))
          (exp h2 (mul x-0 (rec x-4) x-5)) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x-0 x-5)) (pubk b-0))))
    ((send (enc (exp (gen) x-6) a-1 (pubk b-1)))
      (recv
        (enc h2-0 (exp h2-0 x-6)
          (exp h2 (mul x-0 x-1 (rec x-4) x-5 (rec x-6))) b-1
          (pubk a-1)))
      (send (enc (exp h2 (mul x-0 x-1 (rec x-4) x-5)) (pubk b-1))))
    ((send (enc (exp (gen) x-3) a-2 (pubk b-2)))
      (recv
        (enc h2-1 (exp h2-1 x-3) (exp h2-0 (mul x-2 (rec x-3) x-6)) b-2
          (pubk a-2))) (send (enc (exp h2-0 (mul x-2 x-6)) (pubk b-2))))
    ((send (enc (exp (gen) x-8) a-3 (pubk b-3)))
      (recv
        (enc h2-2 (exp h2-2 x-8) (exp h2-1 (mul x-3 x-7 (rec x-8))) b-3
          (pubk a-3)))
      (send (enc (exp h2-1 (mul x-3 x-7)) (pubk b-3)))))
  (label 76)
  (parent 31)
  (unrealized (2 1) (3 1) (4 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 b-3 a-3 name)
    (h2 h3 h2-0 h2-1 base)
    (x x-0 x-1 x-2 z x-3 x-4 x-5 x-6 x-7 x-8 x-9 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-5))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp (gen) x))
    (h3 (exp h2 (mul x-0 (rec x-4) x-5))) (x x-4))
  (defstrand init 3 (a a-1) (b b-1) (h2 h2-0)
    (h3 (exp h2 (mul x-0 x-1 (rec x-4) x-5 (rec x-6)))) (x x-6))
  (defstrand init 3 (a a-2) (b b-2) (h2 (exp h2-1 x-9))
    (h3 (exp h2-0 (mul x-2 (rec x-3) x-6))) (x x-3))
  (defstrand resp 2 (b b-3) (a a-3) (h1 (exp h2-1 (mul x-3 x-7)))
    (y x-8) (z z))
  (precedes ((0 0) (2 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (3 1))
    ((2 2) (1 1)) ((3 0) (4 0)) ((3 2) (2 1)) ((4 1) (3 1)))
  (ind-zero-in (x-8 (exp h2-1 (mul x-3 x-7)))
    (z (exp h2-1 (mul x-3 x-7))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-3 x-4 x-5 x-6 x-8)
  (operation nonce-test (added-strand resp 2) (exp h2-1 (mul x-3 x-9))
    (3 1) (exp (gen) x-3))
  (traces
    ((send (enc (exp (gen) x-5) a (pubk b)))
      (recv (enc h2 (exp h2 x-5) h3 b (pubk a)))
      (send (enc (exp h3 x-5) (pubk b))))
    ((send (enc (exp (gen) x-4) a-0 (pubk b-0)))
      (recv
        (enc (exp (gen) x) (exp (gen) (mul x x-4))
          (exp h2 (mul x-0 (rec x-4) x-5)) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x-0 x-5)) (pubk b-0))))
    ((send (enc (exp (gen) x-6) a-1 (pubk b-1)))
      (recv
        (enc h2-0 (exp h2-0 x-6)
          (exp h2 (mul x-0 x-1 (rec x-4) x-5 (rec x-6))) b-1
          (pubk a-1)))
      (send (enc (exp h2 (mul x-0 x-1 (rec x-4) x-5)) (pubk b-1))))
    ((send (enc (exp (gen) x-3) a-2 (pubk b-2)))
      (recv
        (enc (exp h2-1 x-9) (exp h2-1 (mul x-3 x-9))
          (exp h2-0 (mul x-2 (rec x-3) x-6)) b-2 (pubk a-2)))
      (send (enc (exp h2-0 (mul x-2 x-6)) (pubk b-2))))
    ((recv (enc (exp h2-1 (mul x-3 x-7)) a-3 (pubk b-3)))
      (send
        (enc (exp (gen) x-8) (exp h2-1 (mul x-3 x-7 x-8)) (exp (gen) z)
          b-3 (pubk a-3)))))
  (label 77)
  (parent 31)
  (unrealized (2 1) (3 1) (4 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 name) (h2 h3 h2-0 base)
    (x x-0 x-1 x-2 x-3 x-4 x-5 x-6 x-7 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-3))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp (gen) x))
    (h3 (exp h2 (mul x-0 x-3 (rec x-4)))) (x x-4))
  (defstrand init 3 (a a-1) (b b-1) (h2 h2-0)
    (h3 (exp h2 (mul x-0 x-1 (rec x-2) x-3 (rec x-4)))) (x x-2))
  (defstrand init 3 (a a-2) (b b-2) (h2 (exp (gen) x-7))
    (h3 (exp h2-0 (mul x-2 x-5 (rec x-6)))) (x x-6))
  (precedes ((0 0) (2 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (3 1))
    ((2 2) (1 1)) ((3 2) (2 1)))
  (non-orig (privk a) (privk b))
  (uniq-gen x-2 x-3 x-4 x-6)
  (operation nonce-test (algebra-contracted (h2-1 (exp (gen) x-7)))
    (exp (gen) (mul x-6 x-7)) (3 1) (exp (gen) x-6))
  (traces
    ((send (enc (exp (gen) x-3) a (pubk b)))
      (recv (enc h2 (exp h2 x-3) h3 b (pubk a)))
      (send (enc (exp h3 x-3) (pubk b))))
    ((send (enc (exp (gen) x-4) a-0 (pubk b-0)))
      (recv
        (enc (exp (gen) x) (exp (gen) (mul x x-4))
          (exp h2 (mul x-0 x-3 (rec x-4))) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x-0 x-3)) (pubk b-0))))
    ((send (enc (exp (gen) x-2) a-1 (pubk b-1)))
      (recv
        (enc h2-0 (exp h2-0 x-2)
          (exp h2 (mul x-0 x-1 (rec x-2) x-3 (rec x-4))) b-1
          (pubk a-1)))
      (send (enc (exp h2 (mul x-0 x-1 x-3 (rec x-4))) (pubk b-1))))
    ((send (enc (exp (gen) x-6) a-2 (pubk b-2)))
      (recv
        (enc (exp (gen) x-7) (exp (gen) (mul x-6 x-7))
          (exp h2-0 (mul x-2 x-5 (rec x-6))) b-2 (pubk a-2)))
      (send (enc (exp h2-0 (mul x-2 x-5)) (pubk b-2)))))
  (label 78)
  (parent 31)
  (unrealized (2 1) (3 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 b-2 a-2 a-3 b-3 name)
    (h2 h3 h2-0 h2-1 base)
    (x x-0 x-1 x-2 z x-3 x-4 x-5 x-6 x-7 x-8 x-9 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-4))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp (gen) x))
    (h3 (exp h2 (mul x-0 (rec x-3) x-4))) (x x-3))
  (defstrand init 3 (a a-1) (b b-1) (h2 (exp h2-0 x-2))
    (h3 (exp h2 (mul x-0 x-1 (rec x-3) x-4 (rec x-5)))) (x x-5))
  (defstrand resp 2 (b b-2) (a a-2) (h1 (exp h2-0 (mul x-5 x-7))) (y z)
    (z x-6))
  (defstrand init 3 (a a-3) (b b-3) (h2 h2-1)
    (h3 (exp h2-0 (mul x-5 x-7 x-8 (rec x-9)))) (x x-9))
  (precedes ((0 0) (2 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (4 1))
    ((2 2) (1 1)) ((3 1) (2 1)) ((4 2) (3 0)))
  (ind-zero-in (z (exp h2-0 (mul x-5 x-7)))
    (x-6 (exp h2-0 (mul x-5 x-7))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-3 x-4 x-5 x-6 x-9)
  (operation nonce-test (added-strand init 3) (exp h2-0 (mul x-5 x-7))
    (3 0) (exp (gen) x-5))
  (traces
    ((send (enc (exp (gen) x-4) a (pubk b)))
      (recv (enc h2 (exp h2 x-4) h3 b (pubk a)))
      (send (enc (exp h3 x-4) (pubk b))))
    ((send (enc (exp (gen) x-3) a-0 (pubk b-0)))
      (recv
        (enc (exp (gen) x) (exp (gen) (mul x x-3))
          (exp h2 (mul x-0 (rec x-3) x-4)) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x-0 x-4)) (pubk b-0))))
    ((send (enc (exp (gen) x-5) a-1 (pubk b-1)))
      (recv
        (enc (exp h2-0 x-2) (exp h2-0 (mul x-2 x-5))
          (exp h2 (mul x-0 x-1 (rec x-3) x-4 (rec x-5))) b-1
          (pubk a-1)))
      (send (enc (exp h2 (mul x-0 x-1 (rec x-3) x-4)) (pubk b-1))))
    ((recv (enc (exp h2-0 (mul x-5 x-7)) a-2 (pubk b-2)))
      (send
        (enc (exp (gen) z) (exp h2-0 (mul z x-5 x-7)) (exp (gen) x-6)
          b-2 (pubk a-2))))
    ((send (enc (exp (gen) x-9) a-3 (pubk b-3)))
      (recv
        (enc h2-1 (exp h2-1 x-9) (exp h2-0 (mul x-5 x-7 x-8 (rec x-9)))
          b-3 (pubk a-3)))
      (send (enc (exp h2-0 (mul x-5 x-7 x-8)) (pubk b-3)))))
  (label 79)
  (parent 32)
  (unrealized (2 1) (4 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 b-2 a-2 b-3 a-3 name) (h2 h3 h2-0 base)
    (x x-0 x-1 x-2 z z-0 x-3 x-4 x-5 x-6 x-7 x-8 x-9 x-10 x-11 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-4))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp (gen) x))
    (h3 (exp h2 (mul x-0 (rec x-3) x-4))) (x x-3))
  (defstrand init 3 (a a-1) (b b-1) (h2 (exp h2-0 (mul x-2 x-10)))
    (h3 (exp h2 (mul x-0 x-1 (rec x-3) x-4 (rec x-5)))) (x x-5))
  (defstrand resp 2 (b b-2) (a a-2)
    (h1 (exp h2-0 (mul x-5 x-7 x-8 x-9 (rec x-11)))) (y z-0) (z x-6))
  (defstrand resp 2 (b b-3) (a a-3) (h1 (exp h2-0 (mul x-5 x-7 x-8)))
    (y x-9) (z z))
  (precedes ((0 0) (2 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (4 0))
    ((2 2) (1 1)) ((3 1) (2 1)) ((4 1) (3 0)))
  (ind-zero-in (x-9 (exp h2-0 (mul x-5 x-7 x-8)))
    (z (exp h2-0 (mul x-5 x-7 x-8)))
    (z-0 (exp h2-0 (mul x-5 x-7 x-8 x-9 (rec x-11))))
    (x-6 (exp h2-0 (mul x-5 x-7 x-8 x-9 (rec x-11)))))
  (non-orig (privk a) (privk b))
  (uniq-gen z z-0 x-3 x-4 x-5 x-6 x-9)
  (operation nonce-test (added-strand resp 2)
    (exp h2-0 (mul x-5 x-7 x-8 x-9 (rec x-11))) (3 0) (exp (gen) x-5))
  (traces
    ((send (enc (exp (gen) x-4) a (pubk b)))
      (recv (enc h2 (exp h2 x-4) h3 b (pubk a)))
      (send (enc (exp h3 x-4) (pubk b))))
    ((send (enc (exp (gen) x-3) a-0 (pubk b-0)))
      (recv
        (enc (exp (gen) x) (exp (gen) (mul x x-3))
          (exp h2 (mul x-0 (rec x-3) x-4)) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x-0 x-4)) (pubk b-0))))
    ((send (enc (exp (gen) x-5) a-1 (pubk b-1)))
      (recv
        (enc (exp h2-0 (mul x-2 x-10)) (exp h2-0 (mul x-2 x-5 x-10))
          (exp h2 (mul x-0 x-1 (rec x-3) x-4 (rec x-5))) b-1
          (pubk a-1)))
      (send (enc (exp h2 (mul x-0 x-1 (rec x-3) x-4)) (pubk b-1))))
    ((recv
       (enc (exp h2-0 (mul x-5 x-7 x-8 x-9 (rec x-11))) a-2 (pubk b-2)))
      (send
        (enc (exp (gen) z-0)
          (exp h2-0 (mul z-0 x-5 x-7 x-8 x-9 (rec x-11)))
          (exp (gen) x-6) b-2 (pubk a-2))))
    ((recv (enc (exp h2-0 (mul x-5 x-7 x-8)) a-3 (pubk b-3)))
      (send
        (enc (exp (gen) x-9) (exp h2-0 (mul x-5 x-7 x-8 x-9))
          (exp (gen) z) b-3 (pubk a-3)))))
  (label 80)
  (parent 32)
  (unrealized (2 1) (4 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 b-2 a-2 name) (h2 h3 base)
    (x x-0 x-1 z x-2 x-3 x-4 x-5 x-6 x-7 x-8 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-3))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp (gen) x))
    (h3 (exp h2 (mul x-0 x-3 (rec x-4)))) (x x-4))
  (defstrand init 3 (a a-1) (b b-1)
    (h2 (exp (gen) (mul (rec x-5) x-7 x-8)))
    (h3 (exp h2 (mul x-0 x-1 (rec x-2) x-3 (rec x-4)))) (x x-2))
  (defstrand resp 2 (b b-2) (a a-2) (h1 (exp (gen) (mul x-2 x-8)))
    (y x-6) (z z))
  (precedes ((0 0) (2 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (3 0))
    ((2 2) (1 1)) ((3 1) (2 1)))
  (ind-zero-in (x-6 (exp (gen) (mul x-2 x-8)))
    (z (exp (gen) (mul x-2 x-8))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-2 x-3 x-4 x-6)
  (operation nonce-test
    (algebra-contracted (h2-0 (exp (gen) (mul (rec x-5) x-8))))
    (exp (gen) (mul x-2 x-8)) (3 0) (exp (gen) x-2))
  (traces
    ((send (enc (exp (gen) x-3) a (pubk b)))
      (recv (enc h2 (exp h2 x-3) h3 b (pubk a)))
      (send (enc (exp h3 x-3) (pubk b))))
    ((send (enc (exp (gen) x-4) a-0 (pubk b-0)))
      (recv
        (enc (exp (gen) x) (exp (gen) (mul x x-4))
          (exp h2 (mul x-0 x-3 (rec x-4))) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x-0 x-3)) (pubk b-0))))
    ((send (enc (exp (gen) x-2) a-1 (pubk b-1)))
      (recv
        (enc (exp (gen) (mul (rec x-5) x-7 x-8))
          (exp (gen) (mul x-2 (rec x-5) x-7 x-8))
          (exp h2 (mul x-0 x-1 (rec x-2) x-3 (rec x-4))) b-1
          (pubk a-1)))
      (send (enc (exp h2 (mul x-0 x-1 x-3 (rec x-4))) (pubk b-1))))
    ((recv (enc (exp (gen) (mul x-2 x-8)) a-2 (pubk b-2)))
      (send
        (enc (exp (gen) x-6) (exp (gen) (mul x-2 x-6 x-8)) (exp (gen) z)
          b-2 (pubk a-2)))))
  (label 81)
  (parent 32)
  (unrealized (2 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 name) (h2 h3 h2-0 base)
    (x x-0 x-1 x-2 x-3 x-4 x-5 x-6 x-7 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-2))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp (gen) x))
    (h3 (exp h2 (mul x-2 (rec x-3) x-4))) (x x-3))
  (defstrand init 3 (a a-1) (b b-1) (h2 (exp (gen) x-0))
    (h3 (exp h2 (mul (rec x-1) x-2 (rec x-3) x-4 x-5))) (x x-1))
  (defstrand init 3 (a a-2) (b b-2) (h2 h2-0)
    (h3
      (exp h2
        (mul (rec x-1) x-2 (rec x-3) x-4 x-5 (rec x-6) (rec x-7))))
    (x x-7))
  (precedes ((0 0) (3 1)) ((1 0) (3 1)) ((1 2) (0 1)) ((2 0) (3 1))
    ((2 2) (1 1)) ((3 2) (2 1)))
  (non-orig (privk a) (privk b))
  (uniq-gen x-1 x-2 x-3 x-7)
  (operation nonce-test (added-strand init 3)
    (exp h2 (mul (rec x-1) x-2 (rec x-3) x-4 x-5)) (2 1))
  (traces
    ((send (enc (exp (gen) x-2) a (pubk b)))
      (recv (enc h2 (exp h2 x-2) h3 b (pubk a)))
      (send (enc (exp h3 x-2) (pubk b))))
    ((send (enc (exp (gen) x-3) a-0 (pubk b-0)))
      (recv
        (enc (exp (gen) x) (exp (gen) (mul x x-3))
          (exp h2 (mul x-2 (rec x-3) x-4)) b-0 (pubk a-0)))
      (send (enc (exp h2 (mul x-2 x-4)) (pubk b-0))))
    ((send (enc (exp (gen) x-1) a-1 (pubk b-1)))
      (recv
        (enc (exp (gen) x-0) (exp (gen) (mul x-0 x-1))
          (exp h2 (mul (rec x-1) x-2 (rec x-3) x-4 x-5)) b-1
          (pubk a-1)))
      (send (enc (exp h2 (mul x-2 (rec x-3) x-4 x-5)) (pubk b-1))))
    ((send (enc (exp (gen) x-7) a-2 (pubk b-2)))
      (recv
        (enc h2-0 (exp h2-0 x-7)
          (exp h2
            (mul (rec x-1) x-2 (rec x-3) x-4 x-5 (rec x-6) (rec x-7)))
          b-2 (pubk a-2)))
      (send
        (enc (exp h2 (mul (rec x-1) x-2 (rec x-3) x-4 x-5 (rec x-6)))
          (pubk b-2)))))
  (label 82)
  (parent 33)
  (unrealized (3 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 b-2 a-2 name) (h2 h3 base)
    (x x-0 z x-1 y x-2 x-3 x-4 x-5 x-6 x-7 x-8 x-9 expn))
  (defstrand init 3 (a a) (b b) (h2 (exp h2 x-7)) (h3 h3) (x y))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp (gen) x))
    (h3 (exp h2 (mul y (rec x-2) x-3 x-4 (rec x-5) x-6 x-8 (rec x-9))))
    (x x-2))
  (defstrand init 3 (a a-1) (b b-1) (h2 (exp (gen) x-0))
    (h3 (exp h2 (mul (rec x-1) y (rec x-2) x-3 x-4 (rec x-5) x-6 x-8)))
    (x x-1))
  (defstrand resp 2 (b b-2) (a a-2)
    (h1 (exp h2 (mul (rec x-1) y (rec x-2) x-3 x-4 (rec x-5)))) (y x-6)
    (z z))
  (precedes ((0 0) (3 0)) ((1 0) (3 0)) ((1 2) (0 1)) ((2 0) (3 0))
    ((2 2) (1 1)) ((3 1) (2 1)))
  (ind-zero-in
    (x-6 (exp h2 (mul (rec x-1) y (rec x-2) x-3 x-4 (rec x-5))))
    (z (exp h2 (mul (rec x-1) y (rec x-2) x-3 x-4 (rec x-5)))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-1 y x-2 x-6)
  (operation nonce-test (added-strand resp 2)
    (exp h2 (mul (rec x-1) y (rec x-2) x-3 x-4 (rec x-5) x-6 x-8))
    (2 1))
  (traces
    ((send (enc (exp (gen) y) a (pubk b)))
      (recv (enc (exp h2 x-7) (exp h2 (mul y x-7)) h3 b (pubk a)))
      (send (enc (exp h3 y) (pubk b))))
    ((send (enc (exp (gen) x-2) a-0 (pubk b-0)))
      (recv
        (enc (exp (gen) x) (exp (gen) (mul x x-2))
          (exp h2 (mul y (rec x-2) x-3 x-4 (rec x-5) x-6 x-8 (rec x-9)))
          b-0 (pubk a-0)))
      (send
        (enc (exp h2 (mul y x-3 x-4 (rec x-5) x-6 x-8 (rec x-9)))
          (pubk b-0))))
    ((send (enc (exp (gen) x-1) a-1 (pubk b-1)))
      (recv
        (enc (exp (gen) x-0) (exp (gen) (mul x-0 x-1))
          (exp h2 (mul (rec x-1) y (rec x-2) x-3 x-4 (rec x-5) x-6 x-8))
          b-1 (pubk a-1)))
      (send
        (enc (exp h2 (mul y (rec x-2) x-3 x-4 (rec x-5) x-6 x-8))
          (pubk b-1))))
    ((recv
       (enc (exp h2 (mul (rec x-1) y (rec x-2) x-3 x-4 (rec x-5))) a-2
         (pubk b-2)))
      (send
        (enc (exp (gen) x-6)
          (exp h2 (mul (rec x-1) y (rec x-2) x-3 x-4 (rec x-5) x-6))
          (exp (gen) z) b-2 (pubk a-2)))))
  (label 83)
  (parent 33)
  (unrealized (0 1) (3 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 name) (h3 base)
    (x x-0 x-1 x-2 x-3 x-4 x-5 x-6 expn))
  (defstrand init 3 (a a) (b b)
    (h2 (exp (gen) (mul x-0 (rec x-1) (rec x-2) (rec x-3) x-4 x-4 x-6)))
    (h3 h3) (x x-1))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp (gen) x))
    (h3 (exp (gen) (mul (rec x-3) x-4 x-4 x-6))) (x x-0))
  (defstrand init 3 (a a-1) (b b-1) (h2 (exp (gen) x-5))
    (h3 (exp (gen) (mul x-4 x-6))) (x x-4))
  (precedes ((0 0) (2 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 2) (1 1)))
  (non-orig (privk a) (privk b))
  (uniq-gen x-0 x-1 x-4)
  (operation nonce-test
    (algebra-contracted
      (h2
        (exp (gen)
          (mul x-0 (rec x-1) (rec x-2) (rec x-3) x-4 x-4 x-6))))
    (exp (gen) (mul x-4 x-6)) (2 1))
  (traces
    ((send (enc (exp (gen) x-1) a (pubk b)))
      (recv
        (enc
          (exp (gen)
            (mul x-0 (rec x-1) (rec x-2) (rec x-3) x-4 x-4 x-6))
          (exp (gen) (mul x-0 (rec x-2) (rec x-3) x-4 x-4 x-6)) h3 b
          (pubk a))) (send (enc (exp h3 x-1) (pubk b))))
    ((send (enc (exp (gen) x-0) a-0 (pubk b-0)))
      (recv
        (enc (exp (gen) x) (exp (gen) (mul x x-0))
          (exp (gen) (mul (rec x-3) x-4 x-4 x-6)) b-0 (pubk a-0)))
      (send
        (enc (exp (gen) (mul x-0 (rec x-3) x-4 x-4 x-6)) (pubk b-0))))
    ((send (enc (exp (gen) x-4) a-1 (pubk b-1)))
      (recv
        (enc (exp (gen) x-5) (exp (gen) (mul x-4 x-5))
          (exp (gen) (mul x-4 x-6)) b-1 (pubk a-1)))
      (send (enc (exp (gen) (mul x-4 x-4 x-6)) (pubk b-1)))))
  (label 84)
  (parent 33)
  (seen 84)
  (unrealized (0 1))
  (comment "1 in cohort - 0 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 b-1 a-1 a-2 b-2 a-3 b-3 name)
    (h2 h3 h2-0 h2-1 base)
    (x x-0 x-1 x-2 x-3 x-4 z x-5 x-6 x-7 x-8 x-9 x-10 expn))
  (defstrand init 3 (a a) (b b) (h2 (exp h2 x-0)) (h3 h3) (x x-7))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp (gen) x))
    (h3 (exp h2 (mul (rec x-1) x-2 x-3 (rec x-6) x-7 x-8))) (x x-6))
  (defstrand resp 2 (b b-1) (a a-1)
    (h1 (exp h2 (mul x-2 x-3 (rec x-6) x-7))) (y x-8) (z x-5))
  (defstrand init 3 (a a-2) (b b-2) (h2 h2-0)
    (h3 (exp h2 (mul x-2 x-3 (rec x-4) (rec z) (rec x-6) x-7))) (x z))
  (defstrand init 3 (a a-3) (b b-3) (h2 h2-1)
    (h3 (exp h2-0 (mul z x-9 (rec x-10)))) (x x-10))
  (precedes ((0 0) (3 1)) ((1 0) (3 1)) ((1 2) (0 1)) ((2 1) (1 1))
    ((3 0) (4 1)) ((3 2) (2 0)) ((4 2) (3 1)))
  (ind-zero-in (x-8 (exp h2 (mul x-2 x-3 (rec x-6) x-7)))
    (x-5 (exp h2 (mul x-2 x-3 (rec x-6) x-7))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-5 x-6 x-7 x-8 x-10)
  (operation nonce-test (added-strand init 3) (exp h2-0 z) (3 1)
    (exp (gen) z))
  (traces
    ((send (enc (exp (gen) x-7) a (pubk b)))
      (recv (enc (exp h2 x-0) (exp h2 (mul x-0 x-7)) h3 b (pubk a)))
      (send (enc (exp h3 x-7) (pubk b))))
    ((send (enc (exp (gen) x-6) a-0 (pubk b-0)))
      (recv
        (enc (exp (gen) x) (exp (gen) (mul x x-6))
          (exp h2 (mul (rec x-1) x-2 x-3 (rec x-6) x-7 x-8)) b-0
          (pubk a-0)))
      (send (enc (exp h2 (mul (rec x-1) x-2 x-3 x-7 x-8)) (pubk b-0))))
    ((recv (enc (exp h2 (mul x-2 x-3 (rec x-6) x-7)) a-1 (pubk b-1)))
      (send
        (enc (exp (gen) x-8) (exp h2 (mul x-2 x-3 (rec x-6) x-7 x-8))
          (exp (gen) x-5) b-1 (pubk a-1))))
    ((send (enc (exp (gen) z) a-2 (pubk b-2)))
      (recv
        (enc h2-0 (exp h2-0 z)
          (exp h2 (mul x-2 x-3 (rec x-4) (rec z) (rec x-6) x-7)) b-2
          (pubk a-2)))
      (send
        (enc (exp h2 (mul x-2 x-3 (rec x-4) (rec x-6) x-7))
          (pubk b-2))))
    ((send (enc (exp (gen) x-10) a-3 (pubk b-3)))
      (recv
        (enc h2-1 (exp h2-1 x-10) (exp h2-0 (mul z x-9 (rec x-10))) b-3
          (pubk a-3))) (send (enc (exp h2-0 (mul z x-9)) (pubk b-3)))))
  (label 85)
  (parent 34)
  (unrealized (0 1) (3 1) (4 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 b-1 a-1 a-2 b-2 b-3 a-3 name) (h2 h3 h2-0 base)
    (x x-0 x-1 x-2 x-3 x-4 z z-0 x-5 x-6 x-7 x-8 x-9 x-10 x-11 expn))
  (defstrand init 3 (a a) (b b) (h2 (exp h2 x-0)) (h3 h3) (x x-7))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp (gen) x))
    (h3 (exp h2 (mul (rec x-1) x-2 x-3 (rec x-6) x-7 x-8))) (x x-6))
  (defstrand resp 2 (b b-1) (a a-1)
    (h1 (exp h2 (mul x-2 x-3 (rec x-6) x-7))) (y x-8) (z x-5))
  (defstrand init 3 (a a-2) (b b-2) (h2 (exp h2-0 x-11))
    (h3 (exp h2 (mul x-2 x-3 (rec x-4) (rec z-0) (rec x-6) x-7)))
    (x z-0))
  (defstrand resp 2 (b b-3) (a a-3) (h1 (exp h2-0 (mul z-0 x-9)))
    (y x-10) (z z))
  (precedes ((0 0) (3 1)) ((1 0) (3 1)) ((1 2) (0 1)) ((2 1) (1 1))
    ((3 0) (4 0)) ((3 2) (2 0)) ((4 1) (3 1)))
  (ind-zero-in (x-10 (exp h2-0 (mul z-0 x-9)))
    (z (exp h2-0 (mul z-0 x-9)))
    (x-8 (exp h2 (mul x-2 x-3 (rec x-6) x-7)))
    (x-5 (exp h2 (mul x-2 x-3 (rec x-6) x-7))))
  (non-orig (privk a) (privk b))
  (uniq-gen z z-0 x-5 x-6 x-7 x-8 x-10)
  (operation nonce-test (added-strand resp 2) (exp h2-0 (mul z-0 x-11))
    (3 1) (exp (gen) z-0))
  (traces
    ((send (enc (exp (gen) x-7) a (pubk b)))
      (recv (enc (exp h2 x-0) (exp h2 (mul x-0 x-7)) h3 b (pubk a)))
      (send (enc (exp h3 x-7) (pubk b))))
    ((send (enc (exp (gen) x-6) a-0 (pubk b-0)))
      (recv
        (enc (exp (gen) x) (exp (gen) (mul x x-6))
          (exp h2 (mul (rec x-1) x-2 x-3 (rec x-6) x-7 x-8)) b-0
          (pubk a-0)))
      (send (enc (exp h2 (mul (rec x-1) x-2 x-3 x-7 x-8)) (pubk b-0))))
    ((recv (enc (exp h2 (mul x-2 x-3 (rec x-6) x-7)) a-1 (pubk b-1)))
      (send
        (enc (exp (gen) x-8) (exp h2 (mul x-2 x-3 (rec x-6) x-7 x-8))
          (exp (gen) x-5) b-1 (pubk a-1))))
    ((send (enc (exp (gen) z-0) a-2 (pubk b-2)))
      (recv
        (enc (exp h2-0 x-11) (exp h2-0 (mul z-0 x-11))
          (exp h2 (mul x-2 x-3 (rec x-4) (rec z-0) (rec x-6) x-7)) b-2
          (pubk a-2)))
      (send
        (enc (exp h2 (mul x-2 x-3 (rec x-4) (rec x-6) x-7))
          (pubk b-2))))
    ((recv (enc (exp h2-0 (mul z-0 x-9)) a-3 (pubk b-3)))
      (send
        (enc (exp (gen) x-10) (exp h2-0 (mul z-0 x-9 x-10))
          (exp (gen) z) b-3 (pubk a-3)))))
  (label 86)
  (parent 34)
  (unrealized (0 1) (3 1) (4 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 b-1 a-1 a-2 b-2 name) (h2 h3 base)
    (x x-0 x-1 z x-2 x-3 x-4 x-5 x-6 x-7 x-8 x-9 expn))
  (defstrand init 3 (a a) (b b) (h2 (exp h2 x-0)) (h3 h3) (x x-2))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp (gen) x))
    (h3 (exp h2 (mul (rec x-1) z x-2 (rec x-3) x-5 x-6))) (x x-3))
  (defstrand resp 2 (b b-1) (a a-1)
    (h1 (exp h2 (mul x-2 (rec x-3) x-5 x-6))) (y z) (z x-4))
  (defstrand init 3 (a a-2) (b b-2) (h2 (exp (gen) x-9))
    (h3 (exp h2 (mul x-2 (rec x-3) x-5 x-6 (rec x-7) (rec x-8))))
    (x x-8))
  (precedes ((0 0) (3 1)) ((1 0) (3 1)) ((1 2) (0 1)) ((2 1) (1 1))
    ((3 2) (2 0)))
  (ind-zero-in (z (exp h2 (mul x-2 (rec x-3) x-5 x-6)))
    (x-4 (exp h2 (mul x-2 (rec x-3) x-5 x-6))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-2 x-3 x-4 x-8)
  (operation nonce-test (algebra-contracted (h2-0 (exp (gen) x-9)))
    (exp (gen) (mul x-8 x-9)) (3 1) (exp (gen) x-8))
  (traces
    ((send (enc (exp (gen) x-2) a (pubk b)))
      (recv (enc (exp h2 x-0) (exp h2 (mul x-0 x-2)) h3 b (pubk a)))
      (send (enc (exp h3 x-2) (pubk b))))
    ((send (enc (exp (gen) x-3) a-0 (pubk b-0)))
      (recv
        (enc (exp (gen) x) (exp (gen) (mul x x-3))
          (exp h2 (mul (rec x-1) z x-2 (rec x-3) x-5 x-6)) b-0
          (pubk a-0)))
      (send (enc (exp h2 (mul (rec x-1) z x-2 x-5 x-6)) (pubk b-0))))
    ((recv (enc (exp h2 (mul x-2 (rec x-3) x-5 x-6)) a-1 (pubk b-1)))
      (send
        (enc (exp (gen) z) (exp h2 (mul z x-2 (rec x-3) x-5 x-6))
          (exp (gen) x-4) b-1 (pubk a-1))))
    ((send (enc (exp (gen) x-8) a-2 (pubk b-2)))
      (recv
        (enc (exp (gen) x-9) (exp (gen) (mul x-8 x-9))
          (exp h2 (mul x-2 (rec x-3) x-5 x-6 (rec x-7) (rec x-8))) b-2
          (pubk a-2)))
      (send
        (enc (exp h2 (mul x-2 (rec x-3) x-5 x-6 (rec x-7)))
          (pubk b-2)))))
  (label 87)
  (parent 34)
  (unrealized (0 1) (3 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 b-1 a-1 b-2 a-2 a-3 b-3 name) (h2 h3 h2-0 base)
    (x x-0 x-1 x-2 x-3 z z-0 x-4 x-5 x-6 x-7 x-8 y x-9 x-10 x-11 expn))
  (defstrand init 3 (a a) (b b) (h2 (exp h2 (mul x-0 x-2))) (h3 h3)
    (x x-5))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp (gen) x))
    (h3
      (exp h2 (mul (rec x-1) x-3 z (rec x-4) x-5 x-6 x-8 y (rec x-9))))
    (x x-4))
  (defstrand resp 2 (b b-1) (a a-1)
    (h1 (exp h2 (mul x-3 z (rec x-4) x-5 x-8 y (rec x-9)))) (y x-6)
    (z z-0))
  (defstrand resp 2 (b b-2) (a a-2)
    (h1 (exp h2 (mul (rec x-4) x-5 x-8 y (rec x-9)))) (y z) (z x-7))
  (defstrand init 3 (a a-3) (b b-3) (h2 h2-0)
    (h3 (exp h2 (mul (rec x-4) x-5 x-8 y (rec x-9) x-10 (rec x-11))))
    (x x-11))
  (precedes ((0 0) (4 1)) ((1 0) (4 1)) ((1 2) (0 1)) ((2 1) (1 1))
    ((3 1) (2 0)) ((4 2) (3 0)))
  (ind-zero-in (z (exp h2 (mul (rec x-4) x-5 x-8 y (rec x-9))))
    (x-7 (exp h2 (mul (rec x-4) x-5 x-8 y (rec x-9))))
    (x-6 (exp h2 (mul x-3 z (rec x-4) x-5 x-8 y (rec x-9))))
    (z-0 (exp h2 (mul x-3 z (rec x-4) x-5 x-8 y (rec x-9)))))
  (non-orig (privk a) (privk b))
  (uniq-gen z z-0 x-4 x-5 x-6 x-7 x-11)
  (operation nonce-test (added-strand init 3)
    (exp h2 (mul (rec x-4) x-5 x-8 y (rec x-9))) (3 0))
  (traces
    ((send (enc (exp (gen) x-5) a (pubk b)))
      (recv
        (enc (exp h2 (mul x-0 x-2)) (exp h2 (mul x-0 x-2 x-5)) h3 b
          (pubk a))) (send (enc (exp h3 x-5) (pubk b))))
    ((send (enc (exp (gen) x-4) a-0 (pubk b-0)))
      (recv
        (enc (exp (gen) x) (exp (gen) (mul x x-4))
          (exp h2
            (mul (rec x-1) x-3 z (rec x-4) x-5 x-6 x-8 y (rec x-9))) b-0
          (pubk a-0)))
      (send
        (enc (exp h2 (mul (rec x-1) x-3 z x-5 x-6 x-8 y (rec x-9)))
          (pubk b-0))))
    ((recv
       (enc (exp h2 (mul x-3 z (rec x-4) x-5 x-8 y (rec x-9))) a-1
         (pubk b-1)))
      (send
        (enc (exp (gen) x-6)
          (exp h2 (mul x-3 z (rec x-4) x-5 x-6 x-8 y (rec x-9)))
          (exp (gen) z-0) b-1 (pubk a-1))))
    ((recv
       (enc (exp h2 (mul (rec x-4) x-5 x-8 y (rec x-9))) a-2
         (pubk b-2)))
      (send
        (enc (exp (gen) z)
          (exp h2 (mul z (rec x-4) x-5 x-8 y (rec x-9))) (exp (gen) x-7)
          b-2 (pubk a-2))))
    ((send (enc (exp (gen) x-11) a-3 (pubk b-3)))
      (recv
        (enc h2-0 (exp h2-0 x-11)
          (exp h2 (mul (rec x-4) x-5 x-8 y (rec x-9) x-10 (rec x-11)))
          b-3 (pubk a-3)))
      (send
        (enc (exp h2 (mul (rec x-4) x-5 x-8 y (rec x-9) x-10))
          (pubk b-3)))))
  (label 88)
  (parent 35)
  (unrealized (0 1) (4 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 b-1 a-1 b-2 a-2 b-3 a-3 name) (h2 h3 base)
    (x x-0 x-1 x-2 x-3 z z-0 z-1 x-4 y x-5 x-6 x-7 y-0 x-8 x-9 x-10 x-11
      y-1 expn))
  (defstrand init 3 (a a) (b b) (h2 (exp h2 (mul x-0 x-2 x-11))) (h3 h3)
    (x y))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp (gen) x))
    (h3
      (exp h2
        (mul (rec x-1) x-3 z-0 (rec x-4) y x-5 x-7 y-0 (rec x-8) x-9
          x-10 (rec y-1)))) (x x-4))
  (defstrand resp 2 (b b-1) (a a-1)
    (h1
      (exp h2
        (mul x-3 z-0 (rec x-4) y x-7 y-0 (rec x-8) x-9 x-10 (rec y-1))))
    (y x-5) (z z-1))
  (defstrand resp 2 (b b-2) (a a-2)
    (h1 (exp h2 (mul (rec x-4) y x-7 y-0 (rec x-8) x-9 x-10 (rec y-1))))
    (y z-0) (z x-6))
  (defstrand resp 2 (b b-3) (a a-3)
    (h1 (exp h2 (mul (rec x-4) y x-7 y-0 (rec x-8) x-9))) (y x-10)
    (z z))
  (precedes ((0 0) (4 0)) ((1 0) (4 0)) ((1 2) (0 1)) ((2 1) (1 1))
    ((3 1) (2 0)) ((4 1) (3 0)))
  (ind-zero-in (x-10 (exp h2 (mul (rec x-4) y x-7 y-0 (rec x-8) x-9)))
    (z (exp h2 (mul (rec x-4) y x-7 y-0 (rec x-8) x-9)))
    (z-0
      (exp h2 (mul (rec x-4) y x-7 y-0 (rec x-8) x-9 x-10 (rec y-1))))
    (x-6
      (exp h2 (mul (rec x-4) y x-7 y-0 (rec x-8) x-9 x-10 (rec y-1))))
    (x-5
      (exp h2
        (mul x-3 z-0 (rec x-4) y x-7 y-0 (rec x-8) x-9 x-10 (rec y-1))))
    (z-1
      (exp h2
        (mul x-3 z-0 (rec x-4) y x-7 y-0 (rec x-8) x-9 x-10
          (rec y-1)))))
  (non-orig (privk a) (privk b))
  (uniq-gen z z-0 z-1 x-4 y x-5 x-6 x-10)
  (operation nonce-test (added-strand resp 2)
    (exp h2 (mul (rec x-4) y x-7 y-0 (rec x-8) x-9 x-10 (rec y-1)))
    (3 0))
  (traces
    ((send (enc (exp (gen) y) a (pubk b)))
      (recv
        (enc (exp h2 (mul x-0 x-2 x-11)) (exp h2 (mul x-0 x-2 y x-11))
          h3 b (pubk a))) (send (enc (exp h3 y) (pubk b))))
    ((send (enc (exp (gen) x-4) a-0 (pubk b-0)))
      (recv
        (enc (exp (gen) x) (exp (gen) (mul x x-4))
          (exp h2
            (mul (rec x-1) x-3 z-0 (rec x-4) y x-5 x-7 y-0 (rec x-8) x-9
              x-10 (rec y-1))) b-0 (pubk a-0)))
      (send
        (enc
          (exp h2
            (mul (rec x-1) x-3 z-0 y x-5 x-7 y-0 (rec x-8) x-9 x-10
              (rec y-1))) (pubk b-0))))
    ((recv
       (enc
         (exp h2
           (mul x-3 z-0 (rec x-4) y x-7 y-0 (rec x-8) x-9 x-10
             (rec y-1))) a-1 (pubk b-1)))
      (send
        (enc (exp (gen) x-5)
          (exp h2
            (mul x-3 z-0 (rec x-4) y x-5 x-7 y-0 (rec x-8) x-9 x-10
              (rec y-1))) (exp (gen) z-1) b-1 (pubk a-1))))
    ((recv
       (enc
         (exp h2 (mul (rec x-4) y x-7 y-0 (rec x-8) x-9 x-10 (rec y-1)))
         a-2 (pubk b-2)))
      (send
        (enc (exp (gen) z-0)
          (exp h2
            (mul z-0 (rec x-4) y x-7 y-0 (rec x-8) x-9 x-10 (rec y-1)))
          (exp (gen) x-6) b-2 (pubk a-2))))
    ((recv
       (enc (exp h2 (mul (rec x-4) y x-7 y-0 (rec x-8) x-9)) a-3
         (pubk b-3)))
      (send
        (enc (exp (gen) x-10)
          (exp h2 (mul (rec x-4) y x-7 y-0 (rec x-8) x-9 x-10))
          (exp (gen) z) b-3 (pubk a-3)))))
  (label 89)
  (parent 35)
  (unrealized (0 1) (4 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 b-1 a-1 b-2 a-2 name) (h3 base)
    (x x-0 x-1 z z-0 x-2 y x-3 x-4 x-5 x-6 x-7 x-8 x-9 y-0 expn))
  (defstrand init 3 (a a) (b b)
    (h2
      (exp (gen)
        (mul x-0 (rec x-2) y y (rec x-4) (rec x-5) x-6 x-8 y-0)))
    (h3 h3) (x x-2))
  (defstrand init 3 (a a-0) (b b-0) (h2 (exp (gen) x))
    (h3 (exp (gen) (mul (rec x-1) z-0 y x-7 x-9 y-0))) (x y))
  (defstrand resp 2 (b b-1) (a a-1) (h1 (exp (gen) (mul y x-7 x-9 y-0)))
    (y z-0) (z x-3))
  (defstrand resp 2 (b b-2) (a a-2) (h1 (exp (gen) (mul y y-0))) (y x-7)
    (z z))
  (precedes ((0 0) (3 0)) ((1 0) (3 0)) ((1 2) (0 1)) ((2 1) (1 1))
    ((3 1) (2 0)))
  (ind-zero-in (x-7 (exp (gen) (mul y y-0))) (z (exp (gen) (mul y y-0)))
    (z-0 (exp (gen) (mul y x-7 x-9 y-0)))
    (x-3 (exp (gen) (mul y x-7 x-9 y-0))))
  (non-orig (privk a) (privk b))
  (uniq-gen z z-0 x-2 y x-3 x-7)
  (operation nonce-test
    (algebra-contracted
      (h2 (exp (gen) (mul (rec x-2) y y (rec x-4) (rec x-5) x-6 y-0))))
    (exp (gen) (mul y y-0)) (3 0))
  (traces
    ((send (enc (exp (gen) x-2) a (pubk b)))
      (recv
        (enc
          (exp (gen)
            (mul x-0 (rec x-2) y y (rec x-4) (rec x-5) x-6 x-8 y-0))
          (exp (gen) (mul x-0 y y (rec x-4) (rec x-5) x-6 x-8 y-0)) h3 b
          (pubk a))) (send (enc (exp h3 x-2) (pubk b))))
    ((send (enc (exp (gen) y) a-0 (pubk b-0)))
      (recv
        (enc (exp (gen) x) (exp (gen) (mul x y))
          (exp (gen) (mul (rec x-1) z-0 y x-7 x-9 y-0)) b-0 (pubk a-0)))
      (send
        (enc (exp (gen) (mul (rec x-1) z-0 y y x-7 x-9 y-0))
          (pubk b-0))))
    ((recv (enc (exp (gen) (mul y x-7 x-9 y-0)) a-1 (pubk b-1)))
      (send
        (enc (exp (gen) z-0) (exp (gen) (mul z-0 y x-7 x-9 y-0))
          (exp (gen) x-3) b-1 (pubk a-1))))
    ((recv (enc (exp (gen) (mul y y-0)) a-2 (pubk b-2)))
      (send
        (enc (exp (gen) x-7) (exp (gen) (mul y x-7 y-0)) (exp (gen) z)
          b-2 (pubk a-2)))))
  (label 90)
  (parent 35)
  (unrealized (0 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 a-3 b-3 a-4 b-4 a-5 b-5 name)
    (h2 h3 h2-0 h2-1 h2-2 h2-3 h2-4 h2-5 base)
    (x x-0 x-1 x-2 x-3 x-4 x-5 x-6 x-7 x-8 x-9 x-10 x-11 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-7))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x (rec x-6) x-7))) (x x-6))
  (defstrand init 3 (a a-1) (b b-1) (h2 h2-1)
    (h3 (exp h2-0 (mul x-0 x-6 (rec x-8)))) (x x-8))
  (defstrand init 3 (a a-2) (b b-2) (h2 h2-2)
    (h3 (exp h2-1 (mul x-1 (rec x-5) x-8))) (x x-5))
  (defstrand init 3 (a a-3) (b b-3) (h2 h2-3)
    (h3 (exp h2-2 (mul x-2 x-5 (rec x-9)))) (x x-9))
  (defstrand init 3 (a a-4) (b b-4) (h2 h2-4)
    (h3 (exp h2-3 (mul x-3 (rec x-4) x-9))) (x x-4))
  (defstrand init 3 (a a-5) (b b-5) (h2 h2-5)
    (h3 (exp h2-4 (mul x-4 x-10 (rec x-11)))) (x x-11))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (3 1))
    ((2 2) (1 1)) ((3 0) (4 1)) ((3 2) (2 1)) ((4 0) (5 1))
    ((4 2) (3 1)) ((5 0) (6 1)) ((5 2) (4 1)) ((6 2) (5 1)))
  (non-orig (privk a) (privk b))
  (uniq-gen x-4 x-5 x-6 x-7 x-8 x-9 x-11)
  (operation nonce-test (added-strand init 3) (exp h2-4 x-4) (5 1)
    (exp (gen) x-4))
  (traces
    ((send (enc (exp (gen) x-7) a (pubk b)))
      (recv (enc h2 (exp h2 x-7) h3 b (pubk a)))
      (send (enc (exp h3 x-7) (pubk b))))
    ((send (enc (exp (gen) x-6) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-6) (exp h2 (mul x (rec x-6) x-7)) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-7)) (pubk b-0))))
    ((send (enc (exp (gen) x-8) a-1 (pubk b-1)))
      (recv
        (enc h2-1 (exp h2-1 x-8) (exp h2-0 (mul x-0 x-6 (rec x-8))) b-1
          (pubk a-1))) (send (enc (exp h2-0 (mul x-0 x-6)) (pubk b-1))))
    ((send (enc (exp (gen) x-5) a-2 (pubk b-2)))
      (recv
        (enc h2-2 (exp h2-2 x-5) (exp h2-1 (mul x-1 (rec x-5) x-8)) b-2
          (pubk a-2))) (send (enc (exp h2-1 (mul x-1 x-8)) (pubk b-2))))
    ((send (enc (exp (gen) x-9) a-3 (pubk b-3)))
      (recv
        (enc h2-3 (exp h2-3 x-9) (exp h2-2 (mul x-2 x-5 (rec x-9))) b-3
          (pubk a-3))) (send (enc (exp h2-2 (mul x-2 x-5)) (pubk b-3))))
    ((send (enc (exp (gen) x-4) a-4 (pubk b-4)))
      (recv
        (enc h2-4 (exp h2-4 x-4) (exp h2-3 (mul x-3 (rec x-4) x-9)) b-4
          (pubk a-4))) (send (enc (exp h2-3 (mul x-3 x-9)) (pubk b-4))))
    ((send (enc (exp (gen) x-11) a-5 (pubk b-5)))
      (recv
        (enc h2-5 (exp h2-5 x-11) (exp h2-4 (mul x-4 x-10 (rec x-11)))
          b-5 (pubk a-5)))
      (send (enc (exp h2-4 (mul x-4 x-10)) (pubk b-5)))))
  (label 91)
  (parent 37)
  (unrealized (1 1) (2 1) (3 1) (4 1) (5 1) (6 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 a-3 b-3 a-4 b-4 b-5 a-5 name)
    (h2 h3 h2-0 h2-1 h2-2 h2-3 h2-4 base)
    (x x-0 x-1 x-2 x-3 z x-4 x-5 x-6 x-7 x-8 x-9 x-10 x-11 x-12 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-7))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x (rec x-6) x-7))) (x x-6))
  (defstrand init 3 (a a-1) (b b-1) (h2 h2-1)
    (h3 (exp h2-0 (mul x-0 x-6 (rec x-8)))) (x x-8))
  (defstrand init 3 (a a-2) (b b-2) (h2 h2-2)
    (h3 (exp h2-1 (mul x-1 (rec x-5) x-8))) (x x-5))
  (defstrand init 3 (a a-3) (b b-3) (h2 h2-3)
    (h3 (exp h2-2 (mul x-2 x-5 (rec x-9)))) (x x-9))
  (defstrand init 3 (a a-4) (b b-4) (h2 (exp h2-4 x-12))
    (h3 (exp h2-3 (mul x-3 (rec x-4) x-9))) (x x-4))
  (defstrand resp 2 (b b-5) (a a-5) (h1 (exp h2-4 (mul x-4 x-10)))
    (y x-11) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (3 1))
    ((2 2) (1 1)) ((3 0) (4 1)) ((3 2) (2 1)) ((4 0) (5 1))
    ((4 2) (3 1)) ((5 0) (6 0)) ((5 2) (4 1)) ((6 1) (5 1)))
  (ind-zero-in (x-11 (exp h2-4 (mul x-4 x-10)))
    (z (exp h2-4 (mul x-4 x-10))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-4 x-5 x-6 x-7 x-8 x-9 x-11)
  (operation nonce-test (added-strand resp 2) (exp h2-4 (mul x-4 x-12))
    (5 1) (exp (gen) x-4))
  (traces
    ((send (enc (exp (gen) x-7) a (pubk b)))
      (recv (enc h2 (exp h2 x-7) h3 b (pubk a)))
      (send (enc (exp h3 x-7) (pubk b))))
    ((send (enc (exp (gen) x-6) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-6) (exp h2 (mul x (rec x-6) x-7)) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-7)) (pubk b-0))))
    ((send (enc (exp (gen) x-8) a-1 (pubk b-1)))
      (recv
        (enc h2-1 (exp h2-1 x-8) (exp h2-0 (mul x-0 x-6 (rec x-8))) b-1
          (pubk a-1))) (send (enc (exp h2-0 (mul x-0 x-6)) (pubk b-1))))
    ((send (enc (exp (gen) x-5) a-2 (pubk b-2)))
      (recv
        (enc h2-2 (exp h2-2 x-5) (exp h2-1 (mul x-1 (rec x-5) x-8)) b-2
          (pubk a-2))) (send (enc (exp h2-1 (mul x-1 x-8)) (pubk b-2))))
    ((send (enc (exp (gen) x-9) a-3 (pubk b-3)))
      (recv
        (enc h2-3 (exp h2-3 x-9) (exp h2-2 (mul x-2 x-5 (rec x-9))) b-3
          (pubk a-3))) (send (enc (exp h2-2 (mul x-2 x-5)) (pubk b-3))))
    ((send (enc (exp (gen) x-4) a-4 (pubk b-4)))
      (recv
        (enc (exp h2-4 x-12) (exp h2-4 (mul x-4 x-12))
          (exp h2-3 (mul x-3 (rec x-4) x-9)) b-4 (pubk a-4)))
      (send (enc (exp h2-3 (mul x-3 x-9)) (pubk b-4))))
    ((recv (enc (exp h2-4 (mul x-4 x-10)) a-5 (pubk b-5)))
      (send
        (enc (exp (gen) x-11) (exp h2-4 (mul x-4 x-10 x-11))
          (exp (gen) z) b-5 (pubk a-5)))))
  (label 92)
  (parent 37)
  (unrealized (1 1) (2 1) (3 1) (4 1) (5 1) (6 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 a-3 b-3 a-4 b-4 name)
    (h2 h3 h2-0 h2-1 h2-2 h2-3 base)
    (x x-0 x-1 x-2 x-3 x-4 x-5 x-6 x-7 x-8 x-9 x-10 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-5))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x x-5 (rec x-6)))) (x x-6))
  (defstrand init 3 (a a-1) (b b-1) (h2 h2-1)
    (h3 (exp h2-0 (mul x-0 (rec x-4) x-6))) (x x-4))
  (defstrand init 3 (a a-2) (b b-2) (h2 h2-2)
    (h3 (exp h2-1 (mul x-1 x-4 (rec x-7)))) (x x-7))
  (defstrand init 3 (a a-3) (b b-3) (h2 h2-3)
    (h3 (exp h2-2 (mul x-2 (rec x-3) x-7))) (x x-3))
  (defstrand init 3 (a a-4) (b b-4) (h2 (exp (gen) x-10))
    (h3 (exp h2-3 (mul x-3 x-8 (rec x-9)))) (x x-9))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (3 1))
    ((2 2) (1 1)) ((3 0) (4 1)) ((3 2) (2 1)) ((4 0) (5 1))
    ((4 2) (3 1)) ((5 2) (4 1)))
  (non-orig (privk a) (privk b))
  (uniq-gen x-3 x-4 x-5 x-6 x-7 x-9)
  (operation nonce-test (algebra-contracted (h2-4 (exp (gen) x-10)))
    (exp (gen) (mul x-9 x-10)) (5 1) (exp (gen) x-9))
  (traces
    ((send (enc (exp (gen) x-5) a (pubk b)))
      (recv (enc h2 (exp h2 x-5) h3 b (pubk a)))
      (send (enc (exp h3 x-5) (pubk b))))
    ((send (enc (exp (gen) x-6) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-6) (exp h2 (mul x x-5 (rec x-6))) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-5)) (pubk b-0))))
    ((send (enc (exp (gen) x-4) a-1 (pubk b-1)))
      (recv
        (enc h2-1 (exp h2-1 x-4) (exp h2-0 (mul x-0 (rec x-4) x-6)) b-1
          (pubk a-1))) (send (enc (exp h2-0 (mul x-0 x-6)) (pubk b-1))))
    ((send (enc (exp (gen) x-7) a-2 (pubk b-2)))
      (recv
        (enc h2-2 (exp h2-2 x-7) (exp h2-1 (mul x-1 x-4 (rec x-7))) b-2
          (pubk a-2))) (send (enc (exp h2-1 (mul x-1 x-4)) (pubk b-2))))
    ((send (enc (exp (gen) x-3) a-3 (pubk b-3)))
      (recv
        (enc h2-3 (exp h2-3 x-3) (exp h2-2 (mul x-2 (rec x-3) x-7)) b-3
          (pubk a-3))) (send (enc (exp h2-2 (mul x-2 x-7)) (pubk b-3))))
    ((send (enc (exp (gen) x-9) a-4 (pubk b-4)))
      (recv
        (enc (exp (gen) x-10) (exp (gen) (mul x-9 x-10))
          (exp h2-3 (mul x-3 x-8 (rec x-9))) b-4 (pubk a-4)))
      (send (enc (exp h2-3 (mul x-3 x-8)) (pubk b-4)))))
  (label 93)
  (parent 37)
  (unrealized (1 1) (2 1) (3 1) (4 1) (5 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 a-3 b-3 b-4 a-4 a-5 b-5 name)
    (h2 h3 h2-0 h2-1 h2-2 h2-3 h2-4 base)
    (x x-0 x-1 x-2 x-3 z x-4 x-5 x-6 x-7 x-8 x-9 x-10 x-11 x-12 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-6))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x (rec x-5) x-6))) (x x-5))
  (defstrand init 3 (a a-1) (b b-1) (h2 h2-1)
    (h3 (exp h2-0 (mul x-0 x-5 (rec x-7)))) (x x-7))
  (defstrand init 3 (a a-2) (b b-2) (h2 h2-2)
    (h3 (exp h2-1 (mul x-1 (rec x-4) x-7))) (x x-4))
  (defstrand init 3 (a a-3) (b b-3) (h2 (exp h2-3 x-3))
    (h3 (exp h2-2 (mul x-2 x-4 (rec x-8)))) (x x-8))
  (defstrand resp 2 (b b-4) (a a-4) (h1 (exp h2-3 (mul x-8 x-10))) (y z)
    (z x-9))
  (defstrand init 3 (a a-5) (b b-5) (h2 h2-4)
    (h3 (exp h2-3 (mul x-8 x-10 x-11 (rec x-12)))) (x x-12))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (3 1))
    ((2 2) (1 1)) ((3 0) (4 1)) ((3 2) (2 1)) ((4 0) (6 1))
    ((4 2) (3 1)) ((5 1) (4 1)) ((6 2) (5 0)))
  (ind-zero-in (z (exp h2-3 (mul x-8 x-10)))
    (x-9 (exp h2-3 (mul x-8 x-10))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-4 x-5 x-6 x-7 x-8 x-9 x-12)
  (operation nonce-test (added-strand init 3) (exp h2-3 (mul x-8 x-10))
    (5 0) (exp (gen) x-8))
  (traces
    ((send (enc (exp (gen) x-6) a (pubk b)))
      (recv (enc h2 (exp h2 x-6) h3 b (pubk a)))
      (send (enc (exp h3 x-6) (pubk b))))
    ((send (enc (exp (gen) x-5) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-5) (exp h2 (mul x (rec x-5) x-6)) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-6)) (pubk b-0))))
    ((send (enc (exp (gen) x-7) a-1 (pubk b-1)))
      (recv
        (enc h2-1 (exp h2-1 x-7) (exp h2-0 (mul x-0 x-5 (rec x-7))) b-1
          (pubk a-1))) (send (enc (exp h2-0 (mul x-0 x-5)) (pubk b-1))))
    ((send (enc (exp (gen) x-4) a-2 (pubk b-2)))
      (recv
        (enc h2-2 (exp h2-2 x-4) (exp h2-1 (mul x-1 (rec x-4) x-7)) b-2
          (pubk a-2))) (send (enc (exp h2-1 (mul x-1 x-7)) (pubk b-2))))
    ((send (enc (exp (gen) x-8) a-3 (pubk b-3)))
      (recv
        (enc (exp h2-3 x-3) (exp h2-3 (mul x-3 x-8))
          (exp h2-2 (mul x-2 x-4 (rec x-8))) b-3 (pubk a-3)))
      (send (enc (exp h2-2 (mul x-2 x-4)) (pubk b-3))))
    ((recv (enc (exp h2-3 (mul x-8 x-10)) a-4 (pubk b-4)))
      (send
        (enc (exp (gen) z) (exp h2-3 (mul z x-8 x-10)) (exp (gen) x-9)
          b-4 (pubk a-4))))
    ((send (enc (exp (gen) x-12) a-5 (pubk b-5)))
      (recv
        (enc h2-4 (exp h2-4 x-12)
          (exp h2-3 (mul x-8 x-10 x-11 (rec x-12))) b-5 (pubk a-5)))
      (send (enc (exp h2-3 (mul x-8 x-10 x-11)) (pubk b-5)))))
  (label 94)
  (parent 38)
  (unrealized (1 1) (2 1) (3 1) (4 1) (6 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 a-3 b-3 b-4 a-4 b-5 a-5 name)
    (h2 h3 h2-0 h2-1 h2-2 h2-3 base)
    (x x-0 x-1 x-2 x-3 z z-0 x-4 x-5 x-6 x-7 x-8 x-9 x-10 x-11 x-12 x-13
      x-14 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-6))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x (rec x-5) x-6))) (x x-5))
  (defstrand init 3 (a a-1) (b b-1) (h2 h2-1)
    (h3 (exp h2-0 (mul x-0 x-5 (rec x-7)))) (x x-7))
  (defstrand init 3 (a a-2) (b b-2) (h2 h2-2)
    (h3 (exp h2-1 (mul x-1 (rec x-4) x-7))) (x x-4))
  (defstrand init 3 (a a-3) (b b-3) (h2 (exp h2-3 (mul x-3 x-13)))
    (h3 (exp h2-2 (mul x-2 x-4 (rec x-8)))) (x x-8))
  (defstrand resp 2 (b b-4) (a a-4)
    (h1 (exp h2-3 (mul x-8 x-10 x-11 x-12 (rec x-14)))) (y z-0) (z x-9))
  (defstrand resp 2 (b b-5) (a a-5) (h1 (exp h2-3 (mul x-8 x-10 x-11)))
    (y x-12) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (3 1))
    ((2 2) (1 1)) ((3 0) (4 1)) ((3 2) (2 1)) ((4 0) (6 0))
    ((4 2) (3 1)) ((5 1) (4 1)) ((6 1) (5 0)))
  (ind-zero-in (x-12 (exp h2-3 (mul x-8 x-10 x-11)))
    (z (exp h2-3 (mul x-8 x-10 x-11)))
    (z-0 (exp h2-3 (mul x-8 x-10 x-11 x-12 (rec x-14))))
    (x-9 (exp h2-3 (mul x-8 x-10 x-11 x-12 (rec x-14)))))
  (non-orig (privk a) (privk b))
  (uniq-gen z z-0 x-4 x-5 x-6 x-7 x-8 x-9 x-12)
  (operation nonce-test (added-strand resp 2)
    (exp h2-3 (mul x-8 x-10 x-11 x-12 (rec x-14))) (5 0)
    (exp (gen) x-8))
  (traces
    ((send (enc (exp (gen) x-6) a (pubk b)))
      (recv (enc h2 (exp h2 x-6) h3 b (pubk a)))
      (send (enc (exp h3 x-6) (pubk b))))
    ((send (enc (exp (gen) x-5) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-5) (exp h2 (mul x (rec x-5) x-6)) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-6)) (pubk b-0))))
    ((send (enc (exp (gen) x-7) a-1 (pubk b-1)))
      (recv
        (enc h2-1 (exp h2-1 x-7) (exp h2-0 (mul x-0 x-5 (rec x-7))) b-1
          (pubk a-1))) (send (enc (exp h2-0 (mul x-0 x-5)) (pubk b-1))))
    ((send (enc (exp (gen) x-4) a-2 (pubk b-2)))
      (recv
        (enc h2-2 (exp h2-2 x-4) (exp h2-1 (mul x-1 (rec x-4) x-7)) b-2
          (pubk a-2))) (send (enc (exp h2-1 (mul x-1 x-7)) (pubk b-2))))
    ((send (enc (exp (gen) x-8) a-3 (pubk b-3)))
      (recv
        (enc (exp h2-3 (mul x-3 x-13)) (exp h2-3 (mul x-3 x-8 x-13))
          (exp h2-2 (mul x-2 x-4 (rec x-8))) b-3 (pubk a-3)))
      (send (enc (exp h2-2 (mul x-2 x-4)) (pubk b-3))))
    ((recv
       (enc (exp h2-3 (mul x-8 x-10 x-11 x-12 (rec x-14))) a-4
         (pubk b-4)))
      (send
        (enc (exp (gen) z-0)
          (exp h2-3 (mul z-0 x-8 x-10 x-11 x-12 (rec x-14)))
          (exp (gen) x-9) b-4 (pubk a-4))))
    ((recv (enc (exp h2-3 (mul x-8 x-10 x-11)) a-5 (pubk b-5)))
      (send
        (enc (exp (gen) x-12) (exp h2-3 (mul x-8 x-10 x-11 x-12))
          (exp (gen) z) b-5 (pubk a-5)))))
  (label 95)
  (parent 38)
  (unrealized (1 1) (2 1) (3 1) (4 1) (6 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 a-3 b-3 b-4 a-4 name)
    (h2 h3 h2-0 h2-1 h2-2 base)
    (x x-0 x-1 x-2 z x-3 x-4 x-5 x-6 x-7 x-8 x-9 x-10 x-11 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-5))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x x-5 (rec x-6)))) (x x-6))
  (defstrand init 3 (a a-1) (b b-1) (h2 h2-1)
    (h3 (exp h2-0 (mul x-0 (rec x-4) x-6))) (x x-4))
  (defstrand init 3 (a a-2) (b b-2) (h2 h2-2)
    (h3 (exp h2-1 (mul x-1 x-4 (rec x-7)))) (x x-7))
  (defstrand init 3 (a a-3) (b b-3)
    (h2 (exp (gen) (mul (rec x-8) x-10 x-11)))
    (h3 (exp h2-2 (mul x-2 (rec x-3) x-7))) (x x-3))
  (defstrand resp 2 (b b-4) (a a-4) (h1 (exp (gen) (mul x-3 x-11)))
    (y x-9) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (3 1))
    ((2 2) (1 1)) ((3 0) (4 1)) ((3 2) (2 1)) ((4 0) (5 0))
    ((4 2) (3 1)) ((5 1) (4 1)))
  (ind-zero-in (x-9 (exp (gen) (mul x-3 x-11)))
    (z (exp (gen) (mul x-3 x-11))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-3 x-4 x-5 x-6 x-7 x-9)
  (operation nonce-test
    (algebra-contracted (h2-3 (exp (gen) (mul (rec x-8) x-11))))
    (exp (gen) (mul x-3 x-11)) (5 0) (exp (gen) x-3))
  (traces
    ((send (enc (exp (gen) x-5) a (pubk b)))
      (recv (enc h2 (exp h2 x-5) h3 b (pubk a)))
      (send (enc (exp h3 x-5) (pubk b))))
    ((send (enc (exp (gen) x-6) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-6) (exp h2 (mul x x-5 (rec x-6))) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-5)) (pubk b-0))))
    ((send (enc (exp (gen) x-4) a-1 (pubk b-1)))
      (recv
        (enc h2-1 (exp h2-1 x-4) (exp h2-0 (mul x-0 (rec x-4) x-6)) b-1
          (pubk a-1))) (send (enc (exp h2-0 (mul x-0 x-6)) (pubk b-1))))
    ((send (enc (exp (gen) x-7) a-2 (pubk b-2)))
      (recv
        (enc h2-2 (exp h2-2 x-7) (exp h2-1 (mul x-1 x-4 (rec x-7))) b-2
          (pubk a-2))) (send (enc (exp h2-1 (mul x-1 x-4)) (pubk b-2))))
    ((send (enc (exp (gen) x-3) a-3 (pubk b-3)))
      (recv
        (enc (exp (gen) (mul (rec x-8) x-10 x-11))
          (exp (gen) (mul x-3 (rec x-8) x-10 x-11))
          (exp h2-2 (mul x-2 (rec x-3) x-7)) b-3 (pubk a-3)))
      (send (enc (exp h2-2 (mul x-2 x-7)) (pubk b-3))))
    ((recv (enc (exp (gen) (mul x-3 x-11)) a-4 (pubk b-4)))
      (send
        (enc (exp (gen) x-9) (exp (gen) (mul x-3 x-9 x-11))
          (exp (gen) z) b-4 (pubk a-4)))))
  (label 96)
  (parent 38)
  (unrealized (1 1) (2 1) (3 1) (4 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 a-3 b-3 a-4 b-4 name)
    (h2 h3 h2-0 h2-1 h2-2 h2-3 base)
    (x x-0 x-1 x-2 x-3 x-4 x-5 x-6 x-7 x-8 x-9 x-10 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-5))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x x-5 (rec x-6)))) (x x-6))
  (defstrand init 3 (a a-1) (b b-1) (h2 h2-1)
    (h3 (exp h2-0 (mul x-0 (rec x-4) x-6))) (x x-4))
  (defstrand init 3 (a a-2) (b b-2) (h2 h2-2)
    (h3 (exp h2-1 (mul x-1 x-4 (rec x-7)))) (x x-7))
  (defstrand init 3 (a a-3) (b b-3) (h2 (exp (gen) x-2))
    (h3 (exp h2-2 (mul (rec x-3) x-7 x-8))) (x x-3))
  (defstrand init 3 (a a-4) (b b-4) (h2 h2-3)
    (h3 (exp h2-2 (mul (rec x-3) x-7 x-8 x-9 (rec x-10)))) (x x-10))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (3 1))
    ((2 2) (1 1)) ((3 0) (5 1)) ((3 2) (2 1)) ((4 0) (5 1))
    ((4 2) (3 1)) ((5 2) (4 1)))
  (non-orig (privk a) (privk b))
  (uniq-gen x-3 x-4 x-5 x-6 x-7 x-10)
  (operation nonce-test (added-strand init 3)
    (exp h2-2 (mul (rec x-3) x-7 x-8)) (4 1))
  (traces
    ((send (enc (exp (gen) x-5) a (pubk b)))
      (recv (enc h2 (exp h2 x-5) h3 b (pubk a)))
      (send (enc (exp h3 x-5) (pubk b))))
    ((send (enc (exp (gen) x-6) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-6) (exp h2 (mul x x-5 (rec x-6))) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-5)) (pubk b-0))))
    ((send (enc (exp (gen) x-4) a-1 (pubk b-1)))
      (recv
        (enc h2-1 (exp h2-1 x-4) (exp h2-0 (mul x-0 (rec x-4) x-6)) b-1
          (pubk a-1))) (send (enc (exp h2-0 (mul x-0 x-6)) (pubk b-1))))
    ((send (enc (exp (gen) x-7) a-2 (pubk b-2)))
      (recv
        (enc h2-2 (exp h2-2 x-7) (exp h2-1 (mul x-1 x-4 (rec x-7))) b-2
          (pubk a-2))) (send (enc (exp h2-1 (mul x-1 x-4)) (pubk b-2))))
    ((send (enc (exp (gen) x-3) a-3 (pubk b-3)))
      (recv
        (enc (exp (gen) x-2) (exp (gen) (mul x-2 x-3))
          (exp h2-2 (mul (rec x-3) x-7 x-8)) b-3 (pubk a-3)))
      (send (enc (exp h2-2 (mul x-7 x-8)) (pubk b-3))))
    ((send (enc (exp (gen) x-10) a-4 (pubk b-4)))
      (recv
        (enc h2-3 (exp h2-3 x-10)
          (exp h2-2 (mul (rec x-3) x-7 x-8 x-9 (rec x-10))) b-4
          (pubk a-4)))
      (send (enc (exp h2-2 (mul (rec x-3) x-7 x-8 x-9)) (pubk b-4)))))
  (label 97)
  (parent 39)
  (unrealized (1 1) (2 1) (3 1) (5 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 a-3 b-3 b-4 a-4 name)
    (h2 h3 h2-0 h2-1 h2-2 base)
    (x x-0 x-1 x-2 z x-3 x-4 x-5 x-6 x-7 x-8 x-9 x-10 x-11 x-12 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-5))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x x-5 (rec x-6)))) (x x-6))
  (defstrand init 3 (a a-1) (b b-1) (h2 h2-1)
    (h3 (exp h2-0 (mul x-0 (rec x-4) x-6))) (x x-4))
  (defstrand init 3 (a a-2) (b b-2) (h2 (exp h2-2 x-11))
    (h3 (exp h2-1 (mul x-1 x-4 (rec x-7)))) (x x-7))
  (defstrand init 3 (a a-3) (b b-3) (h2 (exp (gen) x-2))
    (h3 (exp h2-2 (mul (rec x-3) x-7 x-8 x-9 x-10 (rec x-12)))) (x x-3))
  (defstrand resp 2 (b b-4) (a a-4)
    (h1 (exp h2-2 (mul (rec x-3) x-7 x-8 x-9))) (y x-10) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (3 1))
    ((2 2) (1 1)) ((3 0) (5 0)) ((3 2) (2 1)) ((4 0) (5 0))
    ((4 2) (3 1)) ((5 1) (4 1)))
  (ind-zero-in (x-10 (exp h2-2 (mul (rec x-3) x-7 x-8 x-9)))
    (z (exp h2-2 (mul (rec x-3) x-7 x-8 x-9))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-3 x-4 x-5 x-6 x-7 x-10)
  (operation nonce-test (added-strand resp 2)
    (exp h2-2 (mul (rec x-3) x-7 x-8 x-9 x-10 (rec x-12))) (4 1))
  (traces
    ((send (enc (exp (gen) x-5) a (pubk b)))
      (recv (enc h2 (exp h2 x-5) h3 b (pubk a)))
      (send (enc (exp h3 x-5) (pubk b))))
    ((send (enc (exp (gen) x-6) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-6) (exp h2 (mul x x-5 (rec x-6))) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-5)) (pubk b-0))))
    ((send (enc (exp (gen) x-4) a-1 (pubk b-1)))
      (recv
        (enc h2-1 (exp h2-1 x-4) (exp h2-0 (mul x-0 (rec x-4) x-6)) b-1
          (pubk a-1))) (send (enc (exp h2-0 (mul x-0 x-6)) (pubk b-1))))
    ((send (enc (exp (gen) x-7) a-2 (pubk b-2)))
      (recv
        (enc (exp h2-2 x-11) (exp h2-2 (mul x-7 x-11))
          (exp h2-1 (mul x-1 x-4 (rec x-7))) b-2 (pubk a-2)))
      (send (enc (exp h2-1 (mul x-1 x-4)) (pubk b-2))))
    ((send (enc (exp (gen) x-3) a-3 (pubk b-3)))
      (recv
        (enc (exp (gen) x-2) (exp (gen) (mul x-2 x-3))
          (exp h2-2 (mul (rec x-3) x-7 x-8 x-9 x-10 (rec x-12))) b-3
          (pubk a-3)))
      (send
        (enc (exp h2-2 (mul x-7 x-8 x-9 x-10 (rec x-12))) (pubk b-3))))
    ((recv (enc (exp h2-2 (mul (rec x-3) x-7 x-8 x-9)) a-4 (pubk b-4)))
      (send
        (enc (exp (gen) x-10)
          (exp h2-2 (mul (rec x-3) x-7 x-8 x-9 x-10)) (exp (gen) z) b-4
          (pubk a-4)))))
  (label 98)
  (parent 39)
  (unrealized (1 1) (2 1) (3 1) (5 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 a-3 b-3 name)
    (h2 h3 h2-0 h2-1 base)
    (x x-0 x-1 x-2 x-3 x-4 x-5 x-6 x-7 x-8 x-9 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-4))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x (rec x-3) x-4))) (x x-3))
  (defstrand init 3 (a a-1) (b b-1) (h2 h2-1)
    (h3 (exp h2-0 (mul x-0 x-3 (rec x-5)))) (x x-5))
  (defstrand init 3 (a a-2) (b b-2)
    (h2 (exp (gen) (mul (rec x-2) (rec x-6) x-7 x-7 x-9)))
    (h3 (exp h2-1 (mul x-1 (rec x-2) x-5))) (x x-2))
  (defstrand init 3 (a a-3) (b b-3) (h2 (exp (gen) x-8))
    (h3 (exp (gen) (mul x-7 x-9))) (x x-7))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (3 1))
    ((2 2) (1 1)) ((3 0) (4 1)) ((3 2) (2 1)) ((4 2) (3 1)))
  (non-orig (privk a) (privk b))
  (uniq-gen x-2 x-3 x-4 x-5 x-7)
  (operation nonce-test
    (algebra-contracted
      (h2-2 (exp (gen) (mul (rec x-2) (rec x-6) x-7 x-7 x-9))))
    (exp (gen) (mul x-7 x-9)) (4 1))
  (traces
    ((send (enc (exp (gen) x-4) a (pubk b)))
      (recv (enc h2 (exp h2 x-4) h3 b (pubk a)))
      (send (enc (exp h3 x-4) (pubk b))))
    ((send (enc (exp (gen) x-3) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-3) (exp h2 (mul x (rec x-3) x-4)) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-4)) (pubk b-0))))
    ((send (enc (exp (gen) x-5) a-1 (pubk b-1)))
      (recv
        (enc h2-1 (exp h2-1 x-5) (exp h2-0 (mul x-0 x-3 (rec x-5))) b-1
          (pubk a-1))) (send (enc (exp h2-0 (mul x-0 x-3)) (pubk b-1))))
    ((send (enc (exp (gen) x-2) a-2 (pubk b-2)))
      (recv
        (enc (exp (gen) (mul (rec x-2) (rec x-6) x-7 x-7 x-9))
          (exp (gen) (mul (rec x-6) x-7 x-7 x-9))
          (exp h2-1 (mul x-1 (rec x-2) x-5)) b-2 (pubk a-2)))
      (send (enc (exp h2-1 (mul x-1 x-5)) (pubk b-2))))
    ((send (enc (exp (gen) x-7) a-3 (pubk b-3)))
      (recv
        (enc (exp (gen) x-8) (exp (gen) (mul x-7 x-8))
          (exp (gen) (mul x-7 x-9)) b-3 (pubk a-3)))
      (send (enc (exp (gen) (mul x-7 x-7 x-9)) (pubk b-3)))))
  (label 99)
  (parent 39)
  (unrealized (1 1) (2 1) (3 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 b-3 a-3 a-4 b-4 a-5 b-5 name)
    (h2 h3 h2-0 h2-1 h2-2 h2-3 h2-4 base)
    (x x-0 x-1 x-2 x-3 x-4 z x-5 x-6 x-7 x-8 x-9 x-10 x-11 x-12 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-8))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x (rec x-7) x-8))) (x x-7))
  (defstrand init 3 (a a-1) (b b-1) (h2 h2-1)
    (h3 (exp h2-0 (mul x-0 x-7 (rec x-9)))) (x x-9))
  (defstrand init 3 (a a-2) (b b-2) (h2 (exp h2-2 x-2))
    (h3 (exp h2-1 (mul x-1 (rec x-6) x-9))) (x x-6))
  (defstrand resp 2 (b b-3) (a a-3) (h1 (exp h2-2 (mul x-3 x-6)))
    (y x-10) (z x-5))
  (defstrand init 3 (a a-4) (b b-4) (h2 h2-3)
    (h3 (exp h2-2 (mul x-3 x-4 (rec z) x-6))) (x z))
  (defstrand init 3 (a a-5) (b b-5) (h2 h2-4)
    (h3 (exp h2-3 (mul z x-11 (rec x-12)))) (x x-12))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (3 1))
    ((2 2) (1 1)) ((3 0) (5 1)) ((3 2) (2 1)) ((4 1) (3 1))
    ((5 0) (6 1)) ((5 2) (4 0)) ((6 2) (5 1)))
  (ind-zero-in (x-10 (exp h2-2 (mul x-3 x-6)))
    (x-5 (exp h2-2 (mul x-3 x-6))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-5 x-6 x-7 x-8 x-9 x-10 x-12)
  (operation nonce-test (added-strand init 3) (exp h2-3 z) (5 1)
    (exp (gen) z))
  (traces
    ((send (enc (exp (gen) x-8) a (pubk b)))
      (recv (enc h2 (exp h2 x-8) h3 b (pubk a)))
      (send (enc (exp h3 x-8) (pubk b))))
    ((send (enc (exp (gen) x-7) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-7) (exp h2 (mul x (rec x-7) x-8)) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-8)) (pubk b-0))))
    ((send (enc (exp (gen) x-9) a-1 (pubk b-1)))
      (recv
        (enc h2-1 (exp h2-1 x-9) (exp h2-0 (mul x-0 x-7 (rec x-9))) b-1
          (pubk a-1))) (send (enc (exp h2-0 (mul x-0 x-7)) (pubk b-1))))
    ((send (enc (exp (gen) x-6) a-2 (pubk b-2)))
      (recv
        (enc (exp h2-2 x-2) (exp h2-2 (mul x-2 x-6))
          (exp h2-1 (mul x-1 (rec x-6) x-9)) b-2 (pubk a-2)))
      (send (enc (exp h2-1 (mul x-1 x-9)) (pubk b-2))))
    ((recv (enc (exp h2-2 (mul x-3 x-6)) a-3 (pubk b-3)))
      (send
        (enc (exp (gen) x-10) (exp h2-2 (mul x-3 x-6 x-10))
          (exp (gen) x-5) b-3 (pubk a-3))))
    ((send (enc (exp (gen) z) a-4 (pubk b-4)))
      (recv
        (enc h2-3 (exp h2-3 z) (exp h2-2 (mul x-3 x-4 (rec z) x-6)) b-4
          (pubk a-4)))
      (send (enc (exp h2-2 (mul x-3 x-4 x-6)) (pubk b-4))))
    ((send (enc (exp (gen) x-12) a-5 (pubk b-5)))
      (recv
        (enc h2-4 (exp h2-4 x-12) (exp h2-3 (mul z x-11 (rec x-12))) b-5
          (pubk a-5))) (send (enc (exp h2-3 (mul z x-11)) (pubk b-5)))))
  (label 100)
  (parent 40)
  (unrealized (1 1) (2 1) (3 1) (5 1) (6 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 b-3 a-3 a-4 b-4 b-5 a-5 name)
    (h2 h3 h2-0 h2-1 h2-2 h2-3 base)
    (x x-0 x-1 x-2 x-3 x-4 z z-0 x-5 x-6 x-7 x-8 x-9 x-10 x-11 x-12 x-13
      expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-8))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x (rec x-7) x-8))) (x x-7))
  (defstrand init 3 (a a-1) (b b-1) (h2 h2-1)
    (h3 (exp h2-0 (mul x-0 x-7 (rec x-9)))) (x x-9))
  (defstrand init 3 (a a-2) (b b-2) (h2 (exp h2-2 x-2))
    (h3 (exp h2-1 (mul x-1 (rec x-6) x-9))) (x x-6))
  (defstrand resp 2 (b b-3) (a a-3) (h1 (exp h2-2 (mul x-3 x-6)))
    (y x-10) (z x-5))
  (defstrand init 3 (a a-4) (b b-4) (h2 (exp h2-3 x-13))
    (h3 (exp h2-2 (mul x-3 x-4 (rec z-0) x-6))) (x z-0))
  (defstrand resp 2 (b b-5) (a a-5) (h1 (exp h2-3 (mul z-0 x-11)))
    (y x-12) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (3 1))
    ((2 2) (1 1)) ((3 0) (5 1)) ((3 2) (2 1)) ((4 1) (3 1))
    ((5 0) (6 0)) ((5 2) (4 0)) ((6 1) (5 1)))
  (ind-zero-in (x-12 (exp h2-3 (mul z-0 x-11)))
    (z (exp h2-3 (mul z-0 x-11))) (x-10 (exp h2-2 (mul x-3 x-6)))
    (x-5 (exp h2-2 (mul x-3 x-6))))
  (non-orig (privk a) (privk b))
  (uniq-gen z z-0 x-5 x-6 x-7 x-8 x-9 x-10 x-12)
  (operation nonce-test (added-strand resp 2) (exp h2-3 (mul z-0 x-13))
    (5 1) (exp (gen) z-0))
  (traces
    ((send (enc (exp (gen) x-8) a (pubk b)))
      (recv (enc h2 (exp h2 x-8) h3 b (pubk a)))
      (send (enc (exp h3 x-8) (pubk b))))
    ((send (enc (exp (gen) x-7) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-7) (exp h2 (mul x (rec x-7) x-8)) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-8)) (pubk b-0))))
    ((send (enc (exp (gen) x-9) a-1 (pubk b-1)))
      (recv
        (enc h2-1 (exp h2-1 x-9) (exp h2-0 (mul x-0 x-7 (rec x-9))) b-1
          (pubk a-1))) (send (enc (exp h2-0 (mul x-0 x-7)) (pubk b-1))))
    ((send (enc (exp (gen) x-6) a-2 (pubk b-2)))
      (recv
        (enc (exp h2-2 x-2) (exp h2-2 (mul x-2 x-6))
          (exp h2-1 (mul x-1 (rec x-6) x-9)) b-2 (pubk a-2)))
      (send (enc (exp h2-1 (mul x-1 x-9)) (pubk b-2))))
    ((recv (enc (exp h2-2 (mul x-3 x-6)) a-3 (pubk b-3)))
      (send
        (enc (exp (gen) x-10) (exp h2-2 (mul x-3 x-6 x-10))
          (exp (gen) x-5) b-3 (pubk a-3))))
    ((send (enc (exp (gen) z-0) a-4 (pubk b-4)))
      (recv
        (enc (exp h2-3 x-13) (exp h2-3 (mul z-0 x-13))
          (exp h2-2 (mul x-3 x-4 (rec z-0) x-6)) b-4 (pubk a-4)))
      (send (enc (exp h2-2 (mul x-3 x-4 x-6)) (pubk b-4))))
    ((recv (enc (exp h2-3 (mul z-0 x-11)) a-5 (pubk b-5)))
      (send
        (enc (exp (gen) x-12) (exp h2-3 (mul z-0 x-11 x-12))
          (exp (gen) z) b-5 (pubk a-5)))))
  (label 101)
  (parent 40)
  (unrealized (1 1) (2 1) (3 1) (5 1) (6 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 b-3 a-3 a-4 b-4 name)
    (h2 h3 h2-0 h2-1 h2-2 base)
    (x x-0 x-1 x-2 z x-3 x-4 x-5 x-6 x-7 x-8 x-9 x-10 x-11 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-4))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x x-4 (rec x-5)))) (x x-5))
  (defstrand init 3 (a a-1) (b b-1) (h2 h2-1)
    (h3 (exp h2-0 (mul x-0 (rec x-3) x-5))) (x x-3))
  (defstrand init 3 (a a-2) (b b-2) (h2 (exp h2-2 x-2))
    (h3 (exp h2-1 (mul x-1 x-3 (rec x-6)))) (x x-6))
  (defstrand resp 2 (b b-3) (a a-3) (h1 (exp h2-2 (mul x-6 x-8))) (y z)
    (z x-7))
  (defstrand init 3 (a a-4) (b b-4) (h2 (exp (gen) x-11))
    (h3 (exp h2-2 (mul x-6 x-8 x-9 (rec x-10)))) (x x-10))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (3 1))
    ((2 2) (1 1)) ((3 0) (5 1)) ((3 2) (2 1)) ((4 1) (3 1))
    ((5 2) (4 0)))
  (ind-zero-in (z (exp h2-2 (mul x-6 x-8)))
    (x-7 (exp h2-2 (mul x-6 x-8))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-3 x-4 x-5 x-6 x-7 x-10)
  (operation nonce-test (algebra-contracted (h2-3 (exp (gen) x-11)))
    (exp (gen) (mul x-10 x-11)) (5 1) (exp (gen) x-10))
  (traces
    ((send (enc (exp (gen) x-4) a (pubk b)))
      (recv (enc h2 (exp h2 x-4) h3 b (pubk a)))
      (send (enc (exp h3 x-4) (pubk b))))
    ((send (enc (exp (gen) x-5) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-5) (exp h2 (mul x x-4 (rec x-5))) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-4)) (pubk b-0))))
    ((send (enc (exp (gen) x-3) a-1 (pubk b-1)))
      (recv
        (enc h2-1 (exp h2-1 x-3) (exp h2-0 (mul x-0 (rec x-3) x-5)) b-1
          (pubk a-1))) (send (enc (exp h2-0 (mul x-0 x-5)) (pubk b-1))))
    ((send (enc (exp (gen) x-6) a-2 (pubk b-2)))
      (recv
        (enc (exp h2-2 x-2) (exp h2-2 (mul x-2 x-6))
          (exp h2-1 (mul x-1 x-3 (rec x-6))) b-2 (pubk a-2)))
      (send (enc (exp h2-1 (mul x-1 x-3)) (pubk b-2))))
    ((recv (enc (exp h2-2 (mul x-6 x-8)) a-3 (pubk b-3)))
      (send
        (enc (exp (gen) z) (exp h2-2 (mul z x-6 x-8)) (exp (gen) x-7)
          b-3 (pubk a-3))))
    ((send (enc (exp (gen) x-10) a-4 (pubk b-4)))
      (recv
        (enc (exp (gen) x-11) (exp (gen) (mul x-10 x-11))
          (exp h2-2 (mul x-6 x-8 x-9 (rec x-10))) b-4 (pubk a-4)))
      (send (enc (exp h2-2 (mul x-6 x-8 x-9)) (pubk b-4)))))
  (label 102)
  (parent 40)
  (unrealized (1 1) (2 1) (3 1) (5 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 b-3 a-3 b-4 a-4 a-5 b-5 name)
    (h2 h3 h2-0 h2-1 h2-2 h2-3 base)
    (x x-0 x-1 x-2 x-3 x-4 z z-0 x-5 x-6 x-7 x-8 x-9 x-10 x-11 x-12 x-13
      x-14 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-7))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x (rec x-6) x-7))) (x x-6))
  (defstrand init 3 (a a-1) (b b-1) (h2 h2-1)
    (h3 (exp h2-0 (mul x-0 x-6 (rec x-8)))) (x x-8))
  (defstrand init 3 (a a-2) (b b-2) (h2 (exp h2-2 (mul x-2 x-3)))
    (h3 (exp h2-1 (mul x-1 (rec x-5) x-8))) (x x-5))
  (defstrand resp 2 (b b-3) (a a-3)
    (h1 (exp h2-2 (mul (rec x-4) z x-5 x-11 x-12))) (y x-9) (z z-0))
  (defstrand resp 2 (b b-4) (a a-4) (h1 (exp h2-2 (mul x-5 x-11 x-12)))
    (y z) (z x-10))
  (defstrand init 3 (a a-5) (b b-5) (h2 h2-3)
    (h3 (exp h2-2 (mul x-5 x-11 x-12 x-13 (rec x-14)))) (x x-14))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (3 1))
    ((2 2) (1 1)) ((3 0) (6 1)) ((3 2) (2 1)) ((4 1) (3 1))
    ((5 1) (4 0)) ((6 2) (5 0)))
  (ind-zero-in (z (exp h2-2 (mul x-5 x-11 x-12)))
    (x-10 (exp h2-2 (mul x-5 x-11 x-12)))
    (x-9 (exp h2-2 (mul (rec x-4) z x-5 x-11 x-12)))
    (z-0 (exp h2-2 (mul (rec x-4) z x-5 x-11 x-12))))
  (non-orig (privk a) (privk b))
  (uniq-gen z z-0 x-5 x-6 x-7 x-8 x-9 x-10 x-14)
  (operation nonce-test (added-strand init 3)
    (exp h2-2 (mul x-5 x-11 x-12)) (5 0) (exp (gen) x-5))
  (traces
    ((send (enc (exp (gen) x-7) a (pubk b)))
      (recv (enc h2 (exp h2 x-7) h3 b (pubk a)))
      (send (enc (exp h3 x-7) (pubk b))))
    ((send (enc (exp (gen) x-6) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-6) (exp h2 (mul x (rec x-6) x-7)) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-7)) (pubk b-0))))
    ((send (enc (exp (gen) x-8) a-1 (pubk b-1)))
      (recv
        (enc h2-1 (exp h2-1 x-8) (exp h2-0 (mul x-0 x-6 (rec x-8))) b-1
          (pubk a-1))) (send (enc (exp h2-0 (mul x-0 x-6)) (pubk b-1))))
    ((send (enc (exp (gen) x-5) a-2 (pubk b-2)))
      (recv
        (enc (exp h2-2 (mul x-2 x-3)) (exp h2-2 (mul x-2 x-3 x-5))
          (exp h2-1 (mul x-1 (rec x-5) x-8)) b-2 (pubk a-2)))
      (send (enc (exp h2-1 (mul x-1 x-8)) (pubk b-2))))
    ((recv
       (enc (exp h2-2 (mul (rec x-4) z x-5 x-11 x-12)) a-3 (pubk b-3)))
      (send
        (enc (exp (gen) x-9)
          (exp h2-2 (mul (rec x-4) z x-5 x-9 x-11 x-12)) (exp (gen) z-0)
          b-3 (pubk a-3))))
    ((recv (enc (exp h2-2 (mul x-5 x-11 x-12)) a-4 (pubk b-4)))
      (send
        (enc (exp (gen) z) (exp h2-2 (mul z x-5 x-11 x-12))
          (exp (gen) x-10) b-4 (pubk a-4))))
    ((send (enc (exp (gen) x-14) a-5 (pubk b-5)))
      (recv
        (enc h2-3 (exp h2-3 x-14)
          (exp h2-2 (mul x-5 x-11 x-12 x-13 (rec x-14))) b-5
          (pubk a-5)))
      (send (enc (exp h2-2 (mul x-5 x-11 x-12 x-13)) (pubk b-5)))))
  (label 103)
  (parent 41)
  (unrealized (1 1) (2 1) (3 1) (6 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 b-3 a-3 b-4 a-4 b-5 a-5 name)
    (h2 h3 h2-0 h2-1 h2-2 base)
    (x x-0 x-1 x-2 x-3 x-4 z z-0 z-1 x-5 x-6 x-7 x-8 x-9 x-10 x-11 x-12
      x-13 x-14 x-15 x-16 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-7))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x (rec x-6) x-7))) (x x-6))
  (defstrand init 3 (a a-1) (b b-1) (h2 h2-1)
    (h3 (exp h2-0 (mul x-0 x-6 (rec x-8)))) (x x-8))
  (defstrand init 3 (a a-2) (b b-2) (h2 (exp h2-2 (mul x-2 x-3 x-15)))
    (h3 (exp h2-1 (mul x-1 (rec x-5) x-8))) (x x-5))
  (defstrand resp 2 (b b-3) (a a-3)
    (h1
      (exp h2-2 (mul (rec x-4) z-0 x-5 x-11 x-12 x-13 x-14 (rec x-16))))
    (y x-9) (z z-1))
  (defstrand resp 2 (b b-4) (a a-4)
    (h1 (exp h2-2 (mul x-5 x-11 x-12 x-13 x-14 (rec x-16)))) (y z-0)
    (z x-10))
  (defstrand resp 2 (b b-5) (a a-5)
    (h1 (exp h2-2 (mul x-5 x-11 x-12 x-13))) (y x-14) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (3 1))
    ((2 2) (1 1)) ((3 0) (6 0)) ((3 2) (2 1)) ((4 1) (3 1))
    ((5 1) (4 0)) ((6 1) (5 0)))
  (ind-zero-in (x-14 (exp h2-2 (mul x-5 x-11 x-12 x-13)))
    (z (exp h2-2 (mul x-5 x-11 x-12 x-13)))
    (z-0 (exp h2-2 (mul x-5 x-11 x-12 x-13 x-14 (rec x-16))))
    (x-10 (exp h2-2 (mul x-5 x-11 x-12 x-13 x-14 (rec x-16))))
    (x-9
      (exp h2-2 (mul (rec x-4) z-0 x-5 x-11 x-12 x-13 x-14 (rec x-16))))
    (z-1
      (exp h2-2
        (mul (rec x-4) z-0 x-5 x-11 x-12 x-13 x-14 (rec x-16)))))
  (non-orig (privk a) (privk b))
  (uniq-gen z z-0 z-1 x-5 x-6 x-7 x-8 x-9 x-10 x-14)
  (operation nonce-test (added-strand resp 2)
    (exp h2-2 (mul x-5 x-11 x-12 x-13 x-14 (rec x-16))) (5 0)
    (exp (gen) x-5))
  (traces
    ((send (enc (exp (gen) x-7) a (pubk b)))
      (recv (enc h2 (exp h2 x-7) h3 b (pubk a)))
      (send (enc (exp h3 x-7) (pubk b))))
    ((send (enc (exp (gen) x-6) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-6) (exp h2 (mul x (rec x-6) x-7)) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-7)) (pubk b-0))))
    ((send (enc (exp (gen) x-8) a-1 (pubk b-1)))
      (recv
        (enc h2-1 (exp h2-1 x-8) (exp h2-0 (mul x-0 x-6 (rec x-8))) b-1
          (pubk a-1))) (send (enc (exp h2-0 (mul x-0 x-6)) (pubk b-1))))
    ((send (enc (exp (gen) x-5) a-2 (pubk b-2)))
      (recv
        (enc (exp h2-2 (mul x-2 x-3 x-15))
          (exp h2-2 (mul x-2 x-3 x-5 x-15))
          (exp h2-1 (mul x-1 (rec x-5) x-8)) b-2 (pubk a-2)))
      (send (enc (exp h2-1 (mul x-1 x-8)) (pubk b-2))))
    ((recv
       (enc
         (exp h2-2
           (mul (rec x-4) z-0 x-5 x-11 x-12 x-13 x-14 (rec x-16))) a-3
         (pubk b-3)))
      (send
        (enc (exp (gen) x-9)
          (exp h2-2
            (mul (rec x-4) z-0 x-5 x-9 x-11 x-12 x-13 x-14 (rec x-16)))
          (exp (gen) z-1) b-3 (pubk a-3))))
    ((recv
       (enc (exp h2-2 (mul x-5 x-11 x-12 x-13 x-14 (rec x-16))) a-4
         (pubk b-4)))
      (send
        (enc (exp (gen) z-0)
          (exp h2-2 (mul z-0 x-5 x-11 x-12 x-13 x-14 (rec x-16)))
          (exp (gen) x-10) b-4 (pubk a-4))))
    ((recv (enc (exp h2-2 (mul x-5 x-11 x-12 x-13)) a-5 (pubk b-5)))
      (send
        (enc (exp (gen) x-14) (exp h2-2 (mul x-5 x-11 x-12 x-13 x-14))
          (exp (gen) z) b-5 (pubk a-5)))))
  (label 104)
  (parent 41)
  (unrealized (1 1) (2 1) (3 1) (6 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 b-3 a-3 b-4 a-4 name)
    (h2 h3 h2-0 h2-1 base)
    (x x-0 x-1 x-2 z z-0 x-3 x-4 x-5 x-6 x-7 x-8 x-9 x-10 x-11 x-12 x-13
      expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-4))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x x-4 (rec x-5)))) (x x-5))
  (defstrand init 3 (a a-1) (b b-1) (h2 h2-1)
    (h3 (exp h2-0 (mul x-0 (rec x-3) x-5))) (x x-3))
  (defstrand init 3 (a a-2) (b b-2)
    (h2 (exp (gen) (mul x-2 (rec x-8) (rec x-9) x-11 x-13)))
    (h3 (exp h2-1 (mul x-1 x-3 (rec x-6)))) (x x-6))
  (defstrand resp 2 (b b-3) (a a-3)
    (h1 (exp (gen) (mul x-6 x-10 (rec x-12) x-13))) (y z-0) (z x-7))
  (defstrand resp 2 (b b-4) (a a-4) (h1 (exp (gen) (mul x-6 x-13)))
    (y x-10) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (3 1))
    ((2 2) (1 1)) ((3 0) (5 0)) ((3 2) (2 1)) ((4 1) (3 1))
    ((5 1) (4 0)))
  (ind-zero-in (x-10 (exp (gen) (mul x-6 x-13)))
    (z (exp (gen) (mul x-6 x-13)))
    (z-0 (exp (gen) (mul x-6 x-10 (rec x-12) x-13)))
    (x-7 (exp (gen) (mul x-6 x-10 (rec x-12) x-13))))
  (non-orig (privk a) (privk b))
  (uniq-gen z z-0 x-3 x-4 x-5 x-6 x-7 x-10)
  (operation nonce-test
    (algebra-contracted
      (h2-2 (exp (gen) (mul (rec x-8) (rec x-9) x-13))))
    (exp (gen) (mul x-6 x-13)) (5 0) (exp (gen) x-6))
  (traces
    ((send (enc (exp (gen) x-4) a (pubk b)))
      (recv (enc h2 (exp h2 x-4) h3 b (pubk a)))
      (send (enc (exp h3 x-4) (pubk b))))
    ((send (enc (exp (gen) x-5) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-5) (exp h2 (mul x x-4 (rec x-5))) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-4)) (pubk b-0))))
    ((send (enc (exp (gen) x-3) a-1 (pubk b-1)))
      (recv
        (enc h2-1 (exp h2-1 x-3) (exp h2-0 (mul x-0 (rec x-3) x-5)) b-1
          (pubk a-1))) (send (enc (exp h2-0 (mul x-0 x-5)) (pubk b-1))))
    ((send (enc (exp (gen) x-6) a-2 (pubk b-2)))
      (recv
        (enc (exp (gen) (mul x-2 (rec x-8) (rec x-9) x-11 x-13))
          (exp (gen) (mul x-2 x-6 (rec x-8) (rec x-9) x-11 x-13))
          (exp h2-1 (mul x-1 x-3 (rec x-6))) b-2 (pubk a-2)))
      (send (enc (exp h2-1 (mul x-1 x-3)) (pubk b-2))))
    ((recv
       (enc (exp (gen) (mul x-6 x-10 (rec x-12) x-13)) a-3 (pubk b-3)))
      (send
        (enc (exp (gen) z-0)
          (exp (gen) (mul z-0 x-6 x-10 (rec x-12) x-13)) (exp (gen) x-7)
          b-3 (pubk a-3))))
    ((recv (enc (exp (gen) (mul x-6 x-13)) a-4 (pubk b-4)))
      (send
        (enc (exp (gen) x-10) (exp (gen) (mul x-6 x-10 x-13))
          (exp (gen) z) b-4 (pubk a-4)))))
  (label 105)
  (parent 41)
  (unrealized (1 1) (2 1) (3 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 b-3 a-3 name) (h2 h3 h2-0 base)
    (x x-0 x-1 z x-2 x-3 x-4 x-5 x-6 x-7 x-8 x-9 x-10 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-4))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x (rec x-3) x-4))) (x x-3))
  (defstrand init 3 (a a-1) (b b-1)
    (h2 (exp (gen) (mul (rec x-1) x-2 (rec x-5) x-7 x-10)))
    (h3 (exp h2-0 (mul x-0 x-3 (rec x-5)))) (x x-5))
  (defstrand init 3 (a a-2) (b b-2)
    (h2 (exp (gen) (mul (rec x-6) x-8 x-9)))
    (h3 (exp (gen) (mul x-7 x-10))) (x x-2))
  (defstrand resp 2 (b b-3) (a a-3) (h1 (exp (gen) (mul x-2 x-9)))
    (y x-7) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (3 1))
    ((2 2) (1 1)) ((3 0) (4 0)) ((3 2) (2 1)) ((4 1) (3 1)))
  (ind-zero-in (x-7 (exp (gen) (mul x-2 x-9)))
    (z (exp (gen) (mul x-2 x-9))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-2 x-3 x-4 x-5 x-7)
  (operation nonce-test
    (algebra-contracted
      (h2-1 (exp (gen) (mul (rec x-1) x-2 (rec x-5) x-7 x-10))))
    (exp (gen) (mul x-7 x-10)) (3 1))
  (traces
    ((send (enc (exp (gen) x-4) a (pubk b)))
      (recv (enc h2 (exp h2 x-4) h3 b (pubk a)))
      (send (enc (exp h3 x-4) (pubk b))))
    ((send (enc (exp (gen) x-3) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-3) (exp h2 (mul x (rec x-3) x-4)) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-4)) (pubk b-0))))
    ((send (enc (exp (gen) x-5) a-1 (pubk b-1)))
      (recv
        (enc (exp (gen) (mul (rec x-1) x-2 (rec x-5) x-7 x-10))
          (exp (gen) (mul (rec x-1) x-2 x-7 x-10))
          (exp h2-0 (mul x-0 x-3 (rec x-5))) b-1 (pubk a-1)))
      (send (enc (exp h2-0 (mul x-0 x-3)) (pubk b-1))))
    ((send (enc (exp (gen) x-2) a-2 (pubk b-2)))
      (recv
        (enc (exp (gen) (mul (rec x-6) x-8 x-9))
          (exp (gen) (mul x-2 (rec x-6) x-8 x-9))
          (exp (gen) (mul x-7 x-10)) b-2 (pubk a-2)))
      (send (enc (exp (gen) (mul x-2 x-7 x-10)) (pubk b-2))))
    ((recv (enc (exp (gen) (mul x-2 x-9)) a-3 (pubk b-3)))
      (send
        (enc (exp (gen) x-7) (exp (gen) (mul x-2 x-7 x-9)) (exp (gen) z)
          b-3 (pubk a-3)))))
  (label 106)
  (parent 42)
  (seen 106)
  (unrealized (1 1) (2 1))
  (comment "1 in cohort - 0 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 a-3 b-3 a-4 b-4 name)
    (h2 h3 h2-0 h2-1 h2-2 h2-3 base)
    (x x-0 x-1 x-2 x-3 x-4 x-5 x-6 x-7 x-8 x-9 x-10 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-6))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x x-6 (rec x-7)))) (x x-7))
  (defstrand init 3 (a a-1) (b b-1) (h2 h2-1)
    (h3 (exp h2-0 (mul x-0 (rec x-5) x-7))) (x x-5))
  (defstrand init 3 (a a-2) (b b-2) (h2 (exp (gen) x-1))
    (h3 (exp h2-1 (mul x-2 x-5 (rec x-8)))) (x x-8))
  (defstrand init 3 (a a-3) (b b-3) (h2 h2-2)
    (h3 (exp h2-1 (mul x-2 x-3 (rec x-4) x-5 (rec x-8)))) (x x-4))
  (defstrand init 3 (a a-4) (b b-4) (h2 h2-3)
    (h3 (exp h2-2 (mul x-4 x-9 (rec x-10)))) (x x-10))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (4 1))
    ((2 2) (1 1)) ((3 0) (4 1)) ((3 2) (2 1)) ((4 0) (5 1))
    ((4 2) (3 1)) ((5 2) (4 1)))
  (non-orig (privk a) (privk b))
  (uniq-gen x-4 x-5 x-6 x-7 x-8 x-10)
  (operation nonce-test (added-strand init 3) (exp h2-2 x-4) (4 1)
    (exp (gen) x-4))
  (traces
    ((send (enc (exp (gen) x-6) a (pubk b)))
      (recv (enc h2 (exp h2 x-6) h3 b (pubk a)))
      (send (enc (exp h3 x-6) (pubk b))))
    ((send (enc (exp (gen) x-7) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-7) (exp h2 (mul x x-6 (rec x-7))) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-6)) (pubk b-0))))
    ((send (enc (exp (gen) x-5) a-1 (pubk b-1)))
      (recv
        (enc h2-1 (exp h2-1 x-5) (exp h2-0 (mul x-0 (rec x-5) x-7)) b-1
          (pubk a-1))) (send (enc (exp h2-0 (mul x-0 x-7)) (pubk b-1))))
    ((send (enc (exp (gen) x-8) a-2 (pubk b-2)))
      (recv
        (enc (exp (gen) x-1) (exp (gen) (mul x-1 x-8))
          (exp h2-1 (mul x-2 x-5 (rec x-8))) b-2 (pubk a-2)))
      (send (enc (exp h2-1 (mul x-2 x-5)) (pubk b-2))))
    ((send (enc (exp (gen) x-4) a-3 (pubk b-3)))
      (recv
        (enc h2-2 (exp h2-2 x-4)
          (exp h2-1 (mul x-2 x-3 (rec x-4) x-5 (rec x-8))) b-3
          (pubk a-3)))
      (send (enc (exp h2-1 (mul x-2 x-3 x-5 (rec x-8))) (pubk b-3))))
    ((send (enc (exp (gen) x-10) a-4 (pubk b-4)))
      (recv
        (enc h2-3 (exp h2-3 x-10) (exp h2-2 (mul x-4 x-9 (rec x-10)))
          b-4 (pubk a-4)))
      (send (enc (exp h2-2 (mul x-4 x-9)) (pubk b-4)))))
  (label 107)
  (parent 43)
  (unrealized (1 1) (2 1) (4 1) (5 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 a-3 b-3 b-4 a-4 name)
    (h2 h3 h2-0 h2-1 h2-2 base)
    (x x-0 x-1 x-2 x-3 z x-4 x-5 x-6 x-7 x-8 x-9 x-10 x-11 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-6))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x x-6 (rec x-7)))) (x x-7))
  (defstrand init 3 (a a-1) (b b-1) (h2 h2-1)
    (h3 (exp h2-0 (mul x-0 (rec x-5) x-7))) (x x-5))
  (defstrand init 3 (a a-2) (b b-2) (h2 (exp (gen) x-1))
    (h3 (exp h2-1 (mul x-2 x-5 (rec x-8)))) (x x-8))
  (defstrand init 3 (a a-3) (b b-3) (h2 (exp h2-2 x-11))
    (h3 (exp h2-1 (mul x-2 x-3 (rec x-4) x-5 (rec x-8)))) (x x-4))
  (defstrand resp 2 (b b-4) (a a-4) (h1 (exp h2-2 (mul x-4 x-9)))
    (y x-10) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (4 1))
    ((2 2) (1 1)) ((3 0) (4 1)) ((3 2) (2 1)) ((4 0) (5 0))
    ((4 2) (3 1)) ((5 1) (4 1)))
  (ind-zero-in (x-10 (exp h2-2 (mul x-4 x-9)))
    (z (exp h2-2 (mul x-4 x-9))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-4 x-5 x-6 x-7 x-8 x-10)
  (operation nonce-test (added-strand resp 2) (exp h2-2 (mul x-4 x-11))
    (4 1) (exp (gen) x-4))
  (traces
    ((send (enc (exp (gen) x-6) a (pubk b)))
      (recv (enc h2 (exp h2 x-6) h3 b (pubk a)))
      (send (enc (exp h3 x-6) (pubk b))))
    ((send (enc (exp (gen) x-7) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-7) (exp h2 (mul x x-6 (rec x-7))) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-6)) (pubk b-0))))
    ((send (enc (exp (gen) x-5) a-1 (pubk b-1)))
      (recv
        (enc h2-1 (exp h2-1 x-5) (exp h2-0 (mul x-0 (rec x-5) x-7)) b-1
          (pubk a-1))) (send (enc (exp h2-0 (mul x-0 x-7)) (pubk b-1))))
    ((send (enc (exp (gen) x-8) a-2 (pubk b-2)))
      (recv
        (enc (exp (gen) x-1) (exp (gen) (mul x-1 x-8))
          (exp h2-1 (mul x-2 x-5 (rec x-8))) b-2 (pubk a-2)))
      (send (enc (exp h2-1 (mul x-2 x-5)) (pubk b-2))))
    ((send (enc (exp (gen) x-4) a-3 (pubk b-3)))
      (recv
        (enc (exp h2-2 x-11) (exp h2-2 (mul x-4 x-11))
          (exp h2-1 (mul x-2 x-3 (rec x-4) x-5 (rec x-8))) b-3
          (pubk a-3)))
      (send (enc (exp h2-1 (mul x-2 x-3 x-5 (rec x-8))) (pubk b-3))))
    ((recv (enc (exp h2-2 (mul x-4 x-9)) a-4 (pubk b-4)))
      (send
        (enc (exp (gen) x-10) (exp h2-2 (mul x-4 x-9 x-10))
          (exp (gen) z) b-4 (pubk a-4)))))
  (label 108)
  (parent 43)
  (unrealized (1 1) (2 1) (4 1) (5 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 a-3 b-3 name)
    (h2 h3 h2-0 h2-1 base)
    (x x-0 x-1 x-2 x-3 x-4 x-5 x-6 x-7 x-8 x-9 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-4))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x (rec x-3) x-4))) (x x-3))
  (defstrand init 3 (a a-1) (b b-1) (h2 h2-1)
    (h3 (exp h2-0 (mul x-0 x-3 (rec x-5)))) (x x-5))
  (defstrand init 3 (a a-2) (b b-2) (h2 (exp (gen) x-1))
    (h3 (exp h2-1 (mul (rec x-2) x-5 x-6))) (x x-2))
  (defstrand init 3 (a a-3) (b b-3) (h2 (exp (gen) x-9))
    (h3 (exp h2-1 (mul (rec x-2) x-5 x-6 x-7 (rec x-8)))) (x x-8))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (4 1))
    ((2 2) (1 1)) ((3 0) (4 1)) ((3 2) (2 1)) ((4 2) (3 1)))
  (non-orig (privk a) (privk b))
  (uniq-gen x-2 x-3 x-4 x-5 x-8)
  (operation nonce-test (algebra-contracted (h2-2 (exp (gen) x-9)))
    (exp (gen) (mul x-8 x-9)) (4 1) (exp (gen) x-8))
  (traces
    ((send (enc (exp (gen) x-4) a (pubk b)))
      (recv (enc h2 (exp h2 x-4) h3 b (pubk a)))
      (send (enc (exp h3 x-4) (pubk b))))
    ((send (enc (exp (gen) x-3) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-3) (exp h2 (mul x (rec x-3) x-4)) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-4)) (pubk b-0))))
    ((send (enc (exp (gen) x-5) a-1 (pubk b-1)))
      (recv
        (enc h2-1 (exp h2-1 x-5) (exp h2-0 (mul x-0 x-3 (rec x-5))) b-1
          (pubk a-1))) (send (enc (exp h2-0 (mul x-0 x-3)) (pubk b-1))))
    ((send (enc (exp (gen) x-2) a-2 (pubk b-2)))
      (recv
        (enc (exp (gen) x-1) (exp (gen) (mul x-1 x-2))
          (exp h2-1 (mul (rec x-2) x-5 x-6)) b-2 (pubk a-2)))
      (send (enc (exp h2-1 (mul x-5 x-6)) (pubk b-2))))
    ((send (enc (exp (gen) x-8) a-3 (pubk b-3)))
      (recv
        (enc (exp (gen) x-9) (exp (gen) (mul x-8 x-9))
          (exp h2-1 (mul (rec x-2) x-5 x-6 x-7 (rec x-8))) b-3
          (pubk a-3)))
      (send (enc (exp h2-1 (mul (rec x-2) x-5 x-6 x-7)) (pubk b-3)))))
  (label 109)
  (parent 43)
  (unrealized (1 1) (2 1) (4 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 b-3 a-3 a-4 b-4 name)
    (h2 h3 h2-0 h2-1 h2-2 base)
    (x x-0 x-1 x-2 x-3 z x-4 x-5 x-6 x-7 x-8 x-9 x-10 x-11 x-12 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-5))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x x-5 (rec x-6)))) (x x-6))
  (defstrand init 3 (a a-1) (b b-1) (h2 (exp h2-1 x-2))
    (h3 (exp h2-0 (mul x-0 (rec x-4) x-6))) (x x-4))
  (defstrand init 3 (a a-2) (b b-2) (h2 (exp (gen) x-1))
    (h3 (exp h2-1 (mul (rec x-3) z x-4 (rec x-7) x-9 x-10))) (x x-7))
  (defstrand resp 2 (b b-3) (a a-3)
    (h1 (exp h2-1 (mul x-4 (rec x-7) x-9 x-10))) (y z) (z x-8))
  (defstrand init 3 (a a-4) (b b-4) (h2 h2-2)
    (h3 (exp h2-1 (mul x-4 (rec x-7) x-9 x-10 (rec x-11) (rec x-12))))
    (x x-12))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (5 1))
    ((2 2) (1 1)) ((3 0) (5 1)) ((3 2) (2 1)) ((4 1) (3 1))
    ((5 2) (4 0)))
  (ind-zero-in (z (exp h2-1 (mul x-4 (rec x-7) x-9 x-10)))
    (x-8 (exp h2-1 (mul x-4 (rec x-7) x-9 x-10))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-4 x-5 x-6 x-7 x-8 x-12)
  (operation nonce-test (added-strand init 3)
    (exp h2-1 (mul x-4 (rec x-7) x-9 x-10)) (4 0))
  (traces
    ((send (enc (exp (gen) x-5) a (pubk b)))
      (recv (enc h2 (exp h2 x-5) h3 b (pubk a)))
      (send (enc (exp h3 x-5) (pubk b))))
    ((send (enc (exp (gen) x-6) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-6) (exp h2 (mul x x-5 (rec x-6))) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-5)) (pubk b-0))))
    ((send (enc (exp (gen) x-4) a-1 (pubk b-1)))
      (recv
        (enc (exp h2-1 x-2) (exp h2-1 (mul x-2 x-4))
          (exp h2-0 (mul x-0 (rec x-4) x-6)) b-1 (pubk a-1)))
      (send (enc (exp h2-0 (mul x-0 x-6)) (pubk b-1))))
    ((send (enc (exp (gen) x-7) a-2 (pubk b-2)))
      (recv
        (enc (exp (gen) x-1) (exp (gen) (mul x-1 x-7))
          (exp h2-1 (mul (rec x-3) z x-4 (rec x-7) x-9 x-10)) b-2
          (pubk a-2)))
      (send (enc (exp h2-1 (mul (rec x-3) z x-4 x-9 x-10)) (pubk b-2))))
    ((recv (enc (exp h2-1 (mul x-4 (rec x-7) x-9 x-10)) a-3 (pubk b-3)))
      (send
        (enc (exp (gen) z) (exp h2-1 (mul z x-4 (rec x-7) x-9 x-10))
          (exp (gen) x-8) b-3 (pubk a-3))))
    ((send (enc (exp (gen) x-12) a-4 (pubk b-4)))
      (recv
        (enc h2-2 (exp h2-2 x-12)
          (exp h2-1 (mul x-4 (rec x-7) x-9 x-10 (rec x-11) (rec x-12)))
          b-4 (pubk a-4)))
      (send
        (enc (exp h2-1 (mul x-4 (rec x-7) x-9 x-10 (rec x-11)))
          (pubk b-4)))))
  (label 110)
  (parent 44)
  (unrealized (1 1) (2 1) (5 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 b-3 a-3 b-4 a-4 name)
    (h2 h3 h2-0 h2-1 base)
    (x x-0 x-1 x-2 x-3 z z-0 x-4 x-5 x-6 y x-7 x-8 x-9 x-10 x-11 x-12
      x-13 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-5))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x x-5 (rec x-6)))) (x x-6))
  (defstrand init 3 (a a-1) (b b-1) (h2 (exp h2-1 (mul x-2 x-12)))
    (h3 (exp h2-0 (mul x-0 (rec x-4) x-6))) (x x-4))
  (defstrand init 3 (a a-2) (b b-2) (h2 (exp (gen) x-1))
    (h3
      (exp h2-1
        (mul (rec x-3) z-0 x-4 (rec y) x-8 x-9 (rec x-10) x-11 x-13)))
    (x y))
  (defstrand resp 2 (b b-3) (a a-3)
    (h1 (exp h2-1 (mul x-4 (rec y) x-8 x-9 (rec x-10) x-11 x-13)))
    (y z-0) (z x-7))
  (defstrand resp 2 (b b-4) (a a-4)
    (h1 (exp h2-1 (mul x-4 (rec y) x-8 x-9 (rec x-10)))) (y x-11) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (5 0))
    ((2 2) (1 1)) ((3 0) (5 0)) ((3 2) (2 1)) ((4 1) (3 1))
    ((5 1) (4 0)))
  (ind-zero-in (x-11 (exp h2-1 (mul x-4 (rec y) x-8 x-9 (rec x-10))))
    (z (exp h2-1 (mul x-4 (rec y) x-8 x-9 (rec x-10))))
    (z-0 (exp h2-1 (mul x-4 (rec y) x-8 x-9 (rec x-10) x-11 x-13)))
    (x-7 (exp h2-1 (mul x-4 (rec y) x-8 x-9 (rec x-10) x-11 x-13))))
  (non-orig (privk a) (privk b))
  (uniq-gen z z-0 x-4 x-5 x-6 y x-7 x-11)
  (operation nonce-test (added-strand resp 2)
    (exp h2-1 (mul x-4 (rec y) x-8 x-9 (rec x-10) x-11 x-13)) (4 0))
  (traces
    ((send (enc (exp (gen) x-5) a (pubk b)))
      (recv (enc h2 (exp h2 x-5) h3 b (pubk a)))
      (send (enc (exp h3 x-5) (pubk b))))
    ((send (enc (exp (gen) x-6) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-6) (exp h2 (mul x x-5 (rec x-6))) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-5)) (pubk b-0))))
    ((send (enc (exp (gen) x-4) a-1 (pubk b-1)))
      (recv
        (enc (exp h2-1 (mul x-2 x-12)) (exp h2-1 (mul x-2 x-4 x-12))
          (exp h2-0 (mul x-0 (rec x-4) x-6)) b-1 (pubk a-1)))
      (send (enc (exp h2-0 (mul x-0 x-6)) (pubk b-1))))
    ((send (enc (exp (gen) y) a-2 (pubk b-2)))
      (recv
        (enc (exp (gen) x-1) (exp (gen) (mul x-1 y))
          (exp h2-1
            (mul (rec x-3) z-0 x-4 (rec y) x-8 x-9 (rec x-10) x-11
              x-13)) b-2 (pubk a-2)))
      (send
        (enc
          (exp h2-1
            (mul (rec x-3) z-0 x-4 x-8 x-9 (rec x-10) x-11 x-13))
          (pubk b-2))))
    ((recv
       (enc (exp h2-1 (mul x-4 (rec y) x-8 x-9 (rec x-10) x-11 x-13))
         a-3 (pubk b-3)))
      (send
        (enc (exp (gen) z-0)
          (exp h2-1 (mul z-0 x-4 (rec y) x-8 x-9 (rec x-10) x-11 x-13))
          (exp (gen) x-7) b-3 (pubk a-3))))
    ((recv
       (enc (exp h2-1 (mul x-4 (rec y) x-8 x-9 (rec x-10))) a-4
         (pubk b-4)))
      (send
        (enc (exp (gen) x-11)
          (exp h2-1 (mul x-4 (rec y) x-8 x-9 (rec x-10) x-11))
          (exp (gen) z) b-4 (pubk a-4)))))
  (label 111)
  (parent 44)
  (unrealized (1 1) (2 1) (5 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 b-3 a-3 name) (h2 h3 h2-0 base)
    (x x-0 x-1 z x-2 x-3 x-4 x-5 x-6 x-7 x-8 x-9 x-10 x-11 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-4))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x (rec x-3) x-4))) (x x-3))
  (defstrand init 3 (a a-1) (b b-1)
    (h2 (exp (gen) (mul x-2 (rec x-6) (rec x-7) x-9 x-11)))
    (h3 (exp h2-0 (mul x-0 x-3 (rec x-5)))) (x x-5))
  (defstrand init 3 (a a-2) (b b-2) (h2 (exp (gen) x-1))
    (h3 (exp (gen) (mul x-5 x-8 (rec x-10) x-11))) (x x-2))
  (defstrand resp 2 (b b-3) (a a-3) (h1 (exp (gen) (mul x-5 x-11)))
    (y x-8) (z z))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (4 0))
    ((2 2) (1 1)) ((3 0) (4 0)) ((3 2) (2 1)) ((4 1) (3 1)))
  (ind-zero-in (x-8 (exp (gen) (mul x-5 x-11)))
    (z (exp (gen) (mul x-5 x-11))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-2 x-3 x-4 x-5 x-8)
  (operation nonce-test
    (algebra-contracted
      (h2-1 (exp (gen) (mul x-2 (rec x-6) (rec x-7) x-11))))
    (exp (gen) (mul x-5 x-11)) (4 0))
  (traces
    ((send (enc (exp (gen) x-4) a (pubk b)))
      (recv (enc h2 (exp h2 x-4) h3 b (pubk a)))
      (send (enc (exp h3 x-4) (pubk b))))
    ((send (enc (exp (gen) x-3) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-3) (exp h2 (mul x (rec x-3) x-4)) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-4)) (pubk b-0))))
    ((send (enc (exp (gen) x-5) a-1 (pubk b-1)))
      (recv
        (enc (exp (gen) (mul x-2 (rec x-6) (rec x-7) x-9 x-11))
          (exp (gen) (mul x-2 x-5 (rec x-6) (rec x-7) x-9 x-11))
          (exp h2-0 (mul x-0 x-3 (rec x-5))) b-1 (pubk a-1)))
      (send (enc (exp h2-0 (mul x-0 x-3)) (pubk b-1))))
    ((send (enc (exp (gen) x-2) a-2 (pubk b-2)))
      (recv
        (enc (exp (gen) x-1) (exp (gen) (mul x-1 x-2))
          (exp (gen) (mul x-5 x-8 (rec x-10) x-11)) b-2 (pubk a-2)))
      (send
        (enc (exp (gen) (mul x-2 x-5 x-8 (rec x-10) x-11)) (pubk b-2))))
    ((recv (enc (exp (gen) (mul x-5 x-11)) a-3 (pubk b-3)))
      (send
        (enc (exp (gen) x-8) (exp (gen) (mul x-5 x-8 x-11))
          (exp (gen) z) b-3 (pubk a-3)))))
  (label 112)
  (parent 44)
  (unrealized (1 1) (2 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 a-2 b-2 name) (h2 h3 h2-0 base)
    (x x-0 x-1 x-2 x-3 x-4 x-5 x-6 x-7 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-1))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x x-1 (rec x-2)))) (x x-2))
  (defstrand init 3 (a a-1) (b b-1) (h2 (exp (gen) (mul x-4 x-7)))
    (h3 (exp h2-0 (mul x-0 x-2 (rec x-7)))) (x x-7))
  (defstrand init 3 (a a-2) (b b-2) (h2 (exp (gen) x-3))
    (h3 (exp (gen) (mul x-5 x-6))) (x x-6))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (3 1))
    ((2 2) (1 1)) ((3 2) (2 1)))
  (non-orig (privk a) (privk b))
  (uniq-gen x-1 x-2 x-6 x-7)
  (operation nonce-test
    (algebra-contracted (x-8 x-7)
      (x-9 (mul (rec x-4) x-5 x-6 x-6 (rec x-7) (rec x-7))) (x-10 x-6)
      (x-11 x-5) (x-12 x-4)) (exp (gen) (mul x-4 x-7)) (2 1))
  (traces
    ((send (enc (exp (gen) x-1) a (pubk b)))
      (recv (enc h2 (exp h2 x-1) h3 b (pubk a)))
      (send (enc (exp h3 x-1) (pubk b))))
    ((send (enc (exp (gen) x-2) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-2) (exp h2 (mul x x-1 (rec x-2))) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-1)) (pubk b-0))))
    ((send (enc (exp (gen) x-7) a-1 (pubk b-1)))
      (recv
        (enc (exp (gen) (mul x-4 x-7)) (exp (gen) (mul x-4 x-7 x-7))
          (exp h2-0 (mul x-0 x-2 (rec x-7))) b-1 (pubk a-1)))
      (send (enc (exp h2-0 (mul x-0 x-2)) (pubk b-1))))
    ((send (enc (exp (gen) x-6) a-2 (pubk b-2)))
      (recv
        (enc (exp (gen) x-3) (exp (gen) (mul x-3 x-6))
          (exp (gen) (mul x-5 x-6)) b-2 (pubk a-2)))
      (send (enc (exp (gen) (mul x-5 x-6 x-6)) (pubk b-2)))))
  (label 113)
  (parent 45)
  (seen 113)
  (unrealized (1 1) (2 1))
  (comment "1 in cohort - 0 not yet seen"))

(defskeleton dhnsl
  (vars (a b a-0 b-0 a-1 b-1 b-2 a-2 a-3 b-3 a-4 b-4 a-5 b-5 name)
    (h2 h3 h2-0 h2-1 h2-2 h2-3 h2-4 base)
    (x x-0 x-1 x-2 x-3 x-4 z x-5 x-6 x-7 x-8 x-9 x-10 x-11 x-12 expn))
  (defstrand init 3 (a a) (b b) (h2 h2) (h3 h3) (x x-7))
  (defstrand init 3 (a a-0) (b b-0) (h2 h2-0)
    (h3 (exp h2 (mul x (rec x-6) x-7))) (x x-6))
  (defstrand init 3 (a a-1) (b b-1) (h2 (exp h2-1 x-1))
    (h3 (exp h2-0 (mul x-0 x-6 (rec x-8)))) (x x-8))
  (defstrand resp 2 (b b-2) (a a-2) (h1 (exp h2-1 (mul x-2 x-8)))
    (y x-5) (z x-9))
  (defstrand init 3 (a a-3) (b b-3) (h2 h2-2)
    (h3 (exp h2-1 (mul x-2 x-3 x-8 (rec x-10)))) (x x-10))
  (defstrand init 3 (a a-4) (b b-4) (h2 h2-3)
    (h3 (exp h2-2 (mul x-4 (rec z) x-10))) (x z))
  (defstrand init 3 (a a-5) (b b-5) (h2 h2-4)
    (h3 (exp h2-3 (mul z x-11 (rec x-12)))) (x x-12))
  (precedes ((0 0) (1 1)) ((1 0) (2 1)) ((1 2) (0 1)) ((2 0) (4 1))
    ((2 2) (1 1)) ((3 1) (2 1)) ((4 0) (5 1)) ((4 2) (3 0))
    ((5 0) (6 1)) ((5 2) (4 1)) ((6 2) (5 1)))
  (ind-zero-in (x-5 (exp h2-1 (mul x-2 x-8)))
    (x-9 (exp h2-1 (mul x-2 x-8))))
  (non-orig (privk a) (privk b))
  (uniq-gen z x-5 x-6 x-7 x-8 x-9 x-10 x-12)
  (operation nonce-test (added-strand init 3) (exp h2-3 z) (5 1)
    (exp (gen) z))
  (traces
    ((send (enc (exp (gen) x-7) a (pubk b)))
      (recv (enc h2 (exp h2 x-7) h3 b (pubk a)))
      (send (enc (exp h3 x-7) (pubk b))))
    ((send (enc (exp (gen) x-6) a-0 (pubk b-0)))
      (recv
        (enc h2-0 (exp h2-0 x-6) (exp h2 (mul x (rec x-6) x-7)) b-0
          (pubk a-0))) (send (enc (exp h2 (mul x x-7)) (pubk b-0))))
    ((send (enc (exp (gen) x-8) a-1 (pubk b-1)))
      (recv
        (enc (exp h2-1 x-1) (exp h2-1 (mul x-1 x-8))
          (exp h2-0 (mul x-0 x-6 (rec x-8))) b-1 (pubk a-1)))
      (send (enc (exp h2-0 (mul x-0 x-6)) (pubk b-1))))
    ((recv (enc (exp h2-1 (mul x-2 x-8)) a-2 (pubk b-2)))
      (send
        (enc (exp (gen) x-5) (exp h2-1 (mul x-2 x-5 x-8))
          (exp (gen) x-9) b-2 (pubk a-2))))
    ((send (enc (exp (gen) x-10) a-3 (pubk b-3)))
      (recv
        (enc h2-2 (exp h2-2 x-10)
          (exp h2-1 (mul x-2 x-3 x-8 (rec x-10))) b-3 (pubk a-3)))
      (send (enc (exp h2-1 (mul x-2 x-3 x-8)) (pubk b-3))))
    ((send (enc (exp (gen) z) a-4 (pubk b-4)))
      (recv
        (enc h2-3 (exp h2-3 z) (exp h2-2 (mul x-4 (rec z) x-10)) b-4
          (pubk a-4)))
      (send (enc (exp h2-2 (mul x-4 x-10)) (pubk b-4))))
    ((send (enc (exp (gen) x-12) a-5 (pubk b-5)))
      (recv
        (enc h2-4 (exp h2-4 x-12) (exp h2-3 (mul z x-11 (rec x-12))) b-5
          (pubk a-5))) (send (enc (exp h2-3 (mul z x-11)) (pubk b-5)))))
  (label 114)
  (parent 46)
  (unrealized (1 1) (2 1) (4 1) (5 1) (6 1))
  (comment "3 in cohort - 3 not yet seen"))
