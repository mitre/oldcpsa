(herald "Plain diffie-hellman protocol with challenge-response"
  (algebra diffie-hellman))

(comment "CPSA 3.2.2")
(comment "All input read from plaindh.scm")

(defprotocol plaindh diffie-hellman
  (defrole init
    (vars (x expn) (h base) (n text))
    (trace (send (exp (gen) x)) (recv h) (send (enc n (exp h x)))
      (recv n))
    (uniq-orig n)
    (uniq-gen x))
  (defrole resp
    (vars (y expn) (h base) (n text))
    (trace (recv h) (send (exp (gen) y)) (recv (enc n (exp h y)))
      (send n))
    (uniq-gen y)
    (ind-zero-in (y h)))
  (comment "Diffie-hellman key exchange followed by an encryption"))

(defskeleton plaindh
  (vars (n text) (h base) (x expn))
  (defstrand init 4 (n n) (h h) (x x))
  (uniq-gen x)
  (uniq-orig n)
  (traces
    ((send (exp (gen) x)) (recv h) (send (enc n (exp h x))) (recv n)))
  (label 0)
  (unrealized (0 3))
  (origs (n (0 2)))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton plaindh
  (vars (n text) (h base) (x y expn))
  (defstrand init 4 (n n) (h (exp h y)) (x x))
  (defstrand resp 4 (n n) (h (exp h x)) (y y))
  (precedes ((0 0) (1 0)) ((0 2) (1 2)) ((1 1) (0 1)) ((1 3) (0 3)))
  (ind-zero-in (y (exp h x)))
  (uniq-gen x y)
  (uniq-orig n)
  (operation nonce-test (added-strand resp 4) n (0 3)
    (enc n (exp h (mul x y))))
  (traces
    ((send (exp (gen) x)) (recv (exp h y))
      (send (enc n (exp h (mul x y)))) (recv n))
    ((recv (exp h x)) (send (exp (gen) y))
      (recv (enc n (exp h (mul x y)))) (send n)))
  (label 1)
  (parent 0)
  (unrealized (0 1) (1 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton plaindh
  (vars (n text) (h base) (x expn))
  (defstrand init 4 (n n) (h h) (x x))
  (deflistener (exp h x))
  (precedes ((0 0) (1 0)) ((1 1) (0 3)))
  (uniq-gen x)
  (uniq-orig n)
  (operation nonce-test (added-listener (exp h x)) n (0 3)
    (enc n (exp h x)))
  (traces
    ((send (exp (gen) x)) (recv h) (send (enc n (exp h x))) (recv n))
    ((recv (exp h x)) (send (exp h x))))
  (label 2)
  (parent 0)
  (unrealized (1 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton plaindh
  (vars (n text) (x y expn) (x-0 expr))
  (defstrand init 4 (n n) (h (exp (gen) (mul y x-0))) (x x))
  (defstrand resp 4 (n n) (h (exp (gen) (mul x x-0))) (y y))
  (precedes ((0 0) (1 0)) ((0 2) (1 2)) ((1 1) (0 1)) ((1 3) (0 3)))
  (ind-zero-in (y (exp (gen) (mul x x-0))))
  (uniq-gen x y)
  (uniq-orig n)
  (operation nonce-test (algebra-contracted (h (exp (gen) x-0)))
    (exp (gen) (mul x x-0)) (1 0) (exp (gen) x))
  (traces
    ((send (exp (gen) x)) (recv (exp (gen) (mul y x-0)))
      (send (enc n (exp (gen) (mul x y x-0)))) (recv n))
    ((recv (exp (gen) (mul x x-0))) (send (exp (gen) y))
      (recv (enc n (exp (gen) (mul x y x-0)))) (send n)))
  (label 3)
  (parent 1)
  (unrealized)
  (shape)
  (maps ((0) ((x x) (h (exp (gen) (mul y x-0))) (n n))))
  (origs (n (0 2))))

(defskeleton plaindh
  (vars (n text) (x expn) (x-0 expr))
  (defstrand init 4 (n n) (h (exp (gen) x-0)) (x x))
  (deflistener (exp (gen) (mul x x-0)))
  (precedes ((0 0) (1 0)) ((1 1) (0 3)))
  (uniq-gen x)
  (uniq-orig n)
  (operation nonce-test (algebra-contracted (h (exp (gen) x-0)))
    (exp (gen) (mul x x-0)) (1 0) (exp (gen) x))
  (traces
    ((send (exp (gen) x)) (recv (exp (gen) x-0))
      (send (enc n (exp (gen) (mul x x-0)))) (recv n))
    ((recv (exp (gen) (mul x x-0))) (send (exp (gen) (mul x x-0)))))
  (label 4)
  (parent 2)
  (unrealized)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton plaindh
  (vars (n text) (x expn) (x-0 expr))
  (defstrand init 4 (n n) (h (exp (gen) x-0)) (x x))
  (uniq-gen x)
  (uniq-orig n)
  (operation generalization deleted (1 0))
  (traces
    ((send (exp (gen) x)) (recv (exp (gen) x-0))
      (send (enc n (exp (gen) (mul x x-0)))) (recv n)))
  (label 5)
  (parent 4)
  (unrealized)
  (shape)
  (maps ((0) ((x x) (h (exp (gen) x-0)) (n n))))
  (origs (n (0 2))))

(comment "Nothing left to do")
