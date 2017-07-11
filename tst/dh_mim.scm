(herald "Diffie-Hellman protocol, man-in-the-middle attack" (algebra diffie-hellman)
; (limit 1)
)

(defprotocol dh_mim diffie-hellman
  (defrole init
    (vars (x expn) (h base) (n text))
    (trace (send (exp (gen) x)) (recv h) (send (enc n (exp h x))))
;    (uniq-orig n)
    (uniq-gen x))
  (defrole resp
    (vars (y expn) (h base) (n text))
    (trace (recv h) (send (exp (gen) y)) (recv (enc n (exp h y))))
    (uniq-gen y))
  (comment "Diffie-hellman key exchange followed by an encryption"))

(comment
(defskeleton dh_mim
  (vars (n text) (hx base) (y x expn))
  (defstrand init 3 (n n) (h (exp hx (mul (rec y) x))) (x y))
  (defstrand resp 3 (n n) (h hx) (y x))
  (precedes ((0 2) (1 2)) ((1 1) (0 1)))
  (pen-non-orig y x)
  (uniq-gen y x)
  (uniq-orig n)
  (operation encryption-test (displaced 2 0 init 3)
    (enc n (exp hx (mul y-0 x-0))) (1 2))
  (traces
    ((send (exp (gen) y)) (recv (exp hx (mul (rec y) x)))
      (send (enc n (exp hx x))))
    ((recv hx) (send (exp (gen) x)) (recv (enc n (exp hx x)))))
  (label 1)
  (parent 0)
  (unrealized (0 1))
  (origs (n (0 2)))
  (comment "empty cohort"))
)

;(comment
(defskeleton dh_mim
  (vars (n text) (hx hy base) (x y expn))
  (defstrand init 3 (n n) (h hy) (x x))
  (defstrand resp 3 (n n) (h hx) (y y))
  (precedes ((0 2) (1 2)))
  (uniq-orig n)
  (pen-non-orig x y)
  (comment "Agreement on the encrypted text only"))
;)

(comment
(defskeleton dh_mim
  (vars (n text) (x z y expn))
  (defstrand init 3 (n n) (h (exp (gen) (mul x z y))) (x y))
  (defstrand resp 3 (n n) (h (exp (gen) (mul z y y))) (y x))
  (precedes ((0 0) (1 0)) ((0 2) (1 2)) ((1 1) (0 1)))
  (pen-non-orig x y)
  (uniq-gen x y)
  (uniq-orig n))
)

(comment
(defskeleton dh_mim
  (vars (n text) (x y y-0 expn))
  (defstrand init 3 (n n) (h (exp (gen) (mul y y-0))) (x y-0))
  (defstrand resp 3 (n n) (h (exp (gen) (mul (rec x) y y-0 y-0))) (y x))
  (precedes ((0 0) (1 0)) ((0 2) (1 2)) ((1 1) (0 1)))
  (pen-non-orig x y-0)
  (uniq-gen x y-0)
  (uniq-orig n)
  (operation nonce-test (displaced 2 0 init 1) (exp (gen) (mul y x-0))
    (0 1))
  (traces
    ((send (exp (gen) y-0)) (recv (exp (gen) (mul y y-0)))
      (send (enc n (exp (gen) (mul y y-0 y-0)))))
    ((recv (exp (gen) (mul (rec x) y y-0 y-0))) (send (exp (gen) x))
      (recv (enc n (exp (gen) (mul y y-0 y-0))))))
  (label 3)
  (parent 1)
  (seen 3)
  (unrealized (1 0))
  (origs (n (0 2)))
  (comment "3 in cohort - 2 not yet seen"))
)
