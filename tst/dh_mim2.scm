(herald "Diffie-Hellman protocol, man-in-the-middle attack" (algebra diffie-hellman)
; (limit 1)
)

(defprotocol dh_mim diffie-hellman
  (defrole init
    (vars (x rndx) (y expt) (n text))
    (trace
     (send (exp (gen) x))
     (recv (exp (gen) y))
     (send (enc n (exp (gen) (mul y x)))))
    (neq (y (one)))
    (uniq-orig n)
    (uniq-gen x))
  (defrole resp
    (vars (y rndx) (x expt) (n text))
    (trace
     (recv (exp (gen) x))
     (send (exp (gen) y))
     (recv (enc n (exp (gen) (mul x y)))))
    (neq (x (one)))
    (uniq-gen y))
  (comment "Diffie-hellman key exchange followed by an encryption"))

;(comment
(defskeleton dh_mim
  (vars (n text) (x0 y0 expt) (x y rndx))
  (defstrand init 3 (n n) (y y0) (x x))
  (defstrand resp 3 (n n) (x x0) (y y))
  (precedes ((0 2) (1 2)))
  (uniq-orig n)
  (pen-non-orig x y)
  (comment "Agreement on the encrypted text only"))
;)

