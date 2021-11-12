(herald "Diffie-Hellman protocol, man-in-the-middle attack"
  (algebra diffie-hellman))

(comment "CPSA 3.6.8")
(comment "All input read from tst/dh_mim.scm")

(defprotocol dh_mim diffie-hellman
  (defrole init
    (vars (x rndx) (y expt) (n text))
    (trace (send (exp (gen) x)) (recv (exp (gen) y))
      (send (enc n (exp (gen) (mul x y)))))
    (uniq-gen x))
  (defrole resp
    (vars (y rndx) (x expt) (n text))
    (trace (recv (exp (gen) x)) (send (exp (gen) y))
      (recv (enc n (exp (gen) (mul y x)))))
    (uniq-gen y)
    (absent (y (exp (gen) x))))
  (comment "Diffie-hellman key exchange followed by an encryption"))

(defskeleton dh_mim
  (vars (n text) (x0 y0 expt) (x y rndx))
  (defstrand init 3 (n n) (x x) (y y0))
  (defstrand resp 3 (n n) (y y) (x x0))
  (precedes ((0 2) (1 2)))
  (absent (y (exp (gen) x0)))
  (pen-non-orig x y)
  (uniq-gen x y)
  (uniq-orig n)
  (comment "Agreement on the encrypted text only")
  (traces
    ((send (exp (gen) x)) (recv (exp (gen) y0))
      (send (enc n (exp (gen) (mul y0 x)))))
    ((recv (exp (gen) x0)) (send (exp (gen) y))
      (recv (enc n (exp (gen) (mul x0 y))))))
  (label 0)
  (unrealized)
  (shape)
  (maps ((0 1) ((n n) (x0 x0) (y0 y0) (x x) (y y))))
  (origs (n (0 2))))

(comment "Nothing left to do")
