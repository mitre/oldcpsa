(herald "Diffie-Hellman protocol, man-in-the-middle attack"
  (algebra diffie-hellman))

(comment "CPSA 3.6.11")
(comment "All input read from tst/dh_mim_nobase.scm")

(defprotocol dh_mim_nobase diffie-hellman
  (defrole init
    (vars (x rndx) (h expt) (n text))
    (trace (send (exp (gen) x)) (recv (exp (gen) h))
      (send (enc n (exp (gen) (mul x h)))))
    (uniq-gen x))
  (defrole resp
    (vars (y rndx) (h expt) (n text))
    (trace (recv (exp (gen) h)) (send (exp (gen) y))
      (recv (enc n (exp (gen) (mul y h)))))
    (uniq-gen y)
    (absent (y (exp (gen) h))))
  (comment "Diffie-hellman key exchange followed by an encryption"))

(defskeleton dh_mim_nobase
  (vars (n text) (hx hy expt) (x y rndx))
  (defstrand init 3 (n n) (x x) (h hy))
  (defstrand resp 3 (n n) (y y) (h hx))
  (precedes ((0 2) (1 2)))
  (absent (y (exp (gen) hx)))
  (pen-non-orig x y)
  (uniq-gen x y)
  (uniq-orig n)
  (comment "Agreement on the encrypted text only")
  (traces
    ((send (exp (gen) x)) (recv (exp (gen) hy))
      (send (enc n (exp (gen) (mul hy x)))))
    ((recv (exp (gen) hx)) (send (exp (gen) y))
      (recv (enc n (exp (gen) (mul hx y))))))
  (label 0)
  (unrealized)
  (shape)
  (maps ((0 1) ((n n) (hx hx) (hy hy) (x x) (y y))))
  (origs (n (0 2))))

(comment "Nothing left to do")
