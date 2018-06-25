(herald "Plain diffie-hellman protocol with challenge-response"
  (algebra diffie-hellman))

(comment "CPSA 3.6.0")
(comment "All input read from plaindh.scm")

(defprotocol plaindh diffie-hellman
  (defrole init
    (vars (x rndx) (y expt) (n text))
    (trace (send (exp (gen) x)) (recv (exp (gen) y))
      (send (enc n (exp (gen) (mul x y)))) (recv n))
    (uniq-orig n)
    (uniq-gen x))
  (defrole resp
    (vars (y rndx) (x expt) (n text))
    (trace (recv (exp (gen) x)) (send (exp (gen) y))
      (recv (enc n (exp (gen) (mul y x)))) (send n))
    (uniq-gen y)
    (absent (y (exp (gen) x))))
  (comment "Diffie-hellman key exchange followed by an encryption"))

(defskeleton plaindh
  (vars (n text) (x rndx) (y expt))
  (defstrand init 4 (n n) (x x) (y y))
  (uniq-gen x)
  (uniq-orig n)
  (traces
    ((send (exp (gen) x)) (recv (exp (gen) y))
      (send (enc n (exp (gen) (mul x y)))) (recv n)))
  (label 0)
  (unrealized)
  (shape)
  (maps ((0) ((x x) (y y) (n n))))
  (origs (n (0 2))))

(comment "Nothing left to do")
