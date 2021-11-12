(herald "Station-to-station protocol" (algebra diffie-hellman)
; (limit 1)
)

(defprotocol station-to-station diffie-hellman
  (defrole init
    (vars (x rndx) (y expt) (a b name))
    (trace
     (send (exp (gen) x))
     (recv (cat (exp (gen) y) (enc (enc (exp (gen) y) (exp (gen)
x) (privk b)) (exp (gen) (mul x y)))))
     (send (enc (enc (exp (gen) x) (exp (gen) y) (privk a)) (exp (gen) (mul x y)))))
    (uniq-gen x))
  (defrole resp
    (vars (y rndx) (x expt) (a b name))
    (trace
     (recv (exp (gen) x))
     (send (cat (exp (gen) y) (enc (enc (exp (gen) y) (exp (gen)
x) (privk b)) (exp (gen) (mul x y)))))
     (recv (enc (enc (exp (gen) x) (exp (gen) y) (privk a)) (exp (gen) (mul x y)))))
    (uniq-gen y))
)

(defskeleton station-to-station
  (vars (a b name))
  (defstrand init 3 (a a) (b b))
  (non-orig (privk b) (privk a))
)

(defskeleton station-to-station
  (vars (a b name))
  (defstrand resp 3 (a a) (b b))
  (non-orig (privk a) (privk b))
)
