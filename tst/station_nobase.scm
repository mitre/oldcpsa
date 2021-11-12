(herald "Station-to-station protocol no base vars" (algebra diffie-hellman)
; (limit 1)
)

(defprotocol station-to-station diffie-hellman
  (defrole init
    (vars (x rndx) (h expt) (a b name))
    (trace
     (send (exp (gen) x))
     (recv (cat (exp (gen) h)
		(enc (enc (exp (gen) h) (exp (gen) x) (privk b))
		     (exp (exp (gen) h) x))))
     (send (enc (enc (exp (gen) x) (exp (gen) h) (privk a))
		(exp (exp (gen) h) x))))
    (uniq-gen x))
  (defrole resp
    (vars (y rndx) (h expt) (a b name))
    (trace
     (recv (exp (gen) h))
     (send (cat (exp (gen) y)
		(enc (enc (exp (gen) y) (exp (gen) h) (privk b))
		     (exp (exp (gen) h) y))))
     (recv (enc (enc (exp (gen) h) (exp (gen) y) (privk a))
		(exp (exp (gen) h) y))))
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
