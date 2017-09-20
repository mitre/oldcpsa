(herald "Station-to-station protocol" (algebra diffie-hellman)
; (limit 1)
)

(defprotocol station-to-station diffie-hellman
  (defrole init
    (vars (x expn) (h base) (a b name))
    (trace
     (send (exp (gen) x))
     (recv (enc h (exp (gen) x) (privk b)))
     (send (enc (enc (exp (gen) x) h (exp h x)) (privk a))))
    (uniq-gen x))
  (defrole resp
    (vars (y expn) (h base) (a b name))
    (trace
     (recv h)
     (send (enc (exp (gen) y) h (privk b)))
     (recv (enc (enc h (exp (gen) y) (exp h y)) (privk a))))
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
