(herald "Plain diffie-hellman protocol with challenge-response" (algebra diffie-hellman)
; (limit 1)
)

(defprotocol plaindh diffie-hellman
  (defrole init
    (vars (x expn) (h base) (n text))
    (trace (send (exp (gen) x)) 
           (recv h) 
           (send (enc n (exp h x)))
           (recv n))
    (uniq-orig n)
    (uniq-gen x))
  (defrole resp
    (vars (y expn) (h base) (n text))
    (trace (recv h) 
           (send (exp (gen) y)) 
           (recv (enc n (exp h y)))
           (send n))
    (uniq-gen y))
  (comment "Diffie-hellman key exchange followed by an encryption"))


(defskeleton plaindh
  (vars )
  (defstrand init 4))
