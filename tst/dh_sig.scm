(herald "Signed DH exchange" (algebra diffie-hellman)
)

(defprotocol dh_sig diffie-hellman
  (defrole init
    (vars (x expn) (h base) (a b name))
    (trace 
     (send (exp (gen) x)) 
     (recv (enc h (exp (gen) x) (privk b)))
     (send (enc h (exp (gen) x) (privk a))))
    (uniq-gen x))
  (defrole resp
    (vars (y expn) (h base) (a b name))
    (trace 
     (recv h) 
     (send (enc (exp (gen) y) h (privk b)))
     (recv (enc (exp (gen) y) h (privk a))))
    (uniq-gen y))
)

(defskeleton dh_sig
  (vars (a b name))
  (defstrand init 3 (a a) (b b))
  (non-orig (privk b) (privk a))
)

(defskeleton dh_sig
  (vars (a b name))
  (defstrand resp 3 (a a) (b b))
  (non-orig (privk a) (privk b))
)


(defprotocol dh_sig2 diffie-hellman
  (defrole init
    (vars (x expn) (h base) (a b name))
    (trace 
     (send (exp (gen) x)) 
     (recv (enc h (exp (gen) x) (privk b)))
     (send (enc (exp (gen) x) h b (privk a))))
    (uniq-gen x))
  (defrole resp
    (vars (y expn) (h base) (a b name))
    (trace 
     (recv h) 
     (send (enc (exp (gen) y) h (privk b)))
     (recv (enc h (exp (gen) y) b (privk a))))
    (uniq-gen y))
)

(defskeleton dh_sig2
  (vars (a b name))
  (defstrand init 3 (a a) (b b))
  (non-orig (privk b) (privk a))
)

(defskeleton dh_sig2
  (vars (a b name))
  (defstrand resp 3 (a a) (b b))
  (non-orig (privk a) (privk b))
)

