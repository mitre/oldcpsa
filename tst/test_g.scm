(herald "small test" (algebra diffie-hellman)
 ;(limit 1)
)

(defprotocol test1 diffie-hellman
  (defrole init
    (vars (x rndx) (w expt) (k skey))
    (trace
     (send (enc (exp (gen) x) k))
     (recv (exp (gen) (mul x w))))
    (uniq-gen x)
  (non-orig k)
  )
)

;; Should need to use contraction with g to produce a shape.
(defskeleton test1
  (vars (k skey))
  (defstrand init 2 (k k))
  )
