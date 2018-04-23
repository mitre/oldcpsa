(herald "Static DH key exchange"
  (algebra diffie-hellman))

(defprotocol staticdh diffie-hellman
  (defrole init
    (vars (b ca name) (h base) (x rndx) (n text))
    (trace
       (recv (enc "cert" h b (privk ca)))
       (send (enc n (exp h x)))
       (recv n))
    (uniq-orig n)
    (non-orig (privk ca) x))
  (defrole resp
    (vars (a ca name) (h base) (y rndx) (n text))
    (trace
       (recv (enc "cert" h a (privk ca)))
       (recv (enc n (exp h y)))
       (send n))
    (non-orig (privk ca) y))
  (defrole ca
    (vars (p ca name) (x rndx))
    (trace
       (send (enc "cert" (exp (gen) x) p (privk ca))))
    (non-orig x)
    (fn-of (owner-of (p x)))))

(comment
(defskeleton staticdh
  (vars )
  (defstrand init 3))
)

(defprotocol staticdh1 diffie-hellman
  (defrole init
    (vars (a b ca name) (h base) (x rndx) (n text))
    (trace
       (recv (enc "cert" (exp (gen) x) a (privk ca)))
       (recv (enc "cert" h b (privk ca)))
       (send (enc n (exp h x)))
       (recv n))
    (uniq-orig n)
    (neq (a b))
    (non-orig (privk ca)))
  (defrole resp
    (vars (a b ca name) (h base) (y rndx) (n text))
    (trace
       (recv (enc "cert" h a (privk ca)))
       (recv (enc "cert" (exp (gen) y) b (privk ca)))
       (recv (enc n (exp h y)))
       (send n))
    (neq (a b))
    (non-orig (privk ca)))
  (defrole ca
    (vars (p ca name) (x rndx) (n text))
    (trace
       (send (enc "cert" (exp (gen) x) p (privk ca))))
    (non-orig x)
    (fn-of (owner-of (p x)))))

(defskeleton staticdh1
  (vars )
  (defstrand init 4))
