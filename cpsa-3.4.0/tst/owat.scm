(herald "One-way Authentication Test with bltk keys"
	(algebra diffie-hellman)
	(bound 12))

(defprotocol owa diffie-hellman
  (defrole init
    (vars (a b name) (n text))
    (trace
     (send a)
     (recv n)
     (send (enc n (bltk a b)))))
  (defrole resp
    (vars (a b name) (n text))
    (trace
     (recv a)
     (send n)
     (recv (enc n (bltk a b))))))

(defskeleton owa
  (vars (a b name) (n text))
  (defstrand init 3 (a a) (b b) (n n))
  (non-orig (bltk a b)))

(defskeleton owa
  (vars (a b name) (n text))
  (defstrand resp 3 (a a) (b b) (n n))
  (non-orig (bltk a b))
  (uniq-orig n))
