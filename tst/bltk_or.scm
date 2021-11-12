(herald "Otway-Rees Protocol"
	(comment "Standard version using variables of sort mesg")
	(algebra diffie-hellman))

(defprotocol or diffie-hellman
  (defrole init (vars (a b s name) (na text) (k skey) (m text))
    (trace
     (send (cat m a b (enc na m a b (bltk a s))))
     (recv (cat m (enc na k (bltk a s))))))
  (defrole resp
    (vars (a b s name) (nb text) (k skey) (m text) (x y mesg))
    (trace
     (recv (cat m a b x))
     (send (cat m a b x (enc nb m a b (bltk b s))))
     (recv (cat m y (enc nb k (bltk b s))))
     (send y)))
  (defrole serv (vars (a b s name) (na nb text) (k skey) (m text))
    (trace
     (recv (cat m a b (enc na m a b (bltk a s))
		(enc nb m a b (bltk b s))))
     (send (cat m (enc na k (bltk a s)) (enc nb k (bltk b s)))))
    (uniq-orig k)))

(defskeleton or
  (vars (nb text) (s a b name))
  (defstrand resp 4 (a a) (b b) (s s) (nb nb))
  (non-orig (bltk a s) (bltk b s))
  (uniq-orig nb))
