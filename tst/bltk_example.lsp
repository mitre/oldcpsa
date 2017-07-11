(herald "bltk example" (algebra diffie-hellman) 
  ;(noisochk)
(limit 100))

(defprotocol bltks diffie-hellman
	     (defrole init
		      (vars (I R name) (n m data))
		      (trace
			(send (enc I R n (bltk I R)))
			(recv (enc R I m (bltk I R))))
		      (uniq-orig n))
             (defrole resp
	              (vars (I R name) (n m data))
	              (trace
	        	(recv (enc I R n (bltk I R)))
	        	(send (enc R I m (bltk I R))))
	              (uniq-orig m))
             )

(defskeleton bltks
  (vars (I R name))
  (defstrand init 2 (I I) (R R))
  (non-orig (bltk I R)))

