(herald thinning (algebra diffie-hellman))

;; This examples shows a problem with thinning.
;;
;; Skeleton 12 should lead to a shape, but a bug in thinning causes it
;; to be dead.  Adding one neq in the point-of-view fixes the problem,
;; and adding two causes the problem to re-emerge.  Skeleton 36 is
;; what is being erroneously thinned in the first run.

;; Blindvault provisioner
;;
;; Diffie-Hellman algebra used so that bltk is available.

(defprotocol provision diffie-hellman

  (defrole supplicant
    (vars (self pal admin name) (meas text) (count data))
    (trace
     (send (enc "endpoint" self pal meas (pubk self) (pubk admin)))
     (recv (enc "admit"
		(enc "admitted" count meas (hash (bltk self pal)) (privk admin))
		(bltk self pal)
		(pubk self))))
    (uniq-orig meas))

  (defrole admission
    (vars (self pal admin counter name)
	  (meas-self meas-pal text) (token count data))
    (trace
     (recv (enc "endpoint" self pal meas-self (pubk self) (pubk admin)))
     (send (enc "incr" token (pubk counter)))
     (recv (enc "count" count token (privk counter)))
     (send (enc "admit"
		(enc "admitted" count meas-self
		     (hash (bltk self pal)) (privk admin))
		(bltk self pal)
		(pubk self))))
    (uniq-orig token)
    (non-orig (privk counter)))

  (defrole counter
    (vars (t n data) (k akey))
    (trace
     (recv (enc "incr" t k))
     (send (enc "count" n t (invk k))))
    (uniq-orig n))

  (defrule one-counter
    (forall ((z w strd) (counter1 counter2 name))
	    (implies
	     (and (p "admission" z 2)
		  (p "admission" "counter" z counter1)
		  (p "admission" w 2)
		  (p "admission" "counter" w counter2))
	     (= counter1 counter2))))

  )

(defskeleton provision
  (vars (left right admin name))
  (defstrand supplicant 2 (self left) (pal right) (admin admin))
  (defstrand supplicant 2 (self right) (pal left) (admin admin))
  (pen-non-orig (bltk left right))
  ;; (neq (left right))  ;; One fixes the problem.
  ;; (neq (right left))  ;; Two makes the problem reappear.
  (non-orig (privk left) (privk right) (privk admin)))


(defskeleton provision
  (vars (left right admin name))
  (defstrand supplicant 2 (self left) (pal right) (admin admin))
  (defstrand supplicant 2 (self right) (pal left) (admin admin))
  (pen-non-orig (bltk left right))
  (neq (left right)) ;; One fixes the problem.
  (non-orig (privk left) (privk right) (privk admin)))


(defskeleton provision
  (vars (left right admin name))
  (defstrand supplicant 2 (self left) (pal right) (admin admin))
  (defstrand supplicant 2 (self right) (pal left) (admin admin))
  (pen-non-orig (bltk left right))
  (neq (left right)) ;; One fixes the problem.
  (neq (right left)) ;; Two makes the problem reappear.
  (non-orig (privk left) (privk right) (privk admin)))
