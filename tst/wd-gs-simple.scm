(herald "Wrap-Decrypt example, simplified"
	(bound 24)
	(limit 500)
	(goals-sat)
	)

;; In this version, the redirect URI is registered ahead of time
;; and is not carried in the auth or access messages

(defprotocol wrap-decrypt-simple basic
   (defrole create-key
     (vars (k skey))
     (trace
      (recv (cat "CREATE"))
      (send (cat "CREATED" (hash k)))
      (init k))
     (uniq-gen k)
     )
   (defrole do-decrypt
     (vars (k skey) (m mesg))
     (trace
      (recv (cat "DECRYPT" (hash k) (enc m k)))
      (obsv k)
      (send m)))
   (defrole do-wrap
     (vars (wk ck skey) (cktype mesg))
     (trace
      (recv (cat "WRAP" (hash wk) (hash ck)))
      (obsv wk)
      (obsv ck)
      (send (enc ck wk))
      )
     (neq (ck wk))
    )
   ;; --------- RULES -------------
   )

(defgoal wrap-decrypt-simple
  (forall ((z0 z1 strd) (k skey))
  (implies
   (and (p "create-key" z0 3)
	(p "create-key" "k" z0 k)
	(p "" z1 1)
	(p "" "x" z1 k)
	)
   (or
    ;; This is not the first instance of a key leaking
    (exists ((z2 z3 z4 z5 strd) (k2 k3 skey))
	   (and
	    (p "create-key" z2 2)
	    (p "create-key" "k" z2 k2)
	    (p "create-key" z4 2)
	    (p "create-key" "k" z4 k3)
	    (p "" z3 1)
	    (p "" "x" z3 k2)
	    (p "" z5 1)
	    (p "" "x" z5 k3)
	    (prec z3 1 z5 0)))
    ;; A key was used to decrypt after wrapping.
    (exists ((z2 z3 strd) (k2 skey))
	    (and
	     (p "do-decrypt" z2 3)
	     (p "do-decrypt" "k" z2 k2)
	     (p "do-wrap" z3 4)
	     (p "do-wrap" "wk" z3 k2)
	     (prec z3 3 z2 0)
	     ))))))
