(herald "Wrap-Decrypt example"
	(bound 24)
	(limit 2000)
	(goals-sat)
	)

;; In this version, the redirect URI is registered ahead of time
;; and is not carried in the auth or access messages

(defprotocol wrap-decrypt basic
   (defrole create-key
     (vars (k skey))
     (trace
      (recv (cat "CREATE"))
      (send (cat "CREATED" (hash k)))
      (init (cat "init" k)))
     (uniq-gen k)
     )
   (defrole set-wrap
     (vars (k skey) (type mesg))
     (trace
      (recv (cat "SET-WRAP" (hash k)))
      (tran (cat type k) (cat "wrap" k))
      (send (cat "SET-WRAP-DONE"))
      ))
   (defrole set-decrypt
     (vars (k skey) (type mesg))
     (trace
      (recv (cat "SET-DECRYPT" (hash k)))
      (tran (cat type k) (cat "decrypt" k))
      (send (cat "SET-DECRYPT-DONE"))
      ))
   (defrole do-decrypt
     (vars (k skey) (m mesg))
     (trace
      (recv (cat "DECRYPT" (hash k) (enc m k)))
      (obsv (cat "decrypt" k))
      (send m)))
   (defrole do-wrap
     (vars (wk ck skey) (cktype mesg))
     (trace
      (recv (cat "WRAP" (hash wk) (hash ck)))
      (obsv (cat "wrap" wk))
      (obsv (cat cktype ck))
      (send (enc ck wk))
      )
    )
   ;; --------- RULES -------------
   (defrule no-key-cycle
     (forall ((z strd) (k skey))
	     (implies
	      (and
	       (p "do-wrap" z 4)
	       (p "do-wrap" "ck" z k)
	       (p "do-wrap" "wk" z k)
	       )
	      (fact falseFact))))

   (defrule no-set-wrap-noops
     (forall ((z strd))
	     (implies
	      (and
	       (p "set-wrap" z 2)
	       (p "set-wrap" "type" z "wrap")
	       )
	      (fact falseFact))))

   (defrule no-set-decrypt-noops
     (forall ((z strd))
	     (implies
	      (and
	       (p "set-decrypt" z 2)
	       (p "set-decrypt" "type" z "decrypt")
	       )
	      (fact falseFact))))

   (defrule secure-mode
     (forall ((z strd))
     (implies
      (and
       (fact secureMode)
       (p "set-decrypt" z 1)
       (p "set-decrypt" "type" z "wrap")
	   )
      (fact falseFact))))

   (defrule conclusion-of-simple
     (forall ((z0 z1 strd) (k skey))
	     (implies
	      (and
	       (p "create-key" z0 3)
		   (p "create-key" "k" z0 k)
		   (p "" z1 1)
		   (p "" "x" z1 k)
		   )
	      ;; A key was used to decrypt after wrapping.
;	      (fact falseFact))))
	      (exists ((z2 z3 strd) (k2 skey))
		      (and
		       (p "do-decrypt" z2 1)
		       (p "do-decrypt" "k" z2 k2)
		       (p "do-wrap" z3 4)
		       (p "do-wrap" "wk" z3 k2)
		       (prec z3 3 z2 0)
		       (prec z2 0 z1 0)
		       )))))
   )

(comment
(defskeleton wrap-decrypt
  (vars (k skey))
  (defstrand do-decrypt 3 (k k))
  (defstrand do-wrap 3 (wk k))
  (facts (secureMode))
  (precedes ((1 2) (0 0))))
)

;;; Defgoals
;;; Secure mode
(defgoal wrap-decrypt
  (forall ((z0 z1 strd) (k skey))
	  (implies
	   (and
	    (fact secureMode)
	    (p "do-decrypt" z0 3)
	    (p "do-decrypt" "k" z0 k)
	    (p "do-wrap" z1 4)
	    (p "do-wrap" "wk" z1 k)
	    (prec z1 3 z0 0))
;;; Infinite descent: implies there exists two copies of decrypt followed by wrap, one before the other.
	   (or
	    (fact falseFact)
	   (exists
	    ((z2 z3 z4 z5 strd) (k0 k1 skey))
	    (and
	     (p "do-decrypt" z2 3)
	     (p "do-decrypt" "k" z2 k0)
	     (p "do-wrap" z3 4)
	     (p "do-wrap" "wk" z3 k0)
	     (prec z2 3 z3 0)
	     (p "do-decrypt" z4 3)
	     (p "do-decrypt" "k" z4 k1)
	     (p "do-wrap" z5 4)
	     (p "do-wrap" "wk" z5 k1)
	     (prec z4 3 z5 0)
	     (prec z2 3 z4 0))
	    )))))
