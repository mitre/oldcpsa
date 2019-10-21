(herald trust-anchor
	(comment "Tests rule application on initial skeleton"))

(defprotocol trust-anchor basic
  ;;; trust-anchor property guarantees non
  (defrule trust-anchor-inverse-is-non
    (forall ((k akey))
	    (implies (fact trust-anchor k)
		     (non (invk k))))))

(defskeleton trust-anchor
  (vars (f ca name))
  (deflistener (enc f (pubk f) (privk ca)))
  (non-orig (privk ca)))

(defskeleton trust-anchor
  (vars (f ca name))
  (deflistener (enc f (pubk f) (privk ca)))
  (facts (trust-anchor (pubk ca))))
