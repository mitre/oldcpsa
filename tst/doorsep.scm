(herald doorsep (comment "Door Simple Example Protocol"))

(defprotocol doorsep basic
  (defrole person
    (vars (p d akey) (k skey) (t text))
    (trace
     (send (enc (enc k (invk p)) d))
     (recv (enc t k))
     (send t))
    (uniq-orig k)
    )
  (defrole door
    (vars (p d akey) (k skey) (t text))
    (trace
     (recv (enc (enc k (invk p)) d))
     (send (enc t k))
     (recv t))
    )
  (defrule trust
    (forall ((z strd) (p d akey) (k skey))
	    (implies
	     (and (p "person" z 1)
		  (p "person" "p" z p)
		  (p "person" "d" z d)
		  (p "person" "k" z k)
		  (fact trust p))
	     (and (non (invk d))
		  (uniq k))))))

(defskeleton doorsep
  (vars (p akey) (t text))
  (defstrand door 3 (p p) (t t))
  (non-orig (invk p))
  (uniq-orig t)
  (facts (trust p))
  (comment "Analyze from the door's perspective"))

(defskeleton doorsep
  (vars (p akey) (t text))
  (defstrand door 3 (p p) (t t))
  (non-orig (invk p))
  (uniq-orig t)
  (comment "Analyze from the door's perspective when we don't trust p"))
