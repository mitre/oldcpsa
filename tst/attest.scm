(herald attest-door)

(defprotocol attest-door basic
  (defrole appraise
    (vars (d p a akey) (n text))
    (trace
     (recv (enc (enc d n (invk p)) a))
     (send (enc n a p)))
    (comment "The appraiser for the door"))
  (defrole person
    (vars (d p a akey) (k skey) (n t text))
    (trace
     (send (enc (enc d n (invk p)) a))
     (recv (enc n a p))
     (send (enc (enc k (invk p)) d))
     (recv (enc t k))
     (send t))
    (uniq-orig n k)
    (comment "Person requesting door entry"))
  (defrole door
    (vars (d p akey) (k skey) (t text))
    (trace
     (recv (enc (enc k (invk p)) d))
     (send (enc t k))
     (recv t))
    (uniq-orig t))
  (defrole squealer
    (vars (d p akey) (k skey) (t text))
    (trace
     (recv (enc (enc k (invk p)) d))
     (send k))
    (comment "Fake door"))
  (defrule yes
    (forall ((z strd) (a akey))
	    (implies
	     (and (p "appraise" z 2)
		  (p "appraise" "a" z a)
		  (non (invk a)))
	     (exists ((d akey))
		     (and (p "appraise" "d" z d)
			  (non (invk d))))))
    (comment "Appraisal succeeded"))
  (comment "Door attestations protocol"))

(defskeleton attest-door
  (vars (p a akey))
  (defstrand person 5 (p p) (a a))
  (non-orig (invk p) (invk a))
  (comment "Analyze from the person's perspective"))

(defprotocol attest-door-trust basic
  (defrole appraise
    (vars (d p a akey) (n text))
    (trace
     (recv (enc (enc d n (invk p)) a))
     (send (enc n a p)))
    (comment "The appraiser for the door"))
  (defrole person
    (vars (d p a akey) (k skey) (n t text))
    (trace
     (send (enc (enc d n (invk p)) a))
     (recv (enc n a p))
     (send (enc (enc k (invk p)) d))
     (recv (enc t k))
     (send t))
    (uniq-orig n k)
    (comment "Person requesting door entry"))
  (defrole door
    (vars (d p akey) (k skey) (t text))
    (trace
     (recv (enc (enc k (invk p)) d))
     (send (enc t k))
     (recv t))
    (uniq-orig t))
  (defrole squealer
    (vars (d p akey) (k skey) (t text))
    (trace
     (recv (enc (enc k (invk p)) d))
     (send k))
    (comment "Fake door"))
  (defrule yes
    (forall ((z strd) (a akey))
	    (implies
	     (and (p "appraise" z 2)
		  (p "appraise" "a" z a)
		  (non (invk a)))
	     (exists ((d akey))
		     (and (p "appraise" "d" z d)
			  (non (invk d))))))
    (comment "Appraisal succeeded"))
  (defrule trust
    (forall ((z w strd) (d akey))
	    (implies
	     (and (p "appraise" z 2)
		  (p "appraise" "d" z d)
		  (p "squealer" w 2)
		  (p "squealer" "d" w d))
	     (false)))
    (comment "Squealer prohibited due to attestation"))
  (comment "Door attestations protocol with attestation"))

(defskeleton attest-door-trust
  (vars (p a akey))
  (defstrand person 5 (p p) (a a))
  (non-orig (invk p) (invk a))
  (comment "Analyze from the person's perspective"))
