(herald "Rules and Facts")

(defprotocol neq-test basic
  (defrole init
    (vars (n1 n2 text) (k skey))
    (trace
     (send (cat n1 (enc n1 n2 k)))
     (recv n2))
    (non-orig k)
    (uniq-orig n1 n2))
  (defrule neq
    (forall ((a mesg))
	    (implies
	     (fact neq a a)
	     (false))))
  (comment "Impose an nequality constraint using facts and rules"))

;;; With no inequality fact,
;;; a shape should be found where n1 = n2.
(defskeleton neq-test
  (vars)
  (defstrand init 2)
  (comment "This skeleton should have a shape"))

;;; With an inequality fact,
;;; no shape should exist.
(defskeleton neq-test
  (vars (n1 n2 text))
  (defstrand init 2 (n1 n1) (n2 n2))
  (facts (neq n1 n2))			;  assert n1 != n2
  (comment "This skeleton should have no shapes"))

;;;

(herald doorsep (comment "Door Simple Example Protocol"))

(defprotocol doorsep basic
  (defrole person
    (vars (d p akey) (k skey) (t text))
    (trace
     (send (enc (enc k (invk p)) d))
     (recv (enc t k))
     (send t)))
  (defrole door
    (vars (d p akey) (k skey) (t text))
    (trace
     (recv (enc (enc k (invk p)) d))
     (send (enc t k))
     (recv t)))
  (defrule trust
    (forall ((z strd) (p d akey))
	    (implies
	     (and (p "person" z 1)
		  (p "person" "p" z p)
		  (p "person" "d" z d)
		  (non (invk p)))
	     (non (invk d))))
    (comment "The trust rule"))
  (comment "Door Simple Example Protocol"))

(defskeleton doorsep
  (vars (p akey))
  (defstrand door 3 (p p))
  (non-orig (invk p))
  (comment "Analyze from the doors's perspective"))
