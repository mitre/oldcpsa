(herald rule-order)

(defprotocol rule-order basic
  (defrole init
    (vars (s t text))
    (trace
     (send (cat s t))))

  (defrule ge
    (forall ((x y text))
	    (implies
	     (fact le x y)
	     (or
	      (= x y)
	      (fact lt x y)))))
  )

(defskeleton rule-order
  (vars (s t text))
  (defstrand init 1 (s s) (t t))
  (facts (le s t)))
