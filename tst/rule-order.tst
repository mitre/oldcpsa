(herald rule-order)

(comment "CPSA 3.6.8")
(comment "All input read from tst/rule-order.scm")

(defprotocol rule-order basic
  (defrole init (vars (s t text)) (trace (send (cat s t))))
  (defrule ge
    (forall ((x y text))
      (implies (fact le x y) (or (= x y) (fact lt x y))))))

(defskeleton rule-order
  (vars (s t text))
  (defstrand init 1 (s s) (t t))
  (facts (le s t))
  (traces ((send (cat s t))))
  (label 0)
  (unrealized)
  (origs)
  (comment "Not closed under rules"))

(defskeleton rule-order
  (vars (t text))
  (defstrand init 1 (s t) (t t))
  (facts (le t t))
  (rule ge)
  (traces ((send (cat t t))))
  (label 1)
  (parent 0)
  (unrealized)
  (shape)
  (maps ((0) ((s t) (t t))))
  (origs))

(defskeleton rule-order
  (vars (s t text))
  (defstrand init 1 (s s) (t t))
  (facts (lt s t) (le s t))
  (rule ge)
  (traces ((send (cat s t))))
  (label 2)
  (parent 0)
  (unrealized)
  (shape)
  (maps ((0) ((s s) (t t))))
  (origs))

(comment "Nothing left to do")
