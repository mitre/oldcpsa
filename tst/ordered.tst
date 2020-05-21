(herald ordered)

(comment "CPSA 3.6.6")
(comment "All input read from tst/ordered.scm")

(defprotocol ordered basic
  (defrole dec
    (vars (t text) (k akey))
    (trace (recv (enc t k)) (send t)))
  (defrule neq (forall ((x mesg)) (implies (fact neq x x) (false))))
  (defrule order
    (forall ((y z strd))
      (implies
        (and (p "dec" y 2) (p "dec" z 2))
        (or (prec y 1 z 0) (prec z 1 y 0) (= y z))))))

(defskeleton ordered
  (vars (a b text) (k k-0 akey))
  (defstrand dec 2 (t a) (k k))
  (defstrand dec 2 (t b) (k k-0))
  (facts (neq a b))
  (traces ((recv (enc a k)) (send a)) ((recv (enc b k-0)) (send b)))
  (label 0)
  (unrealized)
  (origs)
  (comment "Not closed under rules"))

(defskeleton ordered
  (vars (a b text) (k k-0 akey))
  (defstrand dec 2 (t a) (k k))
  (defstrand dec 2 (t b) (k k-0))
  (precedes ((0 1) (1 0)))
  (facts (neq a b))
  (rule order)
  (traces ((recv (enc a k)) (send a)) ((recv (enc b k-0)) (send b)))
  (label 1)
  (parent 0)
  (unrealized)
  (shape)
  (maps ((0 1) ((a a) (b b) (k k) (k-0 k-0))))
  (origs))

(defskeleton ordered
  (vars (a b text) (k k-0 akey))
  (defstrand dec 2 (t a) (k k))
  (defstrand dec 2 (t b) (k k-0))
  (precedes ((1 1) (0 0)))
  (facts (neq a b))
  (rule order)
  (traces ((recv (enc a k)) (send a)) ((recv (enc b k-0)) (send b)))
  (label 2)
  (parent 0)
  (unrealized)
  (shape)
  (maps ((0 1) ((a a) (b b) (k k) (k-0 k-0))))
  (origs))

(comment "Nothing left to do")
