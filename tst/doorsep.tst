(herald doorsep (comment "Door Simple Example Protocol"))

(comment "CPSA 3.6.11")
(comment "All input read from tst/doorsep.scm")

(defprotocol doorsep basic
  (defrole person
    (vars (p d akey) (k skey) (t text))
    (trace (send (enc (enc k (invk p)) d)) (recv (enc t k)) (send t))
    (uniq-orig k))
  (defrole door
    (vars (p d akey) (k skey) (t text))
    (trace (recv (enc (enc k (invk p)) d)) (send (enc t k)) (recv t)))
  (defrule trust
    (forall ((z strd) (p d akey) (k skey))
      (implies
        (and (p "person" z 1) (p "person" "p" z p) (p "person" "d" z d)
          (p "person" "k" z k) (fact trust p))
        (and (non (invk d)) (uniq k))))))

(defskeleton doorsep
  (vars (t text) (k skey) (p d akey))
  (defstrand door 3 (t t) (k k) (p p) (d d))
  (non-orig (invk p))
  (uniq-orig t)
  (facts (trust p))
  (comment "Analyze from the door's perspective")
  (traces ((recv (enc (enc k (invk p)) d)) (send (enc t k)) (recv t)))
  (label 0)
  (unrealized (0 0))
  (origs (t (0 1)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton doorsep
  (vars (t text) (k skey) (p d d-0 akey))
  (defstrand door 3 (t t) (k k) (p p) (d d))
  (defstrand person 1 (k k) (p p) (d d-0))
  (precedes ((1 0) (0 0)))
  (non-orig (invk p) (invk d-0))
  (uniq-orig t k)
  (facts (trust p))
  (rule trust)
  (operation encryption-test (added-strand person 1) (enc k (invk p))
    (0 0))
  (traces ((recv (enc (enc k (invk p)) d)) (send (enc t k)) (recv t))
    ((send (enc (enc k (invk p)) d-0))))
  (label 1)
  (parent 0)
  (unrealized (0 0) (0 2))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton doorsep
  (vars (t text) (k skey) (p d akey))
  (defstrand door 3 (t t) (k k) (p p) (d d))
  (defstrand person 1 (k k) (p p) (d d))
  (precedes ((1 0) (0 0)))
  (non-orig (invk p) (invk d))
  (uniq-orig t k)
  (facts (trust p))
  (operation encryption-test (contracted (d-0 d)) (enc k (invk p)) (0 0)
    (enc (enc k (invk p)) d))
  (traces ((recv (enc (enc k (invk p)) d)) (send (enc t k)) (recv t))
    ((send (enc (enc k (invk p)) d))))
  (label 2)
  (parent 1)
  (unrealized (0 2))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton doorsep
  (vars (t text) (k skey) (p d akey))
  (defstrand door 3 (t t) (k k) (p p) (d d))
  (defstrand person 3 (t t) (k k) (p p) (d d))
  (precedes ((0 1) (1 1)) ((1 0) (0 0)) ((1 2) (0 2)))
  (non-orig (invk p) (invk d))
  (uniq-orig t k)
  (facts (trust p))
  (operation nonce-test (displaced 1 2 person 3) t (0 2) (enc t k))
  (traces ((recv (enc (enc k (invk p)) d)) (send (enc t k)) (recv t))
    ((send (enc (enc k (invk p)) d)) (recv (enc t k)) (send t)))
  (label 3)
  (parent 2)
  (unrealized)
  (shape)
  (maps ((0) ((p p) (t t) (d d) (k k))))
  (origs (k (1 0)) (t (0 1))))

(defskeleton doorsep
  (vars (t text) (k skey) (p d akey))
  (defstrand door 3 (t t) (k k) (p p) (d d))
  (defstrand person 1 (k k) (p p) (d d))
  (deflistener k)
  (precedes ((1 0) (0 0)) ((1 0) (2 0)) ((2 1) (0 2)))
  (non-orig (invk p) (invk d))
  (uniq-orig t k)
  (facts (trust p))
  (operation nonce-test (added-listener k) t (0 2) (enc t k))
  (traces ((recv (enc (enc k (invk p)) d)) (send (enc t k)) (recv t))
    ((send (enc (enc k (invk p)) d))) ((recv k) (send k)))
  (label 4)
  (parent 2)
  (unrealized (2 0))
  (dead)
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol doorsep basic
  (defrole person
    (vars (p d akey) (k skey) (t text))
    (trace (send (enc (enc k (invk p)) d)) (recv (enc t k)) (send t))
    (uniq-orig k))
  (defrole door
    (vars (p d akey) (k skey) (t text))
    (trace (recv (enc (enc k (invk p)) d)) (send (enc t k)) (recv t)))
  (defrule trust
    (forall ((z strd) (p d akey) (k skey))
      (implies
        (and (p "person" z 1) (p "person" "p" z p) (p "person" "d" z d)
          (p "person" "k" z k) (fact trust p))
        (and (non (invk d)) (uniq k))))))

(defskeleton doorsep
  (vars (t text) (k skey) (p d akey))
  (defstrand door 3 (t t) (k k) (p p) (d d))
  (non-orig (invk p))
  (uniq-orig t)
  (comment "Analyze from the door's perspective when we don't trust p")
  (traces ((recv (enc (enc k (invk p)) d)) (send (enc t k)) (recv t)))
  (label 5)
  (unrealized (0 0))
  (origs (t (0 1)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton doorsep
  (vars (t text) (k skey) (p d d-0 akey))
  (defstrand door 3 (t t) (k k) (p p) (d d))
  (defstrand person 1 (k k) (p p) (d d-0))
  (precedes ((1 0) (0 0)))
  (non-orig (invk p))
  (uniq-orig t k)
  (operation encryption-test (added-strand person 1) (enc k (invk p))
    (0 0))
  (traces ((recv (enc (enc k (invk p)) d)) (send (enc t k)) (recv t))
    ((send (enc (enc k (invk p)) d-0))))
  (label 6)
  (parent 5)
  (unrealized)
  (shape)
  (maps ((0) ((p p) (t t) (d d) (k k))))
  (origs (k (1 0)) (t (0 1))))

(comment "Nothing left to do")
