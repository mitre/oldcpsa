(herald trust-anchor
  (comment "Tests rule application on initial skeleton"))

(comment "CPSA 3.6.10")
(comment "All input read from tst/trust-anchor.scm")

(defprotocol trust-anchor basic
  (defrule trust-anchor-inverse-is-non
    (forall ((k akey)) (implies (fact trust-anchor k) (non (invk k))))))

(defskeleton trust-anchor
  (vars (f ca name))
  (deflistener (enc f (pubk f) (privk ca)))
  (non-orig (privk ca))
  (traces
    ((recv (enc f (pubk f) (privk ca)))
      (send (enc f (pubk f) (privk ca)))))
  (label 0)
  (unrealized (0 0))
  (dead)
  (origs)
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol trust-anchor basic
  (defrule trust-anchor-inverse-is-non
    (forall ((k akey)) (implies (fact trust-anchor k) (non (invk k))))))

(defskeleton trust-anchor
  (vars (f ca name))
  (deflistener (enc f (pubk f) (privk ca)))
  (facts (trust-anchor (pubk ca)))
  (traces
    ((recv (enc f (pubk f) (privk ca)))
      (send (enc f (pubk f) (privk ca)))))
  (label 1)
  (unrealized)
  (origs)
  (comment "Not closed under rules"))

(defskeleton trust-anchor
  (vars (f ca name))
  (deflistener (enc f (pubk f) (privk ca)))
  (non-orig (privk ca))
  (facts (trust-anchor (pubk ca)))
  (rule trust-anchor-inverse-is-non)
  (traces
    ((recv (enc f (pubk f) (privk ca)))
      (send (enc f (pubk f) (privk ca)))))
  (label 2)
  (parent 1)
  (unrealized (0 0))
  (dead)
  (origs)
  (comment "empty cohort"))

(comment "Nothing left to do")
