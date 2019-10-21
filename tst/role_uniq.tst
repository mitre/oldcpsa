(herald "Role Unique Origination")

(comment "CPSA 3.6.4")
(comment "All input read from role_uniq.scm")

(defprotocol blanchet basic
  (defrole init
    (vars (a b akey) (s skey) (d data))
    (trace (send (enc (enc s (invk a)) b)) (recv (enc d s)))
    (uniq-orig s))
  (defrole resp
    (vars (a b akey) (s skey) (d data))
    (trace (recv (enc (enc s (invk a)) b)) (send (enc d s))))
  (comment "Blanchet's protocol using unnamed asymmetric keys"))

(defskeleton blanchet
  (vars (d data) (s skey) (a b akey))
  (defstrand init 2 (d d) (s s) (a a) (b b))
  (non-orig (invk b))
  (uniq-orig s)
  (comment "Analyze from the initiator's perspective")
  (traces ((send (enc (enc s (invk a)) b)) (recv (enc d s))))
  (label 0)
  (unrealized (0 1))
  (origs (s (0 0)))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton blanchet
  (vars (d data) (s skey) (a b a-0 b-0 akey))
  (defstrand init 2 (d d) (s s) (a a) (b b))
  (defstrand resp 2 (d d) (s s) (a a-0) (b b-0))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (non-orig (invk b))
  (uniq-orig s)
  (operation encryption-test (added-strand resp 2) (enc d s) (0 1))
  (traces ((send (enc (enc s (invk a)) b)) (recv (enc d s)))
    ((recv (enc (enc s (invk a-0)) b-0)) (send (enc d s))))
  (label 1)
  (parent 0)
  (unrealized (1 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton blanchet
  (vars (d data) (s skey) (a b akey))
  (defstrand init 2 (d d) (s s) (a a) (b b))
  (deflistener s)
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (non-orig (invk b))
  (uniq-orig s)
  (operation encryption-test (added-listener s) (enc d s) (0 1))
  (traces ((send (enc (enc s (invk a)) b)) (recv (enc d s)))
    ((recv s) (send s)))
  (label 2)
  (parent 0)
  (unrealized (1 0))
  (dead)
  (comment "empty cohort"))

(defskeleton blanchet
  (vars (d data) (s skey) (a b akey))
  (defstrand init 2 (d d) (s s) (a a) (b b))
  (defstrand resp 2 (d d) (s s) (a a) (b b))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (non-orig (invk b))
  (uniq-orig s)
  (operation nonce-test (contracted (a-0 a) (b-0 b)) s (1 0)
    (enc (enc s (invk a)) b))
  (traces ((send (enc (enc s (invk a)) b)) (recv (enc d s)))
    ((recv (enc (enc s (invk a)) b)) (send (enc d s))))
  (label 3)
  (parent 1)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (s s) (d d))))
  (origs (s (0 0))))

(comment "Nothing left to do")

(defprotocol blanchet basic
  (defrole init
    (vars (a b akey) (s skey) (d data))
    (trace (send (enc (enc s (invk a)) b)) (recv (enc d s)))
    (uniq-orig s))
  (defrole resp
    (vars (a b akey) (s skey) (d data))
    (trace (recv (enc (enc s (invk a)) b)) (send (enc d s))))
  (comment "Blanchet's protocol using unnamed asymmetric keys"))

(defskeleton blanchet
  (vars (d data) (s skey) (a b akey))
  (defstrand resp 2 (d d) (s s) (a a) (b b))
  (non-orig (invk a) (invk b))
  (comment "Analyze from the responder's perspective")
  (traces ((recv (enc (enc s (invk a)) b)) (send (enc d s))))
  (label 4)
  (unrealized (0 0))
  (origs)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton blanchet
  (vars (d data) (s skey) (a b b-0 akey))
  (defstrand resp 2 (d d) (s s) (a a) (b b))
  (defstrand init 1 (s s) (a a) (b b-0))
  (precedes ((1 0) (0 0)))
  (non-orig (invk a) (invk b))
  (uniq-orig s)
  (operation encryption-test (added-strand init 1) (enc s (invk a))
    (0 0))
  (traces ((recv (enc (enc s (invk a)) b)) (send (enc d s)))
    ((send (enc (enc s (invk a)) b-0))))
  (label 5)
  (parent 4)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (s s) (d d))))
  (origs (s (1 0))))

(comment "Nothing left to do")

(defprotocol blanchet basic
  (defrole init
    (vars (a b akey) (s skey) (d data))
    (trace (send (enc (enc s (invk a)) b)) (recv (enc d s))))
  (defrole resp
    (vars (a b akey) (s skey) (d data))
    (trace (recv (enc (enc s (invk a)) b)) (send (enc d s))))
  (defrule role-uniq
    (forall ((z strd) (s skey)) (implies (p "init" "s" z s) (uniq s))))
  (comment "Blanchet's protocol using unnamed asymmetric keys"))

(defskeleton blanchet
  (vars (d data) (s skey) (a b akey))
  (defstrand init 2 (d d) (s s) (a a) (b b))
  (non-orig (invk b))
  (comment "Analyze from the initiator's perspective")
  (traces ((send (enc (enc s (invk a)) b)) (recv (enc d s))))
  (label 6)
  (unrealized)
  (origs)
  (comment "Not closed under rules"))

(defskeleton blanchet
  (vars (d data) (s skey) (a b akey))
  (defstrand init 2 (d d) (s s) (a a) (b b))
  (non-orig (invk b))
  (uniq-orig s)
  (rule role-uniq)
  (traces ((send (enc (enc s (invk a)) b)) (recv (enc d s))))
  (label 7)
  (parent 6)
  (unrealized (0 1))
  (origs (s (0 0)))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton blanchet
  (vars (d data) (s skey) (a b a-0 b-0 akey))
  (defstrand init 2 (d d) (s s) (a a) (b b))
  (defstrand resp 2 (d d) (s s) (a a-0) (b b-0))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (non-orig (invk b))
  (uniq-orig s)
  (operation encryption-test (added-strand resp 2) (enc d s) (0 1))
  (traces ((send (enc (enc s (invk a)) b)) (recv (enc d s)))
    ((recv (enc (enc s (invk a-0)) b-0)) (send (enc d s))))
  (label 8)
  (parent 7)
  (unrealized (1 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton blanchet
  (vars (d data) (s skey) (a b akey))
  (defstrand init 2 (d d) (s s) (a a) (b b))
  (deflistener s)
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (non-orig (invk b))
  (uniq-orig s)
  (operation encryption-test (added-listener s) (enc d s) (0 1))
  (traces ((send (enc (enc s (invk a)) b)) (recv (enc d s)))
    ((recv s) (send s)))
  (label 9)
  (parent 7)
  (unrealized (1 0))
  (dead)
  (comment "empty cohort"))

(defskeleton blanchet
  (vars (d data) (s skey) (a b akey))
  (defstrand init 2 (d d) (s s) (a a) (b b))
  (defstrand resp 2 (d d) (s s) (a a) (b b))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (non-orig (invk b))
  (uniq-orig s)
  (operation nonce-test (contracted (a-0 a) (b-0 b)) s (1 0)
    (enc (enc s (invk a)) b))
  (traces ((send (enc (enc s (invk a)) b)) (recv (enc d s)))
    ((recv (enc (enc s (invk a)) b)) (send (enc d s))))
  (label 10)
  (parent 8)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (s s) (d d))))
  (origs (s (0 0))))

(comment "Nothing left to do")

(defprotocol blanchet basic
  (defrole init
    (vars (a b akey) (s skey) (d data))
    (trace (send (enc (enc s (invk a)) b)) (recv (enc d s))))
  (defrole resp
    (vars (a b akey) (s skey) (d data))
    (trace (recv (enc (enc s (invk a)) b)) (send (enc d s))))
  (defrule role-uniq
    (forall ((z strd) (s skey)) (implies (p "init" "s" z s) (uniq s))))
  (comment "Blanchet's protocol using unnamed asymmetric keys"))

(defskeleton blanchet
  (vars (d data) (s skey) (a b akey))
  (defstrand resp 2 (d d) (s s) (a a) (b b))
  (non-orig (invk a) (invk b))
  (comment "Analyze from the responder's perspective")
  (traces ((recv (enc (enc s (invk a)) b)) (send (enc d s))))
  (label 11)
  (unrealized (0 0))
  (origs)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton blanchet
  (vars (d data) (s skey) (a b b-0 akey))
  (defstrand resp 2 (d d) (s s) (a a) (b b))
  (defstrand init 1 (s s) (a a) (b b-0))
  (precedes ((1 0) (0 0)))
  (non-orig (invk a) (invk b))
  (uniq-orig s)
  (rule role-uniq)
  (operation encryption-test (added-strand init 1) (enc s (invk a))
    (0 0))
  (traces ((recv (enc (enc s (invk a)) b)) (send (enc d s)))
    ((send (enc (enc s (invk a)) b-0))))
  (label 12)
  (parent 11)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (s s) (d d))))
  (origs (s (1 0))))

(comment "Nothing left to do")
