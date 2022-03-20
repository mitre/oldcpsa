(herald "Unique generation test protocols."
  (comment "Skeletons 2, 4, and 7 should have no shapes."))

(comment "CPSA 3.6.10")
(comment "All input read from tst/uniq-gen-test.scm")

(defprotocol uniqgentest basic
  (defrole init
    (vars (a name) (k skey))
    (trace (send (enc a k)) (recv (enc a a k))))
  (defrole doubler (vars (a name) (k skey)) (trace (send (enc a a k))))
  (defrole resp
    (vars (a name) (k skey))
    (trace (recv (enc a k)) (send (enc a a k)))))

(defskeleton uniqgentest
  (vars (a name) (k skey))
  (defstrand init 2 (a a) (k k))
  (non-orig k)
  (traces ((send (enc a k)) (recv (enc a a k))))
  (label 0)
  (unrealized (0 1))
  (origs)
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton uniqgentest
  (vars (a name) (k skey))
  (defstrand init 2 (a a) (k k))
  (defstrand doubler 1 (a a) (k k))
  (precedes ((1 0) (0 1)))
  (non-orig k)
  (operation encryption-test (added-strand doubler 1) (enc a a k) (0 1))
  (traces ((send (enc a k)) (recv (enc a a k))) ((send (enc a a k))))
  (label 1)
  (parent 0)
  (unrealized)
  (shape)
  (maps ((0) ((k k) (a a))))
  (origs))

(defskeleton uniqgentest
  (vars (a name) (k skey))
  (defstrand init 2 (a a) (k k))
  (defstrand resp 2 (a a) (k k))
  (precedes ((1 1) (0 1)))
  (non-orig k)
  (operation encryption-test (added-strand resp 2) (enc a a k) (0 1))
  (traces ((send (enc a k)) (recv (enc a a k)))
    ((recv (enc a k)) (send (enc a a k))))
  (label 2)
  (parent 0)
  (unrealized (1 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton uniqgentest
  (vars (a name) (k skey))
  (defstrand init 2 (a a) (k k))
  (defstrand resp 2 (a a) (k k))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (non-orig k)
  (operation encryption-test (displaced 2 0 init 1) (enc a k) (1 0))
  (traces ((send (enc a k)) (recv (enc a a k)))
    ((recv (enc a k)) (send (enc a a k))))
  (label 3)
  (parent 2)
  (unrealized)
  (shape)
  (maps ((0) ((k k) (a a))))
  (origs))

(defskeleton uniqgentest
  (vars (a name) (k skey))
  (defstrand init 2 (a a) (k k))
  (defstrand resp 2 (a a) (k k))
  (defstrand init 1 (a a) (k k))
  (precedes ((1 1) (0 1)) ((2 0) (1 0)))
  (non-orig k)
  (operation encryption-test (added-strand init 1) (enc a k) (1 0))
  (traces ((send (enc a k)) (recv (enc a a k)))
    ((recv (enc a k)) (send (enc a a k))) ((send (enc a k))))
  (label 4)
  (parent 2)
  (unrealized)
  (shape)
  (maps ((0) ((k k) (a a))))
  (origs))

(comment "Nothing left to do")

(defprotocol uniqgentest basic
  (defrole init
    (vars (a name) (k skey))
    (trace (send (enc a k)) (recv (enc a a k))))
  (defrole doubler (vars (a name) (k skey)) (trace (send (enc a a k))))
  (defrole resp
    (vars (a name) (k skey))
    (trace (recv (enc a k)) (send (enc a a k)))))

(defskeleton uniqgentest
  (vars (a name) (k skey))
  (defstrand init 2 (a a) (k k))
  (uniq-gen k)
  (traces ((send (enc a k)) (recv (enc a a k))))
  (label 5)
  (unrealized (0 1))
  (origs)
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton uniqgentest
  (vars (a name) (k skey))
  (defstrand init 2 (a a) (k k))
  (defstrand resp 2 (a a) (k k))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (uniq-gen k)
  (operation encryption-test (added-strand resp 2) (enc a a k) (0 1))
  (traces ((send (enc a k)) (recv (enc a a k)))
    ((recv (enc a k)) (send (enc a a k))))
  (label 6)
  (parent 5)
  (unrealized)
  (shape)
  (maps ((0) ((k k) (a a))))
  (origs))

(defskeleton uniqgentest
  (vars (a name) (k skey))
  (defstrand init 2 (a a) (k k))
  (deflistener k)
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (uniq-gen k)
  (operation encryption-test (added-listener k) (enc a a k) (0 1))
  (traces ((send (enc a k)) (recv (enc a a k))) ((recv k) (send k)))
  (label 7)
  (parent 5)
  (unrealized (1 0))
  (dead)
  (comment "empty cohort"))

(comment "Nothing left to do")
