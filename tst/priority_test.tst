(herald "Receive priority test protocol")

(comment "CPSA 3.6.5")
(comment "All input read from priority_test.scm")

(defprotocol priority_test basic
  (defrole init
    (vars (n1 n2 n3 n4 text) (k skey))
    (trace (send (enc n1 n1 k)) (recv (enc n1 n2 k))
      (recv (enc n1 n3 k)) (recv (enc n1 n4 k)))
    (non-orig k)
    (uniq-orig n1)
    (priority (2 0) (3 10))))

(defskeleton priority_test
  (vars (n1 n2 n3 n4 text) (k skey))
  (defstrand init 4 (n1 n1) (n2 n2) (n3 n3) (n4 n4) (k k))
  (priority ((0 2) 0) ((0 3) 10))
  (non-orig k)
  (uniq-orig n1)
  (traces
    ((send (enc n1 n1 k)) (recv (enc n1 n2 k)) (recv (enc n1 n3 k))
      (recv (enc n1 n4 k))))
  (label 0)
  (unrealized (0 1) (0 2) (0 3))
  (origs (n1 (0 0)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton priority_test
  (vars (n2 n3 n4 text) (k skey))
  (defstrand init 4 (n1 n4) (n2 n2) (n3 n3) (n4 n4) (k k))
  (priority ((0 2) 0) ((0 3) 10))
  (non-orig k)
  (uniq-orig n4)
  (operation encryption-test (displaced 1 0 init 1) (enc n4 n4 k) (0 3))
  (traces
    ((send (enc n4 n4 k)) (recv (enc n4 n2 k)) (recv (enc n4 n3 k))
      (recv (enc n4 n4 k))))
  (label 1)
  (parent 0)
  (unrealized (0 1) (0 2))
  (origs (n4 (0 0)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton priority_test
  (vars (n2 n3 text) (k skey))
  (defstrand init 4 (n1 n2) (n2 n2) (n3 n3) (n4 n2) (k k))
  (priority ((0 2) 0) ((0 3) 10))
  (non-orig k)
  (uniq-orig n2)
  (operation encryption-test (displaced 1 0 init 1) (enc n2 n2 k) (0 1))
  (traces
    ((send (enc n2 n2 k)) (recv (enc n2 n2 k)) (recv (enc n2 n3 k))
      (recv (enc n2 n2 k))))
  (label 2)
  (parent 1)
  (unrealized (0 2))
  (origs (n2 (0 0))))

(comment "Nothing left to do")

(defprotocol priority_test basic
  (defrole init
    (vars (n1 n2 n3 n4 text) (k skey))
    (trace (send (enc n1 n1 k)) (recv (enc n1 n2 k))
      (recv (enc n1 n3 k)) (recv (enc n1 n4 k)))
    (non-orig k)
    (uniq-orig n1)
    (priority (2 0) (3 10))))

(defskeleton priority_test
  (vars (n1 n2 n3 n4 text) (k skey))
  (defstrand init 4 (n1 n1) (n2 n2) (n3 n3) (n4 n4) (k k))
  (priority ((0 2) 10) ((0 3) 0))
  (non-orig k)
  (uniq-orig n1)
  (traces
    ((send (enc n1 n1 k)) (recv (enc n1 n2 k)) (recv (enc n1 n3 k))
      (recv (enc n1 n4 k))))
  (label 3)
  (unrealized (0 1) (0 2) (0 3))
  (origs (n1 (0 0)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton priority_test
  (vars (n2 n3 n4 text) (k skey))
  (defstrand init 4 (n1 n3) (n2 n2) (n3 n3) (n4 n4) (k k))
  (priority ((0 2) 10) ((0 3) 0))
  (non-orig k)
  (uniq-orig n3)
  (operation encryption-test (displaced 1 0 init 1) (enc n3 n3 k) (0 2))
  (traces
    ((send (enc n3 n3 k)) (recv (enc n3 n2 k)) (recv (enc n3 n3 k))
      (recv (enc n3 n4 k))))
  (label 4)
  (parent 3)
  (unrealized (0 1) (0 3))
  (origs (n3 (0 0)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton priority_test
  (vars (n2 n4 text) (k skey))
  (defstrand init 4 (n1 n2) (n2 n2) (n3 n2) (n4 n4) (k k))
  (priority ((0 2) 10) ((0 3) 0))
  (non-orig k)
  (uniq-orig n2)
  (operation encryption-test (displaced 1 0 init 1) (enc n2 n2 k) (0 1))
  (traces
    ((send (enc n2 n2 k)) (recv (enc n2 n2 k)) (recv (enc n2 n2 k))
      (recv (enc n2 n4 k))))
  (label 5)
  (parent 4)
  (unrealized (0 3))
  (origs (n2 (0 0))))

(comment "Nothing left to do")
