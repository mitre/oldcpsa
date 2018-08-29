(herald "Subsort constraint test protocol"
  (comment "First, third, and fourth skeletons should have a shape,"
    "second should be dead."))

(comment "CPSA 3.6.1")
(comment "All input read from subsort_test.scm")

(defprotocol subsorttest basic
  (defrole init
    (vars (n1 n2 text) (k skey))
    (trace (send (cat n1 (enc n1 n2 k))) (recv n2))
    (non-orig k)
    (uniq-orig n1 n2)))

(defskeleton subsorttest
  (vars (n1 n2 text) (k skey))
  (defstrand init 2 (n1 n1) (n2 n2) (k k))
  (non-orig k)
  (uniq-orig n1 n2)
  (traces ((send (cat n1 (enc n1 n2 k))) (recv n2)))
  (label 0)
  (unrealized (0 1))
  (origs (n1 (0 0)) (n2 (0 0)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton subsorttest
  (vars (n1 text) (k skey))
  (defstrand init 2 (n1 n1) (n2 n1) (k k))
  (non-orig k)
  (uniq-orig n1)
  (operation nonce-test (displaced 1 0 init 1) n2 (0 1) (enc n1 n2 k))
  (traces ((send (cat n1 (enc n1 n1 k))) (recv n1)))
  (label 1)
  (parent 0)
  (unrealized)
  (shape)
  (maps ((0) ((n1 n1) (n2 n1) (k k))))
  (origs (n1 (0 0))))

(comment "Nothing left to do")

(defprotocol subsorttest basic
  (defrole init
    (vars (n1 n2 text) (k skey))
    (trace (send (cat n1 (enc n1 n2 k))) (recv n2))
    (non-orig k)
    (uniq-orig n1 n2)))

(defskeleton subsorttest
  (vars (n1 n2 text) (k skey))
  (defstrand init 2 (n1 n1) (n2 n2) (k k))
  (non-orig k)
  (subsort (A n1) (B n2))
  (uniq-orig n1 n2)
  (traces ((send (cat n1 (enc n1 n2 k))) (recv n2)))
  (label 2)
  (unrealized (0 1))
  (origs (n1 (0 0)) (n2 (0 0)))
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol subsorttest basic
  (defrole init
    (vars (n1 n2 text) (k skey))
    (trace (send (cat n1 (enc n1 n2 k))) (recv n2))
    (non-orig k)
    (uniq-orig n1 n2)))

(defskeleton subsorttest
  (vars (n1 n2 text) (k skey))
  (defstrand init 2 (n1 n1) (n2 n2) (k k))
  (non-orig k)
  (subsort (A n1))
  (uniq-orig n1 n2)
  (traces ((send (cat n1 (enc n1 n2 k))) (recv n2)))
  (label 3)
  (unrealized (0 1))
  (origs (n1 (0 0)) (n2 (0 0)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton subsorttest
  (vars (n1 text) (k skey))
  (defstrand init 2 (n1 n1) (n2 n1) (k k))
  (non-orig k)
  (subsort (A n1))
  (uniq-orig n1)
  (operation nonce-test (displaced 1 0 init 1) n2 (0 1) (enc n1 n2 k))
  (traces ((send (cat n1 (enc n1 n1 k))) (recv n1)))
  (label 4)
  (parent 3)
  (unrealized)
  (shape)
  (maps ((0) ((n1 n1) (n2 n1) (k k))))
  (origs (n1 (0 0))))

(comment "Nothing left to do")

(defprotocol subsorttest basic
  (defrole init
    (vars (n1 n2 text) (k skey))
    (trace (send (cat n1 (enc n1 n2 k))) (recv n2))
    (non-orig k)
    (uniq-orig n1 n2)))

(defskeleton subsorttest
  (vars (n1 n2 text) (k skey))
  (defstrand init 2 (n1 n1) (n2 n2) (k k))
  (non-orig k)
  (subsort (A n1 n2))
  (uniq-orig n1 n2)
  (traces ((send (cat n1 (enc n1 n2 k))) (recv n2)))
  (label 5)
  (unrealized (0 1))
  (origs (n1 (0 0)) (n2 (0 0)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton subsorttest
  (vars (n1 text) (k skey))
  (defstrand init 2 (n1 n1) (n2 n1) (k k))
  (non-orig k)
  (subsort (A n1))
  (uniq-orig n1)
  (operation nonce-test (displaced 1 0 init 1) n2 (0 1) (enc n1 n2 k))
  (traces ((send (cat n1 (enc n1 n1 k))) (recv n1)))
  (label 6)
  (parent 5)
  (unrealized)
  (shape)
  (maps ((0) ((n1 n1) (n2 n1) (k k))))
  (origs (n1 (0 0))))

(comment "Nothing left to do")
