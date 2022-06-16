(herald "Equality constraint test protocol"
  (comment "First skeleton should have a shape,"
    "second, and hird should be dead."))

(comment "CPSA 3.6.11")
(comment "All input read from tst/eq_test.scm")

(defprotocol eqtest basic
  (defrole init
    (vars (n1 n2 text) (k skey))
    (trace (send (cat n1 (enc n1 n2 k))) (recv n2))
    (non-orig k)
    (uniq-orig n1 n2)))

(defskeleton eqtest
  (vars (n1 n2 text) (k skey))
  (defstrand init 2 (n1 n1) (n2 n2) (k k))
  (non-orig k)
  (uniq-orig n1 n2)
  (traces ((send (cat n1 (enc n1 n2 k))) (recv n2)))
  (label 0)
  (unrealized (0 1))
  (origs (n1 (0 0)) (n2 (0 0)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton eqtest
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

(defprotocol eqtest basic
  (defrole init
    (vars (n1 n2 text) (k skey))
    (trace (send (cat n1 (enc n1 n2 k))) (recv n2))
    (non-orig k)
    (uniq-orig n1 n2)))

(defskeleton eqtest
  (vars (n1 n2 text) (k skey))
  (defstrand init 2 (n1 n1) (n2 n2) (k k))
  (non-orig k)
  (uniq-orig n1 n2)
  (traces ((send (cat n1 (enc n1 n2 k))) (recv n2)))
  (label 2)
  (unrealized (0 1))
  (origs (n1 (0 0)) (n2 (0 0)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton eqtest
  (vars (n1 text) (k skey))
  (defstrand init 2 (n1 n1) (n2 n1) (k k))
  (non-orig k)
  (uniq-orig n1)
  (operation nonce-test (displaced 1 0 init 1) n2 (0 1) (enc n1 n2 k))
  (traces ((send (cat n1 (enc n1 n1 k))) (recv n1)))
  (label 3)
  (parent 2)
  (unrealized)
  (shape)
  (maps ((0) ((n1 n1) (n2 n1) (k k))))
  (origs (n1 (0 0))))

(comment "Nothing left to do")

(defprotocol eqtest basic
  (defrole init
    (vars (n1 n2 text) (k skey))
    (trace (send (cat n1 (enc n1 n2 k))) (recv n2))
    (non-orig k)
    (uniq-orig n1 n2)))

(defskeleton eqtest
  (vars (n1 n2 text) (k skey))
  (defstrand init 2 (n1 n1) (n2 n2) (k k))
  (fn-of ("foo" (n1 "bar") (n2 "bar")))
  (non-orig k)
  (uniq-orig n1 n2)
  (traces ((send (cat n1 (enc n1 n2 k))) (recv n2)))
  (label 4)
  (unrealized (0 1))
  (preskeleton)
  (origs (n1 (0 0)) (n2 (0 0)))
  (comment "Not a skeleton"))

(defskeleton eqtest
  (vars (n2 text) (k skey))
  (defstrand init 2 (n1 n2) (n2 n2) (k k))
  (fn-of ("foo" (n2 "bar")))
  (non-orig k)
  (uniq-orig n2)
  (traces ((send (cat n2 (enc n2 n2 k))) (recv n2)))
  (label 5)
  (parent 4)
  (unrealized)
  (shape)
  (maps ((0) ((n2 n2) (k k))))
  (origs (n2 (0 0))))

(comment "Nothing left to do")
