(herald "Inequality constraint test protocol"
  (comment "First skeleton should have a shape,"
    "second, and hird should be dead."))

(comment "CPSA 3.6.1")
(comment "All input read from neq_test.scm")

(defprotocol neqtest basic
  (defrole init
    (vars (n1 n2 text) (k skey))
    (trace (send (cat n1 (enc n1 n2 k))) (recv n2))
    (non-orig k)
    (uniq-orig n1 n2)))

(defskeleton neqtest
  (vars (n1 n2 text) (k skey))
  (defstrand init 2 (n1 n1) (n2 n2) (k k))
  (non-orig k)
  (uniq-orig n1 n2)
  (traces ((send (cat n1 (enc n1 n2 k))) (recv n2)))
  (label 0)
  (unrealized (0 1))
  (origs (n1 (0 0)) (n2 (0 0)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton neqtest
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

(defprotocol neqtest basic
  (defrole init
    (vars (n1 n2 text) (k skey))
    (trace (send (cat n1 (enc n1 n2 k))) (recv n2))
    (non-orig k)
    (uniq-orig n1 n2)))

(defskeleton neqtest
  (vars (n1 n2 text) (k skey))
  (defstrand init 2 (n1 n1) (n2 n2) (k k))
  (neq (n1 n2))
  (non-orig k)
  (uniq-orig n1 n2)
  (traces ((send (cat n1 (enc n1 n2 k))) (recv n2)))
  (label 2)
  (unrealized (0 1))
  (origs (n1 (0 0)) (n2 (0 0)))
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol neqtest2 basic
  (defrole init
    (vars (n1 n2 n3 text) (k skey))
    (trace (send (cat n1 (enc n1 n2 n3 k))) (recv n2))
    (non-orig k)
    (uniq-orig n1 n2)))

(defskeleton neqtest2
  (vars (n1 n2 n3 text) (k skey))
  (defstrand init 2 (n1 n1) (n2 n2) (n3 n3) (k k))
  (neqlist ((n1 n2 n3)))
  (non-orig k)
  (uniq-orig n1 n2)
  (traces ((send (cat n1 (enc n1 n2 n3 k))) (recv n2)))
  (label 3)
  (unrealized (0 1))
  (origs (n1 (0 0)) (n2 (0 0)))
  (comment "empty cohort"))

(comment "Nothing left to do")
