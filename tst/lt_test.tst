(herald "Less-than constraint test protocol"
  (comment "First and third skeletons should have a shape,"
    "second and fourth should be dead."))

(comment "CPSA 3.6.4")
(comment "All input read from lt_test.scm")

(defprotocol lttest basic
  (defrole init
    (vars (n1 n2 text) (k skey))
    (trace (send (cat n1 (enc n1 n2 k))) (recv n2))
    (non-orig k)
    (uniq-orig n1 n2)))

(defskeleton lttest
  (vars (n1 n2 text) (k skey))
  (defstrand init 2 (n1 n1) (n2 n2) (k k))
  (non-orig k)
  (uniq-orig n1 n2)
  (traces ((send (cat n1 (enc n1 n2 k))) (recv n2)))
  (label 0)
  (unrealized (0 1))
  (origs (n1 (0 0)) (n2 (0 0)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton lttest
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

(defprotocol lttest basic
  (defrole init
    (vars (n1 n2 text) (k skey))
    (trace (send (cat n1 (enc n1 n2 k))) (recv n2))
    (non-orig k)
    (uniq-orig n1 n2)))

(defskeleton lttest
  (vars (n1 n2 text) (k skey))
  (defstrand init 2 (n1 n1) (n2 n2) (k k))
  (lt (n1 n2))
  (non-orig k)
  (uniq-orig n1 n2)
  (traces ((send (cat n1 (enc n1 n2 k))) (recv n2)))
  (label 2)
  (unrealized (0 1))
  (dead)
  (origs (n1 (0 0)) (n2 (0 0)))
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol lttest2 basic
  (defrole init
    (vars (n1 n2 n text) (k skey))
    (trace (send n) (send (cat n1 (enc n1 n2 k))) (recv n2))
    (non-orig k)
    (uniq-orig n1 n2)
    (lt (n2 n))))

(defskeleton lttest2
  (vars (n1 n2 n text) (k skey))
  (defstrand init 3 (n1 n1) (n2 n2) (n n) (k k))
  (lt (n2 n))
  (non-orig k)
  (uniq-orig n1 n2)
  (traces ((send n) (send (cat n1 (enc n1 n2 k))) (recv n2)))
  (label 3)
  (unrealized (0 2))
  (origs (n1 (0 1)) (n2 (0 1)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton lttest2
  (vars (n1 n text) (k skey))
  (defstrand init 3 (n1 n1) (n2 n1) (n n) (k k))
  (lt (n1 n))
  (non-orig k)
  (uniq-orig n1)
  (operation nonce-test (displaced 1 0 init 2) n2 (0 2) (enc n1 n2 k))
  (traces ((send n) (send (cat n1 (enc n1 n1 k))) (recv n1)))
  (label 4)
  (parent 3)
  (unrealized)
  (shape)
  (maps ((0) ((n1 n1) (n2 n1) (n n) (k k))))
  (origs (n1 (0 1))))

(comment "Nothing left to do")

(defprotocol lttest2 basic
  (defrole init
    (vars (n1 n2 n text) (k skey))
    (trace (send n) (send (cat n1 (enc n1 n2 k))) (recv n2))
    (non-orig k)
    (uniq-orig n1 n2)
    (lt (n2 n))))

(defskeleton lttest2
  (vars (n n1 n2 text) (k skey))
  (defstrand init 3 (n1 n1) (n2 n2) (n n) (k k))
  (lt (n n1) (n2 n))
  (non-orig k)
  (uniq-orig n1 n2)
  (traces ((send n) (send (cat n1 (enc n1 n2 k))) (recv n2)))
  (label 5)
  (unrealized (0 2))
  (dead)
  (origs (n1 (0 1)) (n2 (0 1)))
  (comment "empty cohort"))

(comment "Nothing left to do")
