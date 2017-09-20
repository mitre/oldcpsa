(herald "Function constraint test protocol"
  (comment "Skeletons 2, 4, and 7 should have no shapes."))

(comment "CPSA 3.4.0")
(comment "All input read from fnof_test.scm")

(defprotocol fnoftest basic
  (defrole init
    (vars (an0 an1 n0 n1 text) (k skey))
    (trace (send (cat n0 (enc an0 an1 n0 n1 k))) (recv n1))
    (non-orig k)
    (uniq-orig n0 n1)))

(defskeleton fnoftest
  (vars (an0 an1 n0 n1 text) (k skey))
  (defstrand init 2 (an0 an0) (an1 an1) (n0 n0) (n1 n1) (k k))
  (non-orig k)
  (uniq-orig n0 n1)
  (traces ((send (cat n0 (enc an0 an1 n0 n1 k))) (recv n1)))
  (label 0)
  (unrealized (0 1))
  (origs (n0 (0 0)) (n1 (0 0)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton fnoftest
  (vars (an0 an1 n0 text) (k skey))
  (defstrand init 2 (an0 an0) (an1 an1) (n0 n0) (n1 n0) (k k))
  (non-orig k)
  (uniq-orig n0)
  (operation nonce-test (displaced 1 0 init 1) n1 (0 1)
    (enc an0 an1 n0 n1 k))
  (traces ((send (cat n0 (enc an0 an1 n0 n0 k))) (recv n0)))
  (label 1)
  (parent 0)
  (unrealized)
  (shape)
  (maps ((0) ((an0 an0) (an1 an1) (n0 n0) (n1 n0) (k k))))
  (origs (n0 (0 0))))

(comment "Nothing left to do")

(defprotocol fnoftest basic
  (defrole init
    (vars (an0 an1 n0 n1 text) (k skey))
    (trace (send (cat n0 (enc an0 an1 n0 n1 k))) (recv n1))
    (non-orig k)
    (uniq-orig n0 n1)))

(defskeleton fnoftest
  (vars (an0 an1 n0 n1 text) (k skey))
  (defstrand init 2 (an0 an0) (an1 an1) (n0 n0) (n1 n1) (k k))
  (fn-of (a (an0 n0) (an1 n1)))
  (neq (an0 an1))
  (non-orig k)
  (uniq-orig n0 n1)
  (traces ((send (cat n0 (enc an0 an1 n0 n1 k))) (recv n1)))
  (label 2)
  (unrealized (0 1))
  (origs (n0 (0 0)) (n1 (0 0)))
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol fnoftest basic
  (defrole init
    (vars (an0 an1 n0 n1 text) (k skey))
    (trace (send (cat n0 (enc an0 an1 n0 n1 k))) (recv n1))
    (non-orig k)
    (uniq-orig n0 n1)))

(defskeleton fnoftest
  (vars (an0 an1 n1 text) (k skey))
  (defstrand init 2 (an0 an0) (an1 an1) (n0 n1) (n1 n1) (k k))
  (fn-of (a (an0 n1) (an1 n1)))
  (non-orig k)
  (uniq-orig n1)
  (traces ((send (cat n1 (enc an0 an1 n1 n1 k))) (recv n1)))
  (label 3)
  (unrealized)
  (preskeleton)
  (comment "Not a skeleton"))

(defskeleton fnoftest
  (vars (an1 n1 text) (k skey))
  (defstrand init 2 (an0 an1) (an1 an1) (n0 n1) (n1 n1) (k k))
  (fn-of (a (an1 n1)))
  (non-orig k)
  (uniq-orig n1)
  (traces ((send (cat n1 (enc an1 an1 n1 n1 k))) (recv n1)))
  (label 4)
  (parent 3)
  (unrealized)
  (shape)
  (maps ((0) ((an1 an1) (n1 n1) (k k))))
  (origs (n1 (0 0))))

(comment "Nothing left to do")

(defprotocol fnoftest basic
  (defrole init
    (vars (an0 an1 n0 n1 text) (k skey))
    (trace (send (cat n0 (enc an0 an1 n0 n1 k))) (recv n1))
    (non-orig k)
    (uniq-orig n0 n1)))

(defskeleton fnoftest
  (vars (an0 an1 n1 text) (k skey))
  (defstrand init 2 (an0 an0) (an1 an1) (n0 n1) (n1 n1) (k k))
  (fn-of (a (an0 n1) (an1 n1)))
  (neq (an0 an1))
  (non-orig k)
  (uniq-orig n1)
  (traces ((send (cat n1 (enc an0 an1 n1 n1 k))) (recv n1)))
  (label 5)
  (unrealized)
  (comment "Input cannot be made into a skeleton--nothing to do"))

(defprotocol fnoftest2 basic
  (defrole init
    (vars (bn0 bn1 name) (an0 an1 n0 n1 text) (k skey))
    (trace (send (cat n0 (enc bn0 bn1 an0 an1 n0 n1 k))) (recv n1))
    (non-orig k)
    (uniq-orig n0 n1)
    (fn-of (a (an0 n0) (an1 n1)) (b ((pubk bn0) an0)))))

(defskeleton fnoftest2
  (vars (an0 an1 n0 n1 text) (bn0 bn1 name) (k skey))
  (defstrand init 2 (an0 an0) (an1 an1) (n0 n0) (n1 n1) (bn0 bn0)
    (bn1 bn1) (k k))
  (fn-of (a (an0 n0) (an1 n1)) (b ((pubk bn1) an1) ((pubk bn0) an0)))
  (non-orig k)
  (uniq-orig n0 n1)
  (traces ((send (cat n0 (enc bn0 bn1 an0 an1 n0 n1 k))) (recv n1)))
  (label 6)
  (unrealized (0 1))
  (origs (n0 (0 0)) (n1 (0 0)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton fnoftest2
  (vars (an1 n0 text) (bn1 name) (k skey))
  (defstrand init 2 (an0 an1) (an1 an1) (n0 n0) (n1 n0) (bn0 bn1)
    (bn1 bn1) (k k))
  (fn-of (a (an1 n0)) (b ((pubk bn1) an1)))
  (non-orig k)
  (uniq-orig n0)
  (operation nonce-test (displaced 1 0 init 1) n1 (0 1)
    (enc bn0 bn1 an0 an1 n0 n1 k))
  (traces ((send (cat n0 (enc bn1 bn1 an1 an1 n0 n0 k))) (recv n0)))
  (label 7)
  (parent 6)
  (unrealized)
  (shape)
  (maps
    ((0)
      ((bn0 bn1) (bn1 bn1) (an0 an1) (an1 an1) (n0 n0) (n1 n0) (k k))))
  (origs (n0 (0 0))))

(comment "Nothing left to do")

(defprotocol fnoftest2 basic
  (defrole init
    (vars (bn0 bn1 name) (an0 an1 n0 n1 text) (k skey))
    (trace (send (cat n0 (enc bn0 bn1 an0 an1 n0 n1 k))) (recv n1))
    (non-orig k)
    (uniq-orig n0 n1)
    (fn-of (a (an0 n0) (an1 n1)) (b ((pubk bn0) an0)))))

(defskeleton fnoftest2
  (vars (an0 an1 n0 n1 text) (bn0 bn1 name) (k skey))
  (defstrand init 2 (an0 an0) (an1 an1) (n0 n0) (n1 n1) (bn0 bn0)
    (bn1 bn1) (k k))
  (decl foo ((n0 n1) (0 1)) ((k k k n0) (0 0) (0 0) (0 1)))
  (fn-of (a (an0 n0) (an1 n1)) (b ((pubk bn0) an0))
    (c ((pubk bn1) an1)))
  (non-orig k)
  (uniq-orig n0 n1)
  (traces ((send (cat n0 (enc bn0 bn1 an0 an1 n0 n1 k))) (recv n1)))
  (label 8)
  (unrealized (0 1))
  (origs (n0 (0 0)) (n1 (0 0)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton fnoftest2
  (vars (an1 n0 text) (bn0 bn1 name) (k skey))
  (defstrand init 2 (an0 an1) (an1 an1) (n0 n0) (n1 n0) (bn0 bn0)
    (bn1 bn1) (k k))
  (decl foo ((n0 n0) (0 1)) ((k k k n0) (0 0) (0 0) (0 1)))
  (fn-of (a (an1 n0)) (b ((pubk bn0) an1)) (c ((pubk bn1) an1)))
  (non-orig k)
  (uniq-orig n0)
  (operation nonce-test (displaced 1 0 init 1) n1 (0 1)
    (enc bn0 bn1 an0 an1 n0 n1 k))
  (traces ((send (cat n0 (enc bn0 bn1 an1 an1 n0 n0 k))) (recv n0)))
  (label 9)
  (parent 8)
  (unrealized)
  (shape)
  (maps
    ((0)
      ((bn0 bn0) (bn1 bn1) (an0 an1) (an1 an1) (n0 n0) (n1 n0) (k k))))
  (origs (n0 (0 0))))

(comment "Nothing left to do")

(defprotocol fnoftest2 basic
  (defrole init
    (vars (bn0 bn1 name) (an0 an1 n0 n1 text) (k skey))
    (trace (send (cat n0 (enc bn0 bn1 an0 an1 n0 n1 k))) (recv n1))
    (non-orig k)
    (uniq-orig n0 n1)
    (fn-of (a (an0 n0) (an1 n1)) (b ((pubk bn0) an0)))))

(defskeleton fnoftest2
  (vars (an0 an1 n0 n1 text) (bn0 bn1 name) (k skey))
  (defstrand init 2 (an0 an0) (an1 an1) (n0 n0) (n1 n1) (bn0 bn0)
    (bn1 bn1) (k k))
  (decl foo ((n0 n1) (0 1)) ((k k k n0) (0 0) (0 0) (0 1)))
  (fn-of (a (an0 n0) (an1 n1)) (b ((privk bn0) an1) ((pubk bn0) an0)))
  (non-orig k)
  (uniq-orig n0 n1)
  (traces ((send (cat n0 (enc bn0 bn1 an0 an1 n0 n1 k))) (recv n1)))
  (label 10)
  (unrealized (0 1))
  (origs (n0 (0 0)) (n1 (0 0)))
  (comment "empty cohort"))

(comment "Nothing left to do")
