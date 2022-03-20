(comment "CPSA 3.6.10")
(comment "All input read from tst/target.scm")

(defprotocol target-simple basic
  (defrole comb
    (vars (k1 k2 skey) (n1 n2 text))
    (trace (send (enc "oo" (enc "bar" n1 k1) (enc "baz" n2 k1) k2))
      (recv n1))
    (non-orig k1 k2)
    (uniq-orig n1 n2))
  (defrole trans1
    (vars (n text) (k1 k2 skey) (m mesg))
    (trace (recv (enc "oo" (enc "bar" n k1) m k2)) (send m))
    (non-orig k1 k2))
  (defrole trans2
    (vars (n text) (k skey))
    (trace (recv (enc "baz" n k)) (send n))
    (non-orig k)))

(defskeleton target-simple
  (vars (n1 n2 text) (k1 k2 skey))
  (defstrand comb 2 (n1 n1) (n2 n2) (k1 k1) (k2 k2))
  (non-orig k1 k2)
  (uniq-orig n1 n2)
  (traces
    ((send (enc "oo" (enc "bar" n1 k1) (enc "baz" n2 k1) k2))
      (recv n1)))
  (label 0)
  (unrealized (0 1))
  (origs (n1 (0 0)) (n2 (0 0)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton target-simple
  (vars (n2 text) (k1 k2 skey))
  (defstrand comb 2 (n1 n2) (n2 n2) (k1 k1) (k2 k2))
  (non-orig k1 k2)
  (uniq-orig n2)
  (operation nonce-test (displaced 1 0 comb 1) n1 (0 1)
    (enc "oo" (enc "bar" n1 k1) (enc "baz" n2 k1) k2))
  (traces
    ((send (enc "oo" (enc "bar" n2 k1) (enc "baz" n2 k1) k2))
      (recv n2)))
  (label 1)
  (parent 0)
  (unrealized (0 1))
  (origs (n2 (0 0)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton target-simple
  (vars (n2 text) (k1 k2 skey))
  (defstrand comb 2 (n1 n2) (n2 n2) (k1 k1) (k2 k2))
  (defstrand trans1 2 (m (enc "baz" n2 k1)) (n n2) (k1 k1) (k2 k2))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (non-orig k1 k2)
  (uniq-orig n2)
  (operation nonce-test (added-strand trans1 2) n2 (0 1)
    (enc "oo" (enc "bar" n2 k1) (enc "baz" n2 k1) k2))
  (traces
    ((send (enc "oo" (enc "bar" n2 k1) (enc "baz" n2 k1) k2)) (recv n2))
    ((recv (enc "oo" (enc "bar" n2 k1) (enc "baz" n2 k1) k2))
      (send (enc "baz" n2 k1))))
  (label 2)
  (parent 1)
  (unrealized (0 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton target-simple
  (vars (n2 text) (k1 k2 skey))
  (defstrand comb 2 (n1 n2) (n2 n2) (k1 k1) (k2 k2))
  (defstrand trans1 2 (m (enc "baz" n2 k1)) (n n2) (k1 k1) (k2 k2))
  (defstrand trans2 2 (n n2) (k k1))
  (precedes ((0 0) (1 0)) ((0 0) (2 0)) ((1 1) (0 1)) ((2 1) (0 1)))
  (non-orig k1 k2)
  (uniq-orig n2)
  (operation nonce-test (added-strand trans2 2) n2 (0 1)
    (enc "baz" n2 k1) (enc "oo" (enc "bar" n2 k1) (enc "baz" n2 k1) k2))
  (traces
    ((send (enc "oo" (enc "bar" n2 k1) (enc "baz" n2 k1) k2)) (recv n2))
    ((recv (enc "oo" (enc "bar" n2 k1) (enc "baz" n2 k1) k2))
      (send (enc "baz" n2 k1))) ((recv (enc "baz" n2 k1)) (send n2)))
  (label 3)
  (parent 2)
  (unrealized (2 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton target-simple
  (vars (n2 text) (k1 k2 skey))
  (defstrand comb 2 (n1 n2) (n2 n2) (k1 k1) (k2 k2))
  (defstrand trans1 2 (m (enc "baz" n2 k1)) (n n2) (k1 k1) (k2 k2))
  (defstrand trans2 2 (n n2) (k k1))
  (precedes ((0 0) (1 0)) ((1 1) (2 0)) ((2 1) (0 1)))
  (non-orig k1 k2)
  (uniq-orig n2)
  (operation encryption-test (displaced 3 1 trans1 2) (enc "baz" n2 k1)
    (2 0) (enc "oo" (enc "bar" n2 k1) (enc "baz" n2 k1) k2))
  (traces
    ((send (enc "oo" (enc "bar" n2 k1) (enc "baz" n2 k1) k2)) (recv n2))
    ((recv (enc "oo" (enc "bar" n2 k1) (enc "baz" n2 k1) k2))
      (send (enc "baz" n2 k1))) ((recv (enc "baz" n2 k1)) (send n2)))
  (label 4)
  (parent 3)
  (unrealized)
  (shape)
  (maps ((0) ((k1 k1) (k2 k2) (n1 n2) (n2 n2))))
  (origs (n2 (0 0))))

(defskeleton target-simple
  (vars (n2 text) (k1 k2 skey))
  (defstrand comb 2 (n1 n2) (n2 n2) (k1 k1) (k2 k2))
  (defstrand trans1 2 (m (enc "baz" n2 k1)) (n n2) (k1 k1) (k2 k2))
  (defstrand trans2 2 (n n2) (k k1))
  (defstrand trans1 2 (m (enc "baz" n2 k1)) (n n2) (k1 k1) (k2 k2))
  (precedes ((0 0) (1 0)) ((0 0) (3 0)) ((1 1) (0 1)) ((2 1) (0 1))
    ((3 1) (2 0)))
  (non-orig k1 k2)
  (uniq-orig n2)
  (operation encryption-test (added-strand trans1 2) (enc "baz" n2 k1)
    (2 0) (enc "oo" (enc "bar" n2 k1) (enc "baz" n2 k1) k2))
  (traces
    ((send (enc "oo" (enc "bar" n2 k1) (enc "baz" n2 k1) k2)) (recv n2))
    ((recv (enc "oo" (enc "bar" n2 k1) (enc "baz" n2 k1) k2))
      (send (enc "baz" n2 k1))) ((recv (enc "baz" n2 k1)) (send n2))
    ((recv (enc "oo" (enc "bar" n2 k1) (enc "baz" n2 k1) k2))
      (send (enc "baz" n2 k1))))
  (label 5)
  (parent 3)
  (seen 4)
  (unrealized)
  (comment "1 in cohort - 0 not yet seen"))

(comment "Nothing left to do")
