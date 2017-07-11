(herald "Needham-Schroeder Public-Key Protocol" (algebra diffie-hellman)
  (comment "This protocol contains a man-in-the-middle"
    "attack discovered by Galvin Lowe."))

(comment "CPSA 3.2.2")
(comment "All input read from injection.scm")

(defprotocol ns diffie-hellman
  (defrole init
    (vars (a b c d name) (n1 n2 text))
    (trace (send (enc n1 a c d (pubk b)))
      (recv (enc (enc n1 n2 (pubk a)) (pubk c)))
      (send (enc n2 (pubk b)))))
  (defrole resp
    (vars (b a c d name) (n2 n1 text))
    (trace (recv (enc n1 a c d (pubk b)))
      (send (enc (enc n1 n2 (pubk a)) (pubk c)))
      (recv (enc n2 (pubk b)))))
  (comment "Needham-Schroeder variant 1"))

(defskeleton ns
  (vars (n2 n1 text) (a b c d name))
  (defstrand resp 3 (n2 n2) (n1 n1) (b b) (a a) (c c) (d d))
  (non-orig (privk a))
  (uniq-orig n2)
  (comment "Responder point-of-view")
  (traces
    ((recv (enc n1 a c d (pubk b)))
      (send (enc (enc n1 n2 (pubk a)) (pubk c)))
      (recv (enc n2 (pubk b)))))
  (label 0)
  (unrealized (0 2))
  (origs (n2 (0 1)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton ns
  (vars (n2 n1 text) (a b c d b-0 c-0 d-0 name))
  (defstrand resp 3 (n2 n2) (n1 n1) (b b) (a a) (c c) (d d))
  (defstrand init 3 (n1 n1) (n2 n2) (a a) (b b-0) (c c-0) (d d-0))
  (precedes ((0 1) (1 1)) ((1 2) (0 2)))
  (non-orig (privk a))
  (uniq-orig n2)
  (operation nonce-test (added-strand init 3) n2 (0 2)
    (enc n1 n2 (pubk a)))
  (traces
    ((recv (enc n1 a c d (pubk b)))
      (send (enc (enc n1 n2 (pubk a)) (pubk c)))
      (recv (enc n2 (pubk b))))
    ((send (enc n1 a c-0 d-0 (pubk b-0)))
      (recv (enc (enc n1 n2 (pubk a)) (pubk c-0)))
      (send (enc n2 (pubk b-0)))))
  (label 1)
  (parent 0)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (n2 n2) (b b) (c c) (d d) (n1 n1))))
  (origs (n2 (0 1))))

(comment "Nothing left to do")

(defprotocol ns-inj diffie-hellman
  (defrole init
    (vars (a b c d name) (n1 n2 text))
    (trace (send (enc n1 a c d (pubk b)))
      (recv (enc (enc (enc n1 n2 (pubk a)) (pubk c)) (pubk d)))
      (send (enc n2 (pubk b)))))
  (defrole resp
    (vars (b a c d name) (n2 n1 text))
    (trace (recv (enc n1 a c d (pubk b)))
      (send (enc (enc (enc n1 n2 (pubk a)) (pubk c)) (pubk d)))
      (recv (enc n2 (pubk b)))))
  (comment "Needham-Schroeder variant 2"))

(defskeleton ns-inj
  (vars (n2 n1 text) (a b c d name))
  (defstrand resp 3 (n2 n2) (n1 n1) (b b) (a a) (c c) (d d))
  (non-orig (privk a))
  (uniq-orig n2)
  (comment "Responder point-of-view")
  (traces
    ((recv (enc n1 a c d (pubk b)))
      (send (enc (enc (enc n1 n2 (pubk a)) (pubk c)) (pubk d)))
      (recv (enc n2 (pubk b)))))
  (label 2)
  (unrealized (0 2))
  (origs (n2 (0 1)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton ns-inj
  (vars (n2 n1 text) (a b c d b-0 c-0 d-0 name))
  (defstrand resp 3 (n2 n2) (n1 n1) (b b) (a a) (c c) (d d))
  (defstrand init 3 (n1 n1) (n2 n2) (a a) (b b-0) (c c-0) (d d-0))
  (precedes ((0 1) (1 1)) ((1 2) (0 2)))
  (non-orig (privk a))
  (uniq-orig n2)
  (operation nonce-test (added-strand init 3) n2 (0 2)
    (enc n1 n2 (pubk a)))
  (traces
    ((recv (enc n1 a c d (pubk b)))
      (send (enc (enc (enc n1 n2 (pubk a)) (pubk c)) (pubk d)))
      (recv (enc n2 (pubk b))))
    ((send (enc n1 a c-0 d-0 (pubk b-0)))
      (recv (enc (enc (enc n1 n2 (pubk a)) (pubk c-0)) (pubk d-0)))
      (send (enc n2 (pubk b-0)))))
  (label 3)
  (parent 2)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (n2 n2) (b b) (c c) (d d) (n1 n1))))
  (origs (n2 (0 1))))

(comment "Nothing left to do")
