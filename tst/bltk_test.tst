(herald "bltk Test File" (algebra diffie-hellman) (bound 12))

(comment "CPSA 3.6.2")
(comment "All input read from bltk_test.scm")

(defprotocol test diffie-hellman
  (defrole r
    (vars (a b name) (n text))
    (trace (send (enc n (bltk a b))) (recv (enc n (bltk a b))))
    (uniq-orig n)))

(defskeleton test
  (vars (n text) (a b name))
  (defstrand r 2 (n n) (a a) (b b))
  (non-orig (bltk a b))
  (uniq-orig n)
  (traces ((send (enc n (bltk a b))) (recv (enc n (bltk a b)))))
  (label 0)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (n n))) ((0) ((a b) (b a) (n n))))
  (origs (n (0 0))))

(comment "Nothing left to do")

(defprotocol test diffie-hellman
  (defrole r
    (vars (a b name) (n text))
    (trace (send (enc n (bltk a b))) (recv (enc n (bltk a b))))
    (uniq-orig n)))

(defskeleton test
  (vars (n text) (a b name))
  (defstrand r 2 (n n) (a a) (b b))
  (deflistener n)
  (non-orig (bltk a b))
  (uniq-orig n)
  (traces ((send (enc n (bltk a b))) (recv (enc n (bltk a b))))
    ((recv n) (send n)))
  (label 1)
  (unrealized (1 0))
  (preskeleton)
  (origs (n (0 0)))
  (comment "Not a skeleton"))

(defskeleton test
  (vars (n text) (a b name))
  (defstrand r 2 (n n) (a a) (b b))
  (deflistener n)
  (precedes ((0 0) (1 0)))
  (non-orig (bltk a b))
  (uniq-orig n)
  (traces ((send (enc n (bltk a b))) (recv (enc n (bltk a b))))
    ((recv n) (send n)))
  (label 2)
  (parent 1)
  (unrealized (1 0))
  (dead)
  (origs (n (0 0)))
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol test diffie-hellman
  (defrole r
    (vars (a b name) (n text))
    (trace (send (enc n (bltk a b))) (recv (enc n (bltk a b))))
    (uniq-orig n)))

(defskeleton test
  (vars (n text) (a b name))
  (defstrand r 2 (n n) (a a) (b b))
  (deflistener n)
  (non-orig (bltk a b))
  (uniq-orig n)
  (traces ((send (enc n (bltk a b))) (recv (enc n (bltk a b))))
    ((recv n) (send n)))
  (label 3)
  (unrealized (1 0))
  (preskeleton)
  (origs (n (0 0)))
  (comment "Not a skeleton"))

(defskeleton test
  (vars (n text) (a b name))
  (defstrand r 2 (n n) (a a) (b b))
  (deflistener n)
  (precedes ((0 0) (1 0)))
  (non-orig (bltk a b))
  (uniq-orig n)
  (traces ((send (enc n (bltk a b))) (recv (enc n (bltk a b))))
    ((recv n) (send n)))
  (label 4)
  (parent 3)
  (unrealized (1 0))
  (dead)
  (origs (n (0 0)))
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol test2 diffie-hellman
  (defrole r
    (vars (a b name) (n text))
    (trace (send (enc n (ltk a b))) (recv (enc n (ltk b a))))
    (uniq-orig n)))

(defskeleton test2
  (vars (n text) (a b name))
  (defstrand r 2 (n n) (a a) (b b))
  (non-orig (ltk a b))
  (uniq-orig n)
  (traces ((send (enc n (ltk a b))) (recv (enc n (ltk b a)))))
  (label 5)
  (unrealized (0 1))
  (origs (n (0 0)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton test2
  (vars (n text) (a name))
  (defstrand r 2 (n n) (a a) (b a))
  (non-orig (ltk a a))
  (uniq-orig n)
  (operation nonce-test (contracted (b a)) n (0 1) (enc n (ltk a a)))
  (traces ((send (enc n (ltk a a))) (recv (enc n (ltk a a)))))
  (label 6)
  (parent 5)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b a) (n n))))
  (origs (n (0 0))))

(comment "Nothing left to do")

(defprotocol test3 diffie-hellman
  (defrole recvr
    (vars (a b name) (n text))
    (trace (send (cat "i am" a "you are" b)) (recv (enc n (bltk a b)))))
  (defrole sender
    (vars (a b name) (n text))
    (trace (send (cat "i am" b "you are" a)) (send (enc n (bltk a b))))
    (uniq-orig n)))

(defskeleton test3
  (vars (n text) (a b name))
  (defstrand recvr 2 (n n) (a a) (b b))
  (non-orig (bltk a b))
  (traces ((send (cat "i am" a "you are" b)) (recv (enc n (bltk a b)))))
  (label 7)
  (unrealized (0 1))
  (origs)
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton test3
  (vars (n text) (a b name))
  (defstrand recvr 2 (n n) (a a) (b b))
  (defstrand sender 2 (n n) (a b) (b a))
  (precedes ((1 1) (0 1)))
  (non-orig (bltk a b))
  (uniq-orig n)
  (operation encryption-test (added-strand sender 2) (enc n (bltk a b))
    (0 1))
  (traces ((send (cat "i am" a "you are" b)) (recv (enc n (bltk a b))))
    ((send (cat "i am" a "you are" b)) (send (enc n (bltk a b)))))
  (label 8)
  (parent 7)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (n n))))
  (origs (n (1 1))))

(defskeleton test3
  (vars (n text) (a b name))
  (defstrand recvr 2 (n n) (a a) (b b))
  (defstrand sender 2 (n n) (a a) (b b))
  (precedes ((1 1) (0 1)))
  (non-orig (bltk a b))
  (uniq-orig n)
  (operation encryption-test (added-strand sender 2) (enc n (bltk a b))
    (0 1))
  (traces ((send (cat "i am" a "you are" b)) (recv (enc n (bltk a b))))
    ((send (cat "i am" b "you are" a)) (send (enc n (bltk a b)))))
  (label 9)
  (parent 7)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (n n))))
  (origs (n (1 1))))

(comment "Nothing left to do")
