(herald "Axiom 2 Protocol" (bound 20))

(comment "CPSA 3.6.0")
(comment "All input read from axiom2.scm")
(comment "Strand count bounded at 20")

(defprotocol ax2 basic
  (defrole state-maker (vars (n text)) (trace (init n)) (uniq-gen n))
  (defrole state-changer (vars (n text)) (trace (tran n (cat n n))))
  (defrole state-observer
    (vars (m1 m2 mesg))
    (trace (obsv m1) (obsv m2))))

(defskeleton ax2
  (vars (n text))
  (defstrand state-observer 2 (m1 n) (m2 (cat n n)))
  (traces ((obsv n) (obsv (cat n n))))
  (label 0)
  (unrealized (0 0) (0 1))
  (origs)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton ax2
  (vars (n text))
  (defstrand state-observer 2 (m1 n) (m2 (cat n n)))
  (defstrand state-maker 1 (n n))
  (precedes ((1 0) (0 0)))
  (leadsto ((1 0) (0 0)))
  (uniq-gen n)
  (operation state-passing-test (added-strand state-maker 1) n (0 0))
  (traces ((obsv n) (obsv (cat n n))) ((init n)))
  (label 1)
  (parent 0)
  (unrealized (0 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton ax2
  (vars (n text))
  (defstrand state-observer 2 (m1 n) (m2 (cat n n)))
  (defstrand state-maker 1 (n n))
  (defstrand state-changer 1 (n n))
  (precedes ((1 0) (0 0)) ((1 0) (2 0)) ((2 0) (0 1)))
  (leadsto ((1 0) (0 0)) ((2 0) (0 1)))
  (uniq-gen n)
  (operation state-passing-test (added-strand state-changer 1) (cat n n)
    (0 1))
  (traces ((obsv n) (obsv (cat n n))) ((init n)) ((tran n (cat n n))))
  (label 2)
  (parent 1)
  (unrealized (2 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton ax2
  (vars (n text))
  (defstrand state-observer 2 (m1 n) (m2 (cat n n)))
  (defstrand state-maker 1 (n n))
  (defstrand state-changer 1 (n n))
  (precedes ((0 0) (2 0)) ((1 0) (0 0)) ((1 0) (2 0)) ((2 0) (0 1)))
  (leadsto ((1 0) (0 0)) ((1 0) (2 0)) ((2 0) (0 1)))
  (uniq-gen n)
  (operation state-passing-test (displaced 3 1 state-maker 1) n (2 0))
  (traces ((obsv n) (obsv (cat n n))) ((init n)) ((tran n (cat n n))))
  (label 3)
  (parent 2)
  (unrealized)
  (shape)
  (maps ((0) ((n n))))
  (origs))

(comment "Nothing left to do")

(defprotocol ax2 basic
  (defrole state-maker (vars (n text)) (trace (init n)) (uniq-gen n))
  (defrole state-changer (vars (n text)) (trace (tran n (cat n n))))
  (defrole state-observer
    (vars (m1 m2 mesg))
    (trace (obsv m1) (obsv m2))))

(defskeleton ax2
  (vars (n text))
  (defstrand state-observer 2 (m1 (cat n n)) (m2 n))
  (traces ((obsv (cat n n)) (obsv n)))
  (label 4)
  (unrealized (0 0) (0 1))
  (origs)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton ax2
  (vars (n text))
  (defstrand state-observer 2 (m1 (cat n n)) (m2 n))
  (defstrand state-changer 1 (n n))
  (precedes ((1 0) (0 0)))
  (leadsto ((1 0) (0 0)))
  (operation state-passing-test (added-strand state-changer 1) (cat n n)
    (0 0))
  (traces ((obsv (cat n n)) (obsv n)) ((tran n (cat n n))))
  (label 5)
  (parent 4)
  (unrealized (0 1) (1 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton ax2
  (vars (n text))
  (defstrand state-observer 2 (m1 (cat n n)) (m2 n))
  (defstrand state-changer 1 (n n))
  (defstrand state-maker 1 (n n))
  (precedes ((1 0) (0 0)) ((2 0) (1 0)))
  (leadsto ((1 0) (0 0)) ((2 0) (1 0)))
  (uniq-gen n)
  (operation state-passing-test (added-strand state-maker 1) n (1 0))
  (traces ((obsv (cat n n)) (obsv n)) ((tran n (cat n n))) ((init n)))
  (label 6)
  (parent 5)
  (unrealized (0 1))
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol ax2a basic
  (defrole state-maker-changer
    (vars (n text))
    (trace (init n) (tran n (cat n n)))
    (uniq-gen n))
  (defrole state-observer
    (vars (m1 m2 mesg))
    (trace (obsv m1) (obsv m2))))

(defskeleton ax2a
  (vars (n text))
  (defstrand state-observer 2 (m1 n) (m2 (cat n n)))
  (traces ((obsv n) (obsv (cat n n))))
  (label 7)
  (unrealized (0 0) (0 1))
  (origs)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton ax2a
  (vars (n text))
  (defstrand state-observer 2 (m1 n) (m2 (cat n n)))
  (defstrand state-maker-changer 1 (n n))
  (precedes ((1 0) (0 0)))
  (leadsto ((1 0) (0 0)))
  (uniq-gen n)
  (operation state-passing-test (added-strand state-maker-changer 1) n
    (0 0))
  (traces ((obsv n) (obsv (cat n n))) ((init n)))
  (label 8)
  (parent 7)
  (unrealized (0 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton ax2a
  (vars (n text))
  (defstrand state-observer 2 (m1 n) (m2 (cat n n)))
  (defstrand state-maker-changer 2 (n n))
  (precedes ((1 0) (0 0)) ((1 1) (0 1)))
  (leadsto ((1 0) (0 0)) ((1 1) (0 1)))
  (uniq-gen n)
  (operation state-passing-test (displaced 1 2 state-maker-changer 2)
    (cat n n) (0 1))
  (traces ((obsv n) (obsv (cat n n))) ((init n) (tran n (cat n n))))
  (label 9)
  (parent 8)
  (unrealized (1 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton ax2a
  (vars (n text))
  (defstrand state-observer 2 (m1 n) (m2 (cat n n)))
  (defstrand state-maker-changer 2 (n n))
  (precedes ((0 0) (1 1)) ((1 0) (0 0)) ((1 1) (0 1)))
  (leadsto ((1 0) (0 0)) ((1 0) (1 1)) ((1 1) (0 1)))
  (uniq-gen n)
  (operation state-passing-test (displaced 2 1 state-maker-changer 1) n
    (1 1))
  (traces ((obsv n) (obsv (cat n n))) ((init n) (tran n (cat n n))))
  (label 10)
  (parent 9)
  (unrealized)
  (shape)
  (maps ((0) ((n n))))
  (origs))

(comment "Nothing left to do")

(defprotocol ax2a basic
  (defrole state-maker-changer
    (vars (n text))
    (trace (init n) (tran n (cat n n)))
    (uniq-gen n))
  (defrole state-observer
    (vars (m1 m2 mesg))
    (trace (obsv m1) (obsv m2))))

(defskeleton ax2a
  (vars (n text))
  (defstrand state-observer 2 (m1 (cat n n)) (m2 n))
  (traces ((obsv (cat n n)) (obsv n)))
  (label 11)
  (unrealized (0 0) (0 1))
  (origs)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton ax2a
  (vars (n text))
  (defstrand state-observer 2 (m1 (cat n n)) (m2 n))
  (defstrand state-maker-changer 2 (n n))
  (precedes ((1 1) (0 0)))
  (leadsto ((1 1) (0 0)))
  (uniq-gen n)
  (operation state-passing-test (added-strand state-maker-changer 2)
    (cat n n) (0 0))
  (traces ((obsv (cat n n)) (obsv n)) ((init n) (tran n (cat n n))))
  (label 12)
  (parent 11)
  (unrealized (0 1) (1 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton ax2a
  (vars (n text))
  (defstrand state-observer 2 (m1 (cat n n)) (m2 n))
  (defstrand state-maker-changer 2 (n n))
  (precedes ((1 1) (0 0)))
  (leadsto ((1 0) (1 1)) ((1 1) (0 0)))
  (uniq-gen n)
  (operation state-passing-test (displaced 2 1 state-maker-changer 1) n
    (1 1))
  (traces ((obsv (cat n n)) (obsv n)) ((init n) (tran n (cat n n))))
  (label 13)
  (parent 12)
  (unrealized (0 1))
  (comment "empty cohort"))

(comment "Nothing left to do")
