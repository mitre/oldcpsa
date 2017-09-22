;;; Test file to determine if we have implemented
;;; "axiom 2" of the Stateful Protocols paper correctly.

;;; This axiom states that when E1, E2, and E3 are all
;;; state events, and E1 produces a state that E3 consumes,
;;; and E2 observes the state E1 produces, then E2 occurs before E3.

(herald "Axiom 2 Protocol" (bound 20))

;;; This version places E1 and E3 in distinct strands
(defprotocol ax2 basic
  (defrole state-maker
    (vars (n text))
    (trace
     (init n)
    )
  (uniq-gen n)
  )
  (defrole state-changer
    (vars (n text))
    (trace
     (tran n (cat n n)))
  )
  (defrole state-observer
    (vars (m1 m2 mesg))
    (trace
       (obsv m1)
       (obsv m2))
  )
)

;;; Can we observe the events in the proper order?
(defskeleton ax2
  (vars (n text))
  (defstrand state-observer 2 (m1 n) (m2 (cat n n)))
)

;;; Can we observe the events in the improper order?
(defskeleton ax2
  (vars (n text))
  (defstrand state-observer 2 (m2 n) (m1 (cat n n)))
)

;;; This version places E1 and E3 in the same strand
(defprotocol ax2a basic
  (defrole state-maker-changer
    (vars (n text))
    (trace
     (init n)
     (tran n (cat n n))
    )
  (uniq-gen n)
  )
  (defrole state-observer
    (vars (m1 m2 mesg))
    (trace
       (obsv m1)
       (obsv m2))
  )
)

;;; Can we observe the events in the proper order?
(defskeleton ax2a
  (vars (n text))
  (defstrand state-observer 2 (m1 n) (m2 (cat n n)))
)

;;; Can we observe the events in the improper order?
(defskeleton ax2a
  (vars (n text))
  (defstrand state-observer 2 (m2 n) (m1 (cat n n)))
)
