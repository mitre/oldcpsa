(herald "Receive priority test protocol"
  )

(defprotocol priority_test basic
  (defrole init
     (vars (n1 n2 n3 n4 text) (k skey))
     (trace
       (send (enc n1 n1 k))
       (recv (enc n1 n2 k))
       (recv (enc n1 n3 k))
       (recv (enc n1 n4 k)))
     (priority (2 0) (3 10))
     (non-orig k)
     (uniq-orig n1)
  )
)

; Node 4 should realize first, then node 2, then node 3.
(defskeleton priority_test
   (vars (n1 text))
   (defstrand init 4 (n1 n1)))

; Node 3 should realize first, then node 2, then node 4.
(defskeleton priority_test
   (vars (n1 text))
   (defstrand init 4 (n1 n1))
   (priority ((0 2) 10) ((0 3) 0)))
