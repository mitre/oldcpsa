(herald "Equality constraint test protocol"
   (comment "First skeleton should have a shape,"
            "second, and hird should be dead."))

(defprotocol eqtest basic
  (defrole init
     (vars (n1 n2 text) (k skey))
     (trace
       (send (cat n1 (enc n1 n2 k)))
       (recv n2))
     (non-orig k)
     (uniq-orig n1 n2)
  )
)

;;; With no inequality declaration, a shape
;;; should be found where n1 = n2.
(defskeleton eqtest
   (vars (n1 n2 text) (k skey))
   (defstrand init 2 (n1 n1) (n2 n2) (k k)))

;;; With the equality declaration, n1 = n2 should
;;; be found by skeletonizing.
(defskeleton eqtest
   (vars (n1 n2 text))
   (defstrand init 2 (n1 n1) (n2 n2))
   (eq (n1 n2)))

;;; An fn-of declaration should produce a similar result.
(defskeleton eqtest
  (vars (n1 n2 text))
  (defstrand init 2 (n1 n1) (n2 n2))
  (fn-of ("foo" (n1 "bar") (n2 "bar"))))
