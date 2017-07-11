(herald "Inequality constraint test protocol"
   (comment "First skeleton should have a shape,"
            "second, and hird should be dead."))

(defprotocol neqtest basic
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
(defskeleton neqtest
   (vars (n1 n2 text) (k skey))
   (defstrand init 2 (n1 n1) (n2 n2) (k k)))

;;; With the inequality declaration that n1 != n2,
;;; no shape should exist.
(defskeleton neqtest
   (vars (n1 n2 text))
   (defstrand init 2 (n1 n1) (n2 n2))
   (neq (n1 n2)))

(defprotocol neqtest2 basic
  (defrole init
     (vars (n1 n2 n3 text) (k skey))
     (trace
       (send (cat n1 (enc n1 n2 n3 k)))
       (recv n2))
     (non-orig k)
     (uniq-orig n1 n2)
  )
)

;;; Here, we use the neqlist declaration to force
;;; deadness, as an alternative.
(defskeleton neqtest2
   (vars (n1 n2 n3 text) (k skey))
   (defstrand init 2 (n1 n1) (n2 n2) (n3 n3) (k k))
   (neqlist ((n1 n2 n3)))
)
