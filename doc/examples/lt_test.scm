(herald "Less-than constraint test protocol"
   (comment "First and third skeletons should have a shape,"
            "second and fourth should be dead."))

(defprotocol lttest basic
  (defrole init
     (vars (n1 n2 text) (k skey))
     (trace
       (send (cat n1 (enc n1 n2 k)))
       (recv n2))
     (non-orig k)
     (uniq-orig n1 n2)
  )
)

;;; With no declaration, a shape
;;; should be found where n1 = n2.
(defskeleton lttest
   (vars (n1 n2 text) (k skey))
   (defstrand init 2) (n1 n1) (n2 n2) (k k))

;;; With the declaration that n1 < n2,
;;; no shape should exist, since n1 = n2 would
;;; be a cycle.
(defskeleton lttest
   (vars (n1 n2 text))
   (defstrand init 2 (n1 n1) (n2 n2))
   (lt (n1 n2)))

(defprotocol lttest2 basic
  (defrole init
     (vars (n1 n2 n text) (k skey))
     (trace
       (send n)
       (send (cat n1 (enc n1 n2 k)))
       (recv n2))
     (non-orig k)
     (uniq-orig n1 n2)
     (lt (n2 n))
  )
)

;;; In this version, the single lt declaration
;;; should not interfere.
(defskeleton lttest2
   (vars (n n1 n2 text) (k skey))
   (defstrand init 3) (n1 n1) (n2 n2) (k k))

;;; In this version, the additional requirement
;;; that n1 < n prevents n1 and n2 from being
;;; unified.
(defskeleton lttest2
   (vars (n n1 n2 text) (k skey))
   (defstrand init 3 (n1 n1) (n2 n2) (k k) (n n))
   (lt (n n1)))
