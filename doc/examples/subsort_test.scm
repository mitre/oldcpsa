(herald "Subsort constraint test protocol"
   (comment "First, third, and fourth skeletons should have a shape,"
            "second should be dead."))

(defprotocol subsorttest basic
  (defrole init
     (vars (n1 n2 text) (k skey))
     (trace
       (send (cat n1 (enc n1 n2 k)))
       (recv n2))
     (non-orig k)
     (uniq-orig n1 n2)
  )
)

;;; With no declaration, a shape should be found where n1 = n2.
(defskeleton subsorttest
   (vars (n1 n2 text) (k skey))
   (defstrand init 2) (n1 n1) (n2 n2) (k k))

;;; By declaring n1 to be of subsort A and n2 to be of subsort B, the
;;; unification of n1 and n2 is prevented, so no shape should exist.
(defskeleton subsorttest
   (vars (n1 n2 text))
   (defstrand init 2 (n1 n1) (n2 n2))
   (subsort (A n1) (B n2))
)

;;; Here, only n1 is declared to be of a subsort.  Unification with n2
;;; is allowed, so a shape should be found.
(defskeleton subsorttest
   (vars (n1 n2 text))
   (defstrand init 2 (n1 n1) (n2 n2))
   (subsort (A n1))
)

;;; Here, both n1 and n2 are declared to be of the same subsort (A),
;;; which should allow their unification.
(defskeleton subsorttest
   (vars (n1 n2 text))
   (defstrand init 2 (n1 n1) (n2 n2))
   (subsort (A n1 n2))
)
