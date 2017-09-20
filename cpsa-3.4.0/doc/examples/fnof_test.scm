(herald "Function constraint test protocol"
   (comment "Skeletons 2, 4, and 7 should have no shapes."))

(defprotocol fnoftest basic
  (defrole init
     (vars (an0 an1 n0 n1 text) (k skey))
     (trace
       (send (cat n0 (enc an0 an1 n0 n1 k)))
       (recv n1))
     (non-orig k)
     (uniq-orig n0 n1)
  )
)

;;; In this skeleton, after n0 and n1 are unified, an0 and an1 will
;;; remain distinct.
(defskeleton fnoftest
   (vars (an0 an1 n0 n1 text) (k skey))
   (defstrand init 2 (an0 an0) (an1 an1) (n0 n0) (n1 n1) (k k))
)

;;; This skeleton will be dead.  The fn-of declaration forces an0 =
;;; an1 which violates the neq declaration.
(defskeleton fnoftest
   (vars (an0 an1 n0 n1 text) (k skey))
   (defstrand init 2 (an0 an0) (an1 an1) (n0 n0) (n1 n1) (k k))
   (neq (an0 an1))
   (fn-of (a (an0 n0) (an1 n1)))
)

;;; Tests whether rectification will happen properly on initial load,
;;; since n1 and n0 are already unified.
(defskeleton fnoftest
   (vars (an0 an1 n1 text) (k skey))
   (defstrand init 2 (an0 an0) (an1 an1) (n0 n1) (n1 n1) (k k))
   (fn-of (a (an0 n1) (an1 n1)))
)

;;; Tests whether rectification will happen properly on initial load
;;; when it leads to an impossibility.
(defskeleton fnoftest
   (vars (an0 an1 n1 text) (k skey))
   (defstrand init 2 (an0 an0) (an1 an1) (n0 n1) (n1 n1) (k k))
   (fn-of (a (an0 n1) (an1 n1)))
   (neq (an0 an1))
)

(defprotocol fnoftest2 basic
  (defrole init
     (vars (bn0 bn1 name) (an0 an1 n0 n1 text) (k skey))
     (trace
       (send (cat n0 (enc bn0 bn1 an0 an1 n0 n1 k)))
       (recv n1))
     (non-orig k)
     (uniq-orig n0 n1)
     (fn-of (a (an0 n0) (an1 n1)))
     (fn-of (b ((pubk bn0) an0)))
  )
)

;;; This skeleton should be fine, but bn0 = bn1.  Tests cascading
;;; requirements, as the bn0 = bn1 requirement only arises after an0 =
;;; an1 happens.
(defskeleton fnoftest2
   (vars (bn0 bn1 name) (an0 an1 n0 n1 text) (k skey))
   (defstrand init 2 (bn0 bn0) (bn1 bn1) (an0 an0) (an1 an1) (n0 n0) (n1 n1) (k k))
   (fn-of (b ((pubk bn1) an1)))
)

;;; Here, bn0 and bn1 are not unified because they are the result of
;;; different functions, even once the inputs are unified.
(defskeleton fnoftest2
   (vars (bn0 bn1 name) (an0 an1 n0 n1 text) (k skey))
   (defstrand init 2 (bn0 bn0) (bn1 bn1) (an0 an0) (an1 an1) (n0 n0) (n1 n1) (k k))
   (fn-of (c ((pubk bn1) an1)))
   (decl foo ((n0 n1) (0 1)) ((k k k n0) (0 0) (0 0) (0 1)))
)

;;; This should be dead due to a cascade, because pubk bn0 cannot be
;;; unified with privk bn0.
(defskeleton fnoftest2
   (vars (bn0 bn1 name) (an0 an1 n0 n1 text) (k skey))
   (defstrand init 2 (bn0 bn0) (bn1 bn1) (an0 an0) (an1 an1) (n0 n0) (n1 n1) (k k))
   (fn-of (b ((privk bn0) an1)))
   (decl foo ((n0 n1) (0 1)) ((k k k n0) (0 0) (0 0) (0 1)))
)
