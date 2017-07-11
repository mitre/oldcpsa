(herald "Unique generation test protocols."
   (comment "Skeletons 2, 4, and 7 should have no shapes."))

(defprotocol uniqgentest basic
  (defrole init
    (vars (a name) (k skey))
    (trace
       (send (enc a k))
       (recv (enc a a k))
    )
   )
  (defrole doubler
    (vars (a name) (k skey))
    (trace
       (send (enc a a k)))
  )
  (defrole resp
    (vars (a name) (k skey))
    (trace
     (recv (enc a k))
     (send (enc a a k)))
    )
)

;; This input, if enabled, should cause an 
;; error, since k is not carried anywhere.
;(defskeleton uniqgentest 
;   (vars (k skey))
;   (defstrand init 2 (k k))
;   (uniq-orig k))

;; This input should have a shape in which a
;; doubler produces the message needed by the
;; initiator.
(defskeleton uniqgentest
   (vars (k skey))
   (defstrand init 2 (k k))
   (non-orig k))

;; This input should have no shape, because
;; declaring k to be uniq-gen does not allow
;; a doubler strand: the doubler strand generates
;; its key.
(defskeleton uniqgentest
   (vars (k skey))
   (defstrand init 2 (k k))
   (uniq-gen k))

