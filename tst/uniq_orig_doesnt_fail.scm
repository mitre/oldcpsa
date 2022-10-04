(herald "Uniq orig fail"
	(comment This protocol demonstrates a bug in CPSA: points of
origination are not always preserved for skeleton uniq-orig
assumptions))

(defprotocol uof basic
  (defrole init
    (vars (a b name) (n1 n2 text))
    (trace
     (send n1)
     (recv n2)
     (send (enc n1 n2 n1 (ltk a b)))))
  (defrole resp
    (vars (b a name) (n2 n1 text))
    (trace
     (recv n1)
     (send n2)
     (recv (enc n2 n1 n1 (ltk a b))))
  )
)

;;; The responder point-of-view
(defskeleton uof
  (vars (a b name) (n2 text))
  (defstrand resp 3 (a a) (b b) (n2 n2))
  (non-orig (ltk a b))
  (uniq-orig n2)
  (comment "Responder point-of-view"))

;;; The responder point-of-view
(defskeleton uof
  (vars (a b name) (n2 text))
  (defstrand resp 3 (a a) (b b) (n2 n2))
  (non-orig (ltk a b))
  (uniq-gen n2)
  (comment "Responder point-of-view"))

;;; The responder point-of-view
(defskeleton uof
  (vars (a b name) (n2 text))
  (defstrand resp 3 (a a) (b b) (n2 n2))
  (non-orig (ltk a b))
  (comment "Responder point-of-view"))

(defprotocol uof2 basic
  (defrole init
    (vars (a b name) (n1 n2 text))
    (trace
     (send n1)
     (recv n2)
     (send (enc n1 n2 (ltk a b))))
    (uniq-orig n1)
    )
  (defrole resp
    (vars (b a name) (n2 n1 text))
    (trace
     (recv n1)
     (send n2)
     (recv (enc n2 n1 (ltk a b))))
  )
)

;;; The responder point-of-view
(defskeleton uof2
  (vars (a b name) (n2 text))
  (defstrand resp 3 (a a) (b b) (n2 n2))
  (non-orig (ltk a b))
  (uniq-orig n2)
  (comment "Responder point-of-view"))
