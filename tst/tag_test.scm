(herald "Tag Test File"
	(algebra diffie-hellman)
   (bound 12))

(defprotocol test diffie-hellman
  (defrole rtag
    (vars (a name) (t tag))
    (trace
     (send (enc "hello world" (ltk a a)))
     (recv (enc t (ltk a a))))
    (non-orig (ltk a a)))
  (defrole rtag_mesg
    (vars (a name) (t mesg))
    (trace
     (send (enc "hello world" (ltk a a)))
     (recv (enc t (ltk a a))))
    (non-orig (ltk a a)))
  (defrole stag
    (vars (k skey) (t tag))
    (trace
     (send (enc "init" t k))
     (recv (enc t k)))
    (non-orig k))
)

;; Should force t to be unified with "hello world"
(defskeleton test
  (vars )
  (defstrand rtag 2 )
)

;; Now t could be a pair ("init" t) with an stag instance, or "hello world".
(defskeleton test
  (vars )
  (defstrand rtag_mesg 2 )
  )

;; Demonstrates that t does not have to be acquired
(defskeleton test
  (vars )
  (defstrand stag 2 )
  )

(defskeleton test
  (vars )
  (defstrand rtag_mesg 2 )
  (uniq-orig "hello world")
  )
