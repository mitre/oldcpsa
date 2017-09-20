(herald "Diffie-Hellman protocol, man-in-the-middle attack" (algebra diffie-hellman) (bound 20))

(comment
(defprotocol foo diffie-hellman
  (defrole init
    (vars (h base) (k akey) (x expn))
    (trace
       (send (enc (exp (gen) x) k))
       (recv (enc (exp h x) k))
    )
    (non-orig (invk k))
    (uniq-gen x)
  )
)

(defskeleton foo
   (vars (h base))
   (defstrand init 2 (h h)))

(defprotocol bar diffie-hellman
  (defrole init
    (vars (h base) (x z expn))
    (trace
       (send (exp (gen) x))
       (recv (exp (gen) (mul x z)))
    )
    (uniq-gen x)
  )
)

(defskeleton bar
  (vars (x expn))
  (defstrand init 2 (x x)))

(defprotocol bar2 diffie-hellman
  (defrole init
    (vars (h base) (x z expn))
    (trace
       (send (exp h x))
       (recv (exp (gen) x))
    )
    (uniq-gen x)
  )
)

(defskeleton bar2
  (vars (x expn))
  (defstrand init 2 (x x)))

(defprotocol baz diffie-hellman
  (defrole init
    (vars (h base) (x z expn) (k akey))
    (trace
       (send (enc (exp (gen) x) k))
       (recv (enc (exp (gen) (mul x z)) k))
    )
    (uniq-gen x)
    (non-orig (invk k))
  )
)

(defskeleton baz
  (vars (x expn))
  (defstrand init 2 (x x)))

(defprotocol bazar diffie-hellman
  (defrole init
    (vars (h base) (t text) (x z expn) (k skey))
    (trace
       (send (enc (exp (gen) (mul x z)) k))
       (recv (exp (gen) x))
    )
    (uniq-orig t)
    (pen-non-orig k)
    (uniq-gen x)
  )
  (defrole revealer
    (vars (m mesg) (k skey))
    (trace
       (recv (enc m k))
       (send k)))
)

(defskeleton bazar
  (vars (k skey))
  (defstrand init 2 (k k)))

(defprotocol foo2 diffie-hellman
  (defrole init
    (vars (h base) (x expn))
    (trace
       (send (cat (exp (gen) x)
		  (enc "foo" (exp h x))))
       (recv (exp h x)))
    (uniq-gen x)))

(defskeleton foo2
   (vars )
   (defstrand init 2))
)

(defprotocol foo3 diffie-hellman
  (defrole resp
    (vars (h base))
    (trace (recv h)))
  (defrole init
    (vars (x expn))
    (trace (send (exp (gen) x)))))

(defskeleton foo3
   (vars (x expn))
   (defstrand resp 1 (h (exp (gen) x)))
   (non-orig x))
