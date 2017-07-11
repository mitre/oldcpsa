(herald wrap-decrypt
   (bound 10)
;  (limit 1)
)

(defprotocol wrap-decrypt basic
  (defrole make
    (vars (k skey))
    (trace
     (init (cat k "init"))
     (send (hash k)))
    (uniq-gen k)
)
  (defrole set-wrap
    (vars (k skey) (cur mesg))
    (trace
     (tran (cat k cur) (cat k "wrap")))
    (neq (cur "wrap"))
)
  (defrole set-decrypt
    (vars (k skey) (cur mesg))
    (trace
     (tran (cat k cur) (cat k "decrypt")))
    (neq (cur "decrypt"))
)
  (defrole wrap
    (vars (k0 k1 skey) (cur mesg))
    (trace
     (recv (hash k0))
     (recv (hash k1))
     (obsv (cat k0 cur))
     (obsv (cat k1 "wrap"))
     (send (enc k0 k1))))
  (defrole decrypt
    (vars (x mesg) (k skey))
    (trace
     (recv (enc x k))
     (recv (hash k))
     (obsv (cat k "decrypt"))
     (send x))))

(defskeleton wrap-decrypt
  (vars (k kp skey))
  (deflistener k)
  (defstrand decrypt 4 (x k) (k kp))
  (defstrand make 2 (k kp))
  (defstrand set-decrypt 1 (k kp) (cur "wrap"))
  (defstrand set-wrap 1 (k kp) (cur "init"))
  (defstrand wrap 5 (k0 k) (k1 kp) (cur "init"))
  (defstrand make 2 (k k))
  (precedes ((1 3) (0 0)) ((2 0) (4 0)) ((2 1) (5 1)) ((3 0) (1 2)) ((4 0) (5 3)) 
	    ((4 0) (3 0)) ((5 4) (1 0)) ((6 0) (5 2)) ((6 1) (5 0))))

(defskeleton wrap-decrypt
  (vars (k skey))
  (deflistener k)
  (pen-non-orig k))
