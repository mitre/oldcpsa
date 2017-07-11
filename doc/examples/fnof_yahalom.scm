(herald "Yahalom Protocol with Forwarding Removed, using fnof to emulate ltk function"
   (bound 12))

(defprotocol yahalom basic
  (defrole init
    (vars (a b c name) (n-a n-b text) (ltkac ltkbc k skey))
    ; Including init (c ltkac) to force c, ltkac to be present in the
    ; trace.
    (trace (init (cat c ltkac))
           (send (cat a n-a))
	   (recv (enc b k n-a n-b ltkac))
	   (send (enc n-b k)))
    (fn-of ("ltk" (ltkac (cat a c)) (ltkbc (cat b c))))
    (fn-of ("ltk-inv" ((cat a c) ltkac) ((cat b c) ltkbc)))
  )
  (defrole resp
    (vars (b a c name) (n-a n-b text) (ltkac ltkbc k skey))
    ; Including init (c ltkac) to force c, ltkac to be present in the
    ; trace.
    (trace (init (cat c ltkac))
           (recv (cat a n-a))
	   (send (cat b (enc a n-a n-b ltkbc)))
	   (recv (enc a k ltkbc))
	   (recv (enc n-b k)))
    (fn-of ("ltk" (ltkac (cat a c)) (ltkbc (cat b c))))
    (fn-of ("ltk-inv" ((cat a c) ltkac) ((cat b c) ltkbc)))
  )
  (defrole serv
    (vars (c a b name) (n-a n-b text) (ltkac ltkbc k skey))
    ; Including init c to force c to be present in the trace.
    (trace (init c)
           (recv (cat b (enc a n-a n-b ltkbc)))
	   (send (enc b k n-a n-b ltkac))
	   (send (enc a k ltkbc)))
    (uniq-orig k)
    (fn-of ("ltk" (ltkac (cat a c)) (ltkbc (cat b c))))
    (fn-of ("ltk-inv" ((cat a c) ltkac) ((cat b c) ltkbc)))
  )
)

(defskeleton yahalom
  (vars (a b c name) (n-b text) (ltkac ltkbc skey))
  (defstrand resp 4 (a a) (b b) (c c) (n-b n-b) (ltkac ltkac) (ltkbc ltkbc))
  (non-orig ltkbc ltkac)
  (uniq-orig n-b)
)

;;; Ensure encryption key remains secret.
(defskeleton yahalom
  (vars (a b c name) (n-b text) (ltkac ltkbc k skey))
  (defstrand resp 4 (a a) (b b) (c c) (n-b n-b) (k k) (ltkac ltkac) (ltkbc ltkbc))
  (deflistener k)
  (non-orig ltkbc ltkac)
  (uniq-orig n-b)
)

;;;  In this version, fn-of is only used for the "forward" direction
;;; of the ltk function.  In other words, if y = ltk(a,b) and z =
;;; ltk(a,b) then y = z.  But if y = ltk(a,b) and z = ltk(c,d) it is
;;; not necessarily the case that if y = z then a = b and c = d.
(defprotocol yahalom2 basic
  (defrole init
    (vars (a b c name) (n-a n-b text) (ltkac ltkbc k skey))
    ; Including init (c ltkac) to force c, ltkac to be present in the
    ; trace.
    (trace (init (cat c ltkac))
           (send (cat a n-a))
	   (recv (enc b k n-a n-b ltkac))
	   (send (enc n-b k)))
    (fn-of ("ltk" (ltkac (cat a c)) (ltkbc (cat b c))))
  )
  (defrole resp
    (vars (b a c name) (n-a n-b text) (ltkac ltkbc k skey))
    ; Including init (c ltkac) to force c, ltkac to be present in the
    ; trace.
    (trace (init (cat c ltkac))
           (recv (cat a n-a))
	   (send (cat b (enc a n-a n-b ltkbc)))
	   (recv (enc a k ltkbc))
	   (recv (enc n-b k)))
    (fn-of ("ltk" (ltkac (cat a c)) (ltkbc (cat b c))))
  )
  (defrole serv
    (vars (c a b name) (n-a n-b text) (ltkac ltkbc k skey))
    ; Including init c to force c to be present in the trace.
    (trace (init c)
           (recv (cat b (enc a n-a n-b ltkbc)))
	   (send (enc b k n-a n-b ltkac))
	   (send (enc a k ltkbc)))
    (uniq-orig k)
    (fn-of ("ltk" (ltkac (cat a c)) (ltkbc (cat b c))))
  )
)

(defprotocol yahalom2 basic
  (defrole init
    (vars (a b c name) (n-a n-b text) (ltkac ltkbc k skey))
    (trace (send (cat a n-a (hash ltkbc)))
	   (recv (enc b k n-a n-b ltkac))
	   (send (enc n-b k)))
    (fn-of ("ltk" (ltkac (cat a c)) (ltkbc (cat b c))))
  )
  (defrole resp
    (vars (b a c name) (n-a n-b text) (ltkac ltkbc k skey))
    (trace (recv (cat a n-a))
	   (send (cat b (enc a n-a n-b ltkbc) (hash c ltkac)))
	   (recv (enc a k ltkbc))
	   (recv (enc n-b k)))
    (fn-of ("ltk" (ltkac (cat a c)) (ltkbc (cat b c))))
  )
  (defrole serv
    (vars (c a b name) (n-a n-b text) (ltkac ltkbc k skey))
    (trace (recv (cat b (enc a n-a n-b ltkbc)))
	   (send (cat (hash c) (enc b k n-a n-b ltkac)))
	   (send (enc a k ltkbc)))
    (uniq-orig k)
    (fn-of ("ltk" (ltkac (cat a c)) (ltkbc (cat b c))))
  )
)

(defskeleton yahalom2
  (vars (a b c name) (n-b text) (ltkac ltkbc skey))
  (defstrand resp 4 (a a) (b b) (c c) (n-b n-b) (ltkac ltkac) (ltkbc ltkbc))
  (non-orig ltkbc ltkac)
  (uniq-orig n-b)
)

;;; Ensure encryption key remains secret.
(defskeleton yahalom2
  (vars (a b c name) (n-b text) (ltkac ltkbc k skey))
  (defstrand resp 4 (a a) (b b) (c c) (n-b n-b) (k k) (ltkac ltkac) (ltkbc ltkbc))
  (deflistener k)
  (non-orig ltkbc ltkac)
  (uniq-orig n-b)
)
