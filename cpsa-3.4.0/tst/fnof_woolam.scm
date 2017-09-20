(herald "Woo-Lam Protocol, using fnof to emulate ltk function")

(defprotocol woolam basic
  (defrole init (vars (a s name) (n text) (ltkas skey))
    (trace
     (init s)
     (send a)
     (recv n)
     (send (enc n ltkas)))
    (non-orig ltkas)
    (fn-of ("ltk" (ltkas (cat a s))))
    (fn-of ("ltk-inv" ((cat a s) ltkas)))
 )
  (defrole resp (vars (a s b name) (n text) (ltkas ltkbs skey))
    (trace
     (init (cat b s))
     (recv a)
     (send n)
     (recv (enc n ltkas))
     (send (enc a (enc n ltkas) ltkbs))
     (recv (enc a n ltkbs)))
    (non-orig ltkbs)
    (uniq-orig n)
    (fn-of ("ltk" (ltkas (cat a s)) (ltkbs (cat b s))))
    (fn-of ("ltk-inv" ((cat a s) ltkas) ((cat b s) ltkbs)))
 )
  (defrole serv (vars (a s b name) (n text) (ltkbs ltkas skey))
    (trace
     (init (cat b s))
     (recv (enc a (enc n ltkas) ltkbs))
     (send (enc a n ltkbs)))
    (fn-of ("ltk" (ltkas (cat a s)) (ltkbs (cat b s))))
    (fn-of ("ltk-inv" ((cat a s) ltkas) ((cat b s) ltkbs)))
 )
)

(defskeleton woolam (vars (n text) (a s name) (ltkas skey))
  (defstrand resp 5 (a a) (s s) (ltkas ltkas))
  (non-orig ltkas))

;;;  In this version, fn-of is only used for the "forward" direction
;;; of the ltk function.  In other words, if y = ltk(a,b) and z =
;;; ltk(a,b) then y = z.  But if y = ltk(a,b) and z = ltk(c,d) it is
;;; not necessarily the case that a = b and c = d.
(defprotocol woolam2 basic
  (defrole init (vars (a s name) (n text) (ltkas skey))
    (trace
     (init s)
     (send a)
     (recv n)
     (send (enc n ltkas)))
    (non-orig ltkas)
    (fn-of ("ltk" (ltkas (cat a s))))
 )
  (defrole resp (vars (a s b name) (n text) (ltkas ltkbs skey))
    (trace
     (init (cat b s))
     (recv a)
     (send n)
     (recv (enc n ltkas))
     (send (enc a (enc n ltkas) ltkbs))
     (recv (enc a n ltkbs)))
    (non-orig ltkbs)
    (uniq-orig n)
    (fn-of ("ltk" (ltkas (cat a s)) (ltkbs (cat b s))))
 )
  (defrole serv (vars (a s b name) (n text) (ltkbs ltkas skey))
    (trace
     (init (cat b s))
     (recv (enc a (enc n ltkas) ltkbs))
     (send (enc a n ltkbs)))
    (fn-of ("ltk" (ltkas (cat a s)) (ltkbs (cat b s))))
 )
)

(defskeleton woolam2 (vars (n text) (ltkas skey) (a s name))
  (defstrand resp 5 (a a) (s s) (ltkas ltkas))
  (non-orig ltkas))
