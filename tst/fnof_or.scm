(herald "Otway-Rees Protocol"
	(comment "Version using variables of sort mesg, with ltk function emulated."))

(defprotocol or basic
  (defrole init (vars (a b s name) (na text) (ltkas k skey) (m text))
    (trace
     ;; Including (init s) so that s occurs in the trace.
     (init s)
     (send (cat m a b (enc na m a b ltkas)))
     (recv (cat m (enc na k ltkas))))
  (fn-of ("ltk" (ltkas (cat a s))))
  (fn-of ("ltkinv" ((cat a s) ltkas)))
 )
  (defrole resp
    (vars (a b s name) (nb text) (k ltkbs ltkas skey) (m text) (x y mesg))
    (trace
     ;; Including (init s ltkas) so that s and ltkas occur in the trace.
     (init (cat s ltkas))
     (recv (cat m a b x))
     (send (cat m a b x (enc nb m a b ltkbs)))
     (recv (cat m y (enc nb k ltkbs)))
     (send y))
  (fn-of ("ltk" (ltkas (cat a s)) (ltkbs (cat b s))))
  (fn-of ("ltkinv" ((cat a s) ltkas) ((cat b s) ltkbs)))
 )
  (defrole serv (vars (a b s name) (na nb text) (k ltkas ltkbs skey) (m text))
    (trace
     ;; Including (init s) so that s occurs in the trace.
     (init s)
     (recv (cat m a b (enc na m a b ltkas)
		(enc nb m a b ltkbs)))
     (send (cat m (enc na k ltkas) (enc nb k ltkbs))))
    (uniq-orig k)
  (fn-of ("ltk" (ltkas (cat a s)) (ltkbs (cat b s))))
  (fn-of ("ltkinv" ((cat a s) ltkas) ((cat b s) ltkbs)))
 )
)

(defskeleton or
  (vars (nb text) (s a b name) (ltkas ltkbs skey))
  (defstrand resp 4 (a a) (b b) (s s) (nb nb) (ltkas ltkas) (ltkbs ltkbs))
  (non-orig ltkas ltkbs)
  (uniq-orig nb))

;;;  In this version, fn-of is only used for the "forward" direction
;;; of the ltk function.  In other words, if y = ltk(a,b) and z =
;;; ltk(a,b) then y = z.  But if y = ltk(a,b) and z = ltk(c,d) it is
;;; not necessarily the case that if y=z then a = b and c = d.
(defprotocol or2 basic
  (defrole init (vars (a b s name) (na text) (ltkas k skey) (m text))
    (trace
     ;; Including (init s) so that s occurs in the trace.
     (init s)
     (send (cat m a b (enc na m a b ltkas)))
     (recv (cat m (enc na k ltkas))))
  (fn-of ("ltk" (ltkas (cat a s))))
 )
  (defrole resp
    (vars (a b s name) (nb text) (k ltkbs ltkas skey) (m text) (x y mesg))
    (trace
     ;; Including (init s ltkas) so that s and ltkas occur in the trace.
     (init (cat s ltkas))
     (recv (cat m a b x))
     (send (cat m a b x (enc nb m a b ltkbs)))
     (recv (cat m y (enc nb k ltkbs)))
     (send y))
  (fn-of ("ltk" (ltkas (cat a s)) (ltkbs (cat b s))))
 )
  (defrole serv (vars (a b s name) (na nb text) (k ltkas ltkbs skey) (m text))
    (trace
     ;; Including (init s) so that s occurs in the trace.
     (init s)
     (recv (cat m a b (enc na m a b ltkas)
		(enc nb m a b ltkbs)))
     (send (cat m (enc na k ltkas) (enc nb k ltkbs))))
    (uniq-orig k)
  (fn-of ("ltk" (ltkas (cat a s)) (ltkbs (cat b s))))
 )
)

(defskeleton or2
  (vars (nb text) (s a b name) (ltkas ltkbs skey))
  (defstrand resp 4 (a a) (b b) (s s) (nb nb) (ltkas ltkas) (ltkbs ltkbs))
  (non-orig ltkas ltkbs)
  (uniq-orig nb))
