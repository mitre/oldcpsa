(herald "IADH: unified model (UM)" (bound 20) (algebra diffie-hellman))

(defprotocol iadh-um diffie-hellman
  (defrole init
    (vars (l e expn) (hl he base) (A B CA name) (key n data))
    (trace 
     (recv (enc "cert" (exp (gen) l) A (privk CA)))
     (recv (enc "cert" hl B (privk CA)))
     (send (exp (gen) e))
     (recv he)
     (send key))
    (non-orig (privk CA))
    (fn-of (foo ((hash (exp hl l) (exp he e)) key)))
    (neq (he (gen)) (hl (exp (gen) l))))
  (defrole ltkgen
    (vars (P name) (l expn))
    (trace
     (send (enc "cert-req" P (exp (gen) l) (privk P))))
    (fn-of ("principal-of" (P (exp (gen) l)))))
  (defrole ca-gen
    (vars (P CA name) (h base))
    (trace 
     (recv (enc "cert-req" P h (privk P)))
     (send (enc "cert" h P (privk CA))))
    (non-orig (privk P))
    (neq (h (gen)))
    (fn-of ("principal-of" (P h)))))

;; Implicit authentication: resp and init will agree on the resulting key
;; foo(key).  Therefore they should agree on the names A and B.
(defskeleton iadh-um
  (vars (key data) (e ep l lp expn))
  (defstrand init 5 (key key) (e e) (l l))
  (defstrand init 5 (key key) (e ep) (l lp))
  (defstrand ltkgen 1 (l l))
  (defstrand ltkgen 1 (l lp))
  (uniq-gen e ep l lp)
)

;; Key privacy: all keys good
(defskeleton iadh-um
  (vars (he base) (e l ep lp expn))
  (defstrand init 4 (e e) (l l) (hl (exp (gen) lp)) (he he))
  (defstrand ltkgen 1 (l l))
  (defstrand ltkgen 1 (l lp))
  (deflistener (hash (exp (gen) (mul l lp)) (exp he e)))
  (uniq-gen e lp l)
)

;; Key privacy: both long term keys good, own ephemeral key leaks.
(defskeleton iadh-um
  (vars (he base) (e l ep lp expn))
  (defstrand init 4 (e e) (l l) (hl (exp (gen) lp)) (he he))
  (defstrand ltkgen 1 (l l))
  (defstrand ltkgen 1 (l lp))
  (deflistener (hash (exp (gen) (mul l lp)) (exp he e)))
  (uniq-gen lp l)
)

;; Participant's point of view: own LTK compromised, partner's good
(defskeleton iadh-um
  (vars (he base) (e l ep lp expn))
  (defstrand init 4 (e e) (hl (exp (gen) lp)) (he he))
  (defstrand ltkgen 1 (l lp))
  (deflistener (hash (exp (gen) (mul l lp)) (exp he e)))
  (uniq-gen e lp)
)
;)

;; Participant's point of view: partner's LTK compromised, own good
(defskeleton iadh-um
  (vars (he hl base) (e ep l lp expn))
  (defstrand init 4 (e e) (l l) (hl hl) (he he))
  (defstrand ltkgen 1 (l l))
  (deflistener (hash (exp hl l) (exp he e)))
  (uniq-gen e l)
)
;)

;; Participant's point of view: both LTKs compromised
(defskeleton iadh-um
  (vars (he hl base) (e l ep lp expn))
  (defstrand init 4 (e e) (l l) (he he) (hl hl))
  (deflistener (hash (exp hl l) (exp he e)))
  (uniq-gen e)
)
;)

