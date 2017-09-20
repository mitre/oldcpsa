;; CPSA input file to represent
;; Wang's Fair Exchange Protocol

;; Wang's encrypted format for the
;; message payload.  Observe that m,
;; k get their values from the
;; like-named messages in the context
;; where the macro is expanded.

(defmacro (em)
  (enc m k))

;; The session label L as a fn of
;; the values that *should be* the
;; hashes of the encrypted message EM
;; and the session key K.

(defmacro (l-prime y x)
  (cat a b t y x))

;; The recipient can check that a
;; session label has an l-prime where
;; y is the hash of another parameter.
;; Hence recipient roles use l-star.

(defmacro (l-star e1 x)
  (l-prime (hash e1) x))

;; The Evidence of Origin is checked
;; to be this fn of other parameters
;; available to the recipient.

(defmacro (eoo-star x e1 e2)
  (enc (cat "eootag"
	    (hash (l-star e1 x)) e2)
       (privk "sign" a)))

;; The Evidence of Receipt is computed
;; as this fn of other parameters
;; available to the recipient.

(defmacro (eor-star x e1 e2)
  (enc (cat "eortag"
	    (hash (l-star e1 x)) e2)
       (privk "sign" b)))

;; The Recovery Request as computed by
;; the recipient.

(defmacro (rr-star x e1 e2)
  (cat (l-star e1 x) e2
       (eoo-star x e1 e2)
       (eor-star x e1 e2)
       (enc (cat "rcrq" (l-star e1 x) e2)
            (privk "sign" b))))

;; The Abort Token is checked
;; to be this fn of other parameters
;; available to the recipient.

(defmacro (at-star e1 x)
  (enc (enc "abrq" (l-star e1 x)
            (privk "sign" a))
       (privk "sign" t)))

;; How the initiator computes the
;; encrypted key package:

(defmacro (ek l)
  (enc (cat "keytag" (hash l) k r)
       (pubk "encr" t)))

;; The Initiator's computed
;; version of L.

(defmacro (l)
  (l-prime (hash (enc m k)) (hash k)))

;; The TTP can check that the session
;; label L is this fn of other
;; parameters available to the TTP.

(defmacro (l-prime-prime y)
  (cat a b t y (hash k)))

;; The TTP can check that the EOO
;; is this fn of other parameters
;; available to the TTP.

(defmacro (eoo-prime-prime y)
  (enc (cat "eootag"
            (hash (l-prime-prime y))
	    (ek (l-prime-prime y)))
       (privk "sign" a)))

;; The TTP can check that the EOR
;; is this fn of other parameters
;; available to the TTP.

(defmacro (eor-prime-prime y)
  (enc (cat "eortag"
	    (hash (l-prime-prime y))
	    (ek (l-prime-prime y)))
       (privk "sign" b)))

;; The TTP can check that the RR
;; is this fn of other parameters
;; available to the TTP.

(defmacro (rr-prime-prime y)
  (cat (l-prime-prime y)
       (ek (l-prime-prime y))
       (eoo-prime-prime y)
       (eor-prime-prime y)
       (enc "rcrq" (l-prime-prime y)
	    (ek (l-prime-prime y))
            (privk "sign" b))))

;; The TTP can check that the Confirmation
;; Request is this fn of other parameters
;; available to the TTP.

(defmacro (cf-prime-prime y)
  (cat (l-prime-prime y) (ek (l-prime-prime y))
       (eoo-prime-prime y) (eor-prime-prime y)
       (enc "cfrq" (l-prime-prime y)
	    (ek (l-prime-prime y))
            (privk "sign" b))))

;; Defn of Wang's Fair Exchange Protocol.
;; Closely follows the presentation in the
;; figures in Section 3.  In the TTP roles,
;; we have put a dummy send or receive in place
;; of the state synchronization event to keep
;; the indices matching the spec.

(defprotocol wang basic

  ;; Successful initiator run with no TTP involvement

  (defrole init1
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
     (send (cat (l) (enc m k) (ek (l))
                (eoo-star (hash k) (enc m k) (ek (l)))))
     (recv (eor-star (hash k) (enc m k) (ek (l))))
     (send (cat k r))))

  ;; Aborted initiator run

  (defrole init2
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
     (send (cat (l) (enc m k) (ek (l))
                (eoo-star (hash k) (enc m k) (ek (l)))))
     (send (enc "abrq" (l) (privk "sign" a)))
     (recv (enc "abcf"
                (enc "abrq" (l) (privk "sign" a))
                (privk "sign" t)))))

  ;; Initiator run with abort request and forced recovery

  (defrole init3
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
     (send (cat (l) (enc m k) (ek (l))
                (eoo-star (hash k) (enc m k) (ek (l)))))
     (send (enc "abrq" (l) (privk "sign" a)))
     (recv (eor-star (hash k) (enc m k) (ek (l))))))

  ;; Aborted initiator run, but with EOR received

  (defrole init4
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
     (send (cat (l) (enc m k) (ek (l))
                (eoo-star (hash k) (enc m k) (ek (l)))))
     (recv (eor-star (hash k) (enc m k) (ek (l))))
     (send (enc "abrq" (l) (privk "sign" a)))
     (recv (enc "abcf"
                (enc "abrq" (l) (privk "sign" a))
                (privk "sign" t)))))

  ;; Initiator run with abort requested after EOR received,
  ;; but recovery forced

  (defrole init5
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
     (send (cat (l) (enc m k) (ek (l))
                (eoo-star (hash k) (enc m k) (ek (l)))))
     (recv (eor-star (hash k) (enc m k) (ek (l))))
     (send (enc "abrq" (l) (privk "sign" a)))
     (recv (eor-star (hash k) (enc m k) (ek (l))))))

  ;; Successful responder run with no TTP involvement

  (defrole resp1
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
     (recv (cat (l) (enc m k) (ek (l))
                (eoo-star (hash k) (enc m k) (ek (l)))))
     (send (eor-star (hash k) (enc m k) (ek (l))))
     (recv (cat k r))))

  ;;Responder run with recovery via TTP

  (defrole resp2
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
     (recv (cat (l) (enc m k) (ek (l))
                (eoo-star (hash k) (enc m k) (ek (l)))))
     (send (cat (l) (ek (l)) (eoo-star (hash k) (enc m k) (ek (l)))
                (eor-star (hash k) (enc m k) (ek (l)))))
     (send (cat (l) (ek (l)) (eoo-star (hash k) (enc m k) (ek (l)))
                (eor-star (hash k) (enc m k) (ek (l)))
                (enc "rcrq" (l) (ek (l)) (privk "sign" b))))
     (recv (cat k r))))

  ;; Responder run with recovery request and forced abort

  (defrole resp3
    (vars (a b t name) (e1 e2 x mesg))
    (trace
     (recv (cat (l-star e1 x) e1 e2 (eoo-star x e1 e2)))
     (send (cat (l-star e1 x) e2 (eoo-star x e1 e2) (eor-star x e1 e2)))
     (send (cat (l-star e1 x) e2 (eoo-star x e1 e2) (eor-star x e1 e2)
                (enc "rcrq" (l-star e1 x) e2 (privk "sign" b))))
     (recv (enc "abcf"
                (enc "abrq" (l-star e1 x) (privk "sign" a))
                (privk "sign" t)))))

  ;; TTP handles an abort

  (defrole ttp-ab1
    (vars (a b t name) (y x mesg))
    (trace
     (recv (enc "abrq" (l-prime y x) (privk "sign" a)))
     (send (cat "sync-abrq"
                (enc "abrq" (l-prime y x) (privk "sign" a))))
     (send (enc "abcf"
                (enc "abrq" (l-prime y x) (privk "sign" a))
                (privk "sign" t)))))

  ;; TTP forces recovery in response to an abort request

  (defrole ttp-ab2
    (vars (a b t name) (y x e mesg))
    (trace
     (recv (enc "abrq" (l-prime y x) (privk "sign" a)))
     (recv (cat "sync-abrq"
                (enc "eortag" (hash (l-prime y x)) e
                     (privk "sign" b))))
     (send (enc "eortag" (hash (l-prime y x)) e
                (privk "sign" b)))))

  ;; TTP handles a recovery

  (defrole ttp-rc1
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
     (recv (rr-prime-prime y))
     (send (cat "sync-rc-req"
                (rr-prime-prime y)))
     (send (cat k r))))

  ;; TTP forces abort in response to a recovery request

  (defrole ttp-rc2
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
     (recv (rr-prime-prime y))
     (recv (cat "sync-rc-req"
		(enc "abrq" (l-prime-prime y) (privk "sign" a))))
     (send (enc "abcf"
                (enc "abrq" (l-prime-prime y) (privk "sign" a))
                (privk "sign" t)))))

  ;; TTP handles a confirm request

  (defrole ttp-cf1
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
     (recv (cf-prime-prime y))
     (send (cat "sync-cf-req"
                (cf-prime-prime y)))
     (send (enc (cf-prime-prime y) (privk "sign" t)))))

  ;; TTP replies with abort token given confirm request

  (defrole ttp-cf2
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
     (recv (cf-prime-prime y))
     (recv (cat "sync-cf-req"
                (enc "abrq"(l-prime-prime y) (privk "sign" a))))
     (send (enc "abcf"
                (enc "abrq"(l-prime-prime y) (privk "sign" a))
                (privk "sign" t))))))

;; End of Wang's protocol defn.

;; Two experiments to prove Lemma 4.1

(defskeleton wang
  (vars (a b t name) (m data) (r text) (k skey))
  (defstrand init1 1 (b b) (t t) (m m) (r r) (k k))
  (deflistener m)
  (non-orig
   (privk "encr" t))
  (uniq-orig m k)
  (comment "Experiment 1 to prove Lemma 4.1."))

(defskeleton wang
  (vars (a b t name) (m data) (r text) (k skey))
  (defstrand init1 1 (b b) (t t) (m m) (r r) (k k))
  (deflistener k)
  (non-orig
   (privk "encr" t))
  (uniq-orig m k)
  (comment "Experiment 2 to prove Lemma 4.1."))

;; We now have three experiments
;; to prove Lemma 4.2, clause 1.

(defskeleton wang
  (vars (a b t name) (m data) (r text) (k skey))
  (defstrand init1 3 (b b) (t t) (m m) (r r) (k k))
  (non-orig
   (privk "sign" b))
  (comment "First of three experiments to prove Lemma 4.2, clause 1."))

(defskeleton wang
  (vars (a b t name) (m data) (r text) (k skey))
  (defstrand init3 3 (b b) (t t) (m m) (r r) (k k))
  (non-orig
   (privk "sign" b))
  (comment "Second of three experiments to prove Lemma 4.2, clause 1."))

(defskeleton wang
  (vars (a b t name) (m data) (r text) (k skey))
  (defstrand init5 4 (b b) (t t) (m m) (r r) (k k))
  (non-orig
   (privk "sign" b))
  (comment "Third of three experiments to prove Lemma 4.2, clause 1."))

;; Two experiments to prove 4.2, Clause 2.

(defskeleton wang
  (vars (a b t name) (m data) (r text) (k skey))
  (defstrand resp1 3 (a a) (t t) (m m) (r r) (k k))
  (non-orig
   (privk "sign" a))
  (comment
   "First of two experiments to prove Lemma 4.2, clause 2."))

(defskeleton wang
  (vars (a b t name) (m data) (r text) (k skey))
  (defstrand resp2 4 (a a) (t t) (m m) (r r) (k k))
  (non-orig
   (privk "sign" a))
  (comment
   "Second of two experiments to prove Lemma 4.2, clause 2."))

;; Experiment to prove Lemma 4.2, clause 3.

(defskeleton wang
  (vars (a b t name) (e1 e2 x mesg))
  (defstrand resp3 4 (a a) (t t))
  (non-orig
   (privk "sign" a))
  (comment
   "Experiments to prove Lemma 4.2, clause 3."))

;; One experiment to prove Lemma 4.3, Clause 1.

(defskeleton wang
  (vars (a b t name) (y x mesg))
  (deflistener
    (enc "abcf" (enc "abrq" (l-prime y x) (privk "sign" a))
	 (privk "sign" t)))
  (non-orig
   (privk "sign" t))
  (comment
   "Experiments to prove Lemma 4.3, clause 1."))

;; Three experiments to prove Lemma 4.3, Clause 2.

(defskeleton wang
  (vars (y x mesg) (a b t name))
  (defstrand ttp-ab1 3 (y y) (x x) (a a) (b b) (t t))
  (non-orig (privk "sign" a)  (privk "encr" t))
  (comment
   "Experiment 1 to prove Lemma 4.3, clause 2."))

(defskeleton wang
  (vars (a b t name) (r text) (k skey) (y mesg))
  (defstrand ttp-rc2 3 (a a) (b b) (t t))
  (non-orig (privk "sign" a)  (privk "encr" t))
  (comment
   "Experiment 2 to prove Lemma 4.3, clause 2."))

(defskeleton wang
  (vars (a b t name) (r text) (k skey) (y mesg))
  (defstrand ttp-cf2 3 (a a) (b b) (t t))
  (non-orig (privk "sign" a)  (privk "encr" t))
  (comment
   "Experiment 3 to prove Lemma 4.3, clause 2."))
