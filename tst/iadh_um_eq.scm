; Unified Model

; This file models the "Unified Model" method of determining a fresh
; key from long-term and ephemeral Diffie-Hellman exponents.  Since
; CPSA does not provide an intrinsic association between names and
; Diffie-Hellman values, we use digital signatures to link
; authentication of a Diffie-Hellman long-term public value to a
; public key.

; A role is provided in which a party signs a fresh long-term
; Diffie-Hellman value, and then leaks the exponent.  The latter step
; is used to test the notion of forward security.

; Two inputs are analyzed.  In the first, we model that two
; participants exist that agree on the UM key.  The notion of
; "implicit authentication" suggests that if one participant exists
; and another party knows the same key, that party must be either the
; participant or the participant's intended partner.

; In this version, we assume that the long-term keys of the
; participants do not leak.

; The second input checks whether the key can be learned when the key
; is generated honestly.  In this version, we do NOT assume that the
; long-term keys of the participants do not leak.

(herald "IADH: unified model (UM)" (bound 20) (limit 8000) (algebra diffie-hellman))

(defprotocol iadh-um diffie-hellman
  (defrole participant
    (vars (l lp e rndx) (ep expt) (self peer name) (n data))
    (trace
     (send (cat self peer (exp (gen) l) (exp (gen) lp)))
     (send (cat (exp (gen) l) (exp (gen) e)))
     (recv (exp (gen) ep))
     )
    (fn-of ("principal-of" (l self) (lp peer))
           ("ltx-of" (self l) (peer lp)))
    (uniq-gen e)
    (neq (ep (one))))
  (defrole ltx-gen
    (vars (self name) (l rndx))
    (trace
     (send (cat self l)))
    (uniq-orig l)
    (fn-of ("principal-of" (l self))
	   ("ltx-of" (self l)))

  )
)

;; Implicit authentication: participants will agree on the resulting key
;; foo(key).  Therefore they should agree on the names A and B.
 (defskeleton iadh-um
  (vars (eA lA lAp eB lB lBp rndx) (ep ep-0 expt))
  (defstrand participant 3 (lp lAp) (ep ep) (e eA) (l lA))
  (defstrand participant 3 (lp lBp) (ep ep-0) (e eB) (l lB))
  (eq ((hash (exp (gen) (mul lBp lB)) (exp (gen) (mul ep-0 eB)))
       (hash (exp (gen) (mul lAp lA)) (exp (gen) (mul ep eA)))))
  (non-orig lA lB)
  (neq (eA eB))
  (comment "Implicit authentication")
)
