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

(herald "IADH: unified model (UM)" (bound 20) (limit 2000) (algebra diffie-hellman))

(defprotocol iadh-um diffie-hellman
  (defrole participant
    (vars (l e rndx) (hl he base) (self peer name) (kh n data))
    (trace
     (recv (enc "cert" (exp (gen) l) self (privk self)))
     (recv (enc "cert" hl peer (privk peer)))
     (send (exp (gen) e))
     (recv he)
     (send kh))
    (fn-of (foo ((hash (exp hl l) (exp he e)) kh)))
;     (uniq-gen e)
    (neq (he (gen))))
  (defrole ltx-gen
    (vars (self name) (l rndx))
    (trace
     (send (enc "cert" (exp (gen) l) self (privk self)))
     (send l))
    (uniq-gen l)
    (fn-of ("principal-of" (l self))))
  )

;; Implicit authentication: participants will agree on the resulting key
;; foo(key).  Therefore they should agree on the names A and B.

(defskeleton iadh-um
  (vars (kh data) (eA lA eB lB rndx) (A B C D name))
  (defstrand participant 5 (kh kh) (e eA) (l lA) (self A) (peer C))
  (defstrand participant 5 (kh kh) (e eB) (l lB) (self B) (peer D))
  (non-orig lA lB)
  (uniq-gen eA eB)
  (neq (A B)(eA eB) (lA lB) (eA lA) (eB lB) (eA lB) (eB lA))
  (non-orig (privk C) (privk D) (privk A) (privk B))
  (comment "Implicit authentication")
  )

;; Security:

(defskeleton iadh-um
  (vars (kh data) (eA lA eB lB rndx) (A B C D name))
  (defstrand participant 5 (kh kh) (e eA) (l lA) (self A) (peer C))
  (defstrand participant 5 (kh kh) (e eB) (l lB) (self B) (peer D))
  (deflistener (hash (exp (gen) (mul lA lB)) (exp (gen) (mul eA eB))))
  (non-orig lA lB)
  (uniq-gen eA eB)
  (neq (A B)(eA eB) (lA lB) (eA lA) (eB lB) (eA lB) (eB lA))
  (non-orig (privk C) (privk D) (privk A) (privk B))
  (comment "Implicit authentication + disclosure")
  )

(defskeleton iadh-um
  (vars (kh data) (eA lA lB rndx) (hl he base) (A B C D name))
  (defstrand participant 5 (kh kh) (e eA) (l lA) (hl (exp (gen) lB)) (self A) (peer C))
  (deflistener (hash (exp (exp (gen) lB) lA) (exp he eA)))
  (non-orig lA lB)
  (neq (A C) (lA lB) (eA lA) (eA lB))
  (non-orig (privk C) (privk A))
  (comment "Implicit authentication + disclosure")
  )

(defskeleton iadh-um
  (vars (kh data) (eA lA lB rndx) (hl he base) (A B C D name))
  (defstrand participant 5 (kh kh) (e eA) (l lA) (hl (exp (gen) lB)) (self A) (peer C))
  (deflistener (hash (exp (exp (gen) lB) lA) (exp he eA)))
  (non-orig lB)
  (uniq-gen eA)
  (neq (A C) (lA lB) (eA lA) (eA lB))
  (non-orig (privk C) (privk A))
  (comment "Implicit authentication + disclosure")
  )
