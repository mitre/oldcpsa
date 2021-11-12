; Unified Model  This version used in the paper, tooldev/dh/conf_dh/dh_ebn.tex

; This file models the "Unified Model" method of determining a fresh
; key from long-term and ephemeral values.  We use
; function relation declarations to link names to long-term public
; values.

;;; This file contains the standard version, in which the two
;;; ephemeral are mixed, as are the two static long term values.

; A role is provided in which a party signs a fresh long-term
; value, and then leaks the exponent.  The latter step
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

(herald "NHCR: unified model (UM3) original" (bound 20) (limit 8000) (algebra diffie-hellman))

(defmacro (nh_public k1)
  ;(exp (gen) x1))
  (hash "public" k1))

(defmacro (nh_share k1 k2)
  ;(hash "share" (nh_public (mul x1 x2))))
  (hash "share" (nh_public k1) k2))

; Use this macro to generate the hint
(defmacro (nh_hint k1 k2)
  (hash "hint" (nh_share k1 k2)))

(defmacro (nh_key1 k1 k2 h)
  (hash "nh_key" (nh_share k1 k2) h))

(defmacro (nh_key2 k1 k2 h)
  (hash "nh_key" (nh_share k2 k1) h))

(defmacro (key_hint111 ltk1 ltk2 ek1 ek2 h1 h2 h3)
  (hash "key" (nh_key1 ltk1 ek2 h1) (nh_key1 ek1 ltk2 h2) (nh_key1 ek1 ek2 h3)))
(defmacro (key_hint112 ltk1 ltk2 ek1 ek2 h1 h2 h3)
  (hash "key" (nh_key1 ltk1 ek2 h1) (nh_key1 ek1 ltk2 h2) (nh_key2 ek1 ek2 h3)))
(defmacro (key_hint121 ltk1 ltk2 ek1 ek2 h1 h2 h3)
  (hash "key" (nh_key1 ltk1 ek2 h1) (nh_key2 ek1 ltk2 h2) (nh_key1 ek1 ek2 h3)))
(defmacro (key_hint122 ltk1 ltk2 ek1 ek2 h1 h2 h3)
  (hash "key" (nh_key1 ltk1 ek2 h1) (nh_key2 ek1 ltk2 h2) (nh_key2 ek1 ek2 h3)))
(defmacro (key_hint211 ltk1 ltk2 ek1 ek2 h1 h2 h3)
  (hash "key" (nh_key2 ltk1 ek2 h1) (nh_key1 ek1 ltk2 h2) (nh_key1 ek1 ek2 h3)))
(defmacro (key_hint212 ltk1 ltk2 ek1 ek2 h1 h2 h3)
  (hash "key" (nh_key2 ltk1 ek2 h1) (nh_key1 ek1 ltk2 h2) (nh_key2 ek1 ek2 h3)))
(defmacro (key_hint221 ltk1 ltk2 ek1 ek2 h1 h2 h3)
  (hash "key" (nh_key2 ltk1 ek2 h1) (nh_key2 ek1 ltk2 h2) (nh_key1 ek1 ek2 h3)))
(defmacro (key_hint222 ltk1 ltk2 ek1 ek2 h1 h2 h3)
  (hash "key" (nh_key2 ltk1 ek2 h1) (nh_key2 ek1 ltk2 h2) (nh_key2 ek1 ek2 h3)))

(defprotocol nhcr-um3 diffie-hellman
  (defrole init
    (vars (ltxa ltxb x rndx) (y expt) (a b name) (na nb data) (hint1 hint2 hint3 mesg))
    (trace
     (recv (cat (nh_public ltxa) (nh_public ltxb)))
     (send (cat na a b (nh_public x)))
     (recv (cat (nh_public y) hint1 hint2 hint3
                (enc na nb a b hint1 hint2 hint3
                     (key_hint222 ltxa ltxb x y hint1 hint2 hint3))))
     (send nb)
     )
    (fn-of ("principal-of" (ltxa a) (ltxb b))
           ("ltx-of" (a ltxa) (b ltxb)))
    (uniq-gen x)
    (uniq-orig na)
    (neq ((exp (gen) y) (gen))))

  (defrole resp
    (vars (ltxa ltxb y rndx) (x expt) (a b name) (na nb data))
    (trace
     (recv (cat (nh_public ltxa) (nh_public ltxb)))
     (recv (cat na a b (nh_public x)))
     (send (cat (nh_public y) (nh_hint ltxa y) (nh_hint x ltxb) (nh_hint x y)
                (enc na nb a b (nh_hint ltxa y) (nh_hint x ltxb) (nh_hint x y)
           (key_hint111 ltxa ltxb x y (nh_hint ltxa y) (nh_hint x ltxb) (nh_hint x y)))
                (enc na nb a b (nh_hint ltxa y) (nh_hint x ltxb) (nh_hint x y)
           (key_hint112 ltxa ltxb x y (nh_hint ltxa y) (nh_hint x ltxb) (nh_hint x y)))
                (enc na nb a b (nh_hint ltxa y) (nh_hint x ltxb) (nh_hint x y)
           (key_hint121 ltxa ltxb x y (nh_hint ltxa y) (nh_hint x ltxb) (nh_hint x y)))
                (enc na nb a b (nh_hint ltxa y) (nh_hint x ltxb) (nh_hint x y)
           (key_hint122 ltxa ltxb x y (nh_hint ltxa y) (nh_hint x ltxb) (nh_hint x y)))
                (enc na nb a b (nh_hint ltxa y) (nh_hint x ltxb) (nh_hint x y)
           (key_hint211 ltxa ltxb x y (nh_hint ltxa y) (nh_hint x ltxb) (nh_hint x y)))
                (enc na nb a b (nh_hint ltxa y) (nh_hint x ltxb) (nh_hint x y)
           (key_hint212 ltxa ltxb x y (nh_hint ltxa y) (nh_hint x ltxb) (nh_hint x y)))
                (enc na nb a b (nh_hint ltxa y) (nh_hint x ltxb) (nh_hint x y)
           (key_hint221 ltxa ltxb x y (nh_hint ltxa y) (nh_hint x ltxb) (nh_hint x y)))
                (enc na nb a b (nh_hint ltxa y) (nh_hint x ltxb) (nh_hint x y)
           (key_hint222 ltxa ltxb x y (nh_hint ltxa y) (nh_hint x ltxb) (nh_hint x y)))))
     (recv nb)
     )
    (fn-of ("principal-of" (ltxa a) (ltxb b))
	   ("ltx-of" (a ltxa) (b ltxb)))
    (uniq-gen y)
    (uniq-orig nb)
    (neq ((exp (gen) x) (gen))))

  (defrole ltx-gen
    (vars (self name) (l rndx))
    (trace
     (send (cat self (nh_public l)))
     (recv "end-of-protocol")
     (send l))
    (uniq-gen l)
    (fn-of ("principal-of" (l self))
	   ("ltx-of" (self l)))))

; Initiator point of view: both LTX exponents secret
(defskeleton nhcr-um3
  (vars (ltxa ltxb rndx) (a b name))
  (defstrand init 4 (ltxa ltxa) (ltxb ltxb) (a a) (b b))
  (defstrand ltx-gen 1 (l ltxa))
  (defstrand ltx-gen 1 (l ltxb))
  (non-orig ltxa ltxb) (neq (a b)))

; Initiator point of view: partner's exponent secret
(defskeleton nhcr-um3
  (vars (ltxa ltxb rndx) (a b name))
  (defstrand init 4 (ltxa ltxa) (ltxb ltxb) (a a) (b b))
  (defstrand ltx-gen 1 (l ltxb))
  (non-orig ltxb)
  (neq (a b)))

; Responder point of view; both exponents secret
(defskeleton nhcr-um3
  (vars (ltxa ltxb rndx) (a b name))
  (defstrand resp 4 (ltxa ltxa) (ltxb ltxb) (a a) (b b))
  (defstrand ltx-gen 1 (l ltxa))
  (defstrand ltx-gen 1 (l ltxb))
  (non-orig ltxa ltxb)
  (neq (a b)))

; Responder point of view; partner's exponent secre
(defskeleton nhcr-um3
  (vars (ltxa ltxb rndx) (a b name))
  (defstrand resp 4 (ltxa ltxa) (ltxb ltxb) (a a) (b b))
  (defstrand ltx-gen 1 (l ltxa))
  (non-orig ltxa)
  (neq (a b)))

;;; Forward secrecy, neither long-term exponent secure
(defskeleton nhcr-um3
  (vars (ltxa ltxb x rndx) (y expt) (a b name))
  (defstrand init 4 (ltxa ltxa) (ltxb ltxb) (x x) (y y) (a a) (b b))
  (deflistener (key_hint111 ltxa ltxb x y (nh_hint ltxa y) (nh_hint x ltxb) (nh_hint x y)))
  (defstrand ltx-gen 3 (l ltxa))
  (defstrand ltx-gen 3 (l ltxb))
  (precedes ((0 3) (3 1)) ((0 3) (2 1)))
  (neq (a b)))

;;; Forward secrecy, neither long-term exponent secure
(defskeleton nhcr-um3
  (vars (ltxa ltxb x rndx) (y expt) (a b name))
  (defstrand init 4 (ltxa ltxa) (ltxb ltxb) (x x) (y y) (a a) (b b))
  (deflistener (key_hint112 ltxa ltxb x y (nh_hint ltxa y) (nh_hint x ltxb) (nh_hint x y)))
  (defstrand ltx-gen 3 (l ltxa))
  (defstrand ltx-gen 3 (l ltxb))
  (precedes ((0 3) (3 1)) ((0 3) (2 1)))
  (neq (a b)))

;;; Forward secrecy, neither long-term exponent secure
(defskeleton nhcr-um3
  (vars (ltxa ltxb x rndx) (y expt) (a b name))
  (defstrand init 4 (ltxa ltxa) (ltxb ltxb) (x x) (y y) (a a) (b b))
  (deflistener (key_hint121 ltxa ltxb x y (nh_hint ltxa y) (nh_hint x ltxb) (nh_hint x y)))
  (defstrand ltx-gen 3 (l ltxa))
  (defstrand ltx-gen 3 (l ltxb))
  (precedes ((0 3) (3 1)) ((0 3) (2 1)))
  (neq (a b)))

;;; Forward secrecy, neither long-term exponent secure
(defskeleton nhcr-um3
  (vars (ltxa ltxb x rndx) (y expt) (a b name))
  (defstrand init 4 (ltxa ltxa) (ltxb ltxb) (x x) (y y) (a a) (b b))
  (deflistener (key_hint122 ltxa ltxb x y (nh_hint ltxa y) (nh_hint x ltxb) (nh_hint x y)))
  (defstrand ltx-gen 3 (l ltxa))
  (defstrand ltx-gen 3 (l ltxb))
  (precedes ((0 3) (3 1)) ((0 3) (2 1)))
  (neq (a b)))

;;; Forward secrecy, neither long-term exponent secure
(defskeleton nhcr-um3
  (vars (ltxa ltxb x rndx) (y expt) (a b name))
  (defstrand init 4 (ltxa ltxa) (ltxb ltxb) (x x) (y y) (a a) (b b))
  (deflistener (key_hint211 ltxa ltxb x y (nh_hint ltxa y) (nh_hint x ltxb) (nh_hint x y)))
  (defstrand ltx-gen 3 (l ltxa))
  (defstrand ltx-gen 3 (l ltxb))
  (precedes ((0 3) (3 1)) ((0 3) (2 1)))
  (neq (a b)))

;;; Forward secrecy, neither long-term exponent secure
(defskeleton nhcr-um3
  (vars (ltxa ltxb x rndx) (y expt) (a b name))
  (defstrand init 4 (ltxa ltxa) (ltxb ltxb) (x x) (y y) (a a) (b b))
  (deflistener (key_hint212 ltxa ltxb x y (nh_hint ltxa y) (nh_hint x ltxb) (nh_hint x y)))
  (defstrand ltx-gen 3 (l ltxa))
  (defstrand ltx-gen 3 (l ltxb))
  (precedes ((0 3) (3 1)) ((0 3) (2 1)))
  (neq (a b)))

;;; Forward secrecy, neither long-term exponent secure
(defskeleton nhcr-um3
  (vars (ltxa ltxb x rndx) (y expt) (a b name))
  (defstrand init 4 (ltxa ltxa) (ltxb ltxb) (x x) (y y) (a a) (b b))
  (deflistener (key_hint221 ltxa ltxb x y (nh_hint ltxa y) (nh_hint x ltxb) (nh_hint x y)))
  (defstrand ltx-gen 3 (l ltxa))
  (defstrand ltx-gen 3 (l ltxb))
  (precedes ((0 3) (3 1)) ((0 3) (2 1)))
  (neq (a b)))

;;; Forward secrecy, neither long-term exponent secure
(defskeleton nhcr-um3
  (vars (ltxa ltxb x rndx) (y expt) (a b name))
  (defstrand init 4 (ltxa ltxa) (ltxb ltxb) (x x) (y y) (a a) (b b))
  (deflistener (key_hint222 ltxa ltxb x y (nh_hint ltxa y) (nh_hint x ltxb) (nh_hint x y)))
  (defstrand ltx-gen 3 (l ltxa))
  (defstrand ltx-gen 3 (l ltxb))
  (precedes ((0 3) (3 1)) ((0 3) (2 1)))
  (neq (a b)))
