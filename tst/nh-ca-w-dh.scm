; CA
; This is an authenticated form of a new-hope key exchange.

; The file contains one model of the CA protocol with the hint included in the
; encryption and one model fo the CA protocol with the hint not included in the
; encryption.

; Note: This version of nh-ca does use diffie hellman for the defined
; macros.

(herald nhca (algebra diffie-hellman) (bound 20))

(defmacro (nh_public x1)
  (exp (gen) x1))
(defmacro (nh_public2 x1 x2)
  (hash "share" (exp (gen) (mul x1 x2)) x1))
(defmacro (nh_hint x1 x2)
  (hash "hint" (nh_public2 x1 x2)))
(defmacro (nh_key_hint x1 x2 h)
  (hash "key" (nh_public2 x1 x2) h))
(defmacro (nh_key x1 x2)
  (nh_key_hint x1 x2 (nh_hint x1 x2)))

(defprotocol nhca diffie-hellman
  (defrole init
    (vars (xi rndx) (xr expt) (a b ca name) (n text) (hint mesg))
    (trace
     (send (enc "reg" (nh_public xi) a (privk a)))
     (recv (enc (nh_public xi) a (privk ca)))
     (send (cat (nh_public xi) (enc (nh_public xi) a (privk ca))))
     (recv (cat (nh_public xr) hint (enc (nh_public xr) b (privk ca))
        (enc n hint (nh_key_hint xi xr hint))))
     (send (cat (enc "check" n (nh_key_hint xi xr hint))
                (enc "check" n (nh_key_hint xr xi hint)))))
    (uniq-gen xi)
    (non-orig (privk a))
    (non-orig (privk ca)))
  (defrole resp
    (vars (xr rndx) (xi expt) (a b ca name) (n text))
    (trace
     (send (enc "reg" (nh_public xr) b (privk b)))
     (recv (enc (nh_public xr) b (privk ca)))
     (recv (cat (nh_public xi) (enc (nh_public xi) a (privk ca))))
     (send (cat (nh_public xr) (nh_hint xr xi) (enc (nh_public xr) b (privk ca))
		(enc n (nh_hint xr xi) (nh_key xr xi))
        (enc n (nh_hint xr xi) (nh_key_hint xi xr (nh_hint xr xi)))))
     (recv (enc "check" n (nh_key xr xi))))
    (uniq-gen xr)
    (non-orig (privk b))
    (non-orig (privk ca)))
  (defrole ca
    (vars (subject ca name) (x expt))
    (trace
     (recv (enc "reg" (nh_public x) subject (privk subject)))
     (send (enc (nh_public x) subject (privk ca))))
    (non-orig (privk subject))
    )
  (comment A diffie-hellman exchange which uses a certificate
    authority to certify lonh-term NH values)
)

(defskeleton nhca
  (vars )
  (defstrand init 5 )
(comment Full initiator POV No need to make extra assumptions))

(defskeleton nhca
  (vars (n text))
  (defstrand resp 5 (n n))
  (uniq-orig n)
  (comment Full responder point of view with freshly chosen n)
)

(defskeleton nhca
  (vars (a b ca name) (xi xr rndx) (n text))
  (defstrand init 5 (xi xi) (ca ca) (a a) (b b) (n n))
  (defstrand resp 5 (xr xr) (ca ca) (a a) (b b) (n n))
(uniq-orig n)
(comment point of view in which init and resp each complete and
    they agree on the relevant parameters)
)

(defprotocol nhca-nohint diffie-hellman
  (defrole init
    (vars (xi rndx) (xr expt) (a b ca name) (n text) (hint mesg))
    (trace
     (send (enc "reg" (nh_public xi) a (privk a)))
     (recv (enc (nh_public xi) a (privk ca)))
     (send (cat (nh_public xi) (enc (nh_public xi) a (privk ca))))
     (recv (cat (nh_public xr) hint (enc (nh_public xr) b (privk ca))
        (enc n (nh_key_hint xi xr hint))))
     (send (cat (enc "check" n (nh_key_hint xi xr hint))
                (enc "check" n (nh_key_hint xr xi hint)))))
    (uniq-gen xi)
    (non-orig (privk a))
    (non-orig (privk ca)))
  (defrole resp
    (vars (xr rndx) (xi expt) (a b ca name) (n text))
    (trace
     (send (enc "reg" (nh_public xr) b (privk b)))
     (recv (enc (nh_public xr) b (privk ca)))
     (recv (cat (nh_public xi) (enc (nh_public xi) a (privk ca))))
     (send (cat (nh_public xr) (nh_hint xr xi) (enc (nh_public xr) b (privk ca))
		(enc n (nh_key xr xi))
        (enc n (nh_key_hint xi xr (nh_hint xr xi)))))
     (recv (enc "check" n (nh_key xr xi))))
    (uniq-gen xr)
    (non-orig (privk b))
    (non-orig (privk ca)))
  (defrole ca
    (vars (subject ca name) (x expt))
    (trace
     (recv (enc "reg" (nh_public x) subject (privk subject)))
     (send (enc (nh_public x) subject (privk ca))))
    (non-orig (privk subject))
    )
  (comment A diffie-hellman exchange which uses a certificate
    authority to certify lonh-term NH values)
)

(defskeleton nhca-nohint
  (vars )
  (defstrand init 5 )
(comment Full initiator POV No need to make extra assumptions))

(defskeleton nhca-nohint
  (vars (n text))
  (defstrand resp 5 (n n))
  (uniq-orig n)
  (comment Full responder point of view with freshly chosen n)
)

(defskeleton nhca-nohint
  (vars (a b ca name) (xi xr rndx) (n text))
  (defstrand init 5 (xi xi) (ca ca) (a a) (b b) (n n))
  (defstrand resp 5 (xr xr) (ca ca) (a a) (b b) (n n))
(uniq-orig n)
(comment point of view in which init and resp each complete and
    they agree on the relevant parameters)
)
