; Station-to-station

; This is an authenticated form of a Diffie-Hellman key exchange.

; The file contains two different models of the station-to-station
; protocol.  In the first, an assumption is made that *any* initiator
; or responder properly picks a fresh random exponent and does not
; allow it to be obtained by the adversary.

; In the second, we do not include this assumption, and as a result,
; less can be guaranteed.

(herald "Station-to-station protocol" (algebra diffie-hellman))

(defmacro (nh_public x1)
  (exp (gen) x1))
(defmacro (nh_share x1 x2)
  (hash "share" (exp (gen) (mul x1 x2)) x1))
(defmacro (nh_hint x1 x2)
  (hash "hint" (nh_share x1 x2)))
(defmacro (nh_key_hint x1 x2 h)
  (hash "key" (nh_share x1 x2) h))
(defmacro (nh_key x1 x2)
  (nh_key_hint x1 x2 (nh_hint x1 x2)))

(defprotocol station-to-station diffie-hellman
  (defrole init
    (vars (xi rndx) (xr expt) (i r name) (hint mesg))
    (trace
     (send (nh_public xi))
     (recv (cat (nh_public xr) hint
		(enc (enc (nh_public xr) (nh_public xi) hint (privk r)) (nh_key_hint xi xr hint))))
     (send (cat (enc (enc (nh_public xi) (nh_public xr) hint (privk i)) (nh_key_hint xr xi hint))
		(enc (enc (nh_public xi) (nh_public xr) hint (privk i)) (nh_key_hint xi xr hint))))
     )
    )
  (defrole resp
    (vars (xr rndx) (xi expt) (i r name))
    (trace
     (recv (nh_public xi))
     (send (cat (nh_public xr) (nh_hint xr xi)
		(enc (enc (nh_public xr) (nh_public xi) (nh_hint xr xi) (privk r)) (nh_key xr xi))
		(enc (enc (nh_public xr) (nh_public xi) (nh_hint xr xi) (privk r)) (nh_key_hint xi xr (nh_hint xr xi)))))
     (recv (enc (enc (nh_public xi) (nh_public xr) (nh_hint xr xi) (privk i)) (nh_key xr xi))))
    )
  )

(defskeleton station-to-station
  (vars (i r name) (xi rndx))
  (defstrand init 3 (i i) (r r) (xi xi))
  (non-orig (privk i) (privk r))
  (uniq-gen xi)
)

(defskeleton station-to-station
  (vars (i r name) (xr rndx) (xi expt))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xi))
  (non-orig (privk i) (privk r))
  (absent (xr xi))
  (uniq-gen xr)
  )

(defskeleton station-to-station
  (vars (i r name) (xr rndx) (xi expt))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xi))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
)

(defprotocol station-nohint diffie-hellman
  (defrole init
    (vars (xi rndx) (xr expt) (i r name) (hint mesg))
    (trace
     (send (nh_public xi))
     (recv (cat (nh_public xr) hint
		(enc (enc "resp" (nh_public xr) (nh_public xi) (privk r)) (nh_key_hint xi xr hint))))
     (send (cat (enc (enc (nh_public xi) (nh_public xr) (privk i)) (nh_key_hint xr xi hint))
		(enc (enc (nh_public xi) (nh_public xr) (privk i)) (nh_key_hint xi xr hint))))
     )
    )
  (defrole resp
    (vars (xr rndx) (xi expt) (i r name))
    (trace
     (recv (nh_public xi))
     (send (cat (nh_public xr) (nh_hint xr xi)
		(enc (enc "resp" (nh_public xr) (nh_public xi) (privk r)) (nh_key xr xi))
		(enc (enc "resp" (nh_public xr) (nh_public xi) (privk r)) (nh_key_hint xi xr (nh_hint xr xi)))))
     (recv (enc (enc (nh_public xi) (nh_public xr) (privk i)) (nh_key xr xi))))
    )
  )

(defskeleton station-nohint
  (vars (i r name) (xi rndx))
  (defstrand init 3 (i i) (r r) (xi xi))
  (non-orig (privk i) (privk r))
  (uniq-gen xi)
)

(defskeleton station-nohint
  (vars (i r name) (xr rndx) (xi expt))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xi))
  (non-orig (privk i) (privk r))
  (absent (xr xi))
  (uniq-gen xr)
)
