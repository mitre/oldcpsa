; Station-to-station

; This is an authenticated form of a Diffie-Hellman key exchange.

; The file contains two different models of the station-to-station
; protocol.  In the first, an assumption is made that *any* initiator
; or responder properly picks a fresh random exponent and does not
; allow it to be obtained by the adversary.

; In the second, we do not include this assumption, and as a result,
; less can be guaranteed.

(herald "Station-to-station protocol" (algebra diffie-hellman))

(defprotocol station-to-station diffie-hellman
  (defrole init
    (vars (x expn) (h base) (a b name))
    (trace
     (send (exp (gen) x))
     (recv (cat h (enc (enc h (exp (gen) x) (privk b)) (exp h x))))
     (send (enc (enc (exp (gen) x) h (privk a)) (exp h x))))
    (uniq-gen x))
  (defrole resp
    (vars (y expn) (h base) (a b name))
    (trace
     (recv h)
     (send (cat (exp (gen) y) (enc (enc (exp (gen) y) h (privk b)) (exp h y))))
     (recv (enc (enc h (exp (gen) y) (privk a)) (exp h y))))
    (uniq-gen y))
)

(defskeleton station-to-station
  (vars (a b name))
  (defstrand init 3 (a a) (b b))
  (non-orig (privk b) (privk a))
)

(defskeleton station-to-station
  (vars (a b name))
  (defstrand resp 3 (a a) (b b))
  (non-orig (privk a) (privk b))
)

(defprotocol station-weak diffie-hellman
  (defrole weak-init
    (vars (x expn) (h base) (a b name))
    (trace
     (send (exp (gen) x))
     (recv (cat h (enc (enc h (exp (gen) x) (privk b)) (exp h x))))
     (send (enc (enc (exp (gen) x) h (privk a)) (exp h x))))
  )
  (defrole weak-resp
    (vars (y expn) (h base) (a b name))
    (trace
     (recv h)
     (send (cat (exp (gen) y) (enc (enc (exp (gen) y) h (privk b)) (exp h y))))
     (recv (enc (enc h (exp (gen) y) (privk a)) (exp h y))))
    (absent (y h))
    )
)

(defskeleton station-weak
  (vars (a b name) (x expn))
  (defstrand weak-init 3 (a a) (b b) (x x))
  (uniq-gen x)
  (non-orig (privk b) (privk a))
)

(defskeleton station-weak
  (vars (a b name) (y expn))
  (defstrand weak-resp 3 (a a) (b b) (y y))
  (uniq-gen y)
  (non-orig (privk a) (privk b))
)
