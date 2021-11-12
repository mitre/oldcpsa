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
    (vars (x rndx) (y expt) (a b name))
    (trace
     (send (exp (gen) x))
     (recv (cat (exp (gen) y)
		(enc (enc (exp (gen) y)
			  (exp (gen) x) (privk b))
		     (exp (gen) (mul y x)))))
     
     (send (enc (enc (exp (gen) x)
		     (exp (gen) y) (privk a))
		(exp (gen) (mul y x)))))
    (uniq-gen x))
  (defrole resp
    (vars (y rndx) (x expt) (a b name))
    (trace
     (recv (exp (gen) x))
     (send (cat (exp (gen) y)
		(enc (enc (exp (gen) y)
			  (exp (gen) x) (privk b))
		     (exp (gen) (mul x y)))))
     (recv (enc (enc (exp (gen) x)
		     (exp (gen) y) (privk a))
		(exp (gen) (mul x y)))))
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
    (vars (x rndx) (y expt) (a b name))
    (trace
     (send (exp (gen) x))
     (recv (cat (exp (gen) y)
		(enc (enc (exp (gen) y)
			  (exp (gen) x) (privk b)) (exp (gen) (mul y x)))))
     (send (enc (enc (exp (gen) x)
		     (exp (gen) y) (privk a)) (exp (gen) (mul y x)))))
					;(uniq-gen x)
    )
  (defrole weak-resp
    (vars (y rndx) (x expt) (a b name))
    (trace
     (recv (exp (gen) x))
     (send (cat (exp (gen) y)
		(enc (enc (exp (gen) y) (exp (gen) x) (privk b)) (exp (gen) (mul x y)))))
     (recv (enc (enc (exp (gen) x) (exp (gen) y) (privk a)) (exp (gen) (mul x y)))))
					;    (uniq-gen y)
     (absent (y (exp (gen) x)))
    )
)

(defskeleton station-weak
  (vars (a b name) (x rndx))
  (defstrand weak-init 3 (a a) (b b) (x x))
  (uniq-gen x)
  (non-orig (privk b) (privk a))
)

(defskeleton station-weak
  (vars (a b name) (y rndx))
  (defstrand weak-resp 3 (a a) (b b) (y y))
  (uniq-gen y)
  (non-orig (privk a) (privk b))
)
