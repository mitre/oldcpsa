(herald "Signed group DH exchange" (algebra diffie-hellman)
	(limit 100)
	;(limit 20000) (bound 25)
)

(defprotocol dh_sig diffie-hellman
  (defrole group-init
    (vars (alpha rndx) (group text))
    (trace
     (init (cat "Group id" group (exp (gen) alpha))))
    (uniq-gen alpha))
  (defrole init
    (vars (x rndx) (y expt) (g base) (group text) (a b name))
    (trace
     (obsv (cat "Group id" group g))
     (send (enc (exp g x) (privk a)))
     (recv (enc (exp g y) (exp g x) (privk b)))
     (send (enc "final" (exp g y) (exp g x) (privk a))))
    (uniq-gen x))
  (defrole resp
    (vars (y rndx) (x expt) (g base) (group text) (a b name))
    (trace
     (obsv (cat "Group id" group g))
     (recv (enc (exp g x) (privk a)))
     (send (enc (exp g y) (exp g x) (privk b)))
     (recv (enc "final" (exp g y) (exp g x) (privk a))))
    (uniq-gen y))
)

(defskeleton dh_sig
  (vars (a b name))
  (defstrand init 4 (a a) (b b))
  (non-orig (privk b) (privk a))
)

(defskeleton dh_sig
  (vars (a b name))
  (defstrand resp 4 (a a) (b b))
  (non-orig (privk a) (privk b))
)

(defprotocol dh_sig2 diffie-hellman
  (defrole group-init
    (vars (alpha rndx) (group text))
    (trace
     (init (cat "Group id" group (exp (gen) alpha))))
    (uniq-gen alpha))
  (defrole init
    (vars (x rndx) (y expt) (g base) (n group text) (a b name))
    (trace
     (obsv (cat "Group id" group g))
     (send (enc (exp g x) (privk a)))
     (recv (enc (exp g y) (exp g x) (privk b)))
     (send (enc n (exp g (mul x y))))
     (recv n))
    (uniq-gen x n))
  (defrole resp
    (vars (y rndx) (x expt) (g base) (n group text) (a b name))
    (trace
     (obsv (cat "Group id" group g))
     (recv (enc (exp g x) (privk a)))
     (send (enc (exp g y) (exp g x) (privk b)))
     (recv (enc n (exp g (mul x y))))
     (send n))
    (uniq-gen y))
)

(defskeleton dh_sig2
  (vars (a b name))
  (defstrand init 5 (a a) (b b))
  (non-orig (privk b) (privk a))
)

(defskeleton dh_sig2
  (vars (a b name))
  (defstrand resp 5 (a a) (b b))
  (non-orig (privk a) (privk b))
)

