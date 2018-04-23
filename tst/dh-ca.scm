(herald dhca (algebra diffie-hellman))

(defprotocol dhca diffie-hellman
  (defrole init
    (vars (x rndx) (a b ca name) (y expt) (n text))
    (trace
     (send (enc "reg" (exp (gen) x) a (privk a)))
     (recv (enc (exp (gen) x) a (privk ca)))
     (send (cat (exp (gen) x) (enc (exp (gen) x) a (privk ca))))
     (recv (cat (exp (gen) y) (enc (exp (gen) y) b (privk ca)) (enc n (exp (gen) (mul y x)))))
     (send (enc "check" n (exp (gen) (mul y x)))))
    (uniq-gen x)
    (non-orig (privk ca)))
  (defrole resp
    (vars (y rndx) (a b ca name) (x expt) (n text))
    (trace
     (send (enc "reg" (exp (gen) y) b (privk b)))
     (recv (enc (exp (gen) y) b (privk ca)))
     (recv (cat (exp (gen) x) (enc (exp (gen) x) a (privk ca))))
     (send (cat (exp (gen) y) (enc (exp (gen) y) b (privk ca))
		(enc n (exp (gen) (mul x y)))))
     (recv (enc "check" n (exp (gen) (mul x y)))))
    (uniq-gen y)
    (non-orig (privk ca)))
  (defrole ca (vars (subject ca name) (x expt))
    (trace
     (recv (enc "reg" (exp (gen) x) subject (privk subject)))
     (send (enc (exp (gen) x) subject (privk ca))))
    (non-orig (privk subject))
    )
  (comment A diffie-hellman exchange which uses a certificate
    authority to certify long-term DH values)
)

(defskeleton dhca
  (vars )
  (defstrand init 5 )
(comment Full initiator POV No need to make extra assumptions))

(defskeleton dhca
  (vars (n text))
  (defstrand resp 5 (n n))
  (uniq-orig n)
  (comment Full responder point of view with freshly chosen n)
)

(defskeleton dhca
  (vars (a b ca name) (x y rndx) (n text))
  (defstrand init 5 (x x) (y y) (ca ca) (a a) (b b) (n n))
  (defstrand resp 5 (y y) (x x) (ca ca) (a a) (b b) (n n))
(uniq-orig n)
(comment point of view in which init and resp each complete and
    they agree on the relevant parameters)
)
