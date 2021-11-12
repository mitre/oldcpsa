(herald precursor (algebra diffie-hellman) (bound 6))

(defprotocol precursor diffie-hellman
  (defrole init
    (vars (x y rndx))
    (trace (send (cat (exp (gen) x) (exp (gen) y)))
	   (recv (exp (gen) (mul x y))))
    (uniq-gen x y)))

(defskeleton precursor
  (vars)
  (defstrand init 2))

;; (defskeleton precurser
;;   (vars (x y rndx) (x-0 x-1 expt))
;;   (defstrand init 2 (x x) (y y))
;;   (deflistener (cat (exp (gen) (mul x y x-0)) x-0))
;;   (deflistener (cat (exp (gen) (mul x y x-1)) x-1))
;;   (precedes ((0 0) (2 0)) ((1 1) (0 1)) ((2 1) (1 0)))
;;   (uniq-gen x y)
;;   (pov (0) (defskeleton precurser
;; 	     (vars)
;; 	     (defstrand init 2))))
