(herald "Protocol Transformations With Rules")

;; Basic Needham-Schroeder using asymmetric keys

(defprotocol ns basic
  (defrole init
    (vars (a b akey) (na nb text))
    (trace
     (send (enc na a b))
     (recv (enc na nb a))
     (send (enc nb b))))
  (defrole resp
    (vars (b a akey) (nb na text))
    (trace
     (recv (enc na a b))
     (send (enc na nb a))
     (recv (enc nb b))))
  (comment "Needham-Schroeder without rules"))

;;; The initiator point-of-view
(defskeleton ns
  (vars (a b akey) (na text))
  (defstrand init 3 (a a) (b b) (na na))
  (non-orig (invk a) (invk b))
  (uniq-orig na)
  (comment "Initiator point-of-view"))

;;; The responder point-of-view
(defskeleton ns
  (vars (a akey) (nb text))
  (defstrand resp 3 (a a) (nb nb))
  (non-orig (invk a))
  (uniq-orig nb)
  (comment "Responder point-of-view"))

;;; Unilateral protocol
(defprotocol unilateral basic
  (defrole init
     (vars (n text) (k akey))
     (trace
      (send (enc n k))
      (recv n)))
  (defrole resp
     (vars (n text) (k akey))
     (trace
      (recv (enc n k))
      (send n))))

(defskeleton unilateral
  (vars (k akey) (n text))
  (defstrand init 2 (n n) (k k))
  (non-orig (invk k))
  (uniq-orig n)
  (comment "The shape analysis sentence for this problem:"
    (defgoal unilateral
      (forall ((n text) (k akey) (z strd))
        (implies
          (and (p "init" z 2) (p "init" "n" z n) (p "init" "k" z k)
            (non (invk k)) (uniq-at n z 0))
          (exists ((z-0 strd))
            (and (p "resp" z-0 2) (p "resp" "n" z-0 n)
              (p "resp" "k" z-0 k) (prec z 0 z-0 0)
              (prec z-0 1 z 1))))))))

(defprotocol ns-with-rule basic
  (defrole init
    (vars (a b akey) (na nb text))
    (trace
     (send (enc na a b))
     (recv (enc na nb a))
     (send (enc nb b))))
  (defrole resp
    (vars (b a akey) (nb na text))
    (trace
     (recv (enc na a b))
     (send (enc na nb a))
     (recv (enc nb b))))
  (defrule unilateral
    (forall ((r strd) (a akey) (nb text))
	    (implies
	     (and (p "resp" r 3)
		  (p "resp" "a" r a)
		  (p "resp" "nb" r nb)
		  (non (invk a))
		  (uniq nb))
	     (exists ((i strd))
		     (and (p "init" i 3)
			  (p "init" "a" i a)
			  (p "init" "nb" i nb)
			  (prec r 1 i 1)
			  (prec i 2 r 2)))))
    (comment "Unilateral sas under the predicate mapping:")
    (comment (p "init" 2) "->" (p "resp" 3) "and")
    (comment (p "init" "n") "->" (p "resp" "nb") "and")
    (comment (p "init" "k") "->" (p "resp" "a") "and")
    (comment (p "resp" 2) "->" (p "init" 3) "and")
    (comment (p "resp" "n") "->" (p "init" "nb"))
    (comment (p "resp" "k") "->" (p "init" "a")))
  (comment "Needham-Schroeder with rule"))

;;; The responder point-of-view
(defskeleton ns-with-rule
  (vars (a akey) (nb text))
  (defstrand resp 3 (a a) (nb nb))
  (non-orig (invk a))
  (uniq-orig nb)
  (comment "Responder point-of-view"))
