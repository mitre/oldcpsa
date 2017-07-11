(herald "Needham-Schroeder Public-Key Protocol"
	(algebra diffie-hellman)
	(comment "This protocol contains a man-in-the-middle"
		 "attack discovered by Galvin Lowe."))

;;; Used to generate output for inclusion in the primer.
;;; Use margin = 60 (-m 60) to generate the output.

(defprotocol ns diffie-hellman
  (defrole init
    (vars (a b c d name) (n1 n2 text))
    (trace
     (send (enc n1 a c d (pubk b)))
     (recv (enc (enc n1 n2 (pubk a)) (pubk c)))
     (send (enc n2 (pubk b)))))
  (defrole resp
    (vars (b a c d name) (n2 n1 text))
    (trace
     (recv (enc n1 a c d (pubk b)))
     (send (enc (enc n1 n2 (pubk a)) (pubk c)))
     (recv (enc n2 (pubk b)))))
  (comment "Needham-Schroeder variant 1"))


;;; The responder point-of-view
(defskeleton ns
  (vars (a name) (n2 text))
  (defstrand resp 3 (a a) (n2 n2))
  (non-orig (privk a))
  (uniq-orig n2)
  (comment "Responder point-of-view"))


(defprotocol ns-inj diffie-hellman
  (defrole init
    (vars (a b c d name) (n1 n2 text))
    (trace
     (send (enc n1 a c d (pubk b)))
     (recv (enc (enc (enc n1 n2 (pubk a)) (pubk c)) (pubk d)))
     (send (enc n2 (pubk b)))))
  (defrole resp
    (vars (b a c d name) (n2 n1 text))
    (trace
     (recv (enc n1 a c d (pubk b)))
     (send (enc (enc (enc n1 n2 (pubk a)) (pubk c)) (pubk d)))
     (recv (enc n2 (pubk b)))))
  (comment "Needham-Schroeder variant 2"))

;;; The responder point-of-view
(defskeleton ns-inj
  (vars (a name) (n2 text))
  (defstrand resp 3 (a a) (n2 n2))
  (non-orig (privk a))
  (uniq-orig n2)
  (comment "Responder point-of-view"))
