(herald "The ffgg Protocol"
	(comment "From A Necessarily Parallel Attack by Jon K. Millen"))

(defprotocol ffgg basic
  (defrole init (vars (a b name) (n1 n2 m x y text))
    (trace
     (send a)
     (recv (cat b n1 n2))
     (send (cat a (enc n1 n2 m (pubk b))))
     (recv (cat n1 x (enc x y n1 (pubk b))))))
  (defrole resp (vars (b a name) (n1 n2 x y text))
    (trace
     (recv a)
     (send (cat b n1 n2))
     (recv (cat a (enc n1 x y (pubk b))))
     (send (cat n1 x (enc x y n1 (pubk b)))))
    (uniq-orig n1 n2)))

(defskeleton ffgg
  (vars (b name) (n1 n2 m text))
  (defstrand init 4 (b b) (m m))
  (deflistener m)
  (uniq-orig m)
  (non-orig (privk b)))
