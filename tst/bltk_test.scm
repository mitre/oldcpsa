(herald "bltk Test File"
	(algebra diffie-hellman)
   (bound 12))

(defprotocol test diffie-hellman
  (defrole r
    (vars (a b name) (n text))
    (trace 
     (send (enc n (bltk a b)))
     (recv (enc n (bltk b a))))
    (uniq-orig n)))

;; This POV is realized. That demonstrates that (bltk a b) = (bltk b a)
(defskeleton test
   (vars (a b name))
   (defstrand r 2 (a a) (b b))
   (non-orig (bltk a b) (bltk b a)))

;; These next two skeletons show that it is sufficient to declare
;; either (bltk a b) or (bltk b a) to be non.
(defskeleton test
   (vars (a b name) (n text))
   (defstrand r 2 (a a) (b b) (n n))
   (deflistener n)
   (non-orig (bltk a b)))

(defskeleton test
   (vars (a b name) (n text))
   (defstrand r 2 (a a) (b b) (n n))
   (deflistener n)
   (non-orig (bltk b a)))

;; This is the same protocol as above with bltk replaced with ltk
(defprotocol test2 diffie-hellman
  (defrole r
    (vars (a b name) (n text))
    (trace 
     (send (enc n (ltk a b)))
     (recv (enc n (ltk b a))))
    (uniq-orig n)))

;; The only way to realize this skeleton is to learn that a = b. This
;; is in contrast to the first skeleton above. 
(defskeleton test2
   (vars (a b name))
   (defstrand r 2 (a a) (b b))
   (non-orig (ltk a b)))


;; This protocol demonstrates that the position of the names a and b
;; in (bltk a b) are not enough to determine which principal is
;; inhabiting the role. The first send of each role commits to the
;; identity of itself and its peer. 
(defprotocol test3 diffie-hellman
  (defrole recvr
    (vars (a b name) (n text))
    (trace
     (send (cat "i am" a "you are" b))
     (recv (enc n (bltk a b)))))
  (defrole sender
    (vars (a b name) (n text))
    (trace
     (send (cat "i am" b "you are" a))
     (send (enc n (bltk a b))))
    (uniq-orig n)))

;; Since (bltk a b) = (bltk b a) the recvr role may receive the
;; encryption from its inteded peer or from itself. That is, both a
;; and b use the same key when inhabiting the sender role.
(defskeleton test3
  (vars (a b name))
  (defstrand recvr 2 (a a) (b b))
  (non-orig (bltk a b)))

