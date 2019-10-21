(herald "Main Example")

(defprotocol main-ex-src basic
  (defrole qn
    (vars (a b c akey) (i ssn text) (k skey))
    (trace
     (send (cat i a (enc (enc k b c i (invk a)) b) (enc a b i ssn c)))
     (recv (enc "sorry" a b k)))
    (uniq-orig k))
  (defrole qy
    (vars (a b c akey) (i ssn d text) (k skey))
    (trace
     (send (cat i a (enc (enc k b c i (invk a)) b) (enc a b i ssn c)))
     (recv (enc "data" d k)))
    (uniq-orig k))
  (defrole an
    (vars (a b c akey) (i text) (k skey) (x mesg))
    (trace
     (recv (cat i a (enc (enc k b c i (invk a)) b) x))
     (send (enc "sorry" a b k))))
  (defrole ay
    (vars (a b c akey) (i d text) (k skey) (x mesg))
    (trace
     (recv (cat i a (enc (enc k b c i (invk a)) b) x))
     (send (enc "data" d k)))
    (uniq-orig d)))

(defprotocol main-ex-tgt basic
  (defrole qn
    (vars (a b c akey) (i ssn text) (k skey))
    (trace
     (send (cat i a (enc (enc k b c i (invk a)) b) (enc a b i ssn c)))
     (recv (enc "sorry" a b k)))
    (uniq-orig k))
  (defrole qy
    (vars (a b c akey) (i ssn d text) (k skey))
    (trace
     (send (cat i a (enc (enc k b c i (invk a)) b) (enc a b i ssn c)))
     (recv (enc "data" d k)))
    (uniq-orig k))
  (defrole an
    (vars (a b c akey) (i y n text) (k skey) (x mesg))
    (trace
     (recv (cat i a (enc (enc k b c i (invk a)) b) x))
     (send (enc y n i a x c))
     (recv n)
     (send (enc "sorry" a b k)))
    (uniq-orig y n))
  (defrole ay
    (vars (a b c akey) (i y n d text) (k skey) (x mesg))
    (trace
     (recv (cat i a (enc (enc k b c i (invk a)) b) x))
     (send (enc y n i a x c))
     (recv y)
     (send (enc "data" d k)))
    (uniq-orig d y n))
  (defrole sn
    (vars (a b c akey) (i ssn y n text))
    (trace
     (recv (enc y n i a (enc a b i ssn c) c))
     (send n)))
  (defrole sy
    (vars (a b c akey) (i ssn y n text))
    (trace
     (recv (enc y n i a (enc a b i ssn c) c))
     (send y))))

(defprotocol main-ex-tgt-rule basic
  (defrole qn
    (vars (a b c akey) (i ssn text) (k skey))
    (trace
     (send (cat i a (enc (enc k b c i (invk a)) b) (enc a b i ssn c)))
     (recv (enc "sorry" a b k)))
    (uniq-orig k))
  (defrole qy
    (vars (a b c akey) (i ssn d text) (k skey))
    (trace
     (send (cat i a (enc (enc k b c i (invk a)) b) (enc a b i ssn c)))
     (recv (enc "data" d k)))
    (uniq-orig k))
  (defrole an
    (vars (a b c akey) (i y n text) (k skey) (x mesg))
    (trace
     (recv (cat i a (enc (enc k b c i (invk a)) b) x))
     (send (enc y n i a x c))
     (recv n)
     (send (enc "sorry" a b k)))
    (uniq-orig y n))
  (defrole ay
    (vars (a b c akey) (i y n d text) (k skey) (x mesg))
    (trace
     (recv (cat i a (enc (enc k b c i (invk a)) b) x))
     (send (enc y n i a x c))
     (recv y)
     (send (enc "data" d k)))
    (uniq-orig d y n))
  (defrole sn
    (vars (a b c akey) (i ssn y n text))
    (trace
     (recv (enc y n i a (enc a b i ssn c) c))
     (send n)))
  (defrole sy
    (vars (a b c akey) (i ssn y n text))
    (trace
     (recv (enc y n i a (enc a b i ssn c) c))
     (send y)))
  (defrule src
    (forall ((z strd) (i ssn text) (k skey) (a b c akey))
	    (implies
	     (and (p "qn" z 2) (p "qn" "i" z i) (p "qn" "ssn" z ssn)
		  (p "qn" "k" z k) (p "qn" "a" z a) (p "qn" "b" z b)
		  (p "qn" "c" z c) (non (invk a)) (non (invk b)) (non (invk c))
		  (uniq k))
	     (exists ((z-0 strd) (x mesg))
		     (and (p "an" z-0 4) (p "an" "x" z-0 x)
			  (p "an" "i" z-0 i) (p "an" "k" z-0 k)
			  (p "an" "a" z-0 a) (p "an" "b" z-0 b)
			  (p "an" "c" z-0 c) (prec z 0 z-0 0)
			  (prec z-0 3 z 1)))))))

(defskeleton main-ex-src
  (vars (a b c akey))
  (defstrand qn 2 (a a) (b b) (c c))
  (non-orig (invk a) (invk b) (invk c))
  (comment
   (defgoal main-ex-src
     (forall ((i ssn text) (k skey) (a b c akey) (z strd))
	     (implies
	      (and (p "qn" z 2) (p "qn" "i" z i) (p "qn" "ssn" z ssn)
		   (p "qn" "k" z k) (p "qn" "a" z a) (p "qn" "b" z b)
		   (p "qn" "c" z c) (non (invk a)) (non (invk b)) (non (invk c))
		   (uniq-at k z 0))
	      (exists ((x mesg) (z-0 strd))
		      (and (p "an" z-0 2) (p "an" "x" z-0 x) (p "an" "i" z-0 i)
			   (p "an" "k" z-0 k) (p "an" "a" z-0 a) (p "an" "b" z-0 b)
			   (p "an" "c" z-0 c) (prec z 0 z-0 0) (prec z-0 1 z 1))))))))

(defskeleton main-ex-src
  (vars (a b c akey))
  (defstrand qy 2 (a a) (b b) (c c))
  (non-orig (invk a) (invk b) (invk c)))

(defskeleton main-ex-tgt
  (vars (a b c akey))
  (defstrand qn 2 (a a) (b b) (c c))
  (non-orig (invk a) (invk b) (invk c)))

(defskeleton main-ex-tgt
  (vars (a b c akey))
  (defstrand qy 2 (a a) (b b) (c c))
  (non-orig (invk a) (invk b) (invk c)))

(defskeleton main-ex-tgt-rule
  (vars (a b c akey))
  (defstrand qn 2 (a a) (b b) (c c))
  (non-orig (invk a) (invk b) (invk c)))
