(herald "One-way Authentication Test with bltk keys"
  (algebra diffie-hellman) (bound 12))

(comment "CPSA 3.6.1")
(comment "All input read from owat.scm")

(defprotocol owa diffie-hellman
  (defrole init
    (vars (a b name) (n text))
    (trace (send a) (recv n) (send (enc n (bltk a b)))))
  (defrole resp
    (vars (a b name) (n text))
    (trace (recv a) (send n) (recv (enc n (bltk a b))))))

(defskeleton owa
  (vars (n text) (a b name))
  (defstrand init 3 (n n) (a a) (b b))
  (non-orig (bltk a b))
  (traces ((send a) (recv n) (send (enc n (bltk a b)))))
  (label 0)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (n n))))
  (origs))

(comment "Nothing left to do")

(defprotocol owa diffie-hellman
  (defrole init
    (vars (a b name) (n text))
    (trace (send a) (recv n) (send (enc n (bltk a b)))))
  (defrole resp
    (vars (a b name) (n text))
    (trace (recv a) (send n) (recv (enc n (bltk a b))))))

(defskeleton owa
  (vars (n text) (a b name))
  (defstrand resp 3 (n n) (a a) (b b))
  (non-orig (bltk a b))
  (uniq-orig n)
  (traces ((recv a) (send n) (recv (enc n (bltk a b)))))
  (label 1)
  (unrealized (0 2))
  (origs (n (0 1)))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton owa
  (vars (n text) (a b name))
  (defstrand resp 3 (n n) (a a) (b b))
  (defstrand init 3 (n n) (a b) (b a))
  (precedes ((0 1) (1 1)) ((1 2) (0 2)))
  (non-orig (bltk a b))
  (uniq-orig n)
  (operation encryption-test (added-strand init 3) (enc n (bltk a b))
    (0 2))
  (traces ((recv a) (send n) (recv (enc n (bltk a b))))
    ((send b) (recv n) (send (enc n (bltk a b)))))
  (label 2)
  (parent 1)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (n n))))
  (origs (n (0 1))))

(defskeleton owa
  (vars (n text) (a b name))
  (defstrand resp 3 (n n) (a a) (b b))
  (defstrand init 3 (n n) (a a) (b b))
  (precedes ((0 1) (1 1)) ((1 2) (0 2)))
  (non-orig (bltk a b))
  (uniq-orig n)
  (operation encryption-test (added-strand init 3) (enc n (bltk a b))
    (0 2))
  (traces ((recv a) (send n) (recv (enc n (bltk a b))))
    ((send a) (recv n) (send (enc n (bltk a b)))))
  (label 3)
  (parent 1)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (n n))))
  (origs (n (0 1))))

(comment "Nothing left to do")
