(herald "Uniq orig fail"
  (comment This protocol demonstrates a bug in CPSA: points of
    origination are not always preserved for skeleton uniq-orig
    assumptions))

(comment "CPSA 3.6.8")
(comment "All input read from tst/uniq_orig_doesnt_fail.scm")

(defprotocol uof basic
  (defrole init
    (vars (a b name) (n1 n2 text))
    (trace (send n1) (recv n2) (send (enc n1 n2 n1 (ltk a b)))))
  (defrole resp
    (vars (b a name) (n2 n1 text))
    (trace (recv n1) (send n2) (recv (enc n2 n1 n1 (ltk a b))))))

(defskeleton uof
  (vars (n2 n1 text) (a b name))
  (defstrand resp 3 (n2 n2) (n1 n1) (b b) (a a))
  (non-orig (ltk a b))
  (uniq-orig n2)
  (comment "Responder point-of-view")
  (traces ((recv n1) (send n2) (recv (enc n2 n1 n1 (ltk a b)))))
  (label 0)
  (unrealized (0 2))
  (dead)
  (origs (n2 (0 1)))
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol uof basic
  (defrole init
    (vars (a b name) (n1 n2 text))
    (trace (send n1) (recv n2) (send (enc n1 n2 n1 (ltk a b)))))
  (defrole resp
    (vars (b a name) (n2 n1 text))
    (trace (recv n1) (send n2) (recv (enc n2 n1 n1 (ltk a b))))))

(defskeleton uof
  (vars (n2 n1 text) (a b name))
  (defstrand resp 3 (n2 n2) (n1 n1) (b b) (a a))
  (non-orig (ltk a b))
  (uniq-gen n2)
  (comment "Responder point-of-view")
  (traces ((recv n1) (send n2) (recv (enc n2 n1 n1 (ltk a b)))))
  (label 1)
  (unrealized (0 2))
  (dead)
  (origs)
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol uof basic
  (defrole init
    (vars (a b name) (n1 n2 text))
    (trace (send n1) (recv n2) (send (enc n1 n2 n1 (ltk a b)))))
  (defrole resp
    (vars (b a name) (n2 n1 text))
    (trace (recv n1) (send n2) (recv (enc n2 n1 n1 (ltk a b))))))

(defskeleton uof
  (vars (n2 n1 text) (a b name))
  (defstrand resp 3 (n2 n2) (n1 n1) (b b) (a a))
  (non-orig (ltk a b))
  (comment "Responder point-of-view")
  (traces ((recv n1) (send n2) (recv (enc n2 n1 n1 (ltk a b)))))
  (label 2)
  (unrealized (0 2))
  (origs)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton uof
  (vars (n1 text) (a b name))
  (defstrand resp 3 (n2 n1) (n1 n1) (b b) (a a))
  (defstrand init 3 (n1 n1) (n2 n1) (a a) (b b))
  (precedes ((1 2) (0 2)))
  (non-orig (ltk a b))
  (operation encryption-test (added-strand init 3)
    (enc n1 n1 n1 (ltk a b)) (0 2))
  (traces ((recv n1) (send n1) (recv (enc n1 n1 n1 (ltk a b))))
    ((send n1) (recv n1) (send (enc n1 n1 n1 (ltk a b)))))
  (label 3)
  (parent 2)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (n2 n1) (n1 n1))))
  (origs))

(comment "Nothing left to do")

(defprotocol uof2 basic
  (defrole init
    (vars (a b name) (n1 n2 text))
    (trace (send n1) (recv n2) (send (enc n1 n2 (ltk a b))))
    (uniq-orig n1))
  (defrole resp
    (vars (b a name) (n2 n1 text))
    (trace (recv n1) (send n2) (recv (enc n2 n1 (ltk a b))))))

(defskeleton uof2
  (vars (n2 n1 text) (a b name))
  (defstrand resp 3 (n2 n2) (n1 n1) (b b) (a a))
  (non-orig (ltk a b))
  (uniq-orig n2)
  (comment "Responder point-of-view")
  (traces ((recv n1) (send n2) (recv (enc n2 n1 (ltk a b)))))
  (label 4)
  (unrealized (0 2))
  (dead)
  (origs (n2 (0 1)))
  (comment "empty cohort"))

(comment "Nothing left to do")
