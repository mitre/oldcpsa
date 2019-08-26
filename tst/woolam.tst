(herald "Woo-Lam Protocol")

(comment "CPSA 3.6.3")
(comment "All input read from woolam.scm")

(defprotocol woolam basic
  (defrole init
    (vars (a s name) (n text))
    (trace (send a) (recv n) (send (enc n (ltk a s))))
    (non-orig (ltk a s)))
  (defrole resp
    (vars (a s b name) (n text))
    (trace (recv a) (send n) (recv (enc n (ltk a s)))
      (send (enc a (enc n (ltk a s)) (ltk b s)))
      (recv (enc a n (ltk b s))))
    (non-orig (ltk b s))
    (uniq-orig n))
  (defrole serv
    (vars (a s b name) (n text))
    (trace (recv (enc a (enc n (ltk a s)) (ltk b s)))
      (send (enc a n (ltk b s))))))

(defskeleton woolam
  (vars (n text) (a s b name))
  (defstrand resp 5 (n n) (a a) (s s) (b b))
  (non-orig (ltk a s) (ltk b s))
  (uniq-orig n)
  (traces
    ((recv a) (send n) (recv (enc n (ltk a s)))
      (send (enc a (enc n (ltk a s)) (ltk b s)))
      (recv (enc a n (ltk b s)))))
  (label 0)
  (unrealized (0 2) (0 4))
  (origs (n (0 1)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton woolam
  (vars (n text) (a s b name))
  (defstrand resp 5 (n n) (a a) (s s) (b b))
  (defstrand init 3 (n n) (a a) (s s))
  (precedes ((0 1) (1 1)) ((1 2) (0 2)))
  (non-orig (ltk a s) (ltk b s))
  (uniq-orig n)
  (operation encryption-test (added-strand init 3) (enc n (ltk a s))
    (0 2))
  (traces
    ((recv a) (send n) (recv (enc n (ltk a s)))
      (send (enc a (enc n (ltk a s)) (ltk b s)))
      (recv (enc a n (ltk b s))))
    ((send a) (recv n) (send (enc n (ltk a s)))))
  (label 1)
  (parent 0)
  (unrealized (0 4))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton woolam
  (vars (n text) (a s b name))
  (defstrand resp 5 (n n) (a a) (s s) (b b))
  (defstrand init 3 (n n) (a a) (s s))
  (defstrand serv 2 (n n) (a a) (s s) (b b))
  (precedes ((0 1) (1 1)) ((0 1) (2 0)) ((1 2) (0 2)) ((2 1) (0 4)))
  (non-orig (ltk a s) (ltk b s))
  (uniq-orig n)
  (operation encryption-test (added-strand serv 2) (enc a n (ltk b s))
    (0 4))
  (traces
    ((recv a) (send n) (recv (enc n (ltk a s)))
      (send (enc a (enc n (ltk a s)) (ltk b s)))
      (recv (enc a n (ltk b s))))
    ((send a) (recv n) (send (enc n (ltk a s))))
    ((recv (enc a (enc n (ltk a s)) (ltk b s)))
      (send (enc a n (ltk b s)))))
  (label 2)
  (parent 1)
  (unrealized (2 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton woolam
  (vars (n text) (a s b name))
  (defstrand resp 5 (n n) (a a) (s s) (b b))
  (defstrand init 3 (n n) (a a) (s s))
  (defstrand serv 2 (n n) (a a) (s s) (b b))
  (precedes ((0 1) (1 1)) ((0 3) (2 0)) ((1 2) (0 2)) ((2 1) (0 4)))
  (non-orig (ltk a s) (ltk b s))
  (uniq-orig n)
  (operation encryption-test (displaced 3 0 resp 4)
    (enc a (enc n (ltk a s)) (ltk b s)) (2 0))
  (traces
    ((recv a) (send n) (recv (enc n (ltk a s)))
      (send (enc a (enc n (ltk a s)) (ltk b s)))
      (recv (enc a n (ltk b s))))
    ((send a) (recv n) (send (enc n (ltk a s))))
    ((recv (enc a (enc n (ltk a s)) (ltk b s)))
      (send (enc a n (ltk b s)))))
  (label 3)
  (parent 2)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (s s) (b b) (n n))))
  (origs (n (0 1))))

(comment "Nothing left to do")
