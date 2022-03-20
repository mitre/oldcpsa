(herald "Otway-Rees Protocol"
  (comment "Standard version using variables of sort mesg"))

(comment "CPSA 3.6.10")
(comment "All input read from tst/or.scm")

(defprotocol or basic
  (defrole init
    (vars (a b s name) (na text) (k skey) (m text))
    (trace (send (cat m a b (enc na m a b (ltk a s))))
      (recv (cat m (enc na k (ltk a s))))))
  (defrole resp
    (vars (a b s name) (nb text) (k skey) (m text) (x y mesg))
    (trace (recv (cat m a b x))
      (send (cat m a b x (enc nb m a b (ltk b s))))
      (recv (cat m y (enc nb k (ltk b s)))) (send y)))
  (defrole serv
    (vars (a b s name) (na nb text) (k skey) (m text))
    (trace
      (recv
        (cat m a b (enc na m a b (ltk a s)) (enc nb m a b (ltk b s))))
      (send (cat m (enc na k (ltk a s)) (enc nb k (ltk b s)))))
    (uniq-orig k)))

(defskeleton or
  (vars (x y mesg) (nb m text) (s a b name) (k skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a a) (b b) (s s) (k k))
  (non-orig (ltk a s) (ltk b s))
  (uniq-orig nb)
  (traces
    ((recv (cat m a b x)) (send (cat m a b x (enc nb m a b (ltk b s))))
      (recv (cat m y (enc nb k (ltk b s)))) (send y)))
  (label 0)
  (unrealized (0 2))
  (origs (nb (0 1)))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton or
  (vars (x y mesg) (nb m na m-0 text) (s a b a-0 name) (k skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a a) (b b) (s s) (k k))
  (defstrand serv 2 (na na) (nb nb) (m m-0) (a a-0) (b b) (s s) (k k))
  (precedes ((0 1) (1 0)) ((1 1) (0 2)))
  (non-orig (ltk a s) (ltk b s))
  (uniq-orig nb k)
  (operation encryption-test (added-strand serv 2) (enc nb k (ltk b s))
    (0 2))
  (traces
    ((recv (cat m a b x)) (send (cat m a b x (enc nb m a b (ltk b s))))
      (recv (cat m y (enc nb k (ltk b s)))) (send y))
    ((recv
       (cat m-0 a-0 b (enc na m-0 a-0 b (ltk a-0 s))
         (enc nb m-0 a-0 b (ltk b s))))
      (send (cat m-0 (enc na k (ltk a-0 s)) (enc nb k (ltk b s))))))
  (label 1)
  (parent 0)
  (unrealized (1 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton or
  (vars (x y mesg) (nb m nb-0 m-0 text) (s a b b-0 name) (k skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a a) (b b) (s s) (k k))
  (defstrand serv 2 (na nb) (nb nb-0) (m m-0) (a b) (b b-0) (s s) (k k))
  (precedes ((0 1) (1 0)) ((1 1) (0 2)))
  (non-orig (ltk a s) (ltk b s))
  (uniq-orig nb k)
  (operation encryption-test (added-strand serv 2) (enc nb k (ltk b s))
    (0 2))
  (traces
    ((recv (cat m a b x)) (send (cat m a b x (enc nb m a b (ltk b s))))
      (recv (cat m y (enc nb k (ltk b s)))) (send y))
    ((recv
       (cat m-0 b b-0 (enc nb m-0 b b-0 (ltk b s))
         (enc nb-0 m-0 b b-0 (ltk b-0 s))))
      (send (cat m-0 (enc nb k (ltk b s)) (enc nb-0 k (ltk b-0 s))))))
  (label 2)
  (parent 0)
  (unrealized (1 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton or
  (vars (x y mesg) (nb m na text) (s a b name) (k skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a a) (b b) (s s) (k k))
  (defstrand serv 2 (na na) (nb nb) (m m) (a a) (b b) (s s) (k k))
  (precedes ((0 1) (1 0)) ((1 1) (0 2)))
  (non-orig (ltk a s) (ltk b s))
  (uniq-orig nb k)
  (operation encryption-test (displaced 2 0 resp 2)
    (enc nb m-0 a-0 b (ltk b s)) (1 0))
  (traces
    ((recv (cat m a b x)) (send (cat m a b x (enc nb m a b (ltk b s))))
      (recv (cat m y (enc nb k (ltk b s)))) (send y))
    ((recv
       (cat m a b (enc na m a b (ltk a s)) (enc nb m a b (ltk b s))))
      (send (cat m (enc na k (ltk a s)) (enc nb k (ltk b s))))))
  (label 3)
  (parent 1)
  (unrealized (1 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton or
  (vars (x y mesg) (nb m nb-0 text) (s a name) (k skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a a) (b a) (s s) (k k))
  (defstrand serv 2 (na nb) (nb nb-0) (m m) (a a) (b a) (s s) (k k))
  (precedes ((0 1) (1 0)) ((1 1) (0 2)))
  (non-orig (ltk a s))
  (uniq-orig nb k)
  (operation encryption-test (displaced 2 0 resp 2)
    (enc nb m-0 b b (ltk b s)) (1 0))
  (traces
    ((recv (cat m a a x)) (send (cat m a a x (enc nb m a a (ltk a s))))
      (recv (cat m y (enc nb k (ltk a s)))) (send y))
    ((recv
       (cat m a a (enc nb m a a (ltk a s)) (enc nb-0 m a a (ltk a s))))
      (send (cat m (enc nb k (ltk a s)) (enc nb-0 k (ltk a s))))))
  (label 4)
  (parent 2)
  (seen 6)
  (unrealized (1 0))
  (comment "3 in cohort - 2 not yet seen"))

(defskeleton or
  (vars (x y mesg) (nb m na text) (s a b name) (k skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a a) (b b) (s s) (k k))
  (defstrand serv 2 (na na) (nb nb) (m m) (a a) (b b) (s s) (k k))
  (defstrand init 1 (na na) (m m) (a a) (b b) (s s))
  (precedes ((0 1) (1 0)) ((1 1) (0 2)) ((2 0) (1 0)))
  (non-orig (ltk a s) (ltk b s))
  (uniq-orig nb k)
  (operation encryption-test (added-strand init 1)
    (enc na m a b (ltk a s)) (1 0))
  (traces
    ((recv (cat m a b x)) (send (cat m a b x (enc nb m a b (ltk b s))))
      (recv (cat m y (enc nb k (ltk b s)))) (send y))
    ((recv
       (cat m a b (enc na m a b (ltk a s)) (enc nb m a b (ltk b s))))
      (send (cat m (enc na k (ltk a s)) (enc nb k (ltk b s)))))
    ((send (cat m a b (enc na m a b (ltk a s))))))
  (label 5)
  (parent 3)
  (unrealized)
  (shape)
  (maps ((0) ((nb nb) (s s) (a a) (b b) (k k) (m m) (x x) (y y))))
  (origs (k (1 1)) (nb (0 1))))

(defskeleton or
  (vars (x y mesg) (nb m text) (s a name) (k skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a a) (b a) (s s) (k k))
  (defstrand serv 2 (na nb) (nb nb) (m m) (a a) (b a) (s s) (k k))
  (precedes ((0 1) (1 0)) ((1 1) (0 2)))
  (non-orig (ltk a s))
  (uniq-orig nb k)
  (operation encryption-test (displaced 2 0 resp 2)
    (enc na m a a (ltk a s)) (1 0))
  (traces
    ((recv (cat m a a x)) (send (cat m a a x (enc nb m a a (ltk a s))))
      (recv (cat m y (enc nb k (ltk a s)))) (send y))
    ((recv
       (cat m a a (enc nb m a a (ltk a s)) (enc nb m a a (ltk a s))))
      (send (cat m (enc nb k (ltk a s)) (enc nb k (ltk a s))))))
  (label 6)
  (parent 3)
  (unrealized)
  (shape)
  (maps ((0) ((nb nb) (s s) (a a) (b a) (k k) (m m) (x x) (y y))))
  (origs (k (1 1)) (nb (0 1))))

(defskeleton or
  (vars (x y x-0 mesg) (nb m na text) (s a name) (k skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a a) (b a) (s s) (k k))
  (defstrand serv 2 (na na) (nb nb) (m m) (a a) (b a) (s s) (k k))
  (defstrand resp 2 (x x-0) (nb na) (m m) (a a) (b a) (s s))
  (precedes ((0 1) (1 0)) ((1 1) (0 2)) ((2 1) (1 0)))
  (non-orig (ltk a s))
  (uniq-orig nb k)
  (operation encryption-test (added-strand resp 2)
    (enc na m a a (ltk a s)) (1 0))
  (traces
    ((recv (cat m a a x)) (send (cat m a a x (enc nb m a a (ltk a s))))
      (recv (cat m y (enc nb k (ltk a s)))) (send y))
    ((recv
       (cat m a a (enc na m a a (ltk a s)) (enc nb m a a (ltk a s))))
      (send (cat m (enc na k (ltk a s)) (enc nb k (ltk a s)))))
    ((recv (cat m a a x-0))
      (send (cat m a a x-0 (enc na m a a (ltk a s))))))
  (label 7)
  (parent 3)
  (unrealized)
  (shape)
  (maps ((0) ((nb nb) (s s) (a a) (b a) (k k) (m m) (x x) (y y))))
  (origs (k (1 1)) (nb (0 1))))

(defskeleton or
  (vars (x y mesg) (nb m nb-0 text) (s a name) (k skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a a) (b a) (s s) (k k))
  (defstrand serv 2 (na nb) (nb nb-0) (m m) (a a) (b a) (s s) (k k))
  (defstrand init 1 (na nb-0) (m m) (a a) (b a) (s s))
  (precedes ((0 1) (1 0)) ((1 1) (0 2)) ((2 0) (1 0)))
  (non-orig (ltk a s))
  (uniq-orig nb k)
  (operation encryption-test (added-strand init 1)
    (enc nb-0 m a a (ltk a s)) (1 0))
  (traces
    ((recv (cat m a a x)) (send (cat m a a x (enc nb m a a (ltk a s))))
      (recv (cat m y (enc nb k (ltk a s)))) (send y))
    ((recv
       (cat m a a (enc nb m a a (ltk a s)) (enc nb-0 m a a (ltk a s))))
      (send (cat m (enc nb k (ltk a s)) (enc nb-0 k (ltk a s)))))
    ((send (cat m a a (enc nb-0 m a a (ltk a s))))))
  (label 8)
  (parent 4)
  (unrealized)
  (shape)
  (maps ((0) ((nb nb) (s s) (a a) (b a) (k k) (m m) (x x) (y y))))
  (origs (k (1 1)) (nb (0 1))))

(defskeleton or
  (vars (x y x-0 mesg) (nb m nb-0 text) (s a name) (k skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a a) (b a) (s s) (k k))
  (defstrand serv 2 (na nb) (nb nb-0) (m m) (a a) (b a) (s s) (k k))
  (defstrand resp 2 (x x-0) (nb nb-0) (m m) (a a) (b a) (s s))
  (precedes ((0 1) (1 0)) ((1 1) (0 2)) ((2 1) (1 0)))
  (non-orig (ltk a s))
  (uniq-orig nb k)
  (operation encryption-test (added-strand resp 2)
    (enc nb-0 m a a (ltk a s)) (1 0))
  (traces
    ((recv (cat m a a x)) (send (cat m a a x (enc nb m a a (ltk a s))))
      (recv (cat m y (enc nb k (ltk a s)))) (send y))
    ((recv
       (cat m a a (enc nb m a a (ltk a s)) (enc nb-0 m a a (ltk a s))))
      (send (cat m (enc nb k (ltk a s)) (enc nb-0 k (ltk a s)))))
    ((recv (cat m a a x-0))
      (send (cat m a a x-0 (enc nb-0 m a a (ltk a s))))))
  (label 9)
  (parent 4)
  (unrealized)
  (shape)
  (maps ((0) ((nb nb) (s s) (a a) (b a) (k k) (m m) (x x) (y y))))
  (origs (k (1 1)) (nb (0 1))))

(comment "Nothing left to do")
