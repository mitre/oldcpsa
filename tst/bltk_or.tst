(herald "Otway-Rees Protocol"
  (comment "Standard version using variables of sort mesg")
  (algebra diffie-hellman))

(comment "CPSA 3.6.10")
(comment "All input read from tst/bltk_or.scm")

(defprotocol or diffie-hellman
  (defrole init
    (vars (a b s name) (na text) (k skey) (m text))
    (trace (send (cat m a b (enc na m a b (bltk a s))))
      (recv (cat m (enc na k (bltk a s))))))
  (defrole resp
    (vars (a b s name) (nb text) (k skey) (m text) (x y mesg))
    (trace (recv (cat m a b x))
      (send (cat m a b x (enc nb m a b (bltk b s))))
      (recv (cat m y (enc nb k (bltk b s)))) (send y)))
  (defrole serv
    (vars (a b s name) (na nb text) (k skey) (m text))
    (trace
      (recv
        (cat m a b (enc na m a b (bltk a s)) (enc nb m a b (bltk b s))))
      (send (cat m (enc na k (bltk a s)) (enc nb k (bltk b s)))))
    (uniq-orig k)))

(defskeleton or
  (vars (x y mesg) (nb m text) (s a b name) (k skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a a) (b b) (s s) (k k))
  (non-orig (bltk s a) (bltk s b))
  (uniq-orig nb)
  (traces
    ((recv (cat m a b x)) (send (cat m a b x (enc nb m a b (bltk s b))))
      (recv (cat m y (enc nb k (bltk s b)))) (send y)))
  (label 0)
  (unrealized (0 2))
  (origs (nb (0 1)))
  (comment "4 in cohort - 4 not yet seen"))

(defskeleton or
  (vars (x y mesg) (nb m na m-0 text) (s a b a-0 name) (k skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a a) (b b) (s s) (k k))
  (defstrand serv 2 (na na) (nb nb) (m m-0) (a a-0) (b s) (s b) (k k))
  (precedes ((0 1) (1 0)) ((1 1) (0 2)))
  (non-orig (bltk s a) (bltk s b))
  (uniq-orig nb k)
  (operation encryption-test (added-strand serv 2) (enc nb k (bltk s b))
    (0 2))
  (traces
    ((recv (cat m a b x)) (send (cat m a b x (enc nb m a b (bltk s b))))
      (recv (cat m y (enc nb k (bltk s b)))) (send y))
    ((recv
       (cat m-0 a-0 s (enc na m-0 a-0 s (bltk b a-0))
         (enc nb m-0 a-0 s (bltk s b))))
      (send (cat m-0 (enc na k (bltk b a-0)) (enc nb k (bltk s b))))))
  (label 1)
  (parent 0)
  (unrealized (1 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton or
  (vars (x y mesg) (nb m na m-0 text) (s a b a-0 name) (k skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a a) (b b) (s s) (k k))
  (defstrand serv 2 (na na) (nb nb) (m m-0) (a a-0) (b b) (s s) (k k))
  (precedes ((0 1) (1 0)) ((1 1) (0 2)))
  (non-orig (bltk s a) (bltk s b))
  (uniq-orig nb k)
  (operation encryption-test (added-strand serv 2) (enc nb k (bltk s b))
    (0 2))
  (traces
    ((recv (cat m a b x)) (send (cat m a b x (enc nb m a b (bltk s b))))
      (recv (cat m y (enc nb k (bltk s b)))) (send y))
    ((recv
       (cat m-0 a-0 b (enc na m-0 a-0 b (bltk s a-0))
         (enc nb m-0 a-0 b (bltk s b))))
      (send (cat m-0 (enc na k (bltk s a-0)) (enc nb k (bltk s b))))))
  (label 2)
  (parent 0)
  (unrealized (1 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton or
  (vars (x y mesg) (nb m nb-0 m-0 text) (s a b b-0 name) (k skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a a) (b b) (s s) (k k))
  (defstrand serv 2 (na nb) (nb nb-0) (m m-0) (a s) (b b-0) (s b) (k k))
  (precedes ((0 1) (1 0)) ((1 1) (0 2)))
  (non-orig (bltk s a) (bltk s b))
  (uniq-orig nb k)
  (operation encryption-test (added-strand serv 2) (enc nb k (bltk s b))
    (0 2))
  (traces
    ((recv (cat m a b x)) (send (cat m a b x (enc nb m a b (bltk s b))))
      (recv (cat m y (enc nb k (bltk s b)))) (send y))
    ((recv
       (cat m-0 s b-0 (enc nb m-0 s b-0 (bltk s b))
         (enc nb-0 m-0 s b-0 (bltk b b-0))))
      (send (cat m-0 (enc nb k (bltk s b)) (enc nb-0 k (bltk b b-0))))))
  (label 3)
  (parent 0)
  (unrealized (1 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton or
  (vars (x y mesg) (nb m nb-0 m-0 text) (s a b b-0 name) (k skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a a) (b b) (s s) (k k))
  (defstrand serv 2 (na nb) (nb nb-0) (m m-0) (a b) (b b-0) (s s) (k k))
  (precedes ((0 1) (1 0)) ((1 1) (0 2)))
  (non-orig (bltk s a) (bltk s b))
  (uniq-orig nb k)
  (operation encryption-test (added-strand serv 2) (enc nb k (bltk s b))
    (0 2))
  (traces
    ((recv (cat m a b x)) (send (cat m a b x (enc nb m a b (bltk s b))))
      (recv (cat m y (enc nb k (bltk s b)))) (send y))
    ((recv
       (cat m-0 b b-0 (enc nb m-0 b b-0 (bltk s b))
         (enc nb-0 m-0 b b-0 (bltk s b-0))))
      (send (cat m-0 (enc nb k (bltk s b)) (enc nb-0 k (bltk s b-0))))))
  (label 4)
  (parent 0)
  (unrealized (1 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton or
  (vars (x y mesg) (nb m na text) (a b name) (k skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a a) (b b) (s b) (k k))
  (defstrand serv 2 (na na) (nb nb) (m m) (a a) (b b) (s b) (k k))
  (precedes ((0 1) (1 0)) ((1 1) (0 2)))
  (non-orig (bltk a b) (bltk b b))
  (uniq-orig nb k)
  (operation encryption-test (displaced 2 0 resp 2)
    (enc nb m-0 a-0 s (bltk s b)) (1 0))
  (traces
    ((recv (cat m a b x)) (send (cat m a b x (enc nb m a b (bltk b b))))
      (recv (cat m y (enc nb k (bltk b b)))) (send y))
    ((recv
       (cat m a b (enc na m a b (bltk a b)) (enc nb m a b (bltk b b))))
      (send (cat m (enc na k (bltk a b)) (enc nb k (bltk b b))))))
  (label 5)
  (parent 1)
  (unrealized (1 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton or
  (vars (x y mesg) (nb m na text) (s a b name) (k skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a a) (b b) (s s) (k k))
  (defstrand serv 2 (na na) (nb nb) (m m) (a a) (b b) (s s) (k k))
  (precedes ((0 1) (1 0)) ((1 1) (0 2)))
  (non-orig (bltk s a) (bltk s b))
  (uniq-orig nb k)
  (operation encryption-test (displaced 2 0 resp 2)
    (enc nb m-0 a-0 b (bltk s b)) (1 0))
  (traces
    ((recv (cat m a b x)) (send (cat m a b x (enc nb m a b (bltk s b))))
      (recv (cat m y (enc nb k (bltk s b)))) (send y))
    ((recv
       (cat m a b (enc na m a b (bltk s a)) (enc nb m a b (bltk s b))))
      (send (cat m (enc na k (bltk s a)) (enc nb k (bltk s b))))))
  (label 6)
  (parent 2)
  (seen 10)
  (unrealized (1 0))
  (comment "4 in cohort - 3 not yet seen"))

(defskeleton or
  (vars (x y mesg) (nb m nb-0 text) (a b name) (k skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a a) (b b) (s a) (k k))
  (defstrand serv 2 (na nb) (nb nb-0) (m m) (a a) (b b) (s b) (k k))
  (precedes ((0 1) (1 0)) ((1 1) (0 2)))
  (non-orig (bltk a a) (bltk a b))
  (uniq-orig nb k)
  (operation encryption-test (displaced 2 0 resp 2)
    (enc nb m-0 s b (bltk s b)) (1 0))
  (traces
    ((recv (cat m a b x)) (send (cat m a b x (enc nb m a b (bltk a b))))
      (recv (cat m y (enc nb k (bltk a b)))) (send y))
    ((recv
       (cat m a b (enc nb m a b (bltk a b))
         (enc nb-0 m a b (bltk b b))))
      (send (cat m (enc nb k (bltk a b)) (enc nb-0 k (bltk b b))))))
  (label 7)
  (parent 3)
  (unrealized)
  (shape)
  (maps ((0) ((nb nb) (s a) (a a) (b b) (k k) (m m) (x x) (y y))))
  (origs (k (1 1)) (nb (0 1))))

(defskeleton or
  (vars (x y mesg) (nb m nb-0 text) (s a name) (k skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a a) (b a) (s s) (k k))
  (defstrand serv 2 (na nb) (nb nb-0) (m m) (a a) (b a) (s s) (k k))
  (precedes ((0 1) (1 0)) ((1 1) (0 2)))
  (non-orig (bltk s a))
  (uniq-orig nb k)
  (operation encryption-test (displaced 2 0 resp 2)
    (enc nb m-0 b b (bltk s b)) (1 0))
  (traces
    ((recv (cat m a a x)) (send (cat m a a x (enc nb m a a (bltk s a))))
      (recv (cat m y (enc nb k (bltk s a)))) (send y))
    ((recv
       (cat m a a (enc nb m a a (bltk s a))
         (enc nb-0 m a a (bltk s a))))
      (send (cat m (enc nb k (bltk s a)) (enc nb-0 k (bltk s a))))))
  (label 8)
  (parent 4)
  (seen 13)
  (unrealized (1 0))
  (comment "3 in cohort - 2 not yet seen"))

(defskeleton or
  (vars (x y mesg) (nb m na text) (a b name) (k skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a a) (b b) (s b) (k k))
  (defstrand serv 2 (na na) (nb nb) (m m) (a a) (b b) (s b) (k k))
  (defstrand init 1 (na na) (m m) (a a) (b b) (s b))
  (precedes ((0 1) (1 0)) ((1 1) (0 2)) ((2 0) (1 0)))
  (non-orig (bltk a b) (bltk b b))
  (uniq-orig nb k)
  (operation encryption-test (added-strand init 1)
    (enc na m a b (bltk a b)) (1 0))
  (traces
    ((recv (cat m a b x)) (send (cat m a b x (enc nb m a b (bltk b b))))
      (recv (cat m y (enc nb k (bltk b b)))) (send y))
    ((recv
       (cat m a b (enc na m a b (bltk a b)) (enc nb m a b (bltk b b))))
      (send (cat m (enc na k (bltk a b)) (enc nb k (bltk b b)))))
    ((send (cat m a b (enc na m a b (bltk a b))))))
  (label 9)
  (parent 5)
  (seen 12)
  (unrealized)
  (comment "1 in cohort - 0 not yet seen"))

(defskeleton or
  (vars (x y x-0 mesg) (nb m na text) (a b name) (k skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a a) (b b) (s b) (k k))
  (defstrand serv 2 (na na) (nb nb) (m m) (a a) (b b) (s b) (k k))
  (defstrand resp 2 (x x-0) (nb na) (m m) (a a) (b b) (s a))
  (precedes ((0 1) (1 0)) ((1 1) (0 2)) ((2 1) (1 0)))
  (non-orig (bltk a b) (bltk b b))
  (uniq-orig nb k)
  (operation encryption-test (added-strand resp 2)
    (enc na m a b (bltk a b)) (1 0))
  (traces
    ((recv (cat m a b x)) (send (cat m a b x (enc nb m a b (bltk b b))))
      (recv (cat m y (enc nb k (bltk b b)))) (send y))
    ((recv
       (cat m a b (enc na m a b (bltk a b)) (enc nb m a b (bltk b b))))
      (send (cat m (enc na k (bltk a b)) (enc nb k (bltk b b)))))
    ((recv (cat m a b x-0))
      (send (cat m a b x-0 (enc na m a b (bltk a b))))))
  (label 10)
  (parent 5)
  (unrealized)
  (shape)
  (maps ((0) ((nb nb) (s b) (a a) (b b) (k k) (m m) (x x) (y y))))
  (origs (k (1 1)) (nb (0 1))))

(defskeleton or
  (vars (x y mesg) (nb m text) (a name) (k skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a a) (b a) (s a) (k k))
  (defstrand serv 2 (na nb) (nb nb) (m m) (a a) (b a) (s a) (k k))
  (precedes ((0 1) (1 0)) ((1 1) (0 2)))
  (non-orig (bltk a a))
  (uniq-orig nb k)
  (operation encryption-test (displaced 2 0 resp 2)
    (enc na m a a (bltk a a)) (1 0))
  (traces
    ((recv (cat m a a x)) (send (cat m a a x (enc nb m a a (bltk a a))))
      (recv (cat m y (enc nb k (bltk a a)))) (send y))
    ((recv
       (cat m a a (enc nb m a a (bltk a a)) (enc nb m a a (bltk a a))))
      (send (cat m (enc nb k (bltk a a)) (enc nb k (bltk a a))))))
  (label 11)
  (parent 5)
  (seen 13)
  (unrealized)
  (comment "1 in cohort - 0 not yet seen"))

(defskeleton or
  (vars (x y mesg) (nb m na text) (s a b name) (k skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a a) (b b) (s s) (k k))
  (defstrand serv 2 (na na) (nb nb) (m m) (a a) (b b) (s s) (k k))
  (defstrand init 1 (na na) (m m) (a a) (b b) (s s))
  (precedes ((0 1) (1 0)) ((1 1) (0 2)) ((2 0) (1 0)))
  (non-orig (bltk s a) (bltk s b))
  (uniq-orig nb k)
  (operation encryption-test (added-strand init 1)
    (enc na m a b (bltk s a)) (1 0))
  (traces
    ((recv (cat m a b x)) (send (cat m a b x (enc nb m a b (bltk s b))))
      (recv (cat m y (enc nb k (bltk s b)))) (send y))
    ((recv
       (cat m a b (enc na m a b (bltk s a)) (enc nb m a b (bltk s b))))
      (send (cat m (enc na k (bltk s a)) (enc nb k (bltk s b)))))
    ((send (cat m a b (enc na m a b (bltk s a))))))
  (label 12)
  (parent 6)
  (unrealized)
  (shape)
  (maps ((0) ((nb nb) (s s) (a a) (b b) (k k) (m m) (x x) (y y))))
  (origs (k (1 1)) (nb (0 1))))

(defskeleton or
  (vars (x y mesg) (nb m text) (s a name) (k skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a a) (b a) (s s) (k k))
  (defstrand serv 2 (na nb) (nb nb) (m m) (a a) (b a) (s s) (k k))
  (precedes ((0 1) (1 0)) ((1 1) (0 2)))
  (non-orig (bltk s a))
  (uniq-orig nb k)
  (operation encryption-test (displaced 2 0 resp 2)
    (enc na m a a (bltk s a)) (1 0))
  (traces
    ((recv (cat m a a x)) (send (cat m a a x (enc nb m a a (bltk s a))))
      (recv (cat m y (enc nb k (bltk s a)))) (send y))
    ((recv
       (cat m a a (enc nb m a a (bltk s a)) (enc nb m a a (bltk s a))))
      (send (cat m (enc nb k (bltk s a)) (enc nb k (bltk s a))))))
  (label 13)
  (parent 6)
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
  (non-orig (bltk s a))
  (uniq-orig nb k)
  (operation encryption-test (added-strand resp 2)
    (enc na m a a (bltk s a)) (1 0))
  (traces
    ((recv (cat m a a x)) (send (cat m a a x (enc nb m a a (bltk s a))))
      (recv (cat m y (enc nb k (bltk s a)))) (send y))
    ((recv
       (cat m a a (enc na m a a (bltk s a)) (enc nb m a a (bltk s a))))
      (send (cat m (enc na k (bltk s a)) (enc nb k (bltk s a)))))
    ((recv (cat m a a x-0))
      (send (cat m a a x-0 (enc na m a a (bltk s a))))))
  (label 14)
  (parent 6)
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
  (non-orig (bltk s a))
  (uniq-orig nb k)
  (operation encryption-test (added-strand init 1)
    (enc nb-0 m a a (bltk s a)) (1 0))
  (traces
    ((recv (cat m a a x)) (send (cat m a a x (enc nb m a a (bltk s a))))
      (recv (cat m y (enc nb k (bltk s a)))) (send y))
    ((recv
       (cat m a a (enc nb m a a (bltk s a))
         (enc nb-0 m a a (bltk s a))))
      (send (cat m (enc nb k (bltk s a)) (enc nb-0 k (bltk s a)))))
    ((send (cat m a a (enc nb-0 m a a (bltk s a))))))
  (label 15)
  (parent 8)
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
  (non-orig (bltk s a))
  (uniq-orig nb k)
  (operation encryption-test (added-strand resp 2)
    (enc nb-0 m a a (bltk s a)) (1 0))
  (traces
    ((recv (cat m a a x)) (send (cat m a a x (enc nb m a a (bltk s a))))
      (recv (cat m y (enc nb k (bltk s a)))) (send y))
    ((recv
       (cat m a a (enc nb m a a (bltk s a))
         (enc nb-0 m a a (bltk s a))))
      (send (cat m (enc nb k (bltk s a)) (enc nb-0 k (bltk s a)))))
    ((recv (cat m a a x-0))
      (send (cat m a a x-0 (enc nb-0 m a a (bltk s a))))))
  (label 16)
  (parent 8)
  (unrealized)
  (shape)
  (maps ((0) ((nb nb) (s s) (a a) (b a) (k k) (m m) (x x) (y y))))
  (origs (k (1 1)) (nb (0 1))))

(comment "Nothing left to do")