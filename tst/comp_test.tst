(herald "Main Example")

(comment "CPSA 3.6.4")
(comment "All input read from comp_test.scm")

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
    (trace (recv (cat i a (enc (enc k b c i (invk a)) b) x))
      (send (enc "sorry" a b k))))
  (defrole ay
    (vars (a b c akey) (i d text) (k skey) (x mesg))
    (trace (recv (cat i a (enc (enc k b c i (invk a)) b) x))
      (send (enc "data" d k)))
    (uniq-orig d)))

(defskeleton main-ex-src
  (vars (i ssn text) (k skey) (a b c akey))
  (defstrand qn 2 (i i) (ssn ssn) (k k) (a a) (b b) (c c))
  (non-orig (invk a) (invk b) (invk c))
  (uniq-orig k)
  (comment
    (defgoal main-ex-src
      (forall ((i ssn text) (k skey) (a b c akey) (z strd))
        (implies
          (and (p "qn" z 2) (p "qn" "i" z i) (p "qn" "ssn" z ssn)
            (p "qn" "k" z k) (p "qn" "a" z a) (p "qn" "b" z b)
            (p "qn" "c" z c) (non (invk a)) (non (invk b))
            (non (invk c)) (uniq-at k z 0))
          (exists ((x mesg) (z-0 strd))
            (and (p "an" z-0 2) (p "an" "x" z-0 x) (p "an" "i" z-0 i)
              (p "an" "k" z-0 k) (p "an" "a" z-0 a) (p "an" "b" z-0 b)
              (p "an" "c" z-0 c) (prec z 0 z-0 0) (prec z-0 1 z 1)))))))
  (traces
    ((send (cat i a (enc (enc k b c i (invk a)) b) (enc a b i ssn c)))
      (recv (enc "sorry" a b k))))
  (label 0)
  (unrealized (0 1))
  (origs (k (0 0)))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton main-ex-src
  (vars (x mesg) (i ssn i-0 text) (k skey) (a b c c-0 akey))
  (defstrand qn 2 (i i) (ssn ssn) (k k) (a a) (b b) (c c))
  (defstrand an 2 (x x) (i i-0) (k k) (a a) (b b) (c c-0))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (non-orig (invk a) (invk b) (invk c))
  (uniq-orig k)
  (operation encryption-test (added-strand an 2) (enc "sorry" a b k)
    (0 1))
  (traces
    ((send (cat i a (enc (enc k b c i (invk a)) b) (enc a b i ssn c)))
      (recv (enc "sorry" a b k)))
    ((recv (cat i-0 a (enc (enc k b c-0 i-0 (invk a)) b) x))
      (send (enc "sorry" a b k))))
  (label 1)
  (parent 0)
  (unrealized (1 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton main-ex-src
  (vars (i ssn text) (k skey) (a b c akey))
  (defstrand qn 2 (i i) (ssn ssn) (k k) (a a) (b b) (c c))
  (deflistener k)
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (non-orig (invk a) (invk b) (invk c))
  (uniq-orig k)
  (operation encryption-test (added-listener k) (enc "sorry" a b k)
    (0 1))
  (traces
    ((send (cat i a (enc (enc k b c i (invk a)) b) (enc a b i ssn c)))
      (recv (enc "sorry" a b k))) ((recv k) (send k)))
  (label 2)
  (parent 0)
  (unrealized (1 0))
  (dead)
  (comment "empty cohort"))

(defskeleton main-ex-src
  (vars (x mesg) (i ssn text) (k skey) (a b c akey))
  (defstrand qn 2 (i i) (ssn ssn) (k k) (a a) (b b) (c c))
  (defstrand an 2 (x x) (i i) (k k) (a a) (b b) (c c))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (non-orig (invk a) (invk b) (invk c))
  (uniq-orig k)
  (operation encryption-test (displaced 2 0 qy 1)
    (enc k b c-0 i-0 (invk a)) (1 0))
  (traces
    ((send (cat i a (enc (enc k b c i (invk a)) b) (enc a b i ssn c)))
      (recv (enc "sorry" a b k)))
    ((recv (cat i a (enc (enc k b c i (invk a)) b) x))
      (send (enc "sorry" a b k))))
  (label 3)
  (parent 1)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (c c) (i i) (ssn ssn) (k k))))
  (origs (k (0 0))))

(comment "Nothing left to do")

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
    (trace (recv (cat i a (enc (enc k b c i (invk a)) b) x))
      (send (enc "sorry" a b k))))
  (defrole ay
    (vars (a b c akey) (i d text) (k skey) (x mesg))
    (trace (recv (cat i a (enc (enc k b c i (invk a)) b) x))
      (send (enc "data" d k)))
    (uniq-orig d)))

(defskeleton main-ex-src
  (vars (i ssn d text) (k skey) (a b c akey))
  (defstrand qy 2 (i i) (ssn ssn) (d d) (k k) (a a) (b b) (c c))
  (non-orig (invk a) (invk b) (invk c))
  (uniq-orig k)
  (traces
    ((send (cat i a (enc (enc k b c i (invk a)) b) (enc a b i ssn c)))
      (recv (enc "data" d k))))
  (label 4)
  (unrealized (0 1))
  (origs (k (0 0)))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton main-ex-src
  (vars (x mesg) (i ssn d i-0 text) (k skey) (a b c a-0 b-0 c-0 akey))
  (defstrand qy 2 (i i) (ssn ssn) (d d) (k k) (a a) (b b) (c c))
  (defstrand ay 2 (x x) (i i-0) (d d) (k k) (a a-0) (b b-0) (c c-0))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (non-orig (invk a) (invk b) (invk c))
  (uniq-orig d k)
  (operation encryption-test (added-strand ay 2) (enc "data" d k) (0 1))
  (traces
    ((send (cat i a (enc (enc k b c i (invk a)) b) (enc a b i ssn c)))
      (recv (enc "data" d k)))
    ((recv (cat i-0 a-0 (enc (enc k b-0 c-0 i-0 (invk a-0)) b-0) x))
      (send (enc "data" d k))))
  (label 5)
  (parent 4)
  (unrealized (1 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton main-ex-src
  (vars (i ssn d text) (k skey) (a b c akey))
  (defstrand qy 2 (i i) (ssn ssn) (d d) (k k) (a a) (b b) (c c))
  (deflistener k)
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (non-orig (invk a) (invk b) (invk c))
  (uniq-orig k)
  (operation encryption-test (added-listener k) (enc "data" d k) (0 1))
  (traces
    ((send (cat i a (enc (enc k b c i (invk a)) b) (enc a b i ssn c)))
      (recv (enc "data" d k))) ((recv k) (send k)))
  (label 6)
  (parent 4)
  (unrealized (1 0))
  (dead)
  (comment "empty cohort"))

(defskeleton main-ex-src
  (vars (x mesg) (i ssn d text) (k skey) (a b c akey))
  (defstrand qy 2 (i i) (ssn ssn) (d d) (k k) (a a) (b b) (c c))
  (defstrand ay 2 (x x) (i i) (d d) (k k) (a a) (b b) (c c))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (non-orig (invk a) (invk b) (invk c))
  (uniq-orig d k)
  (operation nonce-test (contracted (a-0 a) (b-0 b) (c-0 c) (i-0 i)) k
    (1 0) (enc (enc k b c i (invk a)) b))
  (traces
    ((send (cat i a (enc (enc k b c i (invk a)) b) (enc a b i ssn c)))
      (recv (enc "data" d k)))
    ((recv (cat i a (enc (enc k b c i (invk a)) b) x))
      (send (enc "data" d k))))
  (label 7)
  (parent 5)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (c c) (i i) (ssn ssn) (d d) (k k))))
  (origs (d (1 1)) (k (0 0))))

(comment "Nothing left to do")

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
    (trace (recv (cat i a (enc (enc k b c i (invk a)) b) x))
      (send (enc y n i a x c)) (recv n) (send (enc "sorry" a b k)))
    (uniq-orig y n))
  (defrole ay
    (vars (a b c akey) (i y n d text) (k skey) (x mesg))
    (trace (recv (cat i a (enc (enc k b c i (invk a)) b) x))
      (send (enc y n i a x c)) (recv y) (send (enc "data" d k)))
    (uniq-orig y n d))
  (defrole sn
    (vars (a b c akey) (i ssn y n text))
    (trace (recv (enc y n i a (enc a b i ssn c) c)) (send n)))
  (defrole sy
    (vars (a b c akey) (i ssn y n text))
    (trace (recv (enc y n i a (enc a b i ssn c) c)) (send y))))

(defskeleton main-ex-tgt
  (vars (i ssn text) (k skey) (a b c akey))
  (defstrand qn 2 (i i) (ssn ssn) (k k) (a a) (b b) (c c))
  (non-orig (invk a) (invk b) (invk c))
  (uniq-orig k)
  (traces
    ((send (cat i a (enc (enc k b c i (invk a)) b) (enc a b i ssn c)))
      (recv (enc "sorry" a b k))))
  (label 8)
  (unrealized (0 1))
  (origs (k (0 0)))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton main-ex-tgt
  (vars (x mesg) (i ssn i-0 y n text) (k skey) (a b c c-0 akey))
  (defstrand qn 2 (i i) (ssn ssn) (k k) (a a) (b b) (c c))
  (defstrand an 4 (x x) (i i-0) (y y) (n n) (k k) (a a) (b b) (c c-0))
  (precedes ((0 0) (1 0)) ((1 3) (0 1)))
  (non-orig (invk a) (invk b) (invk c))
  (uniq-orig y n k)
  (operation encryption-test (added-strand an 4) (enc "sorry" a b k)
    (0 1))
  (traces
    ((send (cat i a (enc (enc k b c i (invk a)) b) (enc a b i ssn c)))
      (recv (enc "sorry" a b k)))
    ((recv (cat i-0 a (enc (enc k b c-0 i-0 (invk a)) b) x))
      (send (enc y n i-0 a x c-0)) (recv n) (send (enc "sorry" a b k))))
  (label 9)
  (parent 8)
  (unrealized (1 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton main-ex-tgt
  (vars (i ssn text) (k skey) (a b c akey))
  (defstrand qn 2 (i i) (ssn ssn) (k k) (a a) (b b) (c c))
  (deflistener k)
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (non-orig (invk a) (invk b) (invk c))
  (uniq-orig k)
  (operation encryption-test (added-listener k) (enc "sorry" a b k)
    (0 1))
  (traces
    ((send (cat i a (enc (enc k b c i (invk a)) b) (enc a b i ssn c)))
      (recv (enc "sorry" a b k))) ((recv k) (send k)))
  (label 10)
  (parent 8)
  (unrealized (1 0))
  (dead)
  (comment "empty cohort"))

(defskeleton main-ex-tgt
  (vars (x mesg) (i ssn y n text) (k skey) (a b c akey))
  (defstrand qn 2 (i i) (ssn ssn) (k k) (a a) (b b) (c c))
  (defstrand an 4 (x x) (i i) (y y) (n n) (k k) (a a) (b b) (c c))
  (precedes ((0 0) (1 0)) ((1 3) (0 1)))
  (non-orig (invk a) (invk b) (invk c))
  (uniq-orig y n k)
  (operation encryption-test (displaced 2 0 qy 1)
    (enc k b c-0 i-0 (invk a)) (1 0))
  (traces
    ((send (cat i a (enc (enc k b c i (invk a)) b) (enc a b i ssn c)))
      (recv (enc "sorry" a b k)))
    ((recv (cat i a (enc (enc k b c i (invk a)) b) x))
      (send (enc y n i a x c)) (recv n) (send (enc "sorry" a b k))))
  (label 11)
  (parent 9)
  (unrealized (1 2))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton main-ex-tgt
  (vars (i ssn y n ssn-0 text) (k skey) (a b c b-0 akey))
  (defstrand qn 2 (i i) (ssn ssn) (k k) (a a) (b b) (c c))
  (defstrand an 4 (x (enc a b-0 i ssn-0 c)) (i i) (y y) (n n) (k k)
    (a a) (b b) (c c))
  (defstrand sn 2 (i i) (ssn ssn-0) (y y) (n n) (a a) (b b-0) (c c))
  (precedes ((0 0) (1 0)) ((1 1) (2 0)) ((1 3) (0 1)) ((2 1) (1 2)))
  (non-orig (invk a) (invk b) (invk c))
  (uniq-orig y n k)
  (operation nonce-test (added-strand sn 2) n (1 2)
    (enc y n i a (enc a b-0 i ssn-0 c) c))
  (traces
    ((send (cat i a (enc (enc k b c i (invk a)) b) (enc a b i ssn c)))
      (recv (enc "sorry" a b k)))
    ((recv
       (cat i a (enc (enc k b c i (invk a)) b) (enc a b-0 i ssn-0 c)))
      (send (enc y n i a (enc a b-0 i ssn-0 c) c)) (recv n)
      (send (enc "sorry" a b k)))
    ((recv (enc y n i a (enc a b-0 i ssn-0 c) c)) (send n)))
  (label 12)
  (parent 11)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (c c) (i i) (ssn ssn) (k k))))
  (origs (k (0 0)) (y (1 1)) (n (1 1))))

(comment "Nothing left to do")

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
    (trace (recv (cat i a (enc (enc k b c i (invk a)) b) x))
      (send (enc y n i a x c)) (recv n) (send (enc "sorry" a b k)))
    (uniq-orig y n))
  (defrole ay
    (vars (a b c akey) (i y n d text) (k skey) (x mesg))
    (trace (recv (cat i a (enc (enc k b c i (invk a)) b) x))
      (send (enc y n i a x c)) (recv y) (send (enc "data" d k)))
    (uniq-orig y n d))
  (defrole sn
    (vars (a b c akey) (i ssn y n text))
    (trace (recv (enc y n i a (enc a b i ssn c) c)) (send n)))
  (defrole sy
    (vars (a b c akey) (i ssn y n text))
    (trace (recv (enc y n i a (enc a b i ssn c) c)) (send y))))

(defskeleton main-ex-tgt
  (vars (i ssn d text) (k skey) (a b c akey))
  (defstrand qy 2 (i i) (ssn ssn) (d d) (k k) (a a) (b b) (c c))
  (non-orig (invk a) (invk b) (invk c))
  (uniq-orig k)
  (traces
    ((send (cat i a (enc (enc k b c i (invk a)) b) (enc a b i ssn c)))
      (recv (enc "data" d k))))
  (label 13)
  (unrealized (0 1))
  (origs (k (0 0)))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton main-ex-tgt
  (vars (x mesg) (i ssn d i-0 y n text) (k skey)
    (a b c a-0 b-0 c-0 akey))
  (defstrand qy 2 (i i) (ssn ssn) (d d) (k k) (a a) (b b) (c c))
  (defstrand ay 4 (x x) (i i-0) (y y) (n n) (d d) (k k) (a a-0) (b b-0)
    (c c-0))
  (precedes ((0 0) (1 0)) ((1 3) (0 1)))
  (non-orig (invk a) (invk b) (invk c))
  (uniq-orig d y n k)
  (operation encryption-test (added-strand ay 4) (enc "data" d k) (0 1))
  (traces
    ((send (cat i a (enc (enc k b c i (invk a)) b) (enc a b i ssn c)))
      (recv (enc "data" d k)))
    ((recv (cat i-0 a-0 (enc (enc k b-0 c-0 i-0 (invk a-0)) b-0) x))
      (send (enc y n i-0 a-0 x c-0)) (recv y) (send (enc "data" d k))))
  (label 14)
  (parent 13)
  (unrealized (1 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton main-ex-tgt
  (vars (i ssn d text) (k skey) (a b c akey))
  (defstrand qy 2 (i i) (ssn ssn) (d d) (k k) (a a) (b b) (c c))
  (deflistener k)
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (non-orig (invk a) (invk b) (invk c))
  (uniq-orig k)
  (operation encryption-test (added-listener k) (enc "data" d k) (0 1))
  (traces
    ((send (cat i a (enc (enc k b c i (invk a)) b) (enc a b i ssn c)))
      (recv (enc "data" d k))) ((recv k) (send k)))
  (label 15)
  (parent 13)
  (unrealized (1 0))
  (dead)
  (comment "empty cohort"))

(defskeleton main-ex-tgt
  (vars (x mesg) (i ssn d y n text) (k skey) (a b c akey))
  (defstrand qy 2 (i i) (ssn ssn) (d d) (k k) (a a) (b b) (c c))
  (defstrand ay 4 (x x) (i i) (y y) (n n) (d d) (k k) (a a) (b b) (c c))
  (precedes ((0 0) (1 0)) ((1 3) (0 1)))
  (non-orig (invk a) (invk b) (invk c))
  (uniq-orig d y n k)
  (operation nonce-test (contracted (a-0 a) (b-0 b) (c-0 c) (i-0 i)) k
    (1 0) (enc (enc k b c i (invk a)) b))
  (traces
    ((send (cat i a (enc (enc k b c i (invk a)) b) (enc a b i ssn c)))
      (recv (enc "data" d k)))
    ((recv (cat i a (enc (enc k b c i (invk a)) b) x))
      (send (enc y n i a x c)) (recv y) (send (enc "data" d k))))
  (label 16)
  (parent 14)
  (unrealized (1 2))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton main-ex-tgt
  (vars (x mesg) (i ssn d n text) (k skey) (a b c akey))
  (defstrand qy 2 (i i) (ssn ssn) (d d) (k k) (a a) (b b) (c c))
  (defstrand ay 4 (x x) (i i) (y n) (n n) (d d) (k k) (a a) (b b) (c c))
  (precedes ((0 0) (1 0)) ((1 3) (0 1)))
  (non-orig (invk a) (invk b) (invk c))
  (uniq-orig d n k)
  (operation nonce-test (displaced 2 1 ay 2) y (1 2) (enc y n i a x c))
  (traces
    ((send (cat i a (enc (enc k b c i (invk a)) b) (enc a b i ssn c)))
      (recv (enc "data" d k)))
    ((recv (cat i a (enc (enc k b c i (invk a)) b) x))
      (send (enc n n i a x c)) (recv n) (send (enc "data" d k))))
  (label 17)
  (parent 16)
  (unrealized (1 2))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton main-ex-tgt
  (vars (i ssn d y n ssn-0 text) (k skey) (a b c b-0 akey))
  (defstrand qy 2 (i i) (ssn ssn) (d d) (k k) (a a) (b b) (c c))
  (defstrand ay 4 (x (enc a b-0 i ssn-0 c)) (i i) (y y) (n n) (d d)
    (k k) (a a) (b b) (c c))
  (defstrand sy 2 (i i) (ssn ssn-0) (y y) (n n) (a a) (b b-0) (c c))
  (precedes ((0 0) (1 0)) ((1 1) (2 0)) ((1 3) (0 1)) ((2 1) (1 2)))
  (non-orig (invk a) (invk b) (invk c))
  (uniq-orig d y n k)
  (operation nonce-test (added-strand sy 2) y (1 2)
    (enc y n i a (enc a b-0 i ssn-0 c) c))
  (traces
    ((send (cat i a (enc (enc k b c i (invk a)) b) (enc a b i ssn c)))
      (recv (enc "data" d k)))
    ((recv
       (cat i a (enc (enc k b c i (invk a)) b) (enc a b-0 i ssn-0 c)))
      (send (enc y n i a (enc a b-0 i ssn-0 c) c)) (recv y)
      (send (enc "data" d k)))
    ((recv (enc y n i a (enc a b-0 i ssn-0 c) c)) (send y)))
  (label 18)
  (parent 16)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (c c) (i i) (ssn ssn) (d d) (k k))))
  (origs (y (1 1)) (n (1 1)) (d (1 3)) (k (0 0))))

(defskeleton main-ex-tgt
  (vars (i ssn d n ssn-0 text) (k skey) (a b c b-0 akey))
  (defstrand qy 2 (i i) (ssn ssn) (d d) (k k) (a a) (b b) (c c))
  (defstrand ay 4 (x (enc a b-0 i ssn-0 c)) (i i) (y n) (n n) (d d)
    (k k) (a a) (b b) (c c))
  (defstrand sy 2 (i i) (ssn ssn-0) (y n) (n n) (a a) (b b-0) (c c))
  (precedes ((0 0) (1 0)) ((1 1) (2 0)) ((1 3) (0 1)) ((2 1) (1 2)))
  (non-orig (invk a) (invk b) (invk c))
  (uniq-orig d n k)
  (operation nonce-test (added-strand sy 2) n (1 2)
    (enc n n i a (enc a b-0 i ssn-0 c) c))
  (traces
    ((send (cat i a (enc (enc k b c i (invk a)) b) (enc a b i ssn c)))
      (recv (enc "data" d k)))
    ((recv
       (cat i a (enc (enc k b c i (invk a)) b) (enc a b-0 i ssn-0 c)))
      (send (enc n n i a (enc a b-0 i ssn-0 c) c)) (recv n)
      (send (enc "data" d k)))
    ((recv (enc n n i a (enc a b-0 i ssn-0 c) c)) (send n)))
  (label 19)
  (parent 17)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (c c) (i i) (ssn ssn) (d d) (k k))))
  (origs (n (1 1)) (d (1 3)) (k (0 0))))

(comment "Nothing left to do")

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
    (trace (recv (cat i a (enc (enc k b c i (invk a)) b) x))
      (send (enc y n i a x c)) (recv n) (send (enc "sorry" a b k)))
    (uniq-orig y n))
  (defrole ay
    (vars (a b c akey) (i y n d text) (k skey) (x mesg))
    (trace (recv (cat i a (enc (enc k b c i (invk a)) b) x))
      (send (enc y n i a x c)) (recv y) (send (enc "data" d k)))
    (uniq-orig y n d))
  (defrole sn
    (vars (a b c akey) (i ssn y n text))
    (trace (recv (enc y n i a (enc a b i ssn c) c)) (send n)))
  (defrole sy
    (vars (a b c akey) (i ssn y n text))
    (trace (recv (enc y n i a (enc a b i ssn c) c)) (send y)))
  (defrule src
    (forall ((z strd) (i ssn text) (k skey) (a b c akey))
      (implies
        (and (p "qn" z 2) (p "qn" "i" z i) (p "qn" "ssn" z ssn)
          (p "qn" "k" z k) (p "qn" "a" z a) (p "qn" "b" z b)
          (p "qn" "c" z c) (non (invk a)) (non (invk b)) (non (invk c))
          (uniq k))
        (exists ((z-0 strd) (x mesg))
          (and (p "an" z-0 4) (p "an" "x" z-0 x) (p "an" "i" z-0 i)
            (p "an" "k" z-0 k) (p "an" "a" z-0 a) (p "an" "b" z-0 b)
            (p "an" "c" z-0 c) (prec z 0 z-0 0) (prec z-0 3 z 1)))))))

(defskeleton main-ex-tgt-rule
  (vars (i ssn text) (k skey) (a b c akey))
  (defstrand qn 2 (i i) (ssn ssn) (k k) (a a) (b b) (c c))
  (non-orig (invk a) (invk b) (invk c))
  (uniq-orig k)
  (traces
    ((send (cat i a (enc (enc k b c i (invk a)) b) (enc a b i ssn c)))
      (recv (enc "sorry" a b k))))
  (label 20)
  (unrealized (0 1))
  (origs (k (0 0)))
  (comment "Not closed under rules"))

(defskeleton main-ex-tgt-rule
  (vars (x mesg) (ssn i y n text) (k skey) (a b c akey))
  (defstrand qn 2 (i i) (ssn ssn) (k k) (a a) (b b) (c c))
  (defstrand an 4 (x x) (i i) (y y) (n n) (k k) (a a) (b b) (c c))
  (precedes ((0 0) (1 0)) ((1 3) (0 1)))
  (non-orig (invk a) (invk b) (invk c))
  (uniq-orig y n k)
  (rule src)
  (traces
    ((send (cat i a (enc (enc k b c i (invk a)) b) (enc a b i ssn c)))
      (recv (enc "sorry" a b k)))
    ((recv (cat i a (enc (enc k b c i (invk a)) b) x))
      (send (enc y n i a x c)) (recv n) (send (enc "sorry" a b k))))
  (label 21)
  (parent 20)
  (unrealized (1 2))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton main-ex-tgt-rule
  (vars (ssn i y n ssn-0 text) (k skey) (a b c b-0 akey))
  (defstrand qn 2 (i i) (ssn ssn) (k k) (a a) (b b) (c c))
  (defstrand an 4 (x (enc a b-0 i ssn-0 c)) (i i) (y y) (n n) (k k)
    (a a) (b b) (c c))
  (defstrand sn 2 (i i) (ssn ssn-0) (y y) (n n) (a a) (b b-0) (c c))
  (precedes ((0 0) (1 0)) ((1 1) (2 0)) ((1 3) (0 1)) ((2 1) (1 2)))
  (non-orig (invk a) (invk b) (invk c))
  (uniq-orig y n k)
  (operation nonce-test (added-strand sn 2) n (1 2)
    (enc y n i a (enc a b-0 i ssn-0 c) c))
  (traces
    ((send (cat i a (enc (enc k b c i (invk a)) b) (enc a b i ssn c)))
      (recv (enc "sorry" a b k)))
    ((recv
       (cat i a (enc (enc k b c i (invk a)) b) (enc a b-0 i ssn-0 c)))
      (send (enc y n i a (enc a b-0 i ssn-0 c) c)) (recv n)
      (send (enc "sorry" a b k)))
    ((recv (enc y n i a (enc a b-0 i ssn-0 c) c)) (send n)))
  (label 22)
  (parent 21)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (c c) (i i) (ssn ssn) (k k))))
  (origs (y (1 1)) (n (1 1)) (k (0 0))))

(comment "Nothing left to do")
