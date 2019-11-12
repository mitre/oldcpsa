(herald "Otway-Rees Protocol"
  (comment
    "Version using variables of sort mesg, with ltk function emulated."))

(comment "CPSA 3.6.5")
(comment "All input read from fnof_or.scm")

(defprotocol or basic
  (defrole init
    (vars (a b s name) (na text) (ltkas k skey) (m text))
    (trace (init s) (send (cat m a b (enc na m a b ltkas)))
      (recv (cat m (enc na k ltkas))))
    (fn-of ("ltk" (ltkas (cat a s))) ("ltkinv" ((cat a s) ltkas))))
  (defrole resp
    (vars (a b s name) (nb text) (k ltkbs ltkas skey) (m text)
      (x y mesg))
    (trace (init (cat s ltkas)) (recv (cat m a b x))
      (send (cat m a b x (enc nb m a b ltkbs)))
      (recv (cat m y (enc nb k ltkbs))) (send y))
    (fn-of ("ltk" (ltkas (cat a s)) (ltkbs (cat b s)))
      ("ltkinv" ((cat a s) ltkas) ((cat b s) ltkbs))))
  (defrole serv
    (vars (a b s name) (na nb text) (k ltkas ltkbs skey) (m text))
    (trace (init s)
      (recv (cat m a b (enc na m a b ltkas) (enc nb m a b ltkbs)))
      (send (cat m (enc na k ltkas) (enc nb k ltkbs))))
    (uniq-orig k)
    (fn-of ("ltk" (ltkas (cat a s)) (ltkbs (cat b s)))
      ("ltkinv" ((cat a s) ltkas) ((cat b s) ltkbs)))))

(defskeleton or
  (vars (x y mesg) (nb m text) (s a b name) (ltkas ltkbs k skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a a) (b b) (s s) (k k)
    (ltkbs ltkbs) (ltkas ltkas))
  (fn-of ("ltk" (ltkas (cat a s)) (ltkbs (cat b s)))
    ("ltkinv" ((cat a s) ltkas) ((cat b s) ltkbs)))
  (non-orig ltkas ltkbs)
  (uniq-orig nb)
  (traces
    ((init (cat s ltkas)) (recv (cat m a b x))
      (send (cat m a b x (enc nb m a b ltkbs)))
      (recv (cat m y (enc nb k ltkbs)))))
  (label 0)
  (unrealized (0 3))
  (origs (nb (0 2)))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton or
  (vars (x y mesg) (nb m na m-0 text) (s a b a-0 name)
    (ltkas ltkbs k ltkas-0 skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a a) (b b) (s s) (k k)
    (ltkbs ltkbs) (ltkas ltkas))
  (defstrand serv 3 (na na) (nb nb) (m m-0) (a a-0) (b b) (s s) (k k)
    (ltkas ltkas-0) (ltkbs ltkbs))
  (precedes ((0 2) (1 1)) ((1 2) (0 3)))
  (fn-of
    ("ltk" (ltkas-0 (cat a-0 s)) (ltkbs (cat b s)) (ltkas (cat a s)))
    ("ltkinv" ((cat a-0 s) ltkas-0) ((cat b s) ltkbs)
      ((cat a s) ltkas)))
  (non-orig ltkas ltkbs)
  (uniq-orig nb k)
  (operation encryption-test (added-strand serv 3) (enc nb k ltkbs)
    (0 3))
  (traces
    ((init (cat s ltkas)) (recv (cat m a b x))
      (send (cat m a b x (enc nb m a b ltkbs)))
      (recv (cat m y (enc nb k ltkbs))))
    ((init s)
      (recv
        (cat m-0 a-0 b (enc na m-0 a-0 b ltkas-0)
          (enc nb m-0 a-0 b ltkbs)))
      (send (cat m-0 (enc na k ltkas-0) (enc nb k ltkbs)))))
  (label 1)
  (parent 0)
  (unrealized (1 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton or
  (vars (x y mesg) (nb m nb-0 m-0 text) (s a b b-0 name)
    (ltkas ltkbs k ltkbs-0 skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a a) (b b) (s s) (k k)
    (ltkbs ltkbs) (ltkas ltkas))
  (defstrand serv 3 (na nb) (nb nb-0) (m m-0) (a b) (b b-0) (s s) (k k)
    (ltkas ltkbs) (ltkbs ltkbs-0))
  (precedes ((0 2) (1 1)) ((1 2) (0 3)))
  (fn-of
    ("ltk" (ltkbs (cat b s)) (ltkbs-0 (cat b-0 s)) (ltkas (cat a s)))
    ("ltkinv" ((cat b s) ltkbs) ((cat b-0 s) ltkbs-0)
      ((cat a s) ltkas)))
  (non-orig ltkas ltkbs)
  (uniq-orig nb k)
  (operation encryption-test (added-strand serv 3) (enc nb k ltkbs)
    (0 3))
  (traces
    ((init (cat s ltkas)) (recv (cat m a b x))
      (send (cat m a b x (enc nb m a b ltkbs)))
      (recv (cat m y (enc nb k ltkbs))))
    ((init s)
      (recv
        (cat m-0 b b-0 (enc nb m-0 b b-0 ltkbs)
          (enc nb-0 m-0 b b-0 ltkbs-0)))
      (send (cat m-0 (enc nb k ltkbs) (enc nb-0 k ltkbs-0)))))
  (label 2)
  (parent 0)
  (unrealized (1 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton or
  (vars (x y mesg) (nb m na text) (s a b name) (ltkbs k ltkas skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a a) (b b) (s s) (k k)
    (ltkbs ltkbs) (ltkas ltkas))
  (defstrand serv 3 (na na) (nb nb) (m m) (a a) (b b) (s s) (k k)
    (ltkas ltkas) (ltkbs ltkbs))
  (precedes ((0 2) (1 1)) ((1 2) (0 3)))
  (fn-of ("ltk" (ltkas (cat a s)) (ltkbs (cat b s)))
    ("ltkinv" ((cat a s) ltkas) ((cat b s) ltkbs)))
  (non-orig ltkbs ltkas)
  (uniq-orig nb k)
  (operation encryption-test (displaced 2 0 resp 3)
    (enc nb m-0 a-0 b ltkbs) (1 1))
  (traces
    ((init (cat s ltkas)) (recv (cat m a b x))
      (send (cat m a b x (enc nb m a b ltkbs)))
      (recv (cat m y (enc nb k ltkbs))))
    ((init s)
      (recv (cat m a b (enc na m a b ltkas) (enc nb m a b ltkbs)))
      (send (cat m (enc na k ltkas) (enc nb k ltkbs)))))
  (label 3)
  (parent 1)
  (unrealized (1 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton or
  (vars (x y mesg) (nb m nb-0 text) (s a name) (k ltkbs skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a a) (b a) (s s) (k k)
    (ltkbs ltkbs) (ltkas ltkbs))
  (defstrand serv 3 (na nb) (nb nb-0) (m m) (a a) (b a) (s s) (k k)
    (ltkas ltkbs) (ltkbs ltkbs))
  (precedes ((0 2) (1 1)) ((1 2) (0 3)))
  (fn-of ("ltk" (ltkbs (cat a s))) ("ltkinv" ((cat a s) ltkbs)))
  (non-orig ltkbs)
  (uniq-orig nb k)
  (operation encryption-test (displaced 2 0 resp 3)
    (enc nb m-0 b b-0 ltkbs-0) (1 1))
  (traces
    ((init (cat s ltkbs)) (recv (cat m a a x))
      (send (cat m a a x (enc nb m a a ltkbs)))
      (recv (cat m y (enc nb k ltkbs))))
    ((init s)
      (recv (cat m a a (enc nb m a a ltkbs) (enc nb-0 m a a ltkbs)))
      (send (cat m (enc nb k ltkbs) (enc nb-0 k ltkbs)))))
  (label 4)
  (parent 2)
  (seen 6)
  (unrealized (1 1))
  (comment "3 in cohort - 2 not yet seen"))

(defskeleton or
  (vars (x y mesg) (nb m na text) (s a b name) (ltkbs k ltkas skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a a) (b b) (s s) (k k)
    (ltkbs ltkbs) (ltkas ltkas))
  (defstrand serv 3 (na na) (nb nb) (m m) (a a) (b b) (s s) (k k)
    (ltkas ltkas) (ltkbs ltkbs))
  (defstrand init 2 (na na) (m m) (a a) (b b) (s s) (ltkas ltkas))
  (precedes ((0 2) (1 1)) ((1 2) (0 3)) ((2 1) (1 1)))
  (fn-of ("ltk" (ltkas (cat a s)) (ltkbs (cat b s)))
    ("ltkinv" ((cat a s) ltkas) ((cat b s) ltkbs)))
  (non-orig ltkbs ltkas)
  (uniq-orig nb k)
  (operation encryption-test (added-strand init 2) (enc na m a b ltkas)
    (1 1))
  (traces
    ((init (cat s ltkas)) (recv (cat m a b x))
      (send (cat m a b x (enc nb m a b ltkbs)))
      (recv (cat m y (enc nb k ltkbs))))
    ((init s)
      (recv (cat m a b (enc na m a b ltkas) (enc nb m a b ltkbs)))
      (send (cat m (enc na k ltkas) (enc nb k ltkbs))))
    ((init s) (send (cat m a b (enc na m a b ltkas)))))
  (label 5)
  (parent 3)
  (unrealized)
  (shape)
  (maps
    ((0)
      ((nb nb) (s s) (a a) (b b) (ltkas ltkas) (ltkbs ltkbs) (k k) (m m)
        (x x) (y y))))
  (origs (k (1 2)) (nb (0 2))))

(defskeleton or
  (vars (x y mesg) (nb m text) (s b name) (ltkbs k skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a b) (b b) (s s) (k k)
    (ltkbs ltkbs) (ltkas ltkbs))
  (defstrand serv 3 (na nb) (nb nb) (m m) (a b) (b b) (s s) (k k)
    (ltkas ltkbs) (ltkbs ltkbs))
  (precedes ((0 2) (1 1)) ((1 2) (0 3)))
  (fn-of ("ltk" (ltkbs (cat b s))) ("ltkinv" ((cat b s) ltkbs)))
  (non-orig ltkbs)
  (uniq-orig nb k)
  (operation encryption-test (displaced 2 0 resp 3) (enc na m a b ltkas)
    (1 1))
  (traces
    ((init (cat s ltkbs)) (recv (cat m b b x))
      (send (cat m b b x (enc nb m b b ltkbs)))
      (recv (cat m y (enc nb k ltkbs))))
    ((init s)
      (recv (cat m b b (enc nb m b b ltkbs) (enc nb m b b ltkbs)))
      (send (cat m (enc nb k ltkbs) (enc nb k ltkbs)))))
  (label 6)
  (parent 3)
  (unrealized)
  (shape)
  (maps
    ((0)
      ((nb nb) (s s) (a b) (b b) (ltkas ltkbs) (ltkbs ltkbs) (k k) (m m)
        (x x) (y y))))
  (origs (k (1 2)) (nb (0 2))))

(defskeleton or
  (vars (x y x-0 mesg) (nb m na text) (s a name) (ltkbs k skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a a) (b a) (s s) (k k)
    (ltkbs ltkbs) (ltkas ltkbs))
  (defstrand serv 3 (na na) (nb nb) (m m) (a a) (b a) (s s) (k k)
    (ltkas ltkbs) (ltkbs ltkbs))
  (defstrand resp 3 (x x-0) (nb na) (m m) (a a) (b a) (s s)
    (ltkbs ltkbs) (ltkas ltkbs))
  (precedes ((0 2) (1 1)) ((1 2) (0 3)) ((2 2) (1 1)))
  (fn-of ("ltk" (ltkbs (cat a s))) ("ltkinv" ((cat a s) ltkbs)))
  (non-orig ltkbs)
  (uniq-orig nb k)
  (operation encryption-test (added-strand resp 3) (enc na m a a ltkbs)
    (1 1))
  (traces
    ((init (cat s ltkbs)) (recv (cat m a a x))
      (send (cat m a a x (enc nb m a a ltkbs)))
      (recv (cat m y (enc nb k ltkbs))))
    ((init s)
      (recv (cat m a a (enc na m a a ltkbs) (enc nb m a a ltkbs)))
      (send (cat m (enc na k ltkbs) (enc nb k ltkbs))))
    ((init (cat s ltkbs)) (recv (cat m a a x-0))
      (send (cat m a a x-0 (enc na m a a ltkbs)))))
  (label 7)
  (parent 3)
  (unrealized)
  (shape)
  (maps
    ((0)
      ((nb nb) (s s) (a a) (b a) (ltkas ltkbs) (ltkbs ltkbs) (k k) (m m)
        (x x) (y y))))
  (origs (k (1 2)) (nb (0 2))))

(defskeleton or
  (vars (x y mesg) (nb m nb-0 text) (s a name) (k ltkbs skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a a) (b a) (s s) (k k)
    (ltkbs ltkbs) (ltkas ltkbs))
  (defstrand serv 3 (na nb) (nb nb-0) (m m) (a a) (b a) (s s) (k k)
    (ltkas ltkbs) (ltkbs ltkbs))
  (defstrand init 2 (na nb-0) (m m) (a a) (b a) (s s) (ltkas ltkbs))
  (precedes ((0 2) (1 1)) ((1 2) (0 3)) ((2 1) (1 1)))
  (fn-of ("ltk" (ltkbs (cat a s))) ("ltkinv" ((cat a s) ltkbs)))
  (non-orig ltkbs)
  (uniq-orig nb k)
  (operation encryption-test (added-strand init 2)
    (enc nb-0 m a a ltkbs) (1 1))
  (traces
    ((init (cat s ltkbs)) (recv (cat m a a x))
      (send (cat m a a x (enc nb m a a ltkbs)))
      (recv (cat m y (enc nb k ltkbs))))
    ((init s)
      (recv (cat m a a (enc nb m a a ltkbs) (enc nb-0 m a a ltkbs)))
      (send (cat m (enc nb k ltkbs) (enc nb-0 k ltkbs))))
    ((init s) (send (cat m a a (enc nb-0 m a a ltkbs)))))
  (label 8)
  (parent 4)
  (unrealized)
  (shape)
  (maps
    ((0)
      ((nb nb) (s s) (a a) (b a) (ltkas ltkbs) (ltkbs ltkbs) (k k) (m m)
        (x x) (y y))))
  (origs (k (1 2)) (nb (0 2))))

(defskeleton or
  (vars (x y x-0 mesg) (nb m nb-0 text) (s a name) (k ltkbs skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a a) (b a) (s s) (k k)
    (ltkbs ltkbs) (ltkas ltkbs))
  (defstrand serv 3 (na nb) (nb nb-0) (m m) (a a) (b a) (s s) (k k)
    (ltkas ltkbs) (ltkbs ltkbs))
  (defstrand resp 3 (x x-0) (nb nb-0) (m m) (a a) (b a) (s s)
    (ltkbs ltkbs) (ltkas ltkbs))
  (precedes ((0 2) (1 1)) ((1 2) (0 3)) ((2 2) (1 1)))
  (fn-of ("ltk" (ltkbs (cat a s))) ("ltkinv" ((cat a s) ltkbs)))
  (non-orig ltkbs)
  (uniq-orig nb k)
  (operation encryption-test (added-strand resp 3)
    (enc nb-0 m a a ltkbs) (1 1))
  (traces
    ((init (cat s ltkbs)) (recv (cat m a a x))
      (send (cat m a a x (enc nb m a a ltkbs)))
      (recv (cat m y (enc nb k ltkbs))))
    ((init s)
      (recv (cat m a a (enc nb m a a ltkbs) (enc nb-0 m a a ltkbs)))
      (send (cat m (enc nb k ltkbs) (enc nb-0 k ltkbs))))
    ((init (cat s ltkbs)) (recv (cat m a a x-0))
      (send (cat m a a x-0 (enc nb-0 m a a ltkbs)))))
  (label 9)
  (parent 4)
  (unrealized)
  (shape)
  (maps
    ((0)
      ((nb nb) (s s) (a a) (b a) (ltkas ltkbs) (ltkbs ltkbs) (k k) (m m)
        (x x) (y y))))
  (origs (k (1 2)) (nb (0 2))))

(comment "Nothing left to do")

(defprotocol or2 basic
  (defrole init
    (vars (a b s name) (na text) (ltkas k skey) (m text))
    (trace (init s) (send (cat m a b (enc na m a b ltkas)))
      (recv (cat m (enc na k ltkas))))
    (fn-of ("ltk" (ltkas (cat a s)))))
  (defrole resp
    (vars (a b s name) (nb text) (k ltkbs ltkas skey) (m text)
      (x y mesg))
    (trace (init (cat s ltkas)) (recv (cat m a b x))
      (send (cat m a b x (enc nb m a b ltkbs)))
      (recv (cat m y (enc nb k ltkbs))) (send y))
    (fn-of ("ltk" (ltkas (cat a s)) (ltkbs (cat b s)))))
  (defrole serv
    (vars (a b s name) (na nb text) (k ltkas ltkbs skey) (m text))
    (trace (init s)
      (recv (cat m a b (enc na m a b ltkas) (enc nb m a b ltkbs)))
      (send (cat m (enc na k ltkas) (enc nb k ltkbs))))
    (uniq-orig k)
    (fn-of ("ltk" (ltkas (cat a s)) (ltkbs (cat b s))))))

(defskeleton or2
  (vars (x y mesg) (nb m text) (s a b name) (ltkas ltkbs k skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a a) (b b) (s s) (k k)
    (ltkbs ltkbs) (ltkas ltkas))
  (fn-of ("ltk" (ltkas (cat a s)) (ltkbs (cat b s))))
  (non-orig ltkas ltkbs)
  (uniq-orig nb)
  (traces
    ((init (cat s ltkas)) (recv (cat m a b x))
      (send (cat m a b x (enc nb m a b ltkbs)))
      (recv (cat m y (enc nb k ltkbs)))))
  (label 10)
  (unrealized (0 3))
  (origs (nb (0 2)))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton or2
  (vars (x y mesg) (nb m na m-0 text) (s a b a-0 b-0 s-0 name)
    (ltkas ltkbs k ltkas-0 skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a a) (b b) (s s) (k k)
    (ltkbs ltkbs) (ltkas ltkas))
  (defstrand serv 3 (na na) (nb nb) (m m-0) (a a-0) (b b-0) (s s-0)
    (k k) (ltkas ltkas-0) (ltkbs ltkbs))
  (precedes ((0 2) (1 1)) ((1 2) (0 3)))
  (fn-of
    ("ltk" (ltkas-0 (cat a-0 s-0)) (ltkbs (cat b-0 s-0))
      (ltkas (cat a s)) (ltkbs (cat b s))))
  (non-orig ltkas ltkbs)
  (uniq-orig nb k)
  (operation encryption-test (added-strand serv 3) (enc nb k ltkbs)
    (0 3))
  (traces
    ((init (cat s ltkas)) (recv (cat m a b x))
      (send (cat m a b x (enc nb m a b ltkbs)))
      (recv (cat m y (enc nb k ltkbs))))
    ((init s-0)
      (recv
        (cat m-0 a-0 b-0 (enc na m-0 a-0 b-0 ltkas-0)
          (enc nb m-0 a-0 b-0 ltkbs)))
      (send (cat m-0 (enc na k ltkas-0) (enc nb k ltkbs)))))
  (label 11)
  (parent 10)
  (unrealized (1 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton or2
  (vars (x y mesg) (nb m nb-0 m-0 text) (s a b a-0 b-0 s-0 name)
    (ltkas ltkbs k ltkbs-0 skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a a) (b b) (s s) (k k)
    (ltkbs ltkbs) (ltkas ltkas))
  (defstrand serv 3 (na nb) (nb nb-0) (m m-0) (a a-0) (b b-0) (s s-0)
    (k k) (ltkas ltkbs) (ltkbs ltkbs-0))
  (precedes ((0 2) (1 1)) ((1 2) (0 3)))
  (fn-of
    ("ltk" (ltkbs (cat a-0 s-0)) (ltkbs-0 (cat b-0 s-0))
      (ltkas (cat a s)) (ltkbs (cat b s))))
  (non-orig ltkas ltkbs)
  (uniq-orig nb k)
  (operation encryption-test (added-strand serv 3) (enc nb k ltkbs)
    (0 3))
  (traces
    ((init (cat s ltkas)) (recv (cat m a b x))
      (send (cat m a b x (enc nb m a b ltkbs)))
      (recv (cat m y (enc nb k ltkbs))))
    ((init s-0)
      (recv
        (cat m-0 a-0 b-0 (enc nb m-0 a-0 b-0 ltkbs)
          (enc nb-0 m-0 a-0 b-0 ltkbs-0)))
      (send (cat m-0 (enc nb k ltkbs) (enc nb-0 k ltkbs-0)))))
  (label 12)
  (parent 10)
  (unrealized (1 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton or2
  (vars (x y mesg) (nb m na text) (s a b s-0 name)
    (ltkas ltkbs k ltkas-0 skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a a) (b b) (s s) (k k)
    (ltkbs ltkbs) (ltkas ltkas))
  (defstrand serv 3 (na na) (nb nb) (m m) (a a) (b b) (s s-0) (k k)
    (ltkas ltkas-0) (ltkbs ltkbs))
  (precedes ((0 2) (1 1)) ((1 2) (0 3)))
  (fn-of
    ("ltk" (ltkas (cat a s)) (ltkbs (cat b s)) (ltkas-0 (cat a s-0))
      (ltkbs (cat b s-0))))
  (non-orig ltkas ltkbs)
  (uniq-orig nb k)
  (operation encryption-test (displaced 2 0 resp 3)
    (enc nb m-0 a-0 b-0 ltkbs) (1 1))
  (traces
    ((init (cat s ltkas)) (recv (cat m a b x))
      (send (cat m a b x (enc nb m a b ltkbs)))
      (recv (cat m y (enc nb k ltkbs))))
    ((init s-0)
      (recv (cat m a b (enc na m a b ltkas-0) (enc nb m a b ltkbs)))
      (send (cat m (enc na k ltkas-0) (enc nb k ltkbs)))))
  (label 13)
  (parent 11)
  (unrealized)
  (shape)
  (maps
    ((0)
      ((nb nb) (s s) (a a) (b b) (ltkas ltkas) (ltkbs ltkbs) (k k) (m m)
        (x x) (y y))))
  (origs (k (1 2)) (nb (0 2))))

(defskeleton or2
  (vars (x y mesg) (nb m nb-0 text) (s a b s-0 name)
    (ltkas ltkbs k ltkbs-0 skey))
  (defstrand resp 4 (x x) (y y) (nb nb) (m m) (a a) (b b) (s s) (k k)
    (ltkbs ltkbs) (ltkas ltkas))
  (defstrand serv 3 (na nb) (nb nb-0) (m m) (a a) (b b) (s s-0) (k k)
    (ltkas ltkbs) (ltkbs ltkbs-0))
  (precedes ((0 2) (1 1)) ((1 2) (0 3)))
  (fn-of
    ("ltk" (ltkas (cat a s)) (ltkbs (cat b s)) (ltkbs (cat a s-0))
      (ltkbs-0 (cat b s-0))))
  (non-orig ltkas ltkbs)
  (uniq-orig nb k)
  (operation encryption-test (displaced 2 0 resp 3)
    (enc nb m-0 a-0 b-0 ltkbs) (1 1))
  (traces
    ((init (cat s ltkas)) (recv (cat m a b x))
      (send (cat m a b x (enc nb m a b ltkbs)))
      (recv (cat m y (enc nb k ltkbs))))
    ((init s-0)
      (recv (cat m a b (enc nb m a b ltkbs) (enc nb-0 m a b ltkbs-0)))
      (send (cat m (enc nb k ltkbs) (enc nb-0 k ltkbs-0)))))
  (label 14)
  (parent 12)
  (unrealized)
  (shape)
  (maps
    ((0)
      ((nb nb) (s s) (a a) (b b) (ltkas ltkas) (ltkbs ltkbs) (k k) (m m)
        (x x) (y y))))
  (origs (k (1 2)) (nb (0 2))))

(comment "Nothing left to do")
