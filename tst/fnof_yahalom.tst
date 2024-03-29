(herald
  "Yahalom Protocol with Forwarding Removed, using fnof to emulate ltk function"
  (bound 12))

(comment "CPSA 3.6.11")
(comment "All input read from tst/fnof_yahalom.scm")

(defprotocol yahalom basic
  (defrole init
    (vars (a b c name) (n-a n-b text) (ltkac k skey))
    (trace (init (cat c ltkac)) (send (cat a n-a))
      (recv (enc b k n-a n-b ltkac)) (send (enc n-b k)))
    (fn-of ("ltk" (ltkac (cat a c))) ("ltk-inv" ((cat a c) ltkac))))
  (defrole resp
    (vars (b a c name) (n-a n-b text) (ltkac ltkbc k skey))
    (trace (init (cat c ltkac)) (recv (cat a n-a))
      (send (cat b (enc a n-a n-b ltkbc))) (recv (enc a k ltkbc))
      (recv (enc n-b k)))
    (fn-of ("ltk" (ltkac (cat a c)) (ltkbc (cat b c)))
      ("ltk-inv" ((cat a c) ltkac) ((cat b c) ltkbc))))
  (defrole serv
    (vars (c a b name) (n-a n-b text) (ltkac ltkbc k skey))
    (trace (init c) (recv (cat b (enc a n-a n-b ltkbc)))
      (send (enc b k n-a n-b ltkac)) (send (enc a k ltkbc)))
    (uniq-orig k)
    (fn-of ("ltk" (ltkac (cat a c)) (ltkbc (cat b c)))
      ("ltk-inv" ((cat a c) ltkac) ((cat b c) ltkbc)))))

(defskeleton yahalom
  (vars (n-b n-a text) (a b c name) (ltkac ltkbc k skey))
  (defstrand resp 4 (n-a n-a) (n-b n-b) (b b) (a a) (c c) (ltkac ltkac)
    (ltkbc ltkbc) (k k))
  (fn-of ("ltk" (ltkac (cat a c)) (ltkbc (cat b c)))
    ("ltk-inv" ((cat a c) ltkac) ((cat b c) ltkbc)))
  (non-orig ltkac ltkbc)
  (uniq-orig n-b)
  (traces
    ((init (cat c ltkac)) (recv (cat a n-a))
      (send (cat b (enc a n-a n-b ltkbc))) (recv (enc a k ltkbc))))
  (label 0)
  (unrealized (0 3))
  (origs (n-b (0 2)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton yahalom
  (vars (n-b n-a n-a-0 n-b-0 text) (a b c name) (ltkac ltkbc k skey))
  (defstrand resp 4 (n-a n-a) (n-b n-b) (b b) (a a) (c c) (ltkac ltkac)
    (ltkbc ltkbc) (k k))
  (defstrand serv 4 (n-a n-a-0) (n-b n-b-0) (c c) (a a) (b b)
    (ltkac ltkac) (ltkbc ltkbc) (k k))
  (precedes ((1 3) (0 3)))
  (fn-of ("ltk" (ltkbc (cat b c)) (ltkac (cat a c)))
    ("ltk-inv" ((cat b c) ltkbc) ((cat a c) ltkac)))
  (non-orig ltkac ltkbc)
  (uniq-orig n-b k)
  (operation encryption-test (added-strand serv 4) (enc a k ltkbc)
    (0 3))
  (traces
    ((init (cat c ltkac)) (recv (cat a n-a))
      (send (cat b (enc a n-a n-b ltkbc))) (recv (enc a k ltkbc)))
    ((init c) (recv (cat b (enc a n-a-0 n-b-0 ltkbc)))
      (send (enc b k n-a-0 n-b-0 ltkac)) (send (enc a k ltkbc))))
  (label 1)
  (parent 0)
  (unrealized (1 1))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton yahalom
  (vars (n-b n-a text) (a b c name) (ltkac ltkbc k skey))
  (defstrand resp 4 (n-a n-a) (n-b n-b) (b b) (a a) (c c) (ltkac ltkac)
    (ltkbc ltkbc) (k k))
  (defstrand serv 4 (n-a n-a) (n-b n-b) (c c) (a a) (b b) (ltkac ltkac)
    (ltkbc ltkbc) (k k))
  (precedes ((0 2) (1 1)) ((1 3) (0 3)))
  (fn-of ("ltk" (ltkac (cat a c)) (ltkbc (cat b c)))
    ("ltk-inv" ((cat a c) ltkac) ((cat b c) ltkbc)))
  (non-orig ltkac ltkbc)
  (uniq-orig n-b k)
  (operation encryption-test (displaced 2 0 resp 3)
    (enc a n-a-0 n-b-0 ltkbc) (1 1))
  (traces
    ((init (cat c ltkac)) (recv (cat a n-a))
      (send (cat b (enc a n-a n-b ltkbc))) (recv (enc a k ltkbc)))
    ((init c) (recv (cat b (enc a n-a n-b ltkbc)))
      (send (enc b k n-a n-b ltkac)) (send (enc a k ltkbc))))
  (label 2)
  (parent 1)
  (unrealized)
  (shape)
  (maps
    ((0)
      ((a a) (b b) (c c) (n-b n-b) (ltkac ltkac) (ltkbc ltkbc) (n-a n-a)
        (k k))))
  (origs (k (1 2)) (n-b (0 2))))

(defskeleton yahalom
  (vars (n-b n-a n-a-0 n-b-0 text) (a b c name) (ltkac ltkbc k skey))
  (defstrand resp 4 (n-a n-a) (n-b n-b) (b b) (a a) (c c) (ltkac ltkac)
    (ltkbc ltkbc) (k k))
  (defstrand serv 4 (n-a n-a-0) (n-b n-b-0) (c c) (a a) (b b)
    (ltkac ltkac) (ltkbc ltkbc) (k k))
  (defstrand resp 3 (n-a n-a-0) (n-b n-b-0) (b b) (a a) (c c)
    (ltkac ltkac) (ltkbc ltkbc))
  (precedes ((1 3) (0 3)) ((2 2) (1 1)))
  (fn-of ("ltk" (ltkac (cat a c)) (ltkbc (cat b c)))
    ("ltk-inv" ((cat a c) ltkac) ((cat b c) ltkbc)))
  (non-orig ltkac ltkbc)
  (uniq-orig n-b k)
  (operation encryption-test (added-strand resp 3)
    (enc a n-a-0 n-b-0 ltkbc) (1 1))
  (traces
    ((init (cat c ltkac)) (recv (cat a n-a))
      (send (cat b (enc a n-a n-b ltkbc))) (recv (enc a k ltkbc)))
    ((init c) (recv (cat b (enc a n-a-0 n-b-0 ltkbc)))
      (send (enc b k n-a-0 n-b-0 ltkac)) (send (enc a k ltkbc)))
    ((init (cat c ltkac)) (recv (cat a n-a-0))
      (send (cat b (enc a n-a-0 n-b-0 ltkbc)))))
  (label 3)
  (parent 1)
  (unrealized)
  (shape)
  (maps
    ((0)
      ((a a) (b b) (c c) (n-b n-b) (ltkac ltkac) (ltkbc ltkbc) (n-a n-a)
        (k k))))
  (origs (k (1 2)) (n-b (0 2))))

(comment "Nothing left to do")

(defprotocol yahalom basic
  (defrole init
    (vars (a b c name) (n-a n-b text) (ltkac k skey))
    (trace (init (cat c ltkac)) (send (cat a n-a))
      (recv (enc b k n-a n-b ltkac)) (send (enc n-b k)))
    (fn-of ("ltk" (ltkac (cat a c))) ("ltk-inv" ((cat a c) ltkac))))
  (defrole resp
    (vars (b a c name) (n-a n-b text) (ltkac ltkbc k skey))
    (trace (init (cat c ltkac)) (recv (cat a n-a))
      (send (cat b (enc a n-a n-b ltkbc))) (recv (enc a k ltkbc))
      (recv (enc n-b k)))
    (fn-of ("ltk" (ltkac (cat a c)) (ltkbc (cat b c)))
      ("ltk-inv" ((cat a c) ltkac) ((cat b c) ltkbc))))
  (defrole serv
    (vars (c a b name) (n-a n-b text) (ltkac ltkbc k skey))
    (trace (init c) (recv (cat b (enc a n-a n-b ltkbc)))
      (send (enc b k n-a n-b ltkac)) (send (enc a k ltkbc)))
    (uniq-orig k)
    (fn-of ("ltk" (ltkac (cat a c)) (ltkbc (cat b c)))
      ("ltk-inv" ((cat a c) ltkac) ((cat b c) ltkbc)))))

(defskeleton yahalom
  (vars (n-b n-a text) (a b c name) (ltkac ltkbc k skey))
  (defstrand resp 4 (n-a n-a) (n-b n-b) (b b) (a a) (c c) (ltkac ltkac)
    (ltkbc ltkbc) (k k))
  (deflistener k)
  (fn-of ("ltk" (ltkac (cat a c)) (ltkbc (cat b c)))
    ("ltk-inv" ((cat a c) ltkac) ((cat b c) ltkbc)))
  (non-orig ltkac ltkbc)
  (uniq-orig n-b)
  (traces
    ((init (cat c ltkac)) (recv (cat a n-a))
      (send (cat b (enc a n-a n-b ltkbc))) (recv (enc a k ltkbc)))
    ((recv k) (send k)))
  (label 4)
  (unrealized (0 3))
  (origs (n-b (0 2)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton yahalom
  (vars (n-b n-a n-a-0 n-b-0 text) (a b c name) (ltkac ltkbc k skey))
  (defstrand resp 4 (n-a n-a) (n-b n-b) (b b) (a a) (c c) (ltkac ltkac)
    (ltkbc ltkbc) (k k))
  (deflistener k)
  (defstrand serv 4 (n-a n-a-0) (n-b n-b-0) (c c) (a a) (b b)
    (ltkac ltkac) (ltkbc ltkbc) (k k))
  (precedes ((2 2) (1 0)) ((2 3) (0 3)))
  (fn-of ("ltk" (ltkbc (cat b c)) (ltkac (cat a c)))
    ("ltk-inv" ((cat b c) ltkbc) ((cat a c) ltkac)))
  (non-orig ltkac ltkbc)
  (uniq-orig n-b k)
  (operation encryption-test (added-strand serv 4) (enc a k ltkbc)
    (0 3))
  (traces
    ((init (cat c ltkac)) (recv (cat a n-a))
      (send (cat b (enc a n-a n-b ltkbc))) (recv (enc a k ltkbc)))
    ((recv k) (send k))
    ((init c) (recv (cat b (enc a n-a-0 n-b-0 ltkbc)))
      (send (enc b k n-a-0 n-b-0 ltkac)) (send (enc a k ltkbc))))
  (label 5)
  (parent 4)
  (unrealized (1 0) (2 1))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton yahalom
  (vars (n-b n-a text) (a b c name) (ltkac ltkbc k skey))
  (defstrand resp 4 (n-a n-a) (n-b n-b) (b b) (a a) (c c) (ltkac ltkac)
    (ltkbc ltkbc) (k k))
  (deflistener k)
  (defstrand serv 4 (n-a n-a) (n-b n-b) (c c) (a a) (b b) (ltkac ltkac)
    (ltkbc ltkbc) (k k))
  (precedes ((0 2) (2 1)) ((2 2) (1 0)) ((2 3) (0 3)))
  (fn-of ("ltk" (ltkac (cat a c)) (ltkbc (cat b c)))
    ("ltk-inv" ((cat a c) ltkac) ((cat b c) ltkbc)))
  (non-orig ltkac ltkbc)
  (uniq-orig n-b k)
  (operation encryption-test (displaced 3 0 resp 3)
    (enc a n-a-0 n-b-0 ltkbc) (2 1))
  (traces
    ((init (cat c ltkac)) (recv (cat a n-a))
      (send (cat b (enc a n-a n-b ltkbc))) (recv (enc a k ltkbc)))
    ((recv k) (send k))
    ((init c) (recv (cat b (enc a n-a n-b ltkbc)))
      (send (enc b k n-a n-b ltkac)) (send (enc a k ltkbc))))
  (label 6)
  (parent 5)
  (unrealized (1 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton yahalom
  (vars (n-b n-a n-a-0 n-b-0 text) (a b c name) (ltkac ltkbc k skey))
  (defstrand resp 4 (n-a n-a) (n-b n-b) (b b) (a a) (c c) (ltkac ltkac)
    (ltkbc ltkbc) (k k))
  (deflistener k)
  (defstrand serv 4 (n-a n-a-0) (n-b n-b-0) (c c) (a a) (b b)
    (ltkac ltkac) (ltkbc ltkbc) (k k))
  (defstrand resp 3 (n-a n-a-0) (n-b n-b-0) (b b) (a a) (c c)
    (ltkac ltkac) (ltkbc ltkbc))
  (precedes ((2 2) (1 0)) ((2 3) (0 3)) ((3 2) (2 1)))
  (fn-of ("ltk" (ltkac (cat a c)) (ltkbc (cat b c)))
    ("ltk-inv" ((cat a c) ltkac) ((cat b c) ltkbc)))
  (non-orig ltkac ltkbc)
  (uniq-orig n-b k)
  (operation encryption-test (added-strand resp 3)
    (enc a n-a-0 n-b-0 ltkbc) (2 1))
  (traces
    ((init (cat c ltkac)) (recv (cat a n-a))
      (send (cat b (enc a n-a n-b ltkbc))) (recv (enc a k ltkbc)))
    ((recv k) (send k))
    ((init c) (recv (cat b (enc a n-a-0 n-b-0 ltkbc)))
      (send (enc b k n-a-0 n-b-0 ltkac)) (send (enc a k ltkbc)))
    ((init (cat c ltkac)) (recv (cat a n-a-0))
      (send (cat b (enc a n-a-0 n-b-0 ltkbc)))))
  (label 7)
  (parent 5)
  (unrealized (1 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton yahalom
  (vars (n-b n-a text) (a b c name) (ltkac ltkbc k skey))
  (defstrand resp 4 (n-a n-a) (n-b n-b) (b b) (a a) (c c) (ltkac ltkac)
    (ltkbc ltkbc) (k k))
  (deflistener k)
  (defstrand serv 4 (n-a n-a) (n-b n-b) (c c) (a a) (b b) (ltkac ltkac)
    (ltkbc ltkbc) (k k))
  (precedes ((0 2) (2 1)) ((2 3) (0 3)) ((2 3) (1 0)))
  (fn-of ("ltk" (ltkbc (cat b c)) (ltkac (cat a c)))
    ("ltk-inv" ((cat b c) ltkbc) ((cat a c) ltkac)))
  (non-orig ltkac ltkbc)
  (uniq-orig n-b k)
  (operation nonce-test (displaced 3 2 serv 4) k (1 0)
    (enc b k n-a n-b ltkac))
  (traces
    ((init (cat c ltkac)) (recv (cat a n-a))
      (send (cat b (enc a n-a n-b ltkbc))) (recv (enc a k ltkbc)))
    ((recv k) (send k))
    ((init c) (recv (cat b (enc a n-a n-b ltkbc)))
      (send (enc b k n-a n-b ltkac)) (send (enc a k ltkbc))))
  (label 8)
  (parent 6)
  (unrealized (1 0))
  (dead)
  (comment "empty cohort"))

(defskeleton yahalom
  (vars (n-b n-a n-a-0 n-b-0 text) (a b c name) (ltkac ltkbc k skey))
  (defstrand resp 4 (n-a n-a) (n-b n-b) (b b) (a a) (c c) (ltkac ltkac)
    (ltkbc ltkbc) (k k))
  (deflistener k)
  (defstrand serv 4 (n-a n-a-0) (n-b n-b-0) (c c) (a a) (b b)
    (ltkac ltkac) (ltkbc ltkbc) (k k))
  (defstrand resp 3 (n-a n-a-0) (n-b n-b-0) (b b) (a a) (c c)
    (ltkac ltkac) (ltkbc ltkbc))
  (precedes ((2 3) (0 3)) ((2 3) (1 0)) ((3 2) (2 1)))
  (fn-of ("ltk" (ltkbc (cat b c)) (ltkac (cat a c)))
    ("ltk-inv" ((cat b c) ltkbc) ((cat a c) ltkac)))
  (non-orig ltkac ltkbc)
  (uniq-orig n-b k)
  (operation nonce-test (displaced 4 2 serv 4) k (1 0)
    (enc b k n-a-0 n-b-0 ltkac))
  (traces
    ((init (cat c ltkac)) (recv (cat a n-a))
      (send (cat b (enc a n-a n-b ltkbc))) (recv (enc a k ltkbc)))
    ((recv k) (send k))
    ((init c) (recv (cat b (enc a n-a-0 n-b-0 ltkbc)))
      (send (enc b k n-a-0 n-b-0 ltkac)) (send (enc a k ltkbc)))
    ((init (cat c ltkac)) (recv (cat a n-a-0))
      (send (cat b (enc a n-a-0 n-b-0 ltkbc)))))
  (label 9)
  (parent 7)
  (unrealized (1 0))
  (dead)
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol yahalom2 basic
  (defrole init
    (vars (a b name) (n-a n-b text) (ltkac ltkbc k skey))
    (trace (send (cat a n-a (hash ltkbc)))
      (recv (enc b k n-a n-b ltkac)) (send (enc n-b k))))
  (defrole resp
    (vars (b a c name) (n-a n-b text) (ltkac ltkbc k skey))
    (trace (recv (cat a n-a))
      (send (cat b (enc a n-a n-b ltkbc) (hash c ltkac)))
      (recv (enc a k ltkbc)) (recv (enc n-b k)))
    (fn-of ("ltk" (ltkac (cat a c)) (ltkbc (cat b c)))))
  (defrole serv
    (vars (c a b name) (n-a n-b text) (ltkac ltkbc k skey))
    (trace (recv (cat b (enc a n-a n-b ltkbc)))
      (send (cat (hash c) (enc b k n-a n-b ltkac)))
      (send (enc a k ltkbc)))
    (uniq-orig k)
    (fn-of ("ltk" (ltkac (cat a c)) (ltkbc (cat b c))))))

(defskeleton yahalom2
  (vars (n-b n-a text) (a b c name) (ltkac ltkbc k skey))
  (defstrand resp 4 (n-a n-a) (n-b n-b) (b b) (a a) (c c) (ltkac ltkac)
    (ltkbc ltkbc) (k k))
  (fn-of ("ltk" (ltkac (cat a c)) (ltkbc (cat b c))))
  (non-orig ltkac ltkbc)
  (uniq-orig n-b)
  (traces
    ((recv (cat a n-a))
      (send (cat b (enc a n-a n-b ltkbc) (hash c ltkac)))
      (recv (enc a k ltkbc)) (recv (enc n-b k))))
  (label 10)
  (unrealized (0 2) (0 3))
  (origs (n-b (0 1)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton yahalom2
  (vars (n-b n-a n-a-0 n-b-0 text) (a b c c-0 b-0 name)
    (ltkac ltkbc k ltkac-0 skey))
  (defstrand resp 4 (n-a n-a) (n-b n-b) (b b) (a a) (c c) (ltkac ltkac)
    (ltkbc ltkbc) (k k))
  (defstrand serv 3 (n-a n-a-0) (n-b n-b-0) (c c-0) (a a) (b b-0)
    (ltkac ltkac-0) (ltkbc ltkbc) (k k))
  (precedes ((1 2) (0 2)))
  (fn-of
    ("ltk" (ltkac-0 (cat a c-0)) (ltkbc (cat b-0 c-0)) (ltkac (cat a c))
      (ltkbc (cat b c))))
  (non-orig ltkac ltkbc)
  (uniq-orig n-b k)
  (operation encryption-test (added-strand serv 3) (enc a k ltkbc)
    (0 2))
  (traces
    ((recv (cat a n-a))
      (send (cat b (enc a n-a n-b ltkbc) (hash c ltkac)))
      (recv (enc a k ltkbc)) (recv (enc n-b k)))
    ((recv (cat b-0 (enc a n-a-0 n-b-0 ltkbc)))
      (send (cat (hash c-0) (enc b-0 k n-a-0 n-b-0 ltkac-0)))
      (send (enc a k ltkbc))))
  (label 11)
  (parent 10)
  (unrealized (0 3) (1 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton yahalom2
  (vars (n-b n-a text) (a b c c-0 b-0 name)
    (ltkac ltkbc k ltkac-0 skey))
  (defstrand resp 4 (n-a n-a) (n-b n-b) (b b) (a a) (c c) (ltkac ltkac)
    (ltkbc ltkbc) (k k))
  (defstrand serv 3 (n-a n-a) (n-b n-b) (c c-0) (a a) (b b-0)
    (ltkac ltkac-0) (ltkbc ltkbc) (k k))
  (precedes ((0 1) (1 0)) ((1 2) (0 2)))
  (fn-of
    ("ltk" (ltkac (cat a c)) (ltkbc (cat b c)) (ltkac-0 (cat a c-0))
      (ltkbc (cat b-0 c-0))))
  (non-orig ltkac ltkbc)
  (uniq-orig n-b k)
  (operation encryption-test (displaced 2 0 resp 2)
    (enc a n-a-0 n-b-0 ltkbc) (1 0))
  (traces
    ((recv (cat a n-a))
      (send (cat b (enc a n-a n-b ltkbc) (hash c ltkac)))
      (recv (enc a k ltkbc)) (recv (enc n-b k)))
    ((recv (cat b-0 (enc a n-a n-b ltkbc)))
      (send (cat (hash c-0) (enc b-0 k n-a n-b ltkac-0)))
      (send (enc a k ltkbc))))
  (label 12)
  (parent 11)
  (unrealized)
  (shape)
  (maps
    ((0)
      ((a a) (b b) (c c) (n-b n-b) (ltkac ltkac) (ltkbc ltkbc) (n-a n-a)
        (k k))))
  (origs (k (1 1)) (n-b (0 1))))

(defskeleton yahalom2
  (vars (n-b n-a n-a-0 n-b-0 text) (a b c c-0 b-0 b-1 c-1 name)
    (ltkac ltkbc k ltkac-0 ltkac-1 skey))
  (defstrand resp 4 (n-a n-a) (n-b n-b) (b b) (a a) (c c) (ltkac ltkac)
    (ltkbc ltkbc) (k k))
  (defstrand serv 3 (n-a n-a-0) (n-b n-b-0) (c c-0) (a a) (b b-0)
    (ltkac ltkac-0) (ltkbc ltkbc) (k k))
  (defstrand resp 2 (n-a n-a-0) (n-b n-b-0) (b b-1) (a a) (c c-1)
    (ltkac ltkac-1) (ltkbc ltkbc))
  (precedes ((1 2) (0 2)) ((2 1) (1 0)))
  (fn-of
    ("ltk" (ltkac-1 (cat a c-1)) (ltkbc (cat b-1 c-1))
      (ltkac-0 (cat a c-0)) (ltkbc (cat b-0 c-0)) (ltkac (cat a c))
      (ltkbc (cat b c))))
  (non-orig ltkac ltkbc)
  (uniq-orig n-b k)
  (operation encryption-test (added-strand resp 2)
    (enc a n-a-0 n-b-0 ltkbc) (1 0))
  (traces
    ((recv (cat a n-a))
      (send (cat b (enc a n-a n-b ltkbc) (hash c ltkac)))
      (recv (enc a k ltkbc)) (recv (enc n-b k)))
    ((recv (cat b-0 (enc a n-a-0 n-b-0 ltkbc)))
      (send (cat (hash c-0) (enc b-0 k n-a-0 n-b-0 ltkac-0)))
      (send (enc a k ltkbc)))
    ((recv (cat a n-a-0))
      (send (cat b-1 (enc a n-a-0 n-b-0 ltkbc) (hash c-1 ltkac-1)))))
  (label 13)
  (parent 11)
  (unrealized (0 3))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton yahalom2
  (vars (n-b n-a n-a-0 n-b-0 text) (a b c c-0 b-0 b-1 c-1 c-2 b-2 name)
    (ltkac ltkbc k ltkac-0 ltkac-1 ltkac-2 k-0 skey))
  (defstrand resp 4 (n-a n-a) (n-b n-b) (b b) (a a) (c c) (ltkac ltkac)
    (ltkbc ltkbc) (k k))
  (defstrand serv 3 (n-a n-a-0) (n-b n-b-0) (c c-0) (a a) (b b-0)
    (ltkac ltkac-0) (ltkbc ltkbc) (k k))
  (defstrand resp 2 (n-a n-a-0) (n-b n-b-0) (b b-1) (a a) (c c-1)
    (ltkac ltkac-1) (ltkbc ltkbc))
  (defstrand serv 2 (n-a n-a) (n-b n-b) (c c-2) (a a) (b b-2)
    (ltkac ltkac-2) (ltkbc ltkbc) (k k-0))
  (precedes ((0 1) (3 0)) ((1 2) (0 2)) ((2 1) (1 0)) ((3 1) (0 3)))
  (fn-of
    ("ltk" (ltkac-2 (cat a c-2)) (ltkbc (cat b-2 c-2))
      (ltkac-1 (cat a c-1)) (ltkbc (cat b-1 c-1)) (ltkac-0 (cat a c-0))
      (ltkbc (cat b-0 c-0)) (ltkac (cat a c)) (ltkbc (cat b c))))
  (non-orig ltkac ltkbc)
  (uniq-orig n-b k k-0)
  (operation nonce-test (added-strand serv 2) n-b (0 3)
    (enc a n-a n-b ltkbc))
  (traces
    ((recv (cat a n-a))
      (send (cat b (enc a n-a n-b ltkbc) (hash c ltkac)))
      (recv (enc a k ltkbc)) (recv (enc n-b k)))
    ((recv (cat b-0 (enc a n-a-0 n-b-0 ltkbc)))
      (send (cat (hash c-0) (enc b-0 k n-a-0 n-b-0 ltkac-0)))
      (send (enc a k ltkbc)))
    ((recv (cat a n-a-0))
      (send (cat b-1 (enc a n-a-0 n-b-0 ltkbc) (hash c-1 ltkac-1))))
    ((recv (cat b-2 (enc a n-a n-b ltkbc)))
      (send (cat (hash c-2) (enc b-2 k-0 n-a n-b ltkac-2)))))
  (label 14)
  (parent 13)
  (unrealized)
  (shape)
  (maps
    ((0)
      ((a a) (b b) (c c) (n-b n-b) (ltkac ltkac) (ltkbc ltkbc) (n-a n-a)
        (k k))))
  (origs (k-0 (3 1)) (k (1 1)) (n-b (0 1))))

(comment "Nothing left to do")

(defprotocol yahalom2 basic
  (defrole init
    (vars (a b name) (n-a n-b text) (ltkac ltkbc k skey))
    (trace (send (cat a n-a (hash ltkbc)))
      (recv (enc b k n-a n-b ltkac)) (send (enc n-b k))))
  (defrole resp
    (vars (b a c name) (n-a n-b text) (ltkac ltkbc k skey))
    (trace (recv (cat a n-a))
      (send (cat b (enc a n-a n-b ltkbc) (hash c ltkac)))
      (recv (enc a k ltkbc)) (recv (enc n-b k)))
    (fn-of ("ltk" (ltkac (cat a c)) (ltkbc (cat b c)))))
  (defrole serv
    (vars (c a b name) (n-a n-b text) (ltkac ltkbc k skey))
    (trace (recv (cat b (enc a n-a n-b ltkbc)))
      (send (cat (hash c) (enc b k n-a n-b ltkac)))
      (send (enc a k ltkbc)))
    (uniq-orig k)
    (fn-of ("ltk" (ltkac (cat a c)) (ltkbc (cat b c))))))

(defskeleton yahalom2
  (vars (n-b n-a text) (a b c name) (ltkac ltkbc k skey))
  (defstrand resp 4 (n-a n-a) (n-b n-b) (b b) (a a) (c c) (ltkac ltkac)
    (ltkbc ltkbc) (k k))
  (deflistener k)
  (fn-of ("ltk" (ltkac (cat a c)) (ltkbc (cat b c))))
  (non-orig ltkac ltkbc)
  (uniq-orig n-b)
  (traces
    ((recv (cat a n-a))
      (send (cat b (enc a n-a n-b ltkbc) (hash c ltkac)))
      (recv (enc a k ltkbc)) (recv (enc n-b k))) ((recv k) (send k)))
  (label 15)
  (unrealized (0 2) (0 3))
  (origs (n-b (0 1)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton yahalom2
  (vars (n-b n-a n-a-0 n-b-0 text) (a b c c-0 b-0 name)
    (ltkac ltkbc k ltkac-0 skey))
  (defstrand resp 4 (n-a n-a) (n-b n-b) (b b) (a a) (c c) (ltkac ltkac)
    (ltkbc ltkbc) (k k))
  (deflistener k)
  (defstrand serv 3 (n-a n-a-0) (n-b n-b-0) (c c-0) (a a) (b b-0)
    (ltkac ltkac-0) (ltkbc ltkbc) (k k))
  (precedes ((2 1) (1 0)) ((2 2) (0 2)))
  (fn-of
    ("ltk" (ltkac-0 (cat a c-0)) (ltkbc (cat b-0 c-0)) (ltkac (cat a c))
      (ltkbc (cat b c))))
  (non-orig ltkac ltkbc)
  (uniq-orig n-b k)
  (operation encryption-test (added-strand serv 3) (enc a k ltkbc)
    (0 2))
  (traces
    ((recv (cat a n-a))
      (send (cat b (enc a n-a n-b ltkbc) (hash c ltkac)))
      (recv (enc a k ltkbc)) (recv (enc n-b k))) ((recv k) (send k))
    ((recv (cat b-0 (enc a n-a-0 n-b-0 ltkbc)))
      (send (cat (hash c-0) (enc b-0 k n-a-0 n-b-0 ltkac-0)))
      (send (enc a k ltkbc))))
  (label 16)
  (parent 15)
  (unrealized (0 3) (2 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton yahalom2
  (vars (n-b n-a text) (a b c c-0 b-0 name)
    (ltkac ltkbc k ltkac-0 skey))
  (defstrand resp 4 (n-a n-a) (n-b n-b) (b b) (a a) (c c) (ltkac ltkac)
    (ltkbc ltkbc) (k k))
  (deflistener k)
  (defstrand serv 3 (n-a n-a) (n-b n-b) (c c-0) (a a) (b b-0)
    (ltkac ltkac-0) (ltkbc ltkbc) (k k))
  (precedes ((0 1) (2 0)) ((2 1) (1 0)) ((2 2) (0 2)))
  (fn-of
    ("ltk" (ltkac (cat a c)) (ltkbc (cat b c)) (ltkac-0 (cat a c-0))
      (ltkbc (cat b-0 c-0))))
  (non-orig ltkac ltkbc)
  (uniq-orig n-b k)
  (operation encryption-test (displaced 3 0 resp 2)
    (enc a n-a-0 n-b-0 ltkbc) (2 0))
  (traces
    ((recv (cat a n-a))
      (send (cat b (enc a n-a n-b ltkbc) (hash c ltkac)))
      (recv (enc a k ltkbc)) (recv (enc n-b k))) ((recv k) (send k))
    ((recv (cat b-0 (enc a n-a n-b ltkbc)))
      (send (cat (hash c-0) (enc b-0 k n-a n-b ltkac-0)))
      (send (enc a k ltkbc))))
  (label 17)
  (parent 16)
  (unrealized)
  (shape)
  (maps
    ((0 1)
      ((a a) (b b) (c c) (n-b n-b) (ltkac ltkac) (ltkbc ltkbc) (k k)
        (n-a n-a))))
  (origs (k (2 1)) (n-b (0 1))))

(defskeleton yahalom2
  (vars (n-b n-a n-a-0 n-b-0 text) (a b c c-0 b-0 b-1 c-1 name)
    (ltkac ltkbc k ltkac-0 ltkac-1 skey))
  (defstrand resp 4 (n-a n-a) (n-b n-b) (b b) (a a) (c c) (ltkac ltkac)
    (ltkbc ltkbc) (k k))
  (deflistener k)
  (defstrand serv 3 (n-a n-a-0) (n-b n-b-0) (c c-0) (a a) (b b-0)
    (ltkac ltkac-0) (ltkbc ltkbc) (k k))
  (defstrand resp 2 (n-a n-a-0) (n-b n-b-0) (b b-1) (a a) (c c-1)
    (ltkac ltkac-1) (ltkbc ltkbc))
  (precedes ((2 1) (1 0)) ((2 2) (0 2)) ((3 1) (2 0)))
  (fn-of
    ("ltk" (ltkac-1 (cat a c-1)) (ltkbc (cat b-1 c-1))
      (ltkac-0 (cat a c-0)) (ltkbc (cat b-0 c-0)) (ltkac (cat a c))
      (ltkbc (cat b c))))
  (non-orig ltkac ltkbc)
  (uniq-orig n-b k)
  (operation encryption-test (added-strand resp 2)
    (enc a n-a-0 n-b-0 ltkbc) (2 0))
  (traces
    ((recv (cat a n-a))
      (send (cat b (enc a n-a n-b ltkbc) (hash c ltkac)))
      (recv (enc a k ltkbc)) (recv (enc n-b k))) ((recv k) (send k))
    ((recv (cat b-0 (enc a n-a-0 n-b-0 ltkbc)))
      (send (cat (hash c-0) (enc b-0 k n-a-0 n-b-0 ltkac-0)))
      (send (enc a k ltkbc)))
    ((recv (cat a n-a-0))
      (send (cat b-1 (enc a n-a-0 n-b-0 ltkbc) (hash c-1 ltkac-1)))))
  (label 18)
  (parent 16)
  (unrealized (0 3))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton yahalom2
  (vars (n-b n-a n-a-0 n-b-0 text) (a b c c-0 b-0 b-1 c-1 c-2 b-2 name)
    (ltkac ltkbc k ltkac-0 ltkac-1 ltkac-2 k-0 skey))
  (defstrand resp 4 (n-a n-a) (n-b n-b) (b b) (a a) (c c) (ltkac ltkac)
    (ltkbc ltkbc) (k k))
  (deflistener k)
  (defstrand serv 3 (n-a n-a-0) (n-b n-b-0) (c c-0) (a a) (b b-0)
    (ltkac ltkac-0) (ltkbc ltkbc) (k k))
  (defstrand resp 2 (n-a n-a-0) (n-b n-b-0) (b b-1) (a a) (c c-1)
    (ltkac ltkac-1) (ltkbc ltkbc))
  (defstrand serv 2 (n-a n-a) (n-b n-b) (c c-2) (a a) (b b-2)
    (ltkac ltkac-2) (ltkbc ltkbc) (k k-0))
  (precedes ((0 1) (4 0)) ((2 1) (1 0)) ((2 2) (0 2)) ((3 1) (2 0))
    ((4 1) (0 3)))
  (fn-of
    ("ltk" (ltkac-2 (cat a c-2)) (ltkbc (cat b-2 c-2))
      (ltkac-1 (cat a c-1)) (ltkbc (cat b-1 c-1)) (ltkac-0 (cat a c-0))
      (ltkbc (cat b-0 c-0)) (ltkac (cat a c)) (ltkbc (cat b c))))
  (non-orig ltkac ltkbc)
  (uniq-orig n-b k k-0)
  (operation nonce-test (added-strand serv 2) n-b (0 3)
    (enc a n-a n-b ltkbc))
  (traces
    ((recv (cat a n-a))
      (send (cat b (enc a n-a n-b ltkbc) (hash c ltkac)))
      (recv (enc a k ltkbc)) (recv (enc n-b k))) ((recv k) (send k))
    ((recv (cat b-0 (enc a n-a-0 n-b-0 ltkbc)))
      (send (cat (hash c-0) (enc b-0 k n-a-0 n-b-0 ltkac-0)))
      (send (enc a k ltkbc)))
    ((recv (cat a n-a-0))
      (send (cat b-1 (enc a n-a-0 n-b-0 ltkbc) (hash c-1 ltkac-1))))
    ((recv (cat b-2 (enc a n-a n-b ltkbc)))
      (send (cat (hash c-2) (enc b-2 k-0 n-a n-b ltkac-2)))))
  (label 19)
  (parent 18)
  (unrealized)
  (shape)
  (maps
    ((0 1)
      ((a a) (b b) (c c) (n-b n-b) (ltkac ltkac) (ltkbc ltkbc) (k k)
        (n-a n-a))))
  (origs (k-0 (4 1)) (k (2 1)) (n-b (0 1))))

(comment "Nothing left to do")
