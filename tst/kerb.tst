(comment "CPSA 3.6.0")
(comment "All input read from kerb.scm")

(defprotocol kerb-flawed basic
  (defrole init
    (vars (a b s name) (m n text) (k skey))
    (trace (send (cat a b n))
      (recv (cat (enc k n (ltk a s)) (enc k a b (ltk b s))))
      (send (cat (enc m k) (enc k a b (ltk b s)))))
    (uniq-orig n))
  (defrole resp
    (vars (a b s name) (m text) (k skey))
    (trace (recv (cat (enc m k) (enc k a b (ltk b s))))))
  (defrole keyserv
    (vars (a b s name) (n text) (k skey))
    (trace (recv (cat a b n))
      (send (cat (enc k n (ltk a s)) (enc k a b (ltk b s)))))
    (uniq-orig k)))

(defskeleton kerb-flawed
  (vars (m n text) (a b s name) (k skey))
  (defstrand init 3 (m m) (n n) (a a) (b b) (s s) (k k))
  (deflistener m)
  (non-orig (ltk a s) (ltk b s))
  (uniq-orig m n)
  (traces
    ((send (cat a b n))
      (recv (cat (enc k n (ltk a s)) (enc k a b (ltk b s))))
      (send (cat (enc m k) (enc k a b (ltk b s))))) ((recv m) (send m)))
  (label 0)
  (unrealized (0 1) (1 0))
  (preskeleton)
  (comment "Not a skeleton"))

(defskeleton kerb-flawed
  (vars (m n text) (a b s name) (k skey))
  (defstrand init 3 (m m) (n n) (a a) (b b) (s s) (k k))
  (deflistener m)
  (precedes ((0 2) (1 0)))
  (non-orig (ltk a s) (ltk b s))
  (uniq-orig m n)
  (traces
    ((send (cat a b n))
      (recv (cat (enc k n (ltk a s)) (enc k a b (ltk b s))))
      (send (cat (enc m k) (enc k a b (ltk b s))))) ((recv m) (send m)))
  (label 1)
  (parent 0)
  (unrealized (0 1))
  (origs (n (0 0)) (m (0 2)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton kerb-flawed
  (vars (m n text) (a b s b-0 name) (k skey))
  (defstrand init 3 (m m) (n n) (a a) (b b) (s s) (k k))
  (deflistener m)
  (defstrand keyserv 2 (n n) (a a) (b b-0) (s s) (k k))
  (precedes ((0 0) (2 0)) ((0 2) (1 0)) ((2 1) (0 1)))
  (non-orig (ltk a s) (ltk b s))
  (uniq-orig m n k)
  (operation encryption-test (added-strand keyserv 2)
    (enc k n (ltk a s)) (0 1))
  (traces
    ((send (cat a b n))
      (recv (cat (enc k n (ltk a s)) (enc k a b (ltk b s))))
      (send (cat (enc m k) (enc k a b (ltk b s))))) ((recv m) (send m))
    ((recv (cat a b-0 n))
      (send (cat (enc k n (ltk a s)) (enc k a b-0 (ltk b-0 s))))))
  (label 2)
  (parent 1)
  (unrealized (0 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton kerb-flawed
  (vars (m n text) (a s b name) (k skey))
  (defstrand init 3 (m m) (n n) (a a) (b b) (s s) (k k))
  (deflistener m)
  (defstrand keyserv 2 (n n) (a a) (b b) (s s) (k k))
  (precedes ((0 0) (2 0)) ((0 2) (1 0)) ((2 1) (0 1)))
  (non-orig (ltk a s) (ltk b s))
  (uniq-orig m n k)
  (operation encryption-test (displaced 3 2 keyserv 2)
    (enc k a b-0 (ltk b-0 s)) (0 1))
  (traces
    ((send (cat a b n))
      (recv (cat (enc k n (ltk a s)) (enc k a b (ltk b s))))
      (send (cat (enc m k) (enc k a b (ltk b s))))) ((recv m) (send m))
    ((recv (cat a b n))
      (send (cat (enc k n (ltk a s)) (enc k a b (ltk b s))))))
  (label 3)
  (parent 2)
  (unrealized (1 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton kerb-flawed
  (vars (m n text) (a s b name) (k skey))
  (defstrand init 3 (m m) (n n) (a a) (b b) (s s) (k k))
  (deflistener m)
  (defstrand keyserv 2 (n n) (a a) (b b) (s s) (k k))
  (deflistener k)
  (precedes ((0 0) (2 0)) ((0 2) (1 0)) ((2 1) (0 1)) ((2 1) (3 0))
    ((3 1) (1 0)))
  (non-orig (ltk a s) (ltk b s))
  (uniq-orig m n k)
  (operation nonce-test (added-listener k) m (1 0) (enc m k))
  (traces
    ((send (cat a b n))
      (recv (cat (enc k n (ltk a s)) (enc k a b (ltk b s))))
      (send (cat (enc m k) (enc k a b (ltk b s))))) ((recv m) (send m))
    ((recv (cat a b n))
      (send (cat (enc k n (ltk a s)) (enc k a b (ltk b s)))))
    ((recv k) (send k)))
  (label 4)
  (parent 3)
  (unrealized (3 0))
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol kerb-flawed2 basic
  (defrole init
    (vars (a b s name) (ticket mesg) (m n text) (k skey))
    (trace (send (cat a b n)) (recv (cat (enc k n (ltk a s)) ticket))
      (send (cat (enc m k) ticket)))
    (uniq-orig n))
  (defrole resp
    (vars (a b s name) (m text) (k skey))
    (trace (recv (cat (enc m k) (enc k a b (ltk b s))))))
  (defrole keyserv
    (vars (a b s name) (n text) (k skey))
    (trace (recv (cat a b n))
      (send (cat (enc k n (ltk a s)) (enc k a b (ltk b s)))))
    (uniq-orig k)))

(defskeleton kerb-flawed2
  (vars (ticket mesg) (m n text) (a b s name) (k skey))
  (defstrand init 3 (ticket ticket) (m m) (n n) (a a) (b b) (s s) (k k))
  (deflistener m)
  (non-orig (ltk a s) (ltk b s))
  (uniq-orig m n)
  (traces
    ((send (cat a b n)) (recv (cat (enc k n (ltk a s)) ticket))
      (send (cat (enc m k) ticket))) ((recv m) (send m)))
  (label 5)
  (unrealized (0 1) (1 0))
  (preskeleton)
  (comment "Not a skeleton"))

(defskeleton kerb-flawed2
  (vars (ticket mesg) (m n text) (a b s name) (k skey))
  (defstrand init 3 (ticket ticket) (m m) (n n) (a a) (b b) (s s) (k k))
  (deflistener m)
  (precedes ((0 2) (1 0)))
  (non-orig (ltk a s) (ltk b s))
  (uniq-orig m n)
  (traces
    ((send (cat a b n)) (recv (cat (enc k n (ltk a s)) ticket))
      (send (cat (enc m k) ticket))) ((recv m) (send m)))
  (label 6)
  (parent 5)
  (unrealized (0 1))
  (origs (n (0 0)) (m (0 2)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton kerb-flawed2
  (vars (ticket mesg) (m n text) (a b s b-0 name) (k skey))
  (defstrand init 3 (ticket ticket) (m m) (n n) (a a) (b b) (s s) (k k))
  (deflistener m)
  (defstrand keyserv 2 (n n) (a a) (b b-0) (s s) (k k))
  (precedes ((0 0) (2 0)) ((0 2) (1 0)) ((2 1) (0 1)))
  (non-orig (ltk a s) (ltk b s))
  (uniq-orig m n k)
  (operation encryption-test (added-strand keyserv 2)
    (enc k n (ltk a s)) (0 1))
  (traces
    ((send (cat a b n)) (recv (cat (enc k n (ltk a s)) ticket))
      (send (cat (enc m k) ticket))) ((recv m) (send m))
    ((recv (cat a b-0 n))
      (send (cat (enc k n (ltk a s)) (enc k a b-0 (ltk b-0 s))))))
  (label 7)
  (parent 6)
  (unrealized)
  (shape)
  (maps ((0 1) ((a a) (b b) (s s) (m m) (ticket ticket) (n n) (k k))))
  (origs (k (2 1)) (n (0 0)) (m (0 2))))

(comment "Nothing left to do")
