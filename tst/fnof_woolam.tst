(herald "Woo-Lam Protocol, using fnof to emulate ltk function")

(comment "CPSA 3.6.5")
(comment "All input read from fnof_woolam.scm")

(defprotocol woolam basic
  (defrole init
    (vars (a s name) (n text) (ltkas skey))
    (trace (init s) (send a) (recv n) (send (enc n ltkas)))
    (non-orig ltkas)
    (fn-of ("ltk" (ltkas (cat a s))) ("ltk-inv" ((cat a s) ltkas))))
  (defrole resp
    (vars (a s b name) (n text) (ltkas ltkbs skey))
    (trace (init (cat b s)) (recv a) (send n) (recv (enc n ltkas))
      (send (enc a (enc n ltkas) ltkbs)) (recv (enc a n ltkbs)))
    (non-orig ltkbs)
    (uniq-orig n)
    (fn-of ("ltk" (ltkas (cat a s)) (ltkbs (cat b s)))
      ("ltk-inv" ((cat a s) ltkas) ((cat b s) ltkbs))))
  (defrole serv
    (vars (a s b name) (n text) (ltkbs ltkas skey))
    (trace (init (cat b s)) (recv (enc a (enc n ltkas) ltkbs))
      (send (enc a n ltkbs)))
    (fn-of ("ltk" (ltkas (cat a s)) (ltkbs (cat b s)))
      ("ltk-inv" ((cat a s) ltkas) ((cat b s) ltkbs)))))

(defskeleton woolam
  (vars (n text) (a s b name) (ltkas ltkbs skey))
  (defstrand resp 5 (n n) (a a) (s s) (b b) (ltkas ltkas) (ltkbs ltkbs))
  (fn-of ("ltk" (ltkas (cat a s)) (ltkbs (cat b s)))
    ("ltk-inv" ((cat a s) ltkas) ((cat b s) ltkbs)))
  (non-orig ltkas ltkbs)
  (uniq-orig n)
  (traces
    ((init (cat b s)) (recv a) (send n) (recv (enc n ltkas))
      (send (enc a (enc n ltkas) ltkbs))))
  (label 0)
  (unrealized (0 3))
  (origs (n (0 2)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton woolam
  (vars (n text) (a s b name) (ltkas ltkbs skey))
  (defstrand resp 5 (n n) (a a) (s s) (b b) (ltkas ltkas) (ltkbs ltkbs))
  (defstrand init 4 (n n) (a a) (s s) (ltkas ltkas))
  (precedes ((0 2) (1 2)) ((1 3) (0 3)))
  (fn-of ("ltk" (ltkas (cat a s)) (ltkbs (cat b s)))
    ("ltk-inv" ((cat a s) ltkas) ((cat b s) ltkbs)))
  (non-orig ltkas ltkbs)
  (uniq-orig n)
  (operation encryption-test (added-strand init 4) (enc n ltkas) (0 3))
  (traces
    ((init (cat b s)) (recv a) (send n) (recv (enc n ltkas))
      (send (enc a (enc n ltkas) ltkbs)))
    ((init s) (send a) (recv n) (send (enc n ltkas))))
  (label 1)
  (parent 0)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (s s) (ltkas ltkas) (b b) (n n) (ltkbs ltkbs))))
  (origs (n (0 2))))

(comment "Nothing left to do")

(defprotocol woolam2 basic
  (defrole init
    (vars (a s name) (n text) (ltkas skey))
    (trace (init s) (send a) (recv n) (send (enc n ltkas)))
    (non-orig ltkas)
    (fn-of ("ltk" (ltkas (cat a s)))))
  (defrole resp
    (vars (a s b name) (n text) (ltkas ltkbs skey))
    (trace (init (cat b s)) (recv a) (send n) (recv (enc n ltkas))
      (send (enc a (enc n ltkas) ltkbs)) (recv (enc a n ltkbs)))
    (non-orig ltkbs)
    (uniq-orig n)
    (fn-of ("ltk" (ltkas (cat a s)) (ltkbs (cat b s)))))
  (defrole serv
    (vars (a s b name) (n text) (ltkbs ltkas skey))
    (trace (init (cat b s)) (recv (enc a (enc n ltkas) ltkbs))
      (send (enc a n ltkbs)))
    (fn-of ("ltk" (ltkas (cat a s)) (ltkbs (cat b s))))))

(defskeleton woolam2
  (vars (n text) (a s b name) (ltkas ltkbs skey))
  (defstrand resp 5 (n n) (a a) (s s) (b b) (ltkas ltkas) (ltkbs ltkbs))
  (fn-of ("ltk" (ltkas (cat a s)) (ltkbs (cat b s))))
  (non-orig ltkas ltkbs)
  (uniq-orig n)
  (traces
    ((init (cat b s)) (recv a) (send n) (recv (enc n ltkas))
      (send (enc a (enc n ltkas) ltkbs))))
  (label 2)
  (unrealized (0 3))
  (origs (n (0 2)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton woolam2
  (vars (n text) (a s b a-0 s-0 name) (ltkas ltkbs skey))
  (defstrand resp 5 (n n) (a a) (s s) (b b) (ltkas ltkas) (ltkbs ltkbs))
  (defstrand init 4 (n n) (a a-0) (s s-0) (ltkas ltkas))
  (precedes ((0 2) (1 2)) ((1 3) (0 3)))
  (fn-of
    ("ltk" (ltkas (cat a-0 s-0)) (ltkas (cat a s)) (ltkbs (cat b s))))
  (non-orig ltkas ltkbs)
  (uniq-orig n)
  (operation encryption-test (added-strand init 4) (enc n ltkas) (0 3))
  (traces
    ((init (cat b s)) (recv a) (send n) (recv (enc n ltkas))
      (send (enc a (enc n ltkas) ltkbs)))
    ((init s-0) (send a-0) (recv n) (send (enc n ltkas))))
  (label 3)
  (parent 2)
  (unrealized)
  (shape)
  (maps ((0) ((ltkas ltkas) (a a) (s s) (b b) (n n) (ltkbs ltkbs))))
  (origs (n (0 2))))

(comment "Nothing left to do")
