(comment "CPSA 3.2.2")
(comment "All input read from owang.scm")

(defprotocol wang basic
  (defrole init1
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b))) (send (cat k r))))
  (defrole init2
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (defrole init3
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))))
  (defrole init4
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (defrole init5
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))))
  (defrole resp1
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (recv
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b))) (recv (cat k r))))
  (defrole resp2
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (recv
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))
          (enc "rcrq" (cat a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b)))) (recv (cat k r))))
  (defrole resp3
    (vars (a b t name) (e1 e2 x mesg))
    (trace
      (recv
        (cat (cat a b t (hash e1) x) e1 e2
          (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))))
      (send
        (cat (cat a b t (hash e1) x) e2
          (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))
          (enc "eortag" (hash a b t (hash e1) x) e2 (privk "sign" b))))
      (send
        (cat (cat a b t (hash e1) x) e2
          (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))
          (enc "eortag" (hash a b t (hash e1) x) e2 (privk "sign" b))
          (enc "rcrq" (cat a b t (hash e1) x) e2 (privk "sign" b))))
      (recv
        (enc "abcf" (enc "abrq" a b t (hash e1) x (privk "sign" a))
          (privk "sign" t)))))
  (defrole ttp-ab1
    (vars (a b t name) (y x mesg))
    (trace (recv (enc "abrq" a b t y x (privk "sign" a)))
      (send (cat "sync-abrq" (enc "abrq" a b t y x (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y x (privk "sign" a))
          (privk "sign" t)))))
  (defrole ttp-ab2
    (vars (a b t name) (y x e mesg))
    (trace (recv (enc "abrq" a b t y x (privk "sign" a)))
      (recv
        (cat "sync-abrq"
          (enc "eortag" (hash a b t y x) e (privk "sign" b))))
      (send (enc "eortag" (hash a b t y x) e (privk "sign" b)))))
  (defrole ttp-rc1
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "rcrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (send
        (cat "sync-rc-req" (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "rcrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b)))) (send (cat k r))))
  (defrole ttp-rc2
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "rcrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (recv
        (cat "sync-rc-req"
          (enc "abrq" a b t y (hash k) (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (defrole ttp-cf1
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (send
        (cat "sync-cf-req" (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (send
        (enc (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b)) (privk "sign" t)))))
  (defrole ttp-cf2
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (recv
        (cat "sync-cf-req"
          (enc "abrq" a b t y (hash k) (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y (hash k) (privk "sign" a))
          (privk "sign" t))))))

(defskeleton wang
  (vars (r text) (m data) (b t a name) (k skey))
  (defstrand init1 1 (r r) (m m) (a a) (b b) (t t) (k k))
  (deflistener m)
  (non-orig (privk "encr" t))
  (uniq-orig m k)
  (comment "Experiment 1 to prove Lemma 4.1.")
  (traces
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))) ((recv m) (send m)))
  (label 0)
  (unrealized (1 0))
  (preskeleton)
  (comment "Not a skeleton"))

(defskeleton wang
  (vars (r text) (m data) (b t a name) (k skey))
  (defstrand init1 1 (r r) (m m) (a a) (b b) (t t) (k k))
  (deflistener m)
  (precedes ((0 0) (1 0)))
  (non-orig (privk "encr" t))
  (uniq-orig m k)
  (traces
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))) ((recv m) (send m)))
  (label 1)
  (parent 0)
  (unrealized (1 0))
  (origs (m (0 0)) (k (0 0)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton wang
  (vars (r text) (m data) (b t a name) (k skey))
  (defstrand init1 1 (r r) (m m) (a a) (b b) (t t) (k k))
  (deflistener m)
  (deflistener k)
  (precedes ((0 0) (2 0)) ((2 1) (1 0)))
  (non-orig (privk "encr" t))
  (uniq-orig m k)
  (operation nonce-test (added-listener k) m (1 0) (enc m k))
  (traces
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))) ((recv m) (send m))
    ((recv k) (send k)))
  (label 2)
  (parent 1)
  (unrealized (2 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton wang
  (vars (r text) (m data) (b t a name) (k skey))
  (deflistener m)
  (deflistener k)
  (defstrand init1 3 (r r) (m m) (a a) (b b) (t t) (k k))
  (precedes ((1 1) (0 0)) ((2 2) (1 0)))
  (non-orig (privk "encr" t))
  (uniq-orig m k)
  (operation nonce-test (displaced 0 3 init1 3) k (2 0)
    (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
      (pubk "encr" t)))
  (traces ((recv m) (send m)) ((recv k) (send k))
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b))) (send (cat k r))))
  (label 3)
  (parent 2)
  (unrealized)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton wang
  (vars (r text) (m data) (b t a name) (k skey))
  (defstrand init1 1 (r r) (m m) (a a) (b b) (t t) (k k))
  (deflistener m)
  (deflistener k)
  (defstrand ttp-rc1 3 (y (hash (enc m k))) (r r) (a a) (b b) (t t)
    (k k))
  (precedes ((0 0) (3 0)) ((2 1) (1 0)) ((3 2) (2 0)))
  (non-orig (privk "encr" t))
  (uniq-orig m k)
  (operation nonce-test (added-strand ttp-rc1 3) k (2 0)
    (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
      (pubk "encr" t)))
  (traces
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))) ((recv m) (send m))
    ((recv k) (send k))
    ((recv
       (cat (cat a b t (hash (enc m k)) (hash k))
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))
         (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" b))
         (enc "rcrq" (cat a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" b))))
      (send
        (cat "sync-rc-req" (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))
          (enc "rcrq" (cat a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b)))) (send (cat k r))))
  (label 4)
  (parent 2)
  (unrealized)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton wang
  (vars (r text) (m data) (b t a name) (k skey))
  (deflistener m)
  (defstrand init1 3 (r r) (m m) (a a) (b b) (t t) (k k))
  (precedes ((1 2) (0 0)))
  (non-orig (privk "encr" t))
  (uniq-orig m k)
  (operation generalization deleted (1 0))
  (traces ((recv m) (send m))
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b))) (send (cat k r))))
  (label 5)
  (parent 3)
  (unrealized)
  (shape)
  (maps ((1 0) ((b b) (t t) (m m) (r r) (k k) (a a))))
  (origs (m (1 0)) (k (1 0))))

(defskeleton wang
  (vars (r text) (m data) (b t a name) (k skey))
  (defstrand init1 1 (r r) (m m) (a a) (b b) (t t) (k k))
  (deflistener m)
  (defstrand ttp-rc1 3 (y (hash (enc m k))) (r r) (a a) (b b) (t t)
    (k k))
  (precedes ((0 0) (2 0)) ((2 2) (1 0)))
  (non-orig (privk "encr" t))
  (uniq-orig m k)
  (operation generalization deleted (2 0))
  (traces
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))) ((recv m) (send m))
    ((recv
       (cat (cat a b t (hash (enc m k)) (hash k))
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))
         (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" b))
         (enc "rcrq" (cat a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" b))))
      (send
        (cat "sync-rc-req" (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))
          (enc "rcrq" (cat a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b)))) (send (cat k r))))
  (label 6)
  (parent 4)
  (unrealized)
  (shape)
  (maps ((0 1) ((b b) (t t) (m m) (r r) (k k) (a a))))
  (origs (m (0 0)) (k (0 0))))

(comment "Nothing left to do")

(defprotocol wang basic
  (defrole init1
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b))) (send (cat k r))))
  (defrole init2
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (defrole init3
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))))
  (defrole init4
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (defrole init5
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))))
  (defrole resp1
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (recv
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b))) (recv (cat k r))))
  (defrole resp2
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (recv
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))
          (enc "rcrq" (cat a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b)))) (recv (cat k r))))
  (defrole resp3
    (vars (a b t name) (e1 e2 x mesg))
    (trace
      (recv
        (cat (cat a b t (hash e1) x) e1 e2
          (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))))
      (send
        (cat (cat a b t (hash e1) x) e2
          (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))
          (enc "eortag" (hash a b t (hash e1) x) e2 (privk "sign" b))))
      (send
        (cat (cat a b t (hash e1) x) e2
          (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))
          (enc "eortag" (hash a b t (hash e1) x) e2 (privk "sign" b))
          (enc "rcrq" (cat a b t (hash e1) x) e2 (privk "sign" b))))
      (recv
        (enc "abcf" (enc "abrq" a b t (hash e1) x (privk "sign" a))
          (privk "sign" t)))))
  (defrole ttp-ab1
    (vars (a b t name) (y x mesg))
    (trace (recv (enc "abrq" a b t y x (privk "sign" a)))
      (send (cat "sync-abrq" (enc "abrq" a b t y x (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y x (privk "sign" a))
          (privk "sign" t)))))
  (defrole ttp-ab2
    (vars (a b t name) (y x e mesg))
    (trace (recv (enc "abrq" a b t y x (privk "sign" a)))
      (recv
        (cat "sync-abrq"
          (enc "eortag" (hash a b t y x) e (privk "sign" b))))
      (send (enc "eortag" (hash a b t y x) e (privk "sign" b)))))
  (defrole ttp-rc1
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "rcrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (send
        (cat "sync-rc-req" (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "rcrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b)))) (send (cat k r))))
  (defrole ttp-rc2
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "rcrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (recv
        (cat "sync-rc-req"
          (enc "abrq" a b t y (hash k) (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (defrole ttp-cf1
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (send
        (cat "sync-cf-req" (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (send
        (enc (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b)) (privk "sign" t)))))
  (defrole ttp-cf2
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (recv
        (cat "sync-cf-req"
          (enc "abrq" a b t y (hash k) (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y (hash k) (privk "sign" a))
          (privk "sign" t))))))

(defskeleton wang
  (vars (r text) (m data) (b t a name) (k skey))
  (defstrand init1 1 (r r) (m m) (a a) (b b) (t t) (k k))
  (deflistener k)
  (non-orig (privk "encr" t))
  (uniq-orig m k)
  (comment "Experiment 2 to prove Lemma 4.1.")
  (traces
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))) ((recv k) (send k)))
  (label 7)
  (unrealized (1 0))
  (preskeleton)
  (comment "Not a skeleton"))

(defskeleton wang
  (vars (r text) (m data) (b t a name) (k skey))
  (defstrand init1 1 (r r) (m m) (a a) (b b) (t t) (k k))
  (deflistener k)
  (precedes ((0 0) (1 0)))
  (non-orig (privk "encr" t))
  (uniq-orig m k)
  (traces
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))) ((recv k) (send k)))
  (label 8)
  (parent 7)
  (unrealized (1 0))
  (origs (m (0 0)) (k (0 0)))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton wang
  (vars (r text) (m data) (b t a name) (k skey))
  (deflistener k)
  (defstrand init1 3 (r r) (m m) (a a) (b b) (t t) (k k))
  (precedes ((1 2) (0 0)))
  (non-orig (privk "encr" t))
  (uniq-orig m k)
  (operation nonce-test (displaced 0 2 init1 3) k (1 0)
    (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
      (pubk "encr" t)))
  (traces ((recv k) (send k))
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b))) (send (cat k r))))
  (label 9)
  (parent 8)
  (unrealized)
  (shape)
  (maps ((1 0) ((b b) (t t) (m m) (r r) (k k) (a a))))
  (origs (m (1 0)) (k (1 0))))

(defskeleton wang
  (vars (r text) (m data) (b t a name) (k skey))
  (defstrand init1 1 (r r) (m m) (a a) (b b) (t t) (k k))
  (deflistener k)
  (defstrand ttp-rc1 3 (y (hash (enc m k))) (r r) (a a) (b b) (t t)
    (k k))
  (precedes ((0 0) (2 0)) ((2 2) (1 0)))
  (non-orig (privk "encr" t))
  (uniq-orig m k)
  (operation nonce-test (added-strand ttp-rc1 3) k (1 0)
    (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
      (pubk "encr" t)))
  (traces
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))) ((recv k) (send k))
    ((recv
       (cat (cat a b t (hash (enc m k)) (hash k))
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))
         (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" b))
         (enc "rcrq" (cat a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" b))))
      (send
        (cat "sync-rc-req" (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))
          (enc "rcrq" (cat a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b)))) (send (cat k r))))
  (label 10)
  (parent 8)
  (unrealized)
  (shape)
  (maps ((0 1) ((b b) (t t) (m m) (r r) (k k) (a a))))
  (origs (m (0 0)) (k (0 0))))

(comment "Nothing left to do")

(defprotocol wang basic
  (defrole init1
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b))) (send (cat k r))))
  (defrole init2
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (defrole init3
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))))
  (defrole init4
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (defrole init5
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))))
  (defrole resp1
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (recv
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b))) (recv (cat k r))))
  (defrole resp2
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (recv
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))
          (enc "rcrq" (cat a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b)))) (recv (cat k r))))
  (defrole resp3
    (vars (a b t name) (e1 e2 x mesg))
    (trace
      (recv
        (cat (cat a b t (hash e1) x) e1 e2
          (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))))
      (send
        (cat (cat a b t (hash e1) x) e2
          (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))
          (enc "eortag" (hash a b t (hash e1) x) e2 (privk "sign" b))))
      (send
        (cat (cat a b t (hash e1) x) e2
          (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))
          (enc "eortag" (hash a b t (hash e1) x) e2 (privk "sign" b))
          (enc "rcrq" (cat a b t (hash e1) x) e2 (privk "sign" b))))
      (recv
        (enc "abcf" (enc "abrq" a b t (hash e1) x (privk "sign" a))
          (privk "sign" t)))))
  (defrole ttp-ab1
    (vars (a b t name) (y x mesg))
    (trace (recv (enc "abrq" a b t y x (privk "sign" a)))
      (send (cat "sync-abrq" (enc "abrq" a b t y x (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y x (privk "sign" a))
          (privk "sign" t)))))
  (defrole ttp-ab2
    (vars (a b t name) (y x e mesg))
    (trace (recv (enc "abrq" a b t y x (privk "sign" a)))
      (recv
        (cat "sync-abrq"
          (enc "eortag" (hash a b t y x) e (privk "sign" b))))
      (send (enc "eortag" (hash a b t y x) e (privk "sign" b)))))
  (defrole ttp-rc1
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "rcrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (send
        (cat "sync-rc-req" (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "rcrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b)))) (send (cat k r))))
  (defrole ttp-rc2
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "rcrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (recv
        (cat "sync-rc-req"
          (enc "abrq" a b t y (hash k) (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (defrole ttp-cf1
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (send
        (cat "sync-cf-req" (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (send
        (enc (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b)) (privk "sign" t)))))
  (defrole ttp-cf2
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (recv
        (cat "sync-cf-req"
          (enc "abrq" a b t y (hash k) (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y (hash k) (privk "sign" a))
          (privk "sign" t))))))

(defskeleton wang
  (vars (r text) (m data) (b t a name) (k skey))
  (defstrand init1 3 (r r) (m m) (a a) (b b) (t t) (k k))
  (non-orig (privk "sign" b))
  (comment "First of three experiments to prove Lemma 4.2, clause 1.")
  (traces
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b))) (send (cat k r))))
  (label 11)
  (unrealized (0 1))
  (origs)
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton wang
  (vars (r text) (m data) (b t a name) (k skey))
  (defstrand init1 3 (r r) (m m) (a a) (b b) (t t) (k k))
  (defstrand resp1 2 (r r) (m m) (a a) (b b) (t t) (k k))
  (precedes ((1 1) (0 1)))
  (non-orig (privk "sign" b))
  (operation encryption-test (added-strand resp1 2)
    (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
      (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
        (pubk "encr" t)) (privk "sign" b)) (0 1))
  (traces
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b))) (send (cat k r)))
    ((recv
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))))
  (label 12)
  (parent 11)
  (unrealized)
  (shape)
  (maps ((0) ((b b) (t t) (m m) (r r) (k k) (a a))))
  (origs))

(defskeleton wang
  (vars (r text) (m data) (b t a name) (k skey))
  (defstrand init1 3 (r r) (m m) (a a) (b b) (t t) (k k))
  (defstrand resp3 2 (e1 (enc m k))
    (e2
      (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
        (pubk "encr" t))) (x (hash k)) (a a) (b b) (t t))
  (precedes ((1 1) (0 1)))
  (non-orig (privk "sign" b))
  (operation encryption-test (added-strand resp3 2)
    (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
      (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
        (pubk "encr" t)) (privk "sign" b)) (0 1))
  (traces
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b))) (send (cat k r)))
    ((recv
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))))))
  (label 13)
  (parent 11)
  (unrealized)
  (shape)
  (maps ((0) ((b b) (t t) (m m) (r r) (k k) (a a))))
  (origs))

(comment "Nothing left to do")

(defprotocol wang basic
  (defrole init1
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b))) (send (cat k r))))
  (defrole init2
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (defrole init3
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))))
  (defrole init4
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (defrole init5
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))))
  (defrole resp1
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (recv
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b))) (recv (cat k r))))
  (defrole resp2
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (recv
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))
          (enc "rcrq" (cat a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b)))) (recv (cat k r))))
  (defrole resp3
    (vars (a b t name) (e1 e2 x mesg))
    (trace
      (recv
        (cat (cat a b t (hash e1) x) e1 e2
          (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))))
      (send
        (cat (cat a b t (hash e1) x) e2
          (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))
          (enc "eortag" (hash a b t (hash e1) x) e2 (privk "sign" b))))
      (send
        (cat (cat a b t (hash e1) x) e2
          (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))
          (enc "eortag" (hash a b t (hash e1) x) e2 (privk "sign" b))
          (enc "rcrq" (cat a b t (hash e1) x) e2 (privk "sign" b))))
      (recv
        (enc "abcf" (enc "abrq" a b t (hash e1) x (privk "sign" a))
          (privk "sign" t)))))
  (defrole ttp-ab1
    (vars (a b t name) (y x mesg))
    (trace (recv (enc "abrq" a b t y x (privk "sign" a)))
      (send (cat "sync-abrq" (enc "abrq" a b t y x (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y x (privk "sign" a))
          (privk "sign" t)))))
  (defrole ttp-ab2
    (vars (a b t name) (y x e mesg))
    (trace (recv (enc "abrq" a b t y x (privk "sign" a)))
      (recv
        (cat "sync-abrq"
          (enc "eortag" (hash a b t y x) e (privk "sign" b))))
      (send (enc "eortag" (hash a b t y x) e (privk "sign" b)))))
  (defrole ttp-rc1
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "rcrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (send
        (cat "sync-rc-req" (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "rcrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b)))) (send (cat k r))))
  (defrole ttp-rc2
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "rcrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (recv
        (cat "sync-rc-req"
          (enc "abrq" a b t y (hash k) (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (defrole ttp-cf1
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (send
        (cat "sync-cf-req" (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (send
        (enc (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b)) (privk "sign" t)))))
  (defrole ttp-cf2
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (recv
        (cat "sync-cf-req"
          (enc "abrq" a b t y (hash k) (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y (hash k) (privk "sign" a))
          (privk "sign" t))))))

(defskeleton wang
  (vars (r text) (m data) (b t a name) (k skey))
  (defstrand init3 3 (r r) (m m) (a a) (b b) (t t) (k k))
  (non-orig (privk "sign" b))
  (comment "Second of three experiments to prove Lemma 4.2, clause 1.")
  (traces
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))))
  (label 14)
  (unrealized (0 2))
  (origs)
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton wang
  (vars (r text) (m data) (b t a name) (k skey))
  (defstrand init3 3 (r r) (m m) (a a) (b b) (t t) (k k))
  (defstrand resp1 2 (r r) (m m) (a a) (b b) (t t) (k k))
  (precedes ((1 1) (0 2)))
  (non-orig (privk "sign" b))
  (operation encryption-test (added-strand resp1 2)
    (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
      (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
        (pubk "encr" t)) (privk "sign" b)) (0 2))
  (traces
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b))))
    ((recv
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))))
  (label 15)
  (parent 14)
  (unrealized)
  (shape)
  (maps ((0) ((b b) (t t) (m m) (r r) (k k) (a a))))
  (origs))

(defskeleton wang
  (vars (r text) (m data) (b t a name) (k skey))
  (defstrand init3 3 (r r) (m m) (a a) (b b) (t t) (k k))
  (defstrand resp3 2 (e1 (enc m k))
    (e2
      (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
        (pubk "encr" t))) (x (hash k)) (a a) (b b) (t t))
  (precedes ((1 1) (0 2)))
  (non-orig (privk "sign" b))
  (operation encryption-test (added-strand resp3 2)
    (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
      (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
        (pubk "encr" t)) (privk "sign" b)) (0 2))
  (traces
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b))))
    ((recv
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))))))
  (label 16)
  (parent 14)
  (unrealized)
  (shape)
  (maps ((0) ((b b) (t t) (m m) (r r) (k k) (a a))))
  (origs))

(comment "Nothing left to do")

(defprotocol wang basic
  (defrole init1
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b))) (send (cat k r))))
  (defrole init2
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (defrole init3
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))))
  (defrole init4
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (defrole init5
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))))
  (defrole resp1
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (recv
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b))) (recv (cat k r))))
  (defrole resp2
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (recv
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))
          (enc "rcrq" (cat a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b)))) (recv (cat k r))))
  (defrole resp3
    (vars (a b t name) (e1 e2 x mesg))
    (trace
      (recv
        (cat (cat a b t (hash e1) x) e1 e2
          (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))))
      (send
        (cat (cat a b t (hash e1) x) e2
          (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))
          (enc "eortag" (hash a b t (hash e1) x) e2 (privk "sign" b))))
      (send
        (cat (cat a b t (hash e1) x) e2
          (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))
          (enc "eortag" (hash a b t (hash e1) x) e2 (privk "sign" b))
          (enc "rcrq" (cat a b t (hash e1) x) e2 (privk "sign" b))))
      (recv
        (enc "abcf" (enc "abrq" a b t (hash e1) x (privk "sign" a))
          (privk "sign" t)))))
  (defrole ttp-ab1
    (vars (a b t name) (y x mesg))
    (trace (recv (enc "abrq" a b t y x (privk "sign" a)))
      (send (cat "sync-abrq" (enc "abrq" a b t y x (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y x (privk "sign" a))
          (privk "sign" t)))))
  (defrole ttp-ab2
    (vars (a b t name) (y x e mesg))
    (trace (recv (enc "abrq" a b t y x (privk "sign" a)))
      (recv
        (cat "sync-abrq"
          (enc "eortag" (hash a b t y x) e (privk "sign" b))))
      (send (enc "eortag" (hash a b t y x) e (privk "sign" b)))))
  (defrole ttp-rc1
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "rcrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (send
        (cat "sync-rc-req" (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "rcrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b)))) (send (cat k r))))
  (defrole ttp-rc2
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "rcrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (recv
        (cat "sync-rc-req"
          (enc "abrq" a b t y (hash k) (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (defrole ttp-cf1
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (send
        (cat "sync-cf-req" (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (send
        (enc (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b)) (privk "sign" t)))))
  (defrole ttp-cf2
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (recv
        (cat "sync-cf-req"
          (enc "abrq" a b t y (hash k) (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y (hash k) (privk "sign" a))
          (privk "sign" t))))))

(defskeleton wang
  (vars (r text) (m data) (b t a name) (k skey))
  (defstrand init5 4 (r r) (m m) (a a) (b b) (t t) (k k))
  (non-orig (privk "sign" b))
  (comment "Third of three experiments to prove Lemma 4.2, clause 1.")
  (traces
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))))
  (label 17)
  (unrealized (0 1) (0 3))
  (origs)
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton wang
  (vars (r text) (m data) (b t a name) (k skey))
  (defstrand init5 4 (r r) (m m) (a a) (b b) (t t) (k k))
  (defstrand resp1 2 (r r) (m m) (a a) (b b) (t t) (k k))
  (precedes ((1 1) (0 1)))
  (non-orig (privk "sign" b))
  (operation encryption-test (added-strand resp1 2)
    (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
      (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
        (pubk "encr" t)) (privk "sign" b)) (0 1))
  (traces
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b))))
    ((recv
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))))
  (label 18)
  (parent 17)
  (unrealized)
  (shape)
  (maps ((0) ((b b) (t t) (m m) (r r) (k k) (a a))))
  (origs))

(defskeleton wang
  (vars (r text) (m data) (b t a name) (k skey))
  (defstrand init5 4 (r r) (m m) (a a) (b b) (t t) (k k))
  (defstrand resp3 2 (e1 (enc m k))
    (e2
      (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
        (pubk "encr" t))) (x (hash k)) (a a) (b b) (t t))
  (precedes ((1 1) (0 1)))
  (non-orig (privk "sign" b))
  (operation encryption-test (added-strand resp3 2)
    (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
      (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
        (pubk "encr" t)) (privk "sign" b)) (0 1))
  (traces
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b))))
    ((recv
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))))))
  (label 19)
  (parent 17)
  (unrealized)
  (shape)
  (maps ((0) ((b b) (t t) (m m) (r r) (k k) (a a))))
  (origs))

(comment "Nothing left to do")

(defprotocol wang basic
  (defrole init1
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b))) (send (cat k r))))
  (defrole init2
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (defrole init3
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))))
  (defrole init4
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (defrole init5
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))))
  (defrole resp1
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (recv
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b))) (recv (cat k r))))
  (defrole resp2
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (recv
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))
          (enc "rcrq" (cat a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b)))) (recv (cat k r))))
  (defrole resp3
    (vars (a b t name) (e1 e2 x mesg))
    (trace
      (recv
        (cat (cat a b t (hash e1) x) e1 e2
          (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))))
      (send
        (cat (cat a b t (hash e1) x) e2
          (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))
          (enc "eortag" (hash a b t (hash e1) x) e2 (privk "sign" b))))
      (send
        (cat (cat a b t (hash e1) x) e2
          (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))
          (enc "eortag" (hash a b t (hash e1) x) e2 (privk "sign" b))
          (enc "rcrq" (cat a b t (hash e1) x) e2 (privk "sign" b))))
      (recv
        (enc "abcf" (enc "abrq" a b t (hash e1) x (privk "sign" a))
          (privk "sign" t)))))
  (defrole ttp-ab1
    (vars (a b t name) (y x mesg))
    (trace (recv (enc "abrq" a b t y x (privk "sign" a)))
      (send (cat "sync-abrq" (enc "abrq" a b t y x (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y x (privk "sign" a))
          (privk "sign" t)))))
  (defrole ttp-ab2
    (vars (a b t name) (y x e mesg))
    (trace (recv (enc "abrq" a b t y x (privk "sign" a)))
      (recv
        (cat "sync-abrq"
          (enc "eortag" (hash a b t y x) e (privk "sign" b))))
      (send (enc "eortag" (hash a b t y x) e (privk "sign" b)))))
  (defrole ttp-rc1
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "rcrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (send
        (cat "sync-rc-req" (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "rcrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b)))) (send (cat k r))))
  (defrole ttp-rc2
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "rcrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (recv
        (cat "sync-rc-req"
          (enc "abrq" a b t y (hash k) (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (defrole ttp-cf1
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (send
        (cat "sync-cf-req" (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (send
        (enc (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b)) (privk "sign" t)))))
  (defrole ttp-cf2
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (recv
        (cat "sync-cf-req"
          (enc "abrq" a b t y (hash k) (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y (hash k) (privk "sign" a))
          (privk "sign" t))))))

(defskeleton wang
  (vars (r text) (m data) (a t b name) (k skey))
  (defstrand resp1 3 (r r) (m m) (a a) (b b) (t t) (k k))
  (non-orig (privk "sign" a))
  (comment "First of two experiments to prove Lemma 4.2, clause 2.")
  (traces
    ((recv
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b))) (recv (cat k r))))
  (label 20)
  (unrealized (0 0))
  (origs)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton wang
  (vars (r text) (m data) (a t b name) (k skey))
  (defstrand resp1 3 (r r) (m m) (a a) (b b) (t t) (k k))
  (defstrand init5 1 (r r) (m m) (a a) (b b) (t t) (k k))
  (precedes ((1 0) (0 0)))
  (non-orig (privk "sign" a))
  (operation encryption-test (added-strand init5 1)
    (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
      (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
        (pubk "encr" t)) (privk "sign" a)) (0 0))
  (traces
    ((recv
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b))) (recv (cat k r)))
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))))
  (label 21)
  (parent 20)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (t t) (m m) (r r) (k k) (b b))))
  (origs))

(comment "Nothing left to do")

(defprotocol wang basic
  (defrole init1
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b))) (send (cat k r))))
  (defrole init2
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (defrole init3
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))))
  (defrole init4
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (defrole init5
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))))
  (defrole resp1
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (recv
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b))) (recv (cat k r))))
  (defrole resp2
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (recv
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))
          (enc "rcrq" (cat a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b)))) (recv (cat k r))))
  (defrole resp3
    (vars (a b t name) (e1 e2 x mesg))
    (trace
      (recv
        (cat (cat a b t (hash e1) x) e1 e2
          (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))))
      (send
        (cat (cat a b t (hash e1) x) e2
          (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))
          (enc "eortag" (hash a b t (hash e1) x) e2 (privk "sign" b))))
      (send
        (cat (cat a b t (hash e1) x) e2
          (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))
          (enc "eortag" (hash a b t (hash e1) x) e2 (privk "sign" b))
          (enc "rcrq" (cat a b t (hash e1) x) e2 (privk "sign" b))))
      (recv
        (enc "abcf" (enc "abrq" a b t (hash e1) x (privk "sign" a))
          (privk "sign" t)))))
  (defrole ttp-ab1
    (vars (a b t name) (y x mesg))
    (trace (recv (enc "abrq" a b t y x (privk "sign" a)))
      (send (cat "sync-abrq" (enc "abrq" a b t y x (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y x (privk "sign" a))
          (privk "sign" t)))))
  (defrole ttp-ab2
    (vars (a b t name) (y x e mesg))
    (trace (recv (enc "abrq" a b t y x (privk "sign" a)))
      (recv
        (cat "sync-abrq"
          (enc "eortag" (hash a b t y x) e (privk "sign" b))))
      (send (enc "eortag" (hash a b t y x) e (privk "sign" b)))))
  (defrole ttp-rc1
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "rcrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (send
        (cat "sync-rc-req" (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "rcrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b)))) (send (cat k r))))
  (defrole ttp-rc2
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "rcrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (recv
        (cat "sync-rc-req"
          (enc "abrq" a b t y (hash k) (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (defrole ttp-cf1
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (send
        (cat "sync-cf-req" (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (send
        (enc (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b)) (privk "sign" t)))))
  (defrole ttp-cf2
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (recv
        (cat "sync-cf-req"
          (enc "abrq" a b t y (hash k) (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y (hash k) (privk "sign" a))
          (privk "sign" t))))))

(defskeleton wang
  (vars (r text) (m data) (a t b name) (k skey))
  (defstrand resp2 4 (r r) (m m) (a a) (b b) (t t) (k k))
  (non-orig (privk "sign" a))
  (comment "Second of two experiments to prove Lemma 4.2, clause 2.")
  (traces
    ((recv
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))
          (enc "rcrq" (cat a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b)))) (recv (cat k r))))
  (label 22)
  (unrealized (0 0))
  (origs)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton wang
  (vars (r text) (m data) (a t b name) (k skey))
  (defstrand resp2 4 (r r) (m m) (a a) (b b) (t t) (k k))
  (defstrand init5 1 (r r) (m m) (a a) (b b) (t t) (k k))
  (precedes ((1 0) (0 0)))
  (non-orig (privk "sign" a))
  (operation encryption-test (added-strand init5 1)
    (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
      (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
        (pubk "encr" t)) (privk "sign" a)) (0 0))
  (traces
    ((recv
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))
          (enc "rcrq" (cat a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b)))) (recv (cat k r)))
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))))
  (label 23)
  (parent 22)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (t t) (m m) (r r) (k k) (b b))))
  (origs))

(comment "Nothing left to do")

(defprotocol wang basic
  (defrole init1
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b))) (send (cat k r))))
  (defrole init2
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (defrole init3
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))))
  (defrole init4
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (defrole init5
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))))
  (defrole resp1
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (recv
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b))) (recv (cat k r))))
  (defrole resp2
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (recv
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))
          (enc "rcrq" (cat a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b)))) (recv (cat k r))))
  (defrole resp3
    (vars (a b t name) (e1 e2 x mesg))
    (trace
      (recv
        (cat (cat a b t (hash e1) x) e1 e2
          (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))))
      (send
        (cat (cat a b t (hash e1) x) e2
          (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))
          (enc "eortag" (hash a b t (hash e1) x) e2 (privk "sign" b))))
      (send
        (cat (cat a b t (hash e1) x) e2
          (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))
          (enc "eortag" (hash a b t (hash e1) x) e2 (privk "sign" b))
          (enc "rcrq" (cat a b t (hash e1) x) e2 (privk "sign" b))))
      (recv
        (enc "abcf" (enc "abrq" a b t (hash e1) x (privk "sign" a))
          (privk "sign" t)))))
  (defrole ttp-ab1
    (vars (a b t name) (y x mesg))
    (trace (recv (enc "abrq" a b t y x (privk "sign" a)))
      (send (cat "sync-abrq" (enc "abrq" a b t y x (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y x (privk "sign" a))
          (privk "sign" t)))))
  (defrole ttp-ab2
    (vars (a b t name) (y x e mesg))
    (trace (recv (enc "abrq" a b t y x (privk "sign" a)))
      (recv
        (cat "sync-abrq"
          (enc "eortag" (hash a b t y x) e (privk "sign" b))))
      (send (enc "eortag" (hash a b t y x) e (privk "sign" b)))))
  (defrole ttp-rc1
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "rcrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (send
        (cat "sync-rc-req" (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "rcrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b)))) (send (cat k r))))
  (defrole ttp-rc2
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "rcrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (recv
        (cat "sync-rc-req"
          (enc "abrq" a b t y (hash k) (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (defrole ttp-cf1
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (send
        (cat "sync-cf-req" (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (send
        (enc (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b)) (privk "sign" t)))))
  (defrole ttp-cf2
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (recv
        (cat "sync-cf-req"
          (enc "abrq" a b t y (hash k) (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y (hash k) (privk "sign" a))
          (privk "sign" t))))))

(defskeleton wang
  (vars (e1 e2 x mesg) (a t b name))
  (defstrand resp3 4 (e1 e1) (e2 e2) (x x) (a a) (b b) (t t))
  (non-orig (privk "sign" a))
  (comment "Experiments to prove Lemma 4.2, clause 3.")
  (traces
    ((recv
       (cat (cat a b t (hash e1) x) e1 e2
         (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))))
      (send
        (cat (cat a b t (hash e1) x) e2
          (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))
          (enc "eortag" (hash a b t (hash e1) x) e2 (privk "sign" b))))
      (send
        (cat (cat a b t (hash e1) x) e2
          (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))
          (enc "eortag" (hash a b t (hash e1) x) e2 (privk "sign" b))
          (enc "rcrq" (cat a b t (hash e1) x) e2 (privk "sign" b))))
      (recv
        (enc "abcf" (enc "abrq" a b t (hash e1) x (privk "sign" a))
          (privk "sign" t)))))
  (label 24)
  (unrealized (0 0) (0 3))
  (origs)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton wang
  (vars (r text) (m data) (a t b name) (k skey))
  (defstrand resp3 4 (e1 (enc m k))
    (e2
      (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
        (pubk "encr" t))) (x (hash k)) (a a) (b b) (t t))
  (defstrand init5 1 (r r) (m m) (a a) (b b) (t t) (k k))
  (precedes ((1 0) (0 0)))
  (non-orig (privk "sign" a))
  (operation encryption-test (added-strand init5 1)
    (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
      (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
        (pubk "encr" t)) (privk "sign" a)) (0 0))
  (traces
    ((recv
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))
          (enc "rcrq" (cat a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))))
      (recv
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t))))
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))))
  (label 25)
  (parent 24)
  (unrealized (0 3))
  (comment "4 in cohort - 4 not yet seen"))

(defskeleton wang
  (vars (r text) (m data) (a t b name) (k skey))
  (defstrand resp3 4 (e1 (enc m k))
    (e2
      (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
        (pubk "encr" t))) (x (hash k)) (a a) (b b) (t t))
  (defstrand init3 2 (r r) (m m) (a a) (b b) (t t) (k k))
  (precedes ((1 0) (0 0)) ((1 1) (0 3)))
  (non-orig (privk "sign" a))
  (operation encryption-test (displaced 1 2 init3 2)
    (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)) (0 3))
  (traces
    ((recv
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))
          (enc "rcrq" (cat a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))))
      (recv
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t))))
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))))
  (label 26)
  (parent 25)
  (unrealized)
  (shape)
  (maps
    ((0)
      ((a a) (t t) (b b) (e1 (enc m k))
        (e2
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))) (x (hash k)))))
  (origs))

(defskeleton wang
  (vars (r r-0 text) (m data) (a t b name) (k skey))
  (defstrand resp3 4 (e1 (enc m k))
    (e2
      (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
        (pubk "encr" t))) (x (hash k)) (a a) (b b) (t t))
  (defstrand init5 1 (r r) (m m) (a a) (b b) (t t) (k k))
  (defstrand init3 2 (r r-0) (m m) (a a) (b b) (t t) (k k))
  (precedes ((1 0) (0 0)) ((2 1) (0 3)))
  (non-orig (privk "sign" a))
  (operation encryption-test (added-strand init3 2)
    (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)) (0 3))
  (traces
    ((recv
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))
          (enc "rcrq" (cat a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))))
      (recv
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t))))
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a)))))
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r-0
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r-0
             (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))))
  (label 27)
  (parent 25)
  (unrealized)
  (shape)
  (maps
    ((0)
      ((a a) (t t) (b b) (e1 (enc m k))
        (e2
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))) (x (hash k)))))
  (origs))

(defskeleton wang
  (vars (r text) (m data) (a t b name) (k skey))
  (defstrand resp3 4 (e1 (enc m k))
    (e2
      (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
        (pubk "encr" t))) (x (hash k)) (a a) (b b) (t t))
  (defstrand init5 3 (r r) (m m) (a a) (b b) (t t) (k k))
  (precedes ((1 0) (0 0)) ((1 2) (0 3)))
  (non-orig (privk "sign" a))
  (operation encryption-test (displaced 1 2 init5 3)
    (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)) (0 3))
  (traces
    ((recv
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))
          (enc "rcrq" (cat a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))))
      (recv
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t))))
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))))
  (label 28)
  (parent 25)
  (unrealized)
  (shape)
  (maps
    ((0)
      ((a a) (t t) (b b) (e1 (enc m k))
        (e2
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))) (x (hash k)))))
  (origs))

(defskeleton wang
  (vars (r r-0 text) (m data) (a t b name) (k skey))
  (defstrand resp3 4 (e1 (enc m k))
    (e2
      (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
        (pubk "encr" t))) (x (hash k)) (a a) (b b) (t t))
  (defstrand init5 1 (r r) (m m) (a a) (b b) (t t) (k k))
  (defstrand init5 3 (r r-0) (m m) (a a) (b b) (t t) (k k))
  (precedes ((1 0) (0 0)) ((2 2) (0 3)))
  (non-orig (privk "sign" a))
  (operation encryption-test (added-strand init5 3)
    (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)) (0 3))
  (traces
    ((recv
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))
          (enc "rcrq" (cat a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))))
      (recv
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t))))
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a)))))
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r-0
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r-0
             (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r-0
            (pubk "encr" t)) (privk "sign" b)))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))))
  (label 29)
  (parent 25)
  (unrealized)
  (shape)
  (maps
    ((0)
      ((a a) (t t) (b b) (e1 (enc m k))
        (e2
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))) (x (hash k)))))
  (origs))

(comment "Nothing left to do")

(defprotocol wang basic
  (defrole init1
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b))) (send (cat k r))))
  (defrole init2
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (defrole init3
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))))
  (defrole init4
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (defrole init5
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))))
  (defrole resp1
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (recv
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b))) (recv (cat k r))))
  (defrole resp2
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (recv
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))
          (enc "rcrq" (cat a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b)))) (recv (cat k r))))
  (defrole resp3
    (vars (a b t name) (e1 e2 x mesg))
    (trace
      (recv
        (cat (cat a b t (hash e1) x) e1 e2
          (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))))
      (send
        (cat (cat a b t (hash e1) x) e2
          (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))
          (enc "eortag" (hash a b t (hash e1) x) e2 (privk "sign" b))))
      (send
        (cat (cat a b t (hash e1) x) e2
          (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))
          (enc "eortag" (hash a b t (hash e1) x) e2 (privk "sign" b))
          (enc "rcrq" (cat a b t (hash e1) x) e2 (privk "sign" b))))
      (recv
        (enc "abcf" (enc "abrq" a b t (hash e1) x (privk "sign" a))
          (privk "sign" t)))))
  (defrole ttp-ab1
    (vars (a b t name) (y x mesg))
    (trace (recv (enc "abrq" a b t y x (privk "sign" a)))
      (send (cat "sync-abrq" (enc "abrq" a b t y x (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y x (privk "sign" a))
          (privk "sign" t)))))
  (defrole ttp-ab2
    (vars (a b t name) (y x e mesg))
    (trace (recv (enc "abrq" a b t y x (privk "sign" a)))
      (recv
        (cat "sync-abrq"
          (enc "eortag" (hash a b t y x) e (privk "sign" b))))
      (send (enc "eortag" (hash a b t y x) e (privk "sign" b)))))
  (defrole ttp-rc1
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "rcrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (send
        (cat "sync-rc-req" (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "rcrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b)))) (send (cat k r))))
  (defrole ttp-rc2
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "rcrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (recv
        (cat "sync-rc-req"
          (enc "abrq" a b t y (hash k) (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (defrole ttp-cf1
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (send
        (cat "sync-cf-req" (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (send
        (enc (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b)) (privk "sign" t)))))
  (defrole ttp-cf2
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (recv
        (cat "sync-cf-req"
          (enc "abrq" a b t y (hash k) (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y (hash k) (privk "sign" a))
          (privk "sign" t))))))

(defskeleton wang
  (vars (y x mesg) (a b t name))
  (deflistener
    (enc "abcf" (enc "abrq" a b t y x (privk "sign" a))
      (privk "sign" t)))
  (non-orig (privk "sign" t))
  (comment "Experiments to prove Lemma 4.3, clause 1.")
  (traces
    ((recv
       (enc "abcf" (enc "abrq" a b t y x (privk "sign" a))
         (privk "sign" t)))
      (send
        (enc "abcf" (enc "abrq" a b t y x (privk "sign" a))
          (privk "sign" t)))))
  (label 30)
  (unrealized (0 0))
  (origs)
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton wang
  (vars (y x mesg) (a b t name))
  (deflistener
    (enc "abcf" (enc "abrq" a b t y x (privk "sign" a))
      (privk "sign" t)))
  (defstrand ttp-ab1 3 (y y) (x x) (a a) (b b) (t t))
  (precedes ((1 2) (0 0)))
  (non-orig (privk "sign" t))
  (operation encryption-test (added-strand ttp-ab1 3)
    (enc "abcf" (enc "abrq" a b t y x (privk "sign" a))
      (privk "sign" t)) (0 0))
  (traces
    ((recv
       (enc "abcf" (enc "abrq" a b t y x (privk "sign" a))
         (privk "sign" t)))
      (send
        (enc "abcf" (enc "abrq" a b t y x (privk "sign" a))
          (privk "sign" t))))
    ((recv (enc "abrq" a b t y x (privk "sign" a)))
      (send (cat "sync-abrq" (enc "abrq" a b t y x (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y x (privk "sign" a))
          (privk "sign" t)))))
  (label 31)
  (parent 30)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (t t) (y y) (x x))))
  (origs))

(defskeleton wang
  (vars (y mesg) (r text) (a b t name) (k skey))
  (deflistener
    (enc "abcf" (enc "abrq" a b t y (hash k) (privk "sign" a))
      (privk "sign" t)))
  (defstrand ttp-rc2 3 (y y) (r r) (a a) (b b) (t t) (k k))
  (precedes ((1 2) (0 0)))
  (non-orig (privk "sign" t))
  (operation encryption-test (added-strand ttp-rc2 3)
    (enc "abcf" (enc "abrq" a b t y (hash k) (privk "sign" a))
      (privk "sign" t)) (0 0))
  (traces
    ((recv
       (enc "abcf" (enc "abrq" a b t y (hash k) (privk "sign" a))
         (privk "sign" t)))
      (send
        (enc "abcf" (enc "abrq" a b t y (hash k) (privk "sign" a))
          (privk "sign" t))))
    ((recv
       (cat (cat a b t y (hash k))
         (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
         (enc "eootag" (hash a b t y (hash k))
           (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
           (privk "sign" a))
         (enc "eortag" (hash a b t y (hash k))
           (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
           (privk "sign" b))
         (enc "rcrq" (cat a b t y (hash k))
           (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
           (privk "sign" b))))
      (recv
        (cat "sync-rc-req"
          (enc "abrq" a b t y (hash k) (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (label 32)
  (parent 30)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (t t) (y y) (x (hash k)))))
  (origs))

(defskeleton wang
  (vars (y mesg) (r text) (a b t name) (k skey))
  (deflistener
    (enc "abcf" (enc "abrq" a b t y (hash k) (privk "sign" a))
      (privk "sign" t)))
  (defstrand ttp-cf2 3 (y y) (r r) (a a) (b b) (t t) (k k))
  (precedes ((1 2) (0 0)))
  (non-orig (privk "sign" t))
  (operation encryption-test (added-strand ttp-cf2 3)
    (enc "abcf" (enc "abrq" a b t y (hash k) (privk "sign" a))
      (privk "sign" t)) (0 0))
  (traces
    ((recv
       (enc "abcf" (enc "abrq" a b t y (hash k) (privk "sign" a))
         (privk "sign" t)))
      (send
        (enc "abcf" (enc "abrq" a b t y (hash k) (privk "sign" a))
          (privk "sign" t))))
    ((recv
       (cat (cat a b t y (hash k))
         (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
         (enc "eootag" (hash a b t y (hash k))
           (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
           (privk "sign" a))
         (enc "eortag" (hash a b t y (hash k))
           (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
           (privk "sign" b))
         (enc "cfrq" (cat a b t y (hash k))
           (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
           (privk "sign" b))))
      (recv
        (cat "sync-cf-req"
          (enc "abrq" a b t y (hash k) (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (label 33)
  (parent 30)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (t t) (y y) (x (hash k)))))
  (origs))

(comment "Nothing left to do")

(defprotocol wang basic
  (defrole init1
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b))) (send (cat k r))))
  (defrole init2
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (defrole init3
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))))
  (defrole init4
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (defrole init5
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))))
  (defrole resp1
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (recv
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b))) (recv (cat k r))))
  (defrole resp2
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (recv
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))
          (enc "rcrq" (cat a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b)))) (recv (cat k r))))
  (defrole resp3
    (vars (a b t name) (e1 e2 x mesg))
    (trace
      (recv
        (cat (cat a b t (hash e1) x) e1 e2
          (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))))
      (send
        (cat (cat a b t (hash e1) x) e2
          (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))
          (enc "eortag" (hash a b t (hash e1) x) e2 (privk "sign" b))))
      (send
        (cat (cat a b t (hash e1) x) e2
          (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))
          (enc "eortag" (hash a b t (hash e1) x) e2 (privk "sign" b))
          (enc "rcrq" (cat a b t (hash e1) x) e2 (privk "sign" b))))
      (recv
        (enc "abcf" (enc "abrq" a b t (hash e1) x (privk "sign" a))
          (privk "sign" t)))))
  (defrole ttp-ab1
    (vars (a b t name) (y x mesg))
    (trace (recv (enc "abrq" a b t y x (privk "sign" a)))
      (send (cat "sync-abrq" (enc "abrq" a b t y x (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y x (privk "sign" a))
          (privk "sign" t)))))
  (defrole ttp-ab2
    (vars (a b t name) (y x e mesg))
    (trace (recv (enc "abrq" a b t y x (privk "sign" a)))
      (recv
        (cat "sync-abrq"
          (enc "eortag" (hash a b t y x) e (privk "sign" b))))
      (send (enc "eortag" (hash a b t y x) e (privk "sign" b)))))
  (defrole ttp-rc1
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "rcrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (send
        (cat "sync-rc-req" (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "rcrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b)))) (send (cat k r))))
  (defrole ttp-rc2
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "rcrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (recv
        (cat "sync-rc-req"
          (enc "abrq" a b t y (hash k) (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (defrole ttp-cf1
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (send
        (cat "sync-cf-req" (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (send
        (enc (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b)) (privk "sign" t)))))
  (defrole ttp-cf2
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (recv
        (cat "sync-cf-req"
          (enc "abrq" a b t y (hash k) (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y (hash k) (privk "sign" a))
          (privk "sign" t))))))

(defskeleton wang
  (vars (y x mesg) (a b t name))
  (defstrand ttp-ab1 3 (y y) (x x) (a a) (b b) (t t))
  (non-orig (privk "encr" t) (privk "sign" a))
  (comment "Experiment 1 to prove Lemma 4.3, clause 2.")
  (traces
    ((recv (enc "abrq" a b t y x (privk "sign" a)))
      (send (cat "sync-abrq" (enc "abrq" a b t y x (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y x (privk "sign" a))
          (privk "sign" t)))))
  (label 34)
  (unrealized (0 0))
  (origs)
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton wang
  (vars (r text) (m data) (a b t name) (k skey))
  (defstrand ttp-ab1 3 (y (hash (enc m k))) (x (hash k)) (a a) (b b)
    (t t))
  (defstrand init3 2 (r r) (m m) (a a) (b b) (t t) (k k))
  (precedes ((1 1) (0 0)))
  (non-orig (privk "encr" t) (privk "sign" a))
  (operation encryption-test (added-strand init3 2)
    (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)) (0 0))
  (traces
    ((recv
       (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (send
        (cat "sync-abrq"
          (enc "abrq" a b t (hash (enc m k)) (hash k)
            (privk "sign" a))))
      (send
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t))))
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))))
  (label 35)
  (parent 34)
  (unrealized)
  (shape)
  (maps ((0) ((y (hash (enc m k))) (x (hash k)) (a a) (b b) (t t))))
  (origs))

(defskeleton wang
  (vars (r text) (m data) (a b t name) (k skey))
  (defstrand ttp-ab1 3 (y (hash (enc m k))) (x (hash k)) (a a) (b b)
    (t t))
  (defstrand init5 3 (r r) (m m) (a a) (b b) (t t) (k k))
  (precedes ((1 2) (0 0)))
  (non-orig (privk "encr" t) (privk "sign" a))
  (operation encryption-test (added-strand init5 3)
    (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)) (0 0))
  (traces
    ((recv
       (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (send
        (cat "sync-abrq"
          (enc "abrq" a b t (hash (enc m k)) (hash k)
            (privk "sign" a))))
      (send
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t))))
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))))
  (label 36)
  (parent 34)
  (unrealized)
  (shape)
  (maps ((0) ((y (hash (enc m k))) (x (hash k)) (a a) (b b) (t t))))
  (origs))

(comment "Nothing left to do")

(defprotocol wang basic
  (defrole init1
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b))) (send (cat k r))))
  (defrole init2
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (defrole init3
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))))
  (defrole init4
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (defrole init5
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))))
  (defrole resp1
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (recv
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b))) (recv (cat k r))))
  (defrole resp2
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (recv
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))
          (enc "rcrq" (cat a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b)))) (recv (cat k r))))
  (defrole resp3
    (vars (a b t name) (e1 e2 x mesg))
    (trace
      (recv
        (cat (cat a b t (hash e1) x) e1 e2
          (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))))
      (send
        (cat (cat a b t (hash e1) x) e2
          (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))
          (enc "eortag" (hash a b t (hash e1) x) e2 (privk "sign" b))))
      (send
        (cat (cat a b t (hash e1) x) e2
          (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))
          (enc "eortag" (hash a b t (hash e1) x) e2 (privk "sign" b))
          (enc "rcrq" (cat a b t (hash e1) x) e2 (privk "sign" b))))
      (recv
        (enc "abcf" (enc "abrq" a b t (hash e1) x (privk "sign" a))
          (privk "sign" t)))))
  (defrole ttp-ab1
    (vars (a b t name) (y x mesg))
    (trace (recv (enc "abrq" a b t y x (privk "sign" a)))
      (send (cat "sync-abrq" (enc "abrq" a b t y x (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y x (privk "sign" a))
          (privk "sign" t)))))
  (defrole ttp-ab2
    (vars (a b t name) (y x e mesg))
    (trace (recv (enc "abrq" a b t y x (privk "sign" a)))
      (recv
        (cat "sync-abrq"
          (enc "eortag" (hash a b t y x) e (privk "sign" b))))
      (send (enc "eortag" (hash a b t y x) e (privk "sign" b)))))
  (defrole ttp-rc1
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "rcrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (send
        (cat "sync-rc-req" (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "rcrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b)))) (send (cat k r))))
  (defrole ttp-rc2
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "rcrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (recv
        (cat "sync-rc-req"
          (enc "abrq" a b t y (hash k) (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (defrole ttp-cf1
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (send
        (cat "sync-cf-req" (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (send
        (enc (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b)) (privk "sign" t)))))
  (defrole ttp-cf2
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (recv
        (cat "sync-cf-req"
          (enc "abrq" a b t y (hash k) (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y (hash k) (privk "sign" a))
          (privk "sign" t))))))

(defskeleton wang
  (vars (y mesg) (r text) (a b t name) (k skey))
  (defstrand ttp-rc2 3 (y y) (r r) (a a) (b b) (t t) (k k))
  (non-orig (privk "encr" t) (privk "sign" a))
  (comment "Experiment 2 to prove Lemma 4.3, clause 2.")
  (traces
    ((recv
       (cat (cat a b t y (hash k))
         (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
         (enc "eootag" (hash a b t y (hash k))
           (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
           (privk "sign" a))
         (enc "eortag" (hash a b t y (hash k))
           (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
           (privk "sign" b))
         (enc "rcrq" (cat a b t y (hash k))
           (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
           (privk "sign" b))))
      (recv
        (cat "sync-rc-req"
          (enc "abrq" a b t y (hash k) (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (label 37)
  (unrealized (0 0) (0 1))
  (origs)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton wang
  (vars (r text) (m data) (a b t name) (k skey))
  (defstrand ttp-rc2 3 (y (hash (enc m k))) (r r) (a a) (b b) (t t)
    (k k))
  (defstrand init5 1 (r r) (m m) (a a) (b b) (t t) (k k))
  (precedes ((1 0) (0 0)))
  (non-orig (privk "encr" t) (privk "sign" a))
  (operation encryption-test (added-strand init5 1)
    (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
      (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
        (pubk "encr" t)) (privk "sign" a)) (0 0))
  (traces
    ((recv
       (cat (cat a b t (hash (enc m k)) (hash k))
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))
         (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" b))
         (enc "rcrq" (cat a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" b))))
      (recv
        (cat "sync-rc-req"
          (enc "abrq" a b t (hash (enc m k)) (hash k)
            (privk "sign" a))))
      (send
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t))))
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))))
  (label 38)
  (parent 37)
  (unrealized (0 1))
  (comment "4 in cohort - 4 not yet seen"))

(defskeleton wang
  (vars (r text) (m data) (a b t name) (k skey))
  (defstrand ttp-rc2 3 (y (hash (enc m k))) (r r) (a a) (b b) (t t)
    (k k))
  (defstrand init3 2 (r r) (m m) (a a) (b b) (t t) (k k))
  (precedes ((1 0) (0 0)) ((1 1) (0 1)))
  (non-orig (privk "encr" t) (privk "sign" a))
  (operation encryption-test (displaced 1 2 init3 2)
    (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)) (0 1))
  (traces
    ((recv
       (cat (cat a b t (hash (enc m k)) (hash k))
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))
         (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" b))
         (enc "rcrq" (cat a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" b))))
      (recv
        (cat "sync-rc-req"
          (enc "abrq" a b t (hash (enc m k)) (hash k)
            (privk "sign" a))))
      (send
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t))))
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))))
  (label 39)
  (parent 38)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (t t) (r r) (k k) (y (hash (enc m k))))))
  (origs))

(defskeleton wang
  (vars (r r-0 text) (m data) (a b t name) (k skey))
  (defstrand ttp-rc2 3 (y (hash (enc m k))) (r r) (a a) (b b) (t t)
    (k k))
  (defstrand init5 1 (r r) (m m) (a a) (b b) (t t) (k k))
  (defstrand init3 2 (r r-0) (m m) (a a) (b b) (t t) (k k))
  (precedes ((1 0) (0 0)) ((2 1) (0 1)))
  (non-orig (privk "encr" t) (privk "sign" a))
  (operation encryption-test (added-strand init3 2)
    (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)) (0 1))
  (traces
    ((recv
       (cat (cat a b t (hash (enc m k)) (hash k))
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))
         (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" b))
         (enc "rcrq" (cat a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" b))))
      (recv
        (cat "sync-rc-req"
          (enc "abrq" a b t (hash (enc m k)) (hash k)
            (privk "sign" a))))
      (send
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t))))
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a)))))
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r-0
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r-0
             (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))))
  (label 40)
  (parent 38)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (t t) (r r) (k k) (y (hash (enc m k))))))
  (origs))

(defskeleton wang
  (vars (r text) (m data) (a b t name) (k skey))
  (defstrand ttp-rc2 3 (y (hash (enc m k))) (r r) (a a) (b b) (t t)
    (k k))
  (defstrand init5 3 (r r) (m m) (a a) (b b) (t t) (k k))
  (precedes ((1 0) (0 0)) ((1 2) (0 1)))
  (non-orig (privk "encr" t) (privk "sign" a))
  (operation encryption-test (displaced 1 2 init5 3)
    (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)) (0 1))
  (traces
    ((recv
       (cat (cat a b t (hash (enc m k)) (hash k))
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))
         (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" b))
         (enc "rcrq" (cat a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" b))))
      (recv
        (cat "sync-rc-req"
          (enc "abrq" a b t (hash (enc m k)) (hash k)
            (privk "sign" a))))
      (send
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t))))
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))))
  (label 41)
  (parent 38)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (t t) (r r) (k k) (y (hash (enc m k))))))
  (origs))

(defskeleton wang
  (vars (r r-0 text) (m data) (a b t name) (k skey))
  (defstrand ttp-rc2 3 (y (hash (enc m k))) (r r) (a a) (b b) (t t)
    (k k))
  (defstrand init5 1 (r r) (m m) (a a) (b b) (t t) (k k))
  (defstrand init5 3 (r r-0) (m m) (a a) (b b) (t t) (k k))
  (precedes ((1 0) (0 0)) ((2 2) (0 1)))
  (non-orig (privk "encr" t) (privk "sign" a))
  (operation encryption-test (added-strand init5 3)
    (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)) (0 1))
  (traces
    ((recv
       (cat (cat a b t (hash (enc m k)) (hash k))
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))
         (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" b))
         (enc "rcrq" (cat a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" b))))
      (recv
        (cat "sync-rc-req"
          (enc "abrq" a b t (hash (enc m k)) (hash k)
            (privk "sign" a))))
      (send
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t))))
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a)))))
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r-0
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r-0
             (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r-0
            (pubk "encr" t)) (privk "sign" b)))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))))
  (label 42)
  (parent 38)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (t t) (r r) (k k) (y (hash (enc m k))))))
  (origs))

(comment "Nothing left to do")

(defprotocol wang basic
  (defrole init1
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b))) (send (cat k r))))
  (defrole init2
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (defrole init3
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))))
  (defrole init4
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (defrole init5
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (send
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))))
  (defrole resp1
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (recv
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b))) (recv (cat k r))))
  (defrole resp2
    (vars (a b t name) (m data) (r text) (k skey))
    (trace
      (recv
        (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))))
      (send
        (cat (cat a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t))
          (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" a))
          (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b))
          (enc "rcrq" (cat a b t (hash (enc m k)) (hash k))
            (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
              (pubk "encr" t)) (privk "sign" b)))) (recv (cat k r))))
  (defrole resp3
    (vars (a b t name) (e1 e2 x mesg))
    (trace
      (recv
        (cat (cat a b t (hash e1) x) e1 e2
          (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))))
      (send
        (cat (cat a b t (hash e1) x) e2
          (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))
          (enc "eortag" (hash a b t (hash e1) x) e2 (privk "sign" b))))
      (send
        (cat (cat a b t (hash e1) x) e2
          (enc "eootag" (hash a b t (hash e1) x) e2 (privk "sign" a))
          (enc "eortag" (hash a b t (hash e1) x) e2 (privk "sign" b))
          (enc "rcrq" (cat a b t (hash e1) x) e2 (privk "sign" b))))
      (recv
        (enc "abcf" (enc "abrq" a b t (hash e1) x (privk "sign" a))
          (privk "sign" t)))))
  (defrole ttp-ab1
    (vars (a b t name) (y x mesg))
    (trace (recv (enc "abrq" a b t y x (privk "sign" a)))
      (send (cat "sync-abrq" (enc "abrq" a b t y x (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y x (privk "sign" a))
          (privk "sign" t)))))
  (defrole ttp-ab2
    (vars (a b t name) (y x e mesg))
    (trace (recv (enc "abrq" a b t y x (privk "sign" a)))
      (recv
        (cat "sync-abrq"
          (enc "eortag" (hash a b t y x) e (privk "sign" b))))
      (send (enc "eortag" (hash a b t y x) e (privk "sign" b)))))
  (defrole ttp-rc1
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "rcrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (send
        (cat "sync-rc-req" (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "rcrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b)))) (send (cat k r))))
  (defrole ttp-rc2
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "rcrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (recv
        (cat "sync-rc-req"
          (enc "abrq" a b t y (hash k) (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (defrole ttp-cf1
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (send
        (cat "sync-cf-req" (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (send
        (enc (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b)) (privk "sign" t)))))
  (defrole ttp-cf2
    (vars (a b t name) (r text) (k skey) (y mesg))
    (trace
      (recv
        (cat (cat a b t y (hash k))
          (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
          (enc "eootag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" a))
          (enc "eortag" (hash a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))
          (enc "cfrq" (cat a b t y (hash k))
            (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
            (privk "sign" b))))
      (recv
        (cat "sync-cf-req"
          (enc "abrq" a b t y (hash k) (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y (hash k) (privk "sign" a))
          (privk "sign" t))))))

(defskeleton wang
  (vars (y mesg) (r text) (a b t name) (k skey))
  (defstrand ttp-cf2 3 (y y) (r r) (a a) (b b) (t t) (k k))
  (non-orig (privk "encr" t) (privk "sign" a))
  (comment "Experiment 3 to prove Lemma 4.3, clause 2.")
  (traces
    ((recv
       (cat (cat a b t y (hash k))
         (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
         (enc "eootag" (hash a b t y (hash k))
           (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
           (privk "sign" a))
         (enc "eortag" (hash a b t y (hash k))
           (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
           (privk "sign" b))
         (enc "cfrq" (cat a b t y (hash k))
           (enc "keytag" (hash a b t y (hash k)) k r (pubk "encr" t))
           (privk "sign" b))))
      (recv
        (cat "sync-cf-req"
          (enc "abrq" a b t y (hash k) (privk "sign" a))))
      (send
        (enc "abcf" (enc "abrq" a b t y (hash k) (privk "sign" a))
          (privk "sign" t)))))
  (label 43)
  (unrealized (0 0) (0 1))
  (origs)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton wang
  (vars (r text) (m data) (a b t name) (k skey))
  (defstrand ttp-cf2 3 (y (hash (enc m k))) (r r) (a a) (b b) (t t)
    (k k))
  (defstrand init5 1 (r r) (m m) (a a) (b b) (t t) (k k))
  (precedes ((1 0) (0 0)))
  (non-orig (privk "encr" t) (privk "sign" a))
  (operation encryption-test (added-strand init5 1)
    (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
      (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
        (pubk "encr" t)) (privk "sign" a)) (0 0))
  (traces
    ((recv
       (cat (cat a b t (hash (enc m k)) (hash k))
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))
         (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" b))
         (enc "cfrq" (cat a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" b))))
      (recv
        (cat "sync-cf-req"
          (enc "abrq" a b t (hash (enc m k)) (hash k)
            (privk "sign" a))))
      (send
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t))))
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))))
  (label 44)
  (parent 43)
  (unrealized (0 1))
  (comment "4 in cohort - 4 not yet seen"))

(defskeleton wang
  (vars (r text) (m data) (a b t name) (k skey))
  (defstrand ttp-cf2 3 (y (hash (enc m k))) (r r) (a a) (b b) (t t)
    (k k))
  (defstrand init3 2 (r r) (m m) (a a) (b b) (t t) (k k))
  (precedes ((1 0) (0 0)) ((1 1) (0 1)))
  (non-orig (privk "encr" t) (privk "sign" a))
  (operation encryption-test (displaced 1 2 init3 2)
    (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)) (0 1))
  (traces
    ((recv
       (cat (cat a b t (hash (enc m k)) (hash k))
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))
         (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" b))
         (enc "cfrq" (cat a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" b))))
      (recv
        (cat "sync-cf-req"
          (enc "abrq" a b t (hash (enc m k)) (hash k)
            (privk "sign" a))))
      (send
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t))))
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))))
  (label 45)
  (parent 44)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (t t) (r r) (k k) (y (hash (enc m k))))))
  (origs))

(defskeleton wang
  (vars (r r-0 text) (m data) (a b t name) (k skey))
  (defstrand ttp-cf2 3 (y (hash (enc m k))) (r r) (a a) (b b) (t t)
    (k k))
  (defstrand init5 1 (r r) (m m) (a a) (b b) (t t) (k k))
  (defstrand init3 2 (r r-0) (m m) (a a) (b b) (t t) (k k))
  (precedes ((1 0) (0 0)) ((2 1) (0 1)))
  (non-orig (privk "encr" t) (privk "sign" a))
  (operation encryption-test (added-strand init3 2)
    (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)) (0 1))
  (traces
    ((recv
       (cat (cat a b t (hash (enc m k)) (hash k))
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))
         (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" b))
         (enc "cfrq" (cat a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" b))))
      (recv
        (cat "sync-cf-req"
          (enc "abrq" a b t (hash (enc m k)) (hash k)
            (privk "sign" a))))
      (send
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t))))
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a)))))
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r-0
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r-0
             (pubk "encr" t)) (privk "sign" a))))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))))
  (label 46)
  (parent 44)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (t t) (r r) (k k) (y (hash (enc m k))))))
  (origs))

(defskeleton wang
  (vars (r text) (m data) (a b t name) (k skey))
  (defstrand ttp-cf2 3 (y (hash (enc m k))) (r r) (a a) (b b) (t t)
    (k k))
  (defstrand init5 3 (r r) (m m) (a a) (b b) (t t) (k k))
  (precedes ((1 0) (0 0)) ((1 2) (0 1)))
  (non-orig (privk "encr" t) (privk "sign" a))
  (operation encryption-test (displaced 1 2 init5 3)
    (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)) (0 1))
  (traces
    ((recv
       (cat (cat a b t (hash (enc m k)) (hash k))
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))
         (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" b))
         (enc "cfrq" (cat a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" b))))
      (recv
        (cat "sync-cf-req"
          (enc "abrq" a b t (hash (enc m k)) (hash k)
            (privk "sign" a))))
      (send
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t))))
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
            (pubk "encr" t)) (privk "sign" b)))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))))
  (label 47)
  (parent 44)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (t t) (r r) (k k) (y (hash (enc m k))))))
  (origs))

(defskeleton wang
  (vars (r r-0 text) (m data) (a b t name) (k skey))
  (defstrand ttp-cf2 3 (y (hash (enc m k))) (r r) (a a) (b b) (t t)
    (k k))
  (defstrand init5 1 (r r) (m m) (a a) (b b) (t t) (k k))
  (defstrand init5 3 (r r-0) (m m) (a a) (b b) (t t) (k k))
  (precedes ((1 0) (0 0)) ((2 2) (0 1)))
  (non-orig (privk "encr" t) (privk "sign" a))
  (operation encryption-test (added-strand init5 3)
    (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)) (0 1))
  (traces
    ((recv
       (cat (cat a b t (hash (enc m k)) (hash k))
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a))
         (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" b))
         (enc "cfrq" (cat a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" b))))
      (recv
        (cat "sync-cf-req"
          (enc "abrq" a b t (hash (enc m k)) (hash k)
            (privk "sign" a))))
      (send
        (enc "abcf"
          (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a))
          (privk "sign" t))))
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r
             (pubk "encr" t)) (privk "sign" a)))))
    ((send
       (cat (cat a b t (hash (enc m k)) (hash k)) (enc m k)
         (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r-0
           (pubk "encr" t))
         (enc "eootag" (hash a b t (hash (enc m k)) (hash k))
           (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r-0
             (pubk "encr" t)) (privk "sign" a))))
      (recv
        (enc "eortag" (hash a b t (hash (enc m k)) (hash k))
          (enc "keytag" (hash a b t (hash (enc m k)) (hash k)) k r-0
            (pubk "encr" t)) (privk "sign" b)))
      (send
        (enc "abrq" a b t (hash (enc m k)) (hash k) (privk "sign" a)))))
  (label 48)
  (parent 44)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (t t) (r r) (k k) (y (hash (enc m k))))))
  (origs))

(comment "Nothing left to do")
