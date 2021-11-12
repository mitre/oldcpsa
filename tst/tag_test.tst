(herald "Tag Test File" (algebra diffie-hellman) (bound 12))

(comment "CPSA 3.6.8")
(comment "All input read from tst/tag_test.scm")

(defprotocol test diffie-hellman
  (defrole rtag
    (vars (a name) (t tag))
    (trace (send (enc "hello world" (ltk a a)))
      (recv (enc t (ltk a a))))
    (non-orig (ltk a a)))
  (defrole rtag_mesg
    (vars (a name) (t mesg))
    (trace (send (enc "hello world" (ltk a a)))
      (recv (enc t (ltk a a))))
    (non-orig (ltk a a)))
  (defrole stag
    (vars (k skey) (t tag))
    (trace (send (enc "init" t k)) (recv (enc t k)))
    (non-orig k)))

(defskeleton test
  (vars (a name) (t tag))
  (defstrand rtag 2 (a a) (t t))
  (non-orig (ltk a a))
  (traces
    ((send (enc "hello world" (ltk a a))) (recv (enc t (ltk a a)))))
  (label 0)
  (unrealized (0 1))
  (origs)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton test
  (vars (a name))
  (defstrand rtag 2 (a a) (t "hello world"))
  (non-orig (ltk a a))
  (operation encryption-test (displaced 1 0 rtag_mesg 1)
    (enc "hello world" (ltk a a)) (0 1))
  (traces
    ((send (enc "hello world" (ltk a a)))
      (recv (enc "hello world" (ltk a a)))))
  (label 1)
  (parent 0)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (t "hello world"))))
  (origs))

(comment "Nothing left to do")

(defprotocol test diffie-hellman
  (defrole rtag
    (vars (a name) (t tag))
    (trace (send (enc "hello world" (ltk a a)))
      (recv (enc t (ltk a a))))
    (non-orig (ltk a a)))
  (defrole rtag_mesg
    (vars (a name) (t mesg))
    (trace (send (enc "hello world" (ltk a a)))
      (recv (enc t (ltk a a))))
    (non-orig (ltk a a)))
  (defrole stag
    (vars (k skey) (t tag))
    (trace (send (enc "init" t k)) (recv (enc t k)))
    (non-orig k)))

(defskeleton test
  (vars (t mesg) (a name))
  (defstrand rtag_mesg 2 (t t) (a a))
  (non-orig (ltk a a))
  (traces
    ((send (enc "hello world" (ltk a a))) (recv (enc t (ltk a a)))))
  (label 2)
  (unrealized (0 1))
  (origs)
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton test
  (vars (a name))
  (defstrand rtag_mesg 2 (t "hello world") (a a))
  (non-orig (ltk a a))
  (operation encryption-test (displaced 1 0 rtag_mesg 1)
    (enc "hello world" (ltk a a)) (0 1))
  (traces
    ((send (enc "hello world" (ltk a a)))
      (recv (enc "hello world" (ltk a a)))))
  (label 3)
  (parent 2)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (t "hello world"))))
  (origs))

(defskeleton test
  (vars (a name) (t tag))
  (defstrand rtag_mesg 2 (t (cat "init" t)) (a a))
  (defstrand stag 1 (k (ltk a a)) (t t))
  (precedes ((1 0) (0 1)))
  (non-orig (ltk a a))
  (operation encryption-test (added-strand stag 1)
    (enc "init" t (ltk a a)) (0 1))
  (traces
    ((send (enc "hello world" (ltk a a)))
      (recv (enc "init" t (ltk a a))))
    ((send (enc "init" t (ltk a a)))))
  (label 4)
  (parent 2)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (t (cat "init" t)))))
  (origs))

(comment "Nothing left to do")

(defprotocol test diffie-hellman
  (defrole rtag
    (vars (a name) (t tag))
    (trace (send (enc "hello world" (ltk a a)))
      (recv (enc t (ltk a a))))
    (non-orig (ltk a a)))
  (defrole rtag_mesg
    (vars (a name) (t mesg))
    (trace (send (enc "hello world" (ltk a a)))
      (recv (enc t (ltk a a))))
    (non-orig (ltk a a)))
  (defrole stag
    (vars (k skey) (t tag))
    (trace (send (enc "init" t k)) (recv (enc t k)))
    (non-orig k)))

(defskeleton test
  (vars (k skey) (t tag))
  (defstrand stag 2 (k k) (t t))
  (non-orig k)
  (traces ((send (enc "init" t k)) (recv (enc t k))))
  (label 5)
  (unrealized (0 1))
  (origs)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton test
  (vars (a name))
  (defstrand stag 2 (k (ltk a a)) (t "hello world"))
  (defstrand rtag_mesg 1 (a a))
  (precedes ((1 0) (0 1)))
  (non-orig (ltk a a))
  (operation encryption-test (added-strand rtag_mesg 1)
    (enc "hello world" (ltk a a)) (0 1))
  (traces
    ((send (enc "init" "hello world" (ltk a a)))
      (recv (enc "hello world" (ltk a a))))
    ((send (enc "hello world" (ltk a a)))))
  (label 6)
  (parent 5)
  (unrealized)
  (shape)
  (maps ((0) ((k (ltk a a)) (t "hello world"))))
  (origs))

(comment "Nothing left to do")

(defprotocol test diffie-hellman
  (defrole rtag
    (vars (a name) (t tag))
    (trace (send (enc "hello world" (ltk a a)))
      (recv (enc t (ltk a a))))
    (non-orig (ltk a a)))
  (defrole rtag_mesg
    (vars (a name) (t mesg))
    (trace (send (enc "hello world" (ltk a a)))
      (recv (enc t (ltk a a))))
    (non-orig (ltk a a)))
  (defrole stag
    (vars (k skey) (t tag))
    (trace (send (enc "init" t k)) (recv (enc t k)))
    (non-orig k)))

(defskeleton test
  (vars (t mesg) (a name))
  (defstrand rtag_mesg 2 (t t) (a a))
  (non-orig (ltk a a))
  (uniq-orig "hello world")
  (traces
    ((send (enc "hello world" (ltk a a))) (recv (enc t (ltk a a)))))
  (label 7)
  (unrealized (0 1))
  (origs ("hello world" (0 0)))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton test
  (vars (a name))
  (defstrand rtag_mesg 2 (t "hello world") (a a))
  (non-orig (ltk a a))
  (uniq-orig "hello world")
  (operation encryption-test (displaced 1 0 rtag_mesg 1)
    (enc "hello world" (ltk a a)) (0 1))
  (traces
    ((send (enc "hello world" (ltk a a)))
      (recv (enc "hello world" (ltk a a)))))
  (label 8)
  (parent 7)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (t "hello world"))))
  (origs ("hello world" (0 0))))

(defskeleton test
  (vars (a name) (t tag))
  (defstrand rtag_mesg 2 (t (cat "init" t)) (a a))
  (defstrand stag 1 (k (ltk a a)) (t t))
  (precedes ((1 0) (0 1)))
  (non-orig (ltk a a))
  (uniq-orig "hello world")
  (operation encryption-test (added-strand stag 1)
    (enc "init" t (ltk a a)) (0 1))
  (traces
    ((send (enc "hello world" (ltk a a)))
      (recv (enc "init" t (ltk a a))))
    ((send (enc "init" t (ltk a a)))))
  (label 9)
  (parent 7)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (t (cat "init" t)))))
  (origs ("hello world" (0 0))))

(comment "Nothing left to do")
