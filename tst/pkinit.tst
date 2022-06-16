(herald "Kerberos PKINIT")

(comment "CPSA 3.6.11")
(comment "All input read from tst/pkinit.scm")

(defprotocol pkinit-flawed basic
  (defrole client
    (vars (c t as name) (n2 n1 text) (tc tk tgt data) (k ak skey))
    (trace (send (cat (enc tc n2 (privk c)) c t n1))
      (recv
        (cat (enc (enc k n2 (privk as)) (pubk c)) c tgt
          (enc ak n1 tk t k))))
    (uniq-orig n2 n1))
  (defrole auth
    (vars (c t as name) (n2 n1 text) (tc tk tgt data) (k ak skey))
    (trace (recv (cat (enc tc n2 (privk c)) c t n1))
      (send
        (cat (enc (enc k n2 (privk as)) (pubk c)) c tgt
          (enc ak n1 tk t k))))
    (uniq-orig k ak)))

(defskeleton pkinit-flawed
  (vars (n2 n1 text) (tc tk tgt data) (c as t name) (k ak skey))
  (defstrand client 2 (n2 n2) (n1 n1) (tc tc) (tk tk) (tgt tgt) (c c)
    (t t) (as as) (k k) (ak ak))
  (non-orig (privk c) (privk as))
  (uniq-orig n2 n1)
  (goals
    (forall ((c as name) (k skey) (z strd))
      (implies
        (and (p "client" z 2) (p "client" "c" z c)
          (p "client" "as" z as) (p "client" "k" z k) (non (privk as))
          (non (privk c)))
        (exists ((z-0 strd))
          (and (p "auth" z-0 2) (p "auth" "as" z-0 as)
            (p "auth" "k" z-0 k) (p "auth" "c" z-0 c))))))
  (traces
    ((send (cat (enc tc n2 (privk c)) c t n1))
      (recv
        (cat (enc (enc k n2 (privk as)) (pubk c)) c tgt
          (enc ak n1 tk t k)))))
  (label 0)
  (unrealized (0 1))
  (origs (n1 (0 0)) (n2 (0 0)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton pkinit-flawed
  (vars (n2 n1 n1-0 text) (tc tk tgt tc-0 tk-0 tgt-0 data)
    (c as t c-0 t-0 name) (k ak ak-0 skey))
  (defstrand client 2 (n2 n2) (n1 n1) (tc tc) (tk tk) (tgt tgt) (c c)
    (t t) (as as) (k k) (ak ak))
  (defstrand auth 2 (n2 n2) (n1 n1-0) (tc tc-0) (tk tk-0) (tgt tgt-0)
    (c c-0) (t t-0) (as as) (k k) (ak ak-0))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (non-orig (privk c) (privk as))
  (uniq-orig n2 n1 k ak-0)
  (operation encryption-test (added-strand auth 2) (enc k n2 (privk as))
    (0 1))
  (traces
    ((send (cat (enc tc n2 (privk c)) c t n1))
      (recv
        (cat (enc (enc k n2 (privk as)) (pubk c)) c tgt
          (enc ak n1 tk t k))))
    ((recv (cat (enc tc-0 n2 (privk c-0)) c-0 t-0 n1-0))
      (send
        (cat (enc (enc k n2 (privk as)) (pubk c-0)) c-0 tgt-0
          (enc ak-0 n1-0 tk-0 t-0 k)))))
  (label 1)
  (parent 0)
  (unrealized)
  (shape)
  (satisfies (no (c c) (as as) (k k) (z 0)))
  (maps
    ((0)
      ((c c) (as as) (k k) (t t) (n2 n2) (n1 n1) (tc tc) (tk tk)
        (tgt tgt) (ak ak))))
  (origs (k (1 1)) (ak-0 (1 1)) (n1 (0 0)) (n2 (0 0))))

(comment "Nothing left to do")

(defprotocol pkinit-fix1 basic
  (defrole client
    (vars (c t as name) (n2 n1 text) (tc tk tgt data) (k ak skey))
    (trace (send (cat (enc tc n2 (privk c)) c t n1))
      (recv
        (cat (enc (enc k n2 c (privk as)) (pubk c)) c tgt
          (enc ak n1 tk t k))))
    (uniq-orig n2 n1))
  (defrole auth
    (vars (c t as name) (n2 n1 text) (tc tk tgt data) (k ak skey))
    (trace (recv (cat (enc tc n2 (privk c)) c t n1))
      (send
        (cat (enc (enc k n2 c (privk as)) (pubk c)) c tgt
          (enc ak n1 tk t k))))
    (uniq-orig k ak)))

(defskeleton pkinit-fix1
  (vars (n2 n1 text) (tc tk tgt data) (c as t name) (k ak skey))
  (defstrand client 2 (n2 n2) (n1 n1) (tc tc) (tk tk) (tgt tgt) (c c)
    (t t) (as as) (k k) (ak ak))
  (non-orig (privk c) (privk as))
  (uniq-orig n2 n1)
  (goals
    (forall ((c as name) (k skey) (z strd))
      (implies
        (and (p "client" z 2) (p "client" "c" z c)
          (p "client" "as" z as) (p "client" "k" z k) (non (privk as))
          (non (privk c)))
        (exists ((z-0 strd))
          (and (p "auth" z-0 2) (p "auth" "as" z-0 as)
            (p "auth" "k" z-0 k) (p "auth" "c" z-0 c))))))
  (traces
    ((send (cat (enc tc n2 (privk c)) c t n1))
      (recv
        (cat (enc (enc k n2 c (privk as)) (pubk c)) c tgt
          (enc ak n1 tk t k)))))
  (label 2)
  (unrealized (0 1))
  (origs (n1 (0 0)) (n2 (0 0)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton pkinit-fix1
  (vars (n2 n1 n1-0 text) (tc tk tgt tc-0 tk-0 tgt-0 data)
    (c as t t-0 name) (k ak ak-0 skey))
  (defstrand client 2 (n2 n2) (n1 n1) (tc tc) (tk tk) (tgt tgt) (c c)
    (t t) (as as) (k k) (ak ak))
  (defstrand auth 2 (n2 n2) (n1 n1-0) (tc tc-0) (tk tk-0) (tgt tgt-0)
    (c c) (t t-0) (as as) (k k) (ak ak-0))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (non-orig (privk c) (privk as))
  (uniq-orig n2 n1 k ak-0)
  (operation encryption-test (added-strand auth 2)
    (enc k n2 c (privk as)) (0 1))
  (traces
    ((send (cat (enc tc n2 (privk c)) c t n1))
      (recv
        (cat (enc (enc k n2 c (privk as)) (pubk c)) c tgt
          (enc ak n1 tk t k))))
    ((recv (cat (enc tc-0 n2 (privk c)) c t-0 n1-0))
      (send
        (cat (enc (enc k n2 c (privk as)) (pubk c)) c tgt-0
          (enc ak-0 n1-0 tk-0 t-0 k)))))
  (label 3)
  (parent 2)
  (unrealized (0 1) (1 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton pkinit-fix1
  (vars (n2 n1 n1-0 text) (tc tk tgt tk-0 tgt-0 data) (c as t t-0 name)
    (k ak ak-0 skey))
  (defstrand client 2 (n2 n2) (n1 n1) (tc tc) (tk tk) (tgt tgt) (c c)
    (t t) (as as) (k k) (ak ak))
  (defstrand auth 2 (n2 n2) (n1 n1-0) (tc tc) (tk tk-0) (tgt tgt-0)
    (c c) (t t-0) (as as) (k k) (ak ak-0))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (non-orig (privk c) (privk as))
  (uniq-orig n2 n1 k ak-0)
  (operation encryption-test (displaced 2 0 client 1)
    (enc tc-0 n2 (privk c)) (1 0))
  (traces
    ((send (cat (enc tc n2 (privk c)) c t n1))
      (recv
        (cat (enc (enc k n2 c (privk as)) (pubk c)) c tgt
          (enc ak n1 tk t k))))
    ((recv (cat (enc tc n2 (privk c)) c t-0 n1-0))
      (send
        (cat (enc (enc k n2 c (privk as)) (pubk c)) c tgt-0
          (enc ak-0 n1-0 tk-0 t-0 k)))))
  (label 4)
  (parent 3)
  (unrealized (0 1))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton pkinit-fix1
  (vars (n2 n1 text) (tc tgt tk tgt-0 data) (c as t name) (k ak skey))
  (defstrand client 2 (n2 n2) (n1 n1) (tc tc) (tk tk) (tgt tgt) (c c)
    (t t) (as as) (k k) (ak ak))
  (defstrand auth 2 (n2 n2) (n1 n1) (tc tc) (tk tk) (tgt tgt-0) (c c)
    (t t) (as as) (k k) (ak ak))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (non-orig (privk c) (privk as))
  (uniq-orig n2 n1 k ak)
  (operation encryption-test (displaced 2 1 auth 2)
    (enc ak-0 n1-0 tk-0 t-0 k) (0 1))
  (traces
    ((send (cat (enc tc n2 (privk c)) c t n1))
      (recv
        (cat (enc (enc k n2 c (privk as)) (pubk c)) c tgt
          (enc ak n1 tk t k))))
    ((recv (cat (enc tc n2 (privk c)) c t n1))
      (send
        (cat (enc (enc k n2 c (privk as)) (pubk c)) c tgt-0
          (enc ak n1 tk t k)))))
  (label 5)
  (parent 4)
  (unrealized)
  (shape)
  (satisfies yes)
  (maps
    ((0)
      ((c c) (as as) (k k) (t t) (n2 n2) (n1 n1) (tc tc) (tk tk)
        (tgt tgt) (ak ak))))
  (origs (k (1 1)) (ak (1 1)) (n1 (0 0)) (n2 (0 0))))

(defskeleton pkinit-fix1
  (vars (n2 n1 n1-0 text) (tc tk tgt tk-0 tgt-0 data) (c as t t-0 name)
    (k ak ak-0 skey))
  (defstrand client 2 (n2 n2) (n1 n1) (tc tc) (tk tk) (tgt tgt) (c c)
    (t t) (as as) (k k) (ak ak))
  (defstrand auth 2 (n2 n2) (n1 n1-0) (tc tc) (tk tk-0) (tgt tgt-0)
    (c c) (t t-0) (as as) (k k) (ak ak-0))
  (deflistener k)
  (precedes ((0 0) (1 0)) ((1 1) (2 0)) ((2 1) (0 1)))
  (non-orig (privk c) (privk as))
  (uniq-orig n2 n1 k ak-0)
  (operation encryption-test (added-listener k) (enc ak n1 tk t k)
    (0 1))
  (traces
    ((send (cat (enc tc n2 (privk c)) c t n1))
      (recv
        (cat (enc (enc k n2 c (privk as)) (pubk c)) c tgt
          (enc ak n1 tk t k))))
    ((recv (cat (enc tc n2 (privk c)) c t-0 n1-0))
      (send
        (cat (enc (enc k n2 c (privk as)) (pubk c)) c tgt-0
          (enc ak-0 n1-0 tk-0 t-0 k)))) ((recv k) (send k)))
  (label 6)
  (parent 4)
  (unrealized (2 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton pkinit-fix1
  (vars (n2 n1 n1-0 text) (tc tk tgt tk-0 tgt-0 data) (c as t t-0 name)
    (ak ak-0 skey))
  (defstrand client 2 (n2 n2) (n1 n1) (tc tc) (tk tk) (tgt tgt) (c c)
    (t t) (as as) (k ak-0) (ak ak))
  (defstrand auth 2 (n2 n2) (n1 n1-0) (tc tc) (tk tk-0) (tgt tgt-0)
    (c c) (t t-0) (as as) (k ak-0) (ak ak-0))
  (deflistener ak-0)
  (precedes ((0 0) (1 0)) ((1 1) (2 0)) ((2 1) (0 1)))
  (non-orig (privk c) (privk as))
  (uniq-orig n2 n1 ak-0)
  (operation nonce-test (displaced 3 1 auth 2) k (2 0)
    (enc (enc k n2 c (privk as)) (pubk c)))
  (traces
    ((send (cat (enc tc n2 (privk c)) c t n1))
      (recv
        (cat (enc (enc ak-0 n2 c (privk as)) (pubk c)) c tgt
          (enc ak n1 tk t ak-0))))
    ((recv (cat (enc tc n2 (privk c)) c t-0 n1-0))
      (send
        (cat (enc (enc ak-0 n2 c (privk as)) (pubk c)) c tgt-0
          (enc ak-0 n1-0 tk-0 t-0 ak-0)))) ((recv ak-0) (send ak-0)))
  (label 7)
  (parent 6)
  (unrealized (2 0))
  (dead)
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol pkinit-fix2 basic
  (defrole client
    (vars (c t as name) (n2 n1 text) (tc tk tgt data) (k ak skey))
    (trace (send (cat (enc tc n2 (privk c)) c t n1))
      (recv
        (cat
          (enc (enc k (hash (enc tc n2 (privk c)) c t n1) (privk as))
            (pubk c)) c tgt (enc ak n1 tk t k))))
    (uniq-orig n2 n1))
  (defrole auth
    (vars (c t as name) (n2 n1 text) (tc tk tgt data) (k ak skey))
    (trace (recv (cat (enc tc n2 (privk c)) c t n1))
      (send
        (cat
          (enc (enc k (hash (enc tc n2 (privk c)) c t n1) (privk as))
            (pubk c)) c tgt (enc ak n1 tk t k))))
    (uniq-orig k ak)))

(defskeleton pkinit-fix2
  (vars (n2 n1 text) (tc tk tgt data) (c as t name) (k ak skey))
  (defstrand client 2 (n2 n2) (n1 n1) (tc tc) (tk tk) (tgt tgt) (c c)
    (t t) (as as) (k k) (ak ak))
  (non-orig (privk c) (privk as))
  (uniq-orig n2 n1)
  (goals
    (forall ((c as name) (k skey) (z strd))
      (implies
        (and (p "client" z 2) (p "client" "c" z c)
          (p "client" "as" z as) (p "client" "k" z k) (non (privk as))
          (non (privk c)))
        (exists ((z-0 strd))
          (and (p "auth" z-0 2) (p "auth" "as" z-0 as)
            (p "auth" "k" z-0 k) (p "auth" "c" z-0 c))))))
  (traces
    ((send (cat (enc tc n2 (privk c)) c t n1))
      (recv
        (cat
          (enc (enc k (hash (enc tc n2 (privk c)) c t n1) (privk as))
            (pubk c)) c tgt (enc ak n1 tk t k)))))
  (label 8)
  (unrealized (0 1))
  (origs (n1 (0 0)) (n2 (0 0)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton pkinit-fix2
  (vars (n2 n1 text) (tc tk tgt tk-0 tgt-0 data) (c as t name)
    (k ak ak-0 skey))
  (defstrand client 2 (n2 n2) (n1 n1) (tc tc) (tk tk) (tgt tgt) (c c)
    (t t) (as as) (k k) (ak ak))
  (defstrand auth 2 (n2 n2) (n1 n1) (tc tc) (tk tk-0) (tgt tgt-0) (c c)
    (t t) (as as) (k k) (ak ak-0))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (non-orig (privk c) (privk as))
  (uniq-orig n2 n1 k ak-0)
  (operation encryption-test (added-strand auth 2)
    (enc k (hash (enc tc n2 (privk c)) c t n1) (privk as)) (0 1))
  (traces
    ((send (cat (enc tc n2 (privk c)) c t n1))
      (recv
        (cat
          (enc (enc k (hash (enc tc n2 (privk c)) c t n1) (privk as))
            (pubk c)) c tgt (enc ak n1 tk t k))))
    ((recv (cat (enc tc n2 (privk c)) c t n1))
      (send
        (cat
          (enc (enc k (hash (enc tc n2 (privk c)) c t n1) (privk as))
            (pubk c)) c tgt-0 (enc ak-0 n1 tk-0 t k)))))
  (label 9)
  (parent 8)
  (unrealized (0 1))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton pkinit-fix2
  (vars (n2 n1 text) (tc tgt tk tgt-0 data) (c as t name) (k ak skey))
  (defstrand client 2 (n2 n2) (n1 n1) (tc tc) (tk tk) (tgt tgt) (c c)
    (t t) (as as) (k k) (ak ak))
  (defstrand auth 2 (n2 n2) (n1 n1) (tc tc) (tk tk) (tgt tgt-0) (c c)
    (t t) (as as) (k k) (ak ak))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (non-orig (privk c) (privk as))
  (uniq-orig n2 n1 k ak)
  (operation encryption-test (displaced 2 1 auth 2)
    (enc ak-0 n1 tk-0 t k) (0 1))
  (traces
    ((send (cat (enc tc n2 (privk c)) c t n1))
      (recv
        (cat
          (enc (enc k (hash (enc tc n2 (privk c)) c t n1) (privk as))
            (pubk c)) c tgt (enc ak n1 tk t k))))
    ((recv (cat (enc tc n2 (privk c)) c t n1))
      (send
        (cat
          (enc (enc k (hash (enc tc n2 (privk c)) c t n1) (privk as))
            (pubk c)) c tgt-0 (enc ak n1 tk t k)))))
  (label 10)
  (parent 9)
  (unrealized)
  (shape)
  (satisfies yes)
  (maps
    ((0)
      ((c c) (as as) (k k) (t t) (n2 n2) (n1 n1) (tc tc) (tk tk)
        (tgt tgt) (ak ak))))
  (origs (k (1 1)) (ak (1 1)) (n1 (0 0)) (n2 (0 0))))

(defskeleton pkinit-fix2
  (vars (n2 n1 text) (tc tk tgt tk-0 tgt-0 data) (c as t name)
    (k ak ak-0 skey))
  (defstrand client 2 (n2 n2) (n1 n1) (tc tc) (tk tk) (tgt tgt) (c c)
    (t t) (as as) (k k) (ak ak))
  (defstrand auth 2 (n2 n2) (n1 n1) (tc tc) (tk tk-0) (tgt tgt-0) (c c)
    (t t) (as as) (k k) (ak ak-0))
  (deflistener k)
  (precedes ((0 0) (1 0)) ((1 1) (2 0)) ((2 1) (0 1)))
  (non-orig (privk c) (privk as))
  (uniq-orig n2 n1 k ak-0)
  (operation encryption-test (added-listener k) (enc ak n1 tk t k)
    (0 1))
  (traces
    ((send (cat (enc tc n2 (privk c)) c t n1))
      (recv
        (cat
          (enc (enc k (hash (enc tc n2 (privk c)) c t n1) (privk as))
            (pubk c)) c tgt (enc ak n1 tk t k))))
    ((recv (cat (enc tc n2 (privk c)) c t n1))
      (send
        (cat
          (enc (enc k (hash (enc tc n2 (privk c)) c t n1) (privk as))
            (pubk c)) c tgt-0 (enc ak-0 n1 tk-0 t k))))
    ((recv k) (send k)))
  (label 11)
  (parent 9)
  (unrealized (2 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton pkinit-fix2
  (vars (n2 n1 text) (tc tk tgt tk-0 tgt-0 data) (c as t name)
    (ak ak-0 skey))
  (defstrand client 2 (n2 n2) (n1 n1) (tc tc) (tk tk) (tgt tgt) (c c)
    (t t) (as as) (k ak-0) (ak ak))
  (defstrand auth 2 (n2 n2) (n1 n1) (tc tc) (tk tk-0) (tgt tgt-0) (c c)
    (t t) (as as) (k ak-0) (ak ak-0))
  (deflistener ak-0)
  (precedes ((0 0) (1 0)) ((1 1) (2 0)) ((2 1) (0 1)))
  (non-orig (privk c) (privk as))
  (uniq-orig n2 n1 ak-0)
  (operation nonce-test (displaced 3 1 auth 2) k (2 0)
    (enc (enc k (hash (enc tc n2 (privk c)) c t n1) (privk as))
      (pubk c)))
  (traces
    ((send (cat (enc tc n2 (privk c)) c t n1))
      (recv
        (cat
          (enc (enc ak-0 (hash (enc tc n2 (privk c)) c t n1) (privk as))
            (pubk c)) c tgt (enc ak n1 tk t ak-0))))
    ((recv (cat (enc tc n2 (privk c)) c t n1))
      (send
        (cat
          (enc (enc ak-0 (hash (enc tc n2 (privk c)) c t n1) (privk as))
            (pubk c)) c tgt-0 (enc ak-0 n1 tk-0 t ak-0))))
    ((recv ak-0) (send ak-0)))
  (label 12)
  (parent 11)
  (unrealized (2 0))
  (dead)
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol pkinit-flawed basic
  (defrole client
    (vars (c t as name) (n2 n1 text) (tc tk tgt data) (k ak skey))
    (trace (send (cat (enc tc n2 (privk c)) c t n1))
      (recv
        (cat (enc (enc k n2 (privk as)) (pubk c)) c tgt
          (enc ak n1 tk t k))))
    (uniq-orig n2 n1))
  (defrole auth
    (vars (c t as name) (n2 n1 text) (tc tk tgt data) (k ak skey))
    (trace (recv (cat (enc tc n2 (privk c)) c t n1))
      (send
        (cat (enc (enc k n2 (privk as)) (pubk c)) c tgt
          (enc ak n1 tk t k))))
    (uniq-orig k ak)))

(defskeleton pkinit-flawed
  (vars (n2 n1 n2-0 n1-0 text) (tc tk tgt tc-0 tk-0 tgt-0 data)
    (c c-0 as t as-0 t-0 name) (k ak ak-0 skey))
  (defstrand auth 2 (n2 n2) (n1 n1) (tc tc) (tk tk) (tgt tgt) (c c-0)
    (t t) (as as-0) (k k) (ak ak))
  (defstrand client 2 (n2 n2-0) (n1 n1-0) (tc tc-0) (tk tk-0)
    (tgt tgt-0) (c c) (t t-0) (as as) (k k) (ak ak-0))
  (non-orig (privk c) (privk as))
  (uniq-orig n2-0 n1-0 k ak)
  (goals
    (forall ((c c-0 as name) (k skey) (z z-0 strd))
      (implies
        (and (p "client" z 2) (p "client" "c" z c)
          (p "client" "as" z as) (p "client" "k" z k) (p "auth" z-0 2)
          (p "auth" "k" z-0 k) (p "auth" "c" z-0 c-0) (non (privk as))
          (non (privk c))) (= c c-0))))
  (traces
    ((recv (cat (enc tc n2 (privk c-0)) c-0 t n1))
      (send
        (cat (enc (enc k n2 (privk as-0)) (pubk c-0)) c-0 tgt
          (enc ak n1 tk t k))))
    ((send (cat (enc tc-0 n2-0 (privk c)) c t-0 n1-0))
      (recv
        (cat (enc (enc k n2-0 (privk as)) (pubk c)) c tgt-0
          (enc ak-0 n1-0 tk-0 t-0 k)))))
  (label 13)
  (unrealized (1 1))
  (preskeleton)
  (origs (n1-0 (1 0)) (n2-0 (1 0)) (k (0 1)) (ak (0 1)))
  (comment "Not a skeleton"))

(defskeleton pkinit-flawed
  (vars (n2 n1 n2-0 n1-0 text) (tc tk tgt tc-0 tk-0 tgt-0 data)
    (c c-0 as t as-0 t-0 name) (k ak ak-0 skey))
  (defstrand auth 2 (n2 n2) (n1 n1) (tc tc) (tk tk) (tgt tgt) (c c-0)
    (t t) (as as-0) (k k) (ak ak))
  (defstrand client 2 (n2 n2-0) (n1 n1-0) (tc tc-0) (tk tk-0)
    (tgt tgt-0) (c c) (t t-0) (as as) (k k) (ak ak-0))
  (precedes ((0 1) (1 1)))
  (non-orig (privk c) (privk as))
  (uniq-orig n2-0 n1-0 k ak)
  (goals
    (forall ((c c-0 as name) (k skey) (z z-0 strd))
      (implies
        (and (p "client" z 2) (p "client" "c" z c)
          (p "client" "as" z as) (p "client" "k" z k) (p "auth" z-0 2)
          (p "auth" "k" z-0 k) (p "auth" "c" z-0 c-0) (non (privk as))
          (non (privk c))) (= c c-0))))
  (traces
    ((recv (cat (enc tc n2 (privk c-0)) c-0 t n1))
      (send
        (cat (enc (enc k n2 (privk as-0)) (pubk c-0)) c-0 tgt
          (enc ak n1 tk t k))))
    ((send (cat (enc tc-0 n2-0 (privk c)) c t-0 n1-0))
      (recv
        (cat (enc (enc k n2-0 (privk as)) (pubk c)) c tgt-0
          (enc ak-0 n1-0 tk-0 t-0 k)))))
  (label 14)
  (parent 13)
  (unrealized (1 1))
  (origs (n1-0 (1 0)) (n2-0 (1 0)) (k (0 1)) (ak (0 1)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton pkinit-flawed
  (vars (n2 n1 n1-0 text) (tc tk tgt tc-0 tk-0 tgt-0 data)
    (c c-0 t as t-0 name) (k ak ak-0 skey))
  (defstrand auth 2 (n2 n2) (n1 n1) (tc tc) (tk tk) (tgt tgt) (c c-0)
    (t t) (as as) (k k) (ak ak))
  (defstrand client 2 (n2 n2) (n1 n1-0) (tc tc-0) (tk tk-0) (tgt tgt-0)
    (c c) (t t-0) (as as) (k k) (ak ak-0))
  (precedes ((0 1) (1 1)) ((1 0) (0 0)))
  (non-orig (privk c) (privk as))
  (uniq-orig n2 n1-0 k ak)
  (operation encryption-test (displaced 2 0 auth 2)
    (enc k n2-0 (privk as-0)) (1 1))
  (traces
    ((recv (cat (enc tc n2 (privk c-0)) c-0 t n1))
      (send
        (cat (enc (enc k n2 (privk as)) (pubk c-0)) c-0 tgt
          (enc ak n1 tk t k))))
    ((send (cat (enc tc-0 n2 (privk c)) c t-0 n1-0))
      (recv
        (cat (enc (enc k n2 (privk as)) (pubk c)) c tgt-0
          (enc ak-0 n1-0 tk-0 t-0 k)))))
  (label 15)
  (parent 14)
  (unrealized)
  (shape)
  (satisfies (no (c c) (c-0 c-0) (as as) (k k) (z 1) (z-0 0)))
  (maps
    ((0 1)
      ((c c) (c-0 c-0) (as as) (k k) (t t) (as-0 as) (n2 n2) (n1 n1)
        (tc tc) (tk tk) (tgt tgt) (ak ak) (t-0 t-0) (n2-0 n2)
        (n1-0 n1-0) (tc-0 tc-0) (tk-0 tk-0) (tgt-0 tgt-0) (ak-0 ak-0))))
  (origs (k (0 1)) (ak (0 1)) (n1-0 (1 0)) (n2 (1 0))))

(comment "Nothing left to do")

(defprotocol pkinit-fix1 basic
  (defrole client
    (vars (c t as name) (n2 n1 text) (tc tk tgt data) (k ak skey))
    (trace (send (cat (enc tc n2 (privk c)) c t n1))
      (recv
        (cat (enc (enc k n2 c (privk as)) (pubk c)) c tgt
          (enc ak n1 tk t k))))
    (uniq-orig n2 n1))
  (defrole auth
    (vars (c t as name) (n2 n1 text) (tc tk tgt data) (k ak skey))
    (trace (recv (cat (enc tc n2 (privk c)) c t n1))
      (send
        (cat (enc (enc k n2 c (privk as)) (pubk c)) c tgt
          (enc ak n1 tk t k))))
    (uniq-orig k ak)))

(defskeleton pkinit-fix1
  (vars (n2 n1 n2-0 n1-0 text) (tc tk tgt tc-0 tk-0 tgt-0 data)
    (c c-0 as t as-0 t-0 name) (k ak ak-0 skey))
  (defstrand auth 2 (n2 n2) (n1 n1) (tc tc) (tk tk) (tgt tgt) (c c-0)
    (t t) (as as-0) (k k) (ak ak))
  (defstrand client 2 (n2 n2-0) (n1 n1-0) (tc tc-0) (tk tk-0)
    (tgt tgt-0) (c c) (t t-0) (as as) (k k) (ak ak-0))
  (non-orig (privk c) (privk as))
  (uniq-orig n2-0 n1-0 k ak)
  (goals
    (forall ((c c-0 as name) (k skey) (z z-0 strd))
      (implies
        (and (p "client" z 2) (p "client" "c" z c)
          (p "client" "as" z as) (p "client" "k" z k) (p "auth" z-0 2)
          (p "auth" "k" z-0 k) (p "auth" "c" z-0 c-0) (non (privk as))
          (non (privk c))) (exists ((z-1 strd)) (p "client" z-1 2)))))
  (traces
    ((recv (cat (enc tc n2 (privk c-0)) c-0 t n1))
      (send
        (cat (enc (enc k n2 c-0 (privk as-0)) (pubk c-0)) c-0 tgt
          (enc ak n1 tk t k))))
    ((send (cat (enc tc-0 n2-0 (privk c)) c t-0 n1-0))
      (recv
        (cat (enc (enc k n2-0 c (privk as)) (pubk c)) c tgt-0
          (enc ak-0 n1-0 tk-0 t-0 k)))))
  (label 16)
  (unrealized (1 1))
  (preskeleton)
  (origs (n1-0 (1 0)) (n2-0 (1 0)) (k (0 1)) (ak (0 1)))
  (comment "Not a skeleton"))

(defskeleton pkinit-fix1
  (vars (n2 n1 n2-0 n1-0 text) (tc tk tgt tc-0 tk-0 tgt-0 data)
    (c c-0 as t as-0 t-0 name) (k ak ak-0 skey))
  (defstrand auth 2 (n2 n2) (n1 n1) (tc tc) (tk tk) (tgt tgt) (c c-0)
    (t t) (as as-0) (k k) (ak ak))
  (defstrand client 2 (n2 n2-0) (n1 n1-0) (tc tc-0) (tk tk-0)
    (tgt tgt-0) (c c) (t t-0) (as as) (k k) (ak ak-0))
  (precedes ((0 1) (1 1)))
  (non-orig (privk c) (privk as))
  (uniq-orig n2-0 n1-0 k ak)
  (goals
    (forall ((c c-0 as name) (k skey) (z z-0 strd))
      (implies
        (and (p "client" z 2) (p "client" "c" z c)
          (p "client" "as" z as) (p "client" "k" z k) (p "auth" z-0 2)
          (p "auth" "k" z-0 k) (p "auth" "c" z-0 c-0) (non (privk as))
          (non (privk c))) (exists ((z-1 strd)) (p "client" z-1 2)))))
  (traces
    ((recv (cat (enc tc n2 (privk c-0)) c-0 t n1))
      (send
        (cat (enc (enc k n2 c-0 (privk as-0)) (pubk c-0)) c-0 tgt
          (enc ak n1 tk t k))))
    ((send (cat (enc tc-0 n2-0 (privk c)) c t-0 n1-0))
      (recv
        (cat (enc (enc k n2-0 c (privk as)) (pubk c)) c tgt-0
          (enc ak-0 n1-0 tk-0 t-0 k)))))
  (label 17)
  (parent 16)
  (unrealized (1 1))
  (origs (n1-0 (1 0)) (n2-0 (1 0)) (k (0 1)) (ak (0 1)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton pkinit-fix1
  (vars (n2 n1 n1-0 text) (tc tk tgt tc-0 tk-0 tgt-0 data)
    (c t as t-0 name) (k ak ak-0 skey))
  (defstrand auth 2 (n2 n2) (n1 n1) (tc tc) (tk tk) (tgt tgt) (c c)
    (t t) (as as) (k k) (ak ak))
  (defstrand client 2 (n2 n2) (n1 n1-0) (tc tc-0) (tk tk-0) (tgt tgt-0)
    (c c) (t t-0) (as as) (k k) (ak ak-0))
  (precedes ((0 1) (1 1)) ((1 0) (0 0)))
  (non-orig (privk c) (privk as))
  (uniq-orig n2 n1-0 k ak)
  (operation encryption-test (displaced 2 0 auth 2)
    (enc k n2-0 c-0 (privk as-0)) (1 1))
  (traces
    ((recv (cat (enc tc n2 (privk c)) c t n1))
      (send
        (cat (enc (enc k n2 c (privk as)) (pubk c)) c tgt
          (enc ak n1 tk t k))))
    ((send (cat (enc tc-0 n2 (privk c)) c t-0 n1-0))
      (recv
        (cat (enc (enc k n2 c (privk as)) (pubk c)) c tgt-0
          (enc ak-0 n1-0 tk-0 t-0 k)))))
  (label 18)
  (parent 17)
  (unrealized (0 0) (1 1))
  (origs (k (0 1)) (ak (0 1)) (n1-0 (1 0)) (n2 (1 0)))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton pkinit-fix1
  (vars (n2 n1 text) (tc tk tgt tc-0 tgt-0 data) (c t as name)
    (k ak skey))
  (defstrand auth 2 (n2 n2) (n1 n1) (tc tc) (tk tk) (tgt tgt) (c c)
    (t t) (as as) (k k) (ak ak))
  (defstrand client 2 (n2 n2) (n1 n1) (tc tc-0) (tk tk) (tgt tgt-0)
    (c c) (t t) (as as) (k k) (ak ak))
  (precedes ((0 1) (1 1)) ((1 0) (0 0)))
  (non-orig (privk c) (privk as))
  (uniq-orig n2 n1 k ak)
  (operation encryption-test (displaced 2 0 auth 2)
    (enc ak-0 n1-0 tk-0 t-0 k) (1 1))
  (traces
    ((recv (cat (enc tc n2 (privk c)) c t n1))
      (send
        (cat (enc (enc k n2 c (privk as)) (pubk c)) c tgt
          (enc ak n1 tk t k))))
    ((send (cat (enc tc-0 n2 (privk c)) c t n1))
      (recv
        (cat (enc (enc k n2 c (privk as)) (pubk c)) c tgt-0
          (enc ak n1 tk t k)))))
  (label 19)
  (parent 18)
  (unrealized (0 0))
  (origs (k (0 1)) (ak (0 1)) (n1 (1 0)) (n2 (1 0)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton pkinit-fix1
  (vars (n2 n1 n1-0 text) (tc tk tgt tc-0 tk-0 tgt-0 data)
    (c t as t-0 name) (k ak ak-0 skey))
  (defstrand auth 2 (n2 n2) (n1 n1) (tc tc) (tk tk) (tgt tgt) (c c)
    (t t) (as as) (k k) (ak ak))
  (defstrand client 2 (n2 n2) (n1 n1-0) (tc tc-0) (tk tk-0) (tgt tgt-0)
    (c c) (t t-0) (as as) (k k) (ak ak-0))
  (deflistener k)
  (precedes ((0 1) (2 0)) ((1 0) (0 0)) ((2 1) (1 1)))
  (non-orig (privk c) (privk as))
  (uniq-orig n2 n1-0 k ak)
  (operation encryption-test (added-listener k)
    (enc ak-0 n1-0 tk-0 t-0 k) (1 1))
  (traces
    ((recv (cat (enc tc n2 (privk c)) c t n1))
      (send
        (cat (enc (enc k n2 c (privk as)) (pubk c)) c tgt
          (enc ak n1 tk t k))))
    ((send (cat (enc tc-0 n2 (privk c)) c t-0 n1-0))
      (recv
        (cat (enc (enc k n2 c (privk as)) (pubk c)) c tgt-0
          (enc ak-0 n1-0 tk-0 t-0 k)))) ((recv k) (send k)))
  (label 20)
  (parent 18)
  (unrealized (0 0) (2 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton pkinit-fix1
  (vars (n2 n1 text) (tk tgt tc tgt-0 data) (c t as name) (k ak skey))
  (defstrand auth 2 (n2 n2) (n1 n1) (tc tc) (tk tk) (tgt tgt) (c c)
    (t t) (as as) (k k) (ak ak))
  (defstrand client 2 (n2 n2) (n1 n1) (tc tc) (tk tk) (tgt tgt-0) (c c)
    (t t) (as as) (k k) (ak ak))
  (precedes ((0 1) (1 1)) ((1 0) (0 0)))
  (non-orig (privk c) (privk as))
  (uniq-orig n2 n1 k ak)
  (operation encryption-test (displaced 2 1 client 1)
    (enc tc-0 n2 (privk c)) (0 0))
  (traces
    ((recv (cat (enc tc n2 (privk c)) c t n1))
      (send
        (cat (enc (enc k n2 c (privk as)) (pubk c)) c tgt
          (enc ak n1 tk t k))))
    ((send (cat (enc tc n2 (privk c)) c t n1))
      (recv
        (cat (enc (enc k n2 c (privk as)) (pubk c)) c tgt-0
          (enc ak n1 tk t k)))))
  (label 21)
  (parent 19)
  (unrealized)
  (shape)
  (satisfies yes)
  (maps
    ((0 1)
      ((c c) (c-0 c) (as as) (k k) (t t) (as-0 as) (n2 n2) (n1 n1)
        (tc tc) (tk tk) (tgt tgt) (ak ak) (t-0 t) (n2-0 n2) (n1-0 n1)
        (tc-0 tc) (tk-0 tk) (tgt-0 tgt-0) (ak-0 ak))))
  (origs (n1 (1 0)) (n2 (1 0)) (k (0 1)) (ak (0 1))))

(defskeleton pkinit-fix1
  (vars (n2 n1 n1-0 text) (tc tk tgt tc-0 tk-0 tgt-0 data)
    (c t as t-0 name) (ak ak-0 skey))
  (defstrand auth 2 (n2 n2) (n1 n1) (tc tc) (tk tk) (tgt tgt) (c c)
    (t t) (as as) (k ak) (ak ak))
  (defstrand client 2 (n2 n2) (n1 n1-0) (tc tc-0) (tk tk-0) (tgt tgt-0)
    (c c) (t t-0) (as as) (k ak) (ak ak-0))
  (deflistener ak)
  (precedes ((0 1) (2 0)) ((1 0) (0 0)) ((2 1) (1 1)))
  (non-orig (privk c) (privk as))
  (uniq-orig n2 n1-0 ak)
  (operation nonce-test (displaced 3 0 auth 2) k (2 0)
    (enc (enc k n2 c (privk as)) (pubk c)))
  (traces
    ((recv (cat (enc tc n2 (privk c)) c t n1))
      (send
        (cat (enc (enc ak n2 c (privk as)) (pubk c)) c tgt
          (enc ak n1 tk t ak))))
    ((send (cat (enc tc-0 n2 (privk c)) c t-0 n1-0))
      (recv
        (cat (enc (enc ak n2 c (privk as)) (pubk c)) c tgt-0
          (enc ak-0 n1-0 tk-0 t-0 ak)))) ((recv ak) (send ak)))
  (label 22)
  (parent 20)
  (unrealized (0 0) (2 0))
  (dead)
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol pkinit-fix2 basic
  (defrole client
    (vars (c t as name) (n2 n1 text) (tc tk tgt data) (k ak skey))
    (trace (send (cat (enc tc n2 (privk c)) c t n1))
      (recv
        (cat
          (enc (enc k (hash (enc tc n2 (privk c)) c t n1) (privk as))
            (pubk c)) c tgt (enc ak n1 tk t k))))
    (uniq-orig n2 n1))
  (defrole auth
    (vars (c t as name) (n2 n1 text) (tc tk tgt data) (k ak skey))
    (trace (recv (cat (enc tc n2 (privk c)) c t n1))
      (send
        (cat
          (enc (enc k (hash (enc tc n2 (privk c)) c t n1) (privk as))
            (pubk c)) c tgt (enc ak n1 tk t k))))
    (uniq-orig k ak)))

(defskeleton pkinit-fix2
  (vars (n2 n1 n2-0 n1-0 text) (tc tk tgt tc-0 tk-0 tgt-0 data)
    (c c-0 as t as-0 t-0 name) (k ak ak-0 skey))
  (defstrand auth 2 (n2 n2) (n1 n1) (tc tc) (tk tk) (tgt tgt) (c c-0)
    (t t) (as as-0) (k k) (ak ak))
  (defstrand client 2 (n2 n2-0) (n1 n1-0) (tc tc-0) (tk tk-0)
    (tgt tgt-0) (c c) (t t-0) (as as) (k k) (ak ak-0))
  (non-orig (privk c) (privk as))
  (uniq-orig n2-0 n1-0 k ak)
  (goals
    (forall ((c c-0 as name) (k skey) (z z-0 strd))
      (implies
        (and (p "client" z 2) (p "client" "c" z c)
          (p "client" "as" z as) (p "client" "k" z k) (p "auth" z-0 2)
          (p "auth" "k" z-0 k) (p "auth" "c" z-0 c-0) (non (privk as))
          (non (privk c))) (exists ((z-1 strd)) (p "client" z-1 2)))))
  (traces
    ((recv (cat (enc tc n2 (privk c-0)) c-0 t n1))
      (send
        (cat
          (enc
            (enc k (hash (enc tc n2 (privk c-0)) c-0 t n1) (privk as-0))
            (pubk c-0)) c-0 tgt (enc ak n1 tk t k))))
    ((send (cat (enc tc-0 n2-0 (privk c)) c t-0 n1-0))
      (recv
        (cat
          (enc
            (enc k (hash (enc tc-0 n2-0 (privk c)) c t-0 n1-0)
              (privk as)) (pubk c)) c tgt-0
          (enc ak-0 n1-0 tk-0 t-0 k)))))
  (label 23)
  (unrealized (1 1))
  (preskeleton)
  (origs (n1-0 (1 0)) (n2-0 (1 0)) (k (0 1)) (ak (0 1)))
  (comment "Not a skeleton"))

(defskeleton pkinit-fix2
  (vars (n2 n1 n2-0 n1-0 text) (tc tk tgt tc-0 tk-0 tgt-0 data)
    (c c-0 as t as-0 t-0 name) (k ak ak-0 skey))
  (defstrand auth 2 (n2 n2) (n1 n1) (tc tc) (tk tk) (tgt tgt) (c c-0)
    (t t) (as as-0) (k k) (ak ak))
  (defstrand client 2 (n2 n2-0) (n1 n1-0) (tc tc-0) (tk tk-0)
    (tgt tgt-0) (c c) (t t-0) (as as) (k k) (ak ak-0))
  (precedes ((0 1) (1 1)))
  (non-orig (privk c) (privk as))
  (uniq-orig n2-0 n1-0 k ak)
  (goals
    (forall ((c c-0 as name) (k skey) (z z-0 strd))
      (implies
        (and (p "client" z 2) (p "client" "c" z c)
          (p "client" "as" z as) (p "client" "k" z k) (p "auth" z-0 2)
          (p "auth" "k" z-0 k) (p "auth" "c" z-0 c-0) (non (privk as))
          (non (privk c))) (exists ((z-1 strd)) (p "client" z-1 2)))))
  (traces
    ((recv (cat (enc tc n2 (privk c-0)) c-0 t n1))
      (send
        (cat
          (enc
            (enc k (hash (enc tc n2 (privk c-0)) c-0 t n1) (privk as-0))
            (pubk c-0)) c-0 tgt (enc ak n1 tk t k))))
    ((send (cat (enc tc-0 n2-0 (privk c)) c t-0 n1-0))
      (recv
        (cat
          (enc
            (enc k (hash (enc tc-0 n2-0 (privk c)) c t-0 n1-0)
              (privk as)) (pubk c)) c tgt-0
          (enc ak-0 n1-0 tk-0 t-0 k)))))
  (label 24)
  (parent 23)
  (unrealized (1 1))
  (origs (n1-0 (1 0)) (n2-0 (1 0)) (k (0 1)) (ak (0 1)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton pkinit-fix2
  (vars (n2 n1 text) (tc tk tgt tk-0 tgt-0 data) (c t as name)
    (k ak ak-0 skey))
  (defstrand auth 2 (n2 n2) (n1 n1) (tc tc) (tk tk) (tgt tgt) (c c)
    (t t) (as as) (k k) (ak ak))
  (defstrand client 2 (n2 n2) (n1 n1) (tc tc) (tk tk-0) (tgt tgt-0)
    (c c) (t t) (as as) (k k) (ak ak-0))
  (precedes ((0 1) (1 1)) ((1 0) (0 0)))
  (non-orig (privk c) (privk as))
  (uniq-orig n2 n1 k ak)
  (operation encryption-test (displaced 2 0 auth 2)
    (enc k (hash (enc tc-0 n2-0 (privk c-0)) c-0 t-0 n1-0) (privk as-0))
    (1 1))
  (traces
    ((recv (cat (enc tc n2 (privk c)) c t n1))
      (send
        (cat
          (enc (enc k (hash (enc tc n2 (privk c)) c t n1) (privk as))
            (pubk c)) c tgt (enc ak n1 tk t k))))
    ((send (cat (enc tc n2 (privk c)) c t n1))
      (recv
        (cat
          (enc (enc k (hash (enc tc n2 (privk c)) c t n1) (privk as))
            (pubk c)) c tgt-0 (enc ak-0 n1 tk-0 t k)))))
  (label 25)
  (parent 24)
  (unrealized (1 1))
  (origs (k (0 1)) (ak (0 1)) (n1 (1 0)) (n2 (1 0)))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton pkinit-fix2
  (vars (n2 n1 text) (tc tk tgt tgt-0 data) (c t as name) (k ak skey))
  (defstrand auth 2 (n2 n2) (n1 n1) (tc tc) (tk tk) (tgt tgt) (c c)
    (t t) (as as) (k k) (ak ak))
  (defstrand client 2 (n2 n2) (n1 n1) (tc tc) (tk tk) (tgt tgt-0) (c c)
    (t t) (as as) (k k) (ak ak))
  (precedes ((0 1) (1 1)) ((1 0) (0 0)))
  (non-orig (privk c) (privk as))
  (uniq-orig n2 n1 k ak)
  (operation encryption-test (displaced 2 0 auth 2)
    (enc ak-0 n1 tk-0 t k) (1 1))
  (traces
    ((recv (cat (enc tc n2 (privk c)) c t n1))
      (send
        (cat
          (enc (enc k (hash (enc tc n2 (privk c)) c t n1) (privk as))
            (pubk c)) c tgt (enc ak n1 tk t k))))
    ((send (cat (enc tc n2 (privk c)) c t n1))
      (recv
        (cat
          (enc (enc k (hash (enc tc n2 (privk c)) c t n1) (privk as))
            (pubk c)) c tgt-0 (enc ak n1 tk t k)))))
  (label 26)
  (parent 25)
  (unrealized)
  (shape)
  (satisfies yes)
  (maps
    ((0 1)
      ((c c) (c-0 c) (as as) (k k) (t t) (as-0 as) (n2 n2) (n1 n1)
        (tc tc) (tk tk) (tgt tgt) (ak ak) (t-0 t) (n2-0 n2) (n1-0 n1)
        (tc-0 tc) (tk-0 tk) (tgt-0 tgt-0) (ak-0 ak))))
  (origs (k (0 1)) (ak (0 1)) (n1 (1 0)) (n2 (1 0))))

(defskeleton pkinit-fix2
  (vars (n2 n1 text) (tc tk tgt tk-0 tgt-0 data) (c t as name)
    (k ak ak-0 skey))
  (defstrand auth 2 (n2 n2) (n1 n1) (tc tc) (tk tk) (tgt tgt) (c c)
    (t t) (as as) (k k) (ak ak))
  (defstrand client 2 (n2 n2) (n1 n1) (tc tc) (tk tk-0) (tgt tgt-0)
    (c c) (t t) (as as) (k k) (ak ak-0))
  (deflistener k)
  (precedes ((0 1) (2 0)) ((1 0) (0 0)) ((2 1) (1 1)))
  (non-orig (privk c) (privk as))
  (uniq-orig n2 n1 k ak)
  (operation encryption-test (added-listener k) (enc ak-0 n1 tk-0 t k)
    (1 1))
  (traces
    ((recv (cat (enc tc n2 (privk c)) c t n1))
      (send
        (cat
          (enc (enc k (hash (enc tc n2 (privk c)) c t n1) (privk as))
            (pubk c)) c tgt (enc ak n1 tk t k))))
    ((send (cat (enc tc n2 (privk c)) c t n1))
      (recv
        (cat
          (enc (enc k (hash (enc tc n2 (privk c)) c t n1) (privk as))
            (pubk c)) c tgt-0 (enc ak-0 n1 tk-0 t k))))
    ((recv k) (send k)))
  (label 27)
  (parent 25)
  (unrealized (2 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton pkinit-fix2
  (vars (n2 n1 text) (tc tk tgt tk-0 tgt-0 data) (c t as name)
    (ak ak-0 skey))
  (defstrand auth 2 (n2 n2) (n1 n1) (tc tc) (tk tk) (tgt tgt) (c c)
    (t t) (as as) (k ak) (ak ak))
  (defstrand client 2 (n2 n2) (n1 n1) (tc tc) (tk tk-0) (tgt tgt-0)
    (c c) (t t) (as as) (k ak) (ak ak-0))
  (deflistener ak)
  (precedes ((0 1) (2 0)) ((1 0) (0 0)) ((2 1) (1 1)))
  (non-orig (privk c) (privk as))
  (uniq-orig n2 n1 ak)
  (operation nonce-test (displaced 3 0 auth 2) k (2 0)
    (enc (enc k (hash (enc tc n2 (privk c)) c t n1) (privk as))
      (pubk c)))
  (traces
    ((recv (cat (enc tc n2 (privk c)) c t n1))
      (send
        (cat
          (enc (enc ak (hash (enc tc n2 (privk c)) c t n1) (privk as))
            (pubk c)) c tgt (enc ak n1 tk t ak))))
    ((send (cat (enc tc n2 (privk c)) c t n1))
      (recv
        (cat
          (enc (enc ak (hash (enc tc n2 (privk c)) c t n1) (privk as))
            (pubk c)) c tgt-0 (enc ak-0 n1 tk-0 t ak))))
    ((recv ak) (send ak)))
  (label 28)
  (parent 27)
  (unrealized (2 0))
  (dead)
  (comment "empty cohort"))

(comment "Nothing left to do")
