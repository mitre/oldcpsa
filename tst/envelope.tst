(herald "Envelope Protocol" (bound 20))

(comment "CPSA 3.6.6")
(comment "All input read from tst/envelope.scm")
(comment "Strand count bounded at 20")

(defprotocol envelope basic
  (defrole tpm-power-on (vars) (trace (init "0")))
  (defrole tpm-quote
    (vars (nonce pcr mesg) (aik akey))
    (trace (recv (cat "quote" nonce)) (obsv pcr)
      (send (enc "quote" pcr nonce aik)))
    (non-orig aik))
  (defrole tpm-extend-enc
    (vars (value state mesg) (esk skey) (tne tno data) (tpmkey akey))
    (trace (recv (cat "establish transport" tpmkey (enc esk tpmkey)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc value esk)) tno
          "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc value esk)))
            tne tno "false"))) (tran state (hash value state)))
    (uniq-orig tne))
  (defrole tpm-create-key
    (vars (k aik akey) (pcr mesg) (esk skey))
    (trace (recv (enc "create key" pcr esk))
      (send (enc "created" k pcr aik)))
    (non-orig (invk k) aik esk)
    (uniq-orig k))
  (defrole tpm-decrypt
    (vars (m pcr mesg) (k aik akey))
    (trace (recv (cat "decrypt" (enc m k)))
      (recv (enc "created" k pcr aik)) (obsv pcr) (send m))
    (non-orig aik))
  (defrole alice
    (vars (v n tne tno data) (esk1 esk skey) (k aik tpmkey akey)
      (pcr mesg))
    (trace (send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k)))
    (non-orig aik esk1 (invk tpmkey))
    (uniq-orig v n tno esk)
    (neq (tno n))))

(defskeleton envelope
  (vars (pcr mesg) (v n tne tno data) (esk esk1 skey)
    (k aik tpmkey akey))
  (deflistener esk)
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (neq (tno n))
  (non-orig esk1 aik (invk tpmkey))
  (uniq-orig v n tno esk)
  (traces ((recv esk) (send esk))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k))))
  (label 0)
  (unrealized (0 0) (1 5))
  (preskeleton)
  (origs (esk (1 0)) (n (1 2)) (tno (1 2)) (v (1 6)))
  (comment "Not a skeleton"))

(defskeleton envelope
  (vars (pcr mesg) (v n tne tno data) (esk esk1 skey)
    (k aik tpmkey akey))
  (deflistener esk)
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (precedes ((1 0) (0 0)))
  (neq (tno n))
  (non-orig esk1 aik (invk tpmkey))
  (uniq-orig v n tno esk)
  (traces ((recv esk) (send esk))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k))))
  (label 1)
  (parent 0)
  (unrealized (0 0) (1 5))
  (origs (esk (1 0)) (n (1 2)) (tno (1 2)) (v (1 6)))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton envelope
  (vars (pcr nonce mesg) (v n tne tno data) (esk esk1 skey)
    (k aik tpmkey aik-0 akey))
  (deflistener esk)
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (enc "created" k (hash "obtain" (hash n pcr)) aik))
    (aik aik-0))
  (precedes ((1 0) (0 0)) ((2 2) (1 5)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk tpmkey))
  (uniq-orig v n tno esk)
  (operation encryption-test (added-strand tpm-quote 3)
    (enc "created" k (hash "obtain" (hash n pcr)) aik) (1 5))
  (traces ((recv esk) (send esk))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k)))
    ((recv (cat "quote" nonce))
      (obsv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send
        (enc "quote" (enc "created" k (hash "obtain" (hash n pcr)) aik)
          nonce aik-0))))
  (label 2)
  (parent 1)
  (unrealized (0 0) (2 1))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (pcr mesg) (v n tne tno data) (esk esk1 esk-0 skey)
    (k aik tpmkey akey))
  (deflistener esk)
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n pcr)))
    (esk esk-0) (k k) (aik aik))
  (precedes ((1 0) (0 0)) ((2 1) (1 5)))
  (neq (tno n))
  (non-orig esk1 esk-0 aik (invk k) (invk tpmkey))
  (uniq-orig v n tno esk k)
  (operation encryption-test (added-strand tpm-create-key 2)
    (enc "created" k (hash "obtain" (hash n pcr)) aik) (1 5))
  (traces ((recv esk) (send esk))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n pcr)) esk-0))
      (send (enc "created" k (hash "obtain" (hash n pcr)) aik))))
  (label 3)
  (parent 1)
  (unrealized (0 0) (2 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton envelope
  (vars (pcr nonce mesg) (v n tne tno data) (esk esk1 esk-0 skey)
    (k aik tpmkey aik-0 akey))
  (deflistener esk)
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n pcr)))
    (esk esk-0) (k k) (aik aik))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (enc "create key" (hash "obtain" (hash n pcr)) esk-0))
    (aik aik-0))
  (precedes ((1 0) (0 0)) ((2 1) (1 5)) ((3 2) (2 0)))
  (neq (tno n))
  (non-orig esk1 esk-0 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno esk k)
  (operation encryption-test (added-strand tpm-quote 3)
    (enc "create key" (hash "obtain" (hash n pcr)) esk-0) (2 0))
  (traces ((recv esk) (send esk))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n pcr)) esk-0))
      (send (enc "created" k (hash "obtain" (hash n pcr)) aik)))
    ((recv (cat "quote" nonce))
      (obsv (enc "create key" (hash "obtain" (hash n pcr)) esk-0))
      (send
        (enc "quote"
          (enc "create key" (hash "obtain" (hash n pcr)) esk-0) nonce
          aik-0))))
  (label 4)
  (parent 3)
  (unrealized (0 0) (3 1))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (pcr mesg) (v n tne tno data) (esk esk1 skey)
    (k aik tpmkey akey))
  (deflistener esk)
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n pcr)))
    (esk esk1) (k k) (aik aik))
  (precedes ((1 0) (0 0)) ((1 4) (2 0)) ((2 1) (1 5)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tno esk k)
  (operation encryption-test (displaced 3 1 alice 5)
    (enc "create key" (hash "obtain" (hash n pcr)) esk-0) (2 0))
  (traces ((recv esk) (send esk))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (send (enc "created" k (hash "obtain" (hash n pcr)) aik))))
  (label 5)
  (parent 3)
  (unrealized (0 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton envelope
  (vars (pcr nonce mesg) (v n tne tno data) (esk esk1 skey)
    (k aik tpmkey aik-0 akey))
  (deflistener esk)
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n pcr)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-quote 3 (nonce nonce) (pcr esk) (aik aik-0))
  (precedes ((1 0) (3 1)) ((1 4) (2 0)) ((2 1) (1 5)) ((3 2) (0 0)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno esk k)
  (operation nonce-test (added-strand tpm-quote 3) esk (0 0)
    (enc esk tpmkey))
  (traces ((recv esk) (send esk))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (send (enc "created" k (hash "obtain" (hash n pcr)) aik)))
    ((recv (cat "quote" nonce)) (obsv esk)
      (send (enc "quote" esk nonce aik-0))))
  (label 6)
  (parent 5)
  (unrealized (3 1))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (pcr pcr-0 mesg) (v n tne tno data) (esk esk1 skey)
    (k aik tpmkey aik-0 akey))
  (deflistener esk)
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n pcr)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m esk) (pcr pcr-0) (k tpmkey) (aik aik-0))
  (precedes ((1 0) (3 0)) ((1 4) (2 0)) ((2 1) (1 5)) ((3 3) (0 0)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno esk k)
  (operation nonce-test (added-strand tpm-decrypt 4) esk (0 0)
    (enc esk tpmkey))
  (traces ((recv esk) (send esk))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (send (enc "created" k (hash "obtain" (hash n pcr)) aik)))
    ((recv (cat "decrypt" (enc esk tpmkey)))
      (recv (enc "created" tpmkey pcr-0 aik-0)) (obsv pcr-0)
      (send esk)))
  (label 7)
  (parent 5)
  (unrealized (3 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton envelope
  (vars (pcr pcr-0 nonce mesg) (v n tne tno data) (esk esk1 skey)
    (k aik tpmkey aik-0 aik-1 akey))
  (deflistener esk)
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n pcr)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m esk) (pcr pcr-0) (k tpmkey) (aik aik-0))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (enc "created" tpmkey pcr-0 aik-0)) (aik aik-1))
  (precedes ((1 0) (3 0)) ((1 4) (2 0)) ((2 1) (1 5)) ((3 3) (0 0))
    ((4 2) (3 1)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 aik-1 (invk k) (invk tpmkey))
  (uniq-orig v n tno esk k)
  (operation encryption-test (added-strand tpm-quote 3)
    (enc "created" tpmkey pcr-0 aik-0) (3 1))
  (traces ((recv esk) (send esk))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (send (enc "created" k (hash "obtain" (hash n pcr)) aik)))
    ((recv (cat "decrypt" (enc esk tpmkey)))
      (recv (enc "created" tpmkey pcr-0 aik-0)) (obsv pcr-0) (send esk))
    ((recv (cat "quote" nonce))
      (obsv (enc "created" tpmkey pcr-0 aik-0))
      (send
        (enc "quote" (enc "created" tpmkey pcr-0 aik-0) nonce aik-1))))
  (label 8)
  (parent 7)
  (unrealized (4 1))
  (dead)
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol envelope basic
  (defrole tpm-power-on (vars) (trace (init "0")))
  (defrole tpm-quote
    (vars (nonce pcr mesg) (aik akey))
    (trace (recv (cat "quote" nonce)) (obsv pcr)
      (send (enc "quote" pcr nonce aik)))
    (non-orig aik))
  (defrole tpm-extend-enc
    (vars (value state mesg) (esk skey) (tne tno data) (tpmkey akey))
    (trace (recv (cat "establish transport" tpmkey (enc esk tpmkey)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc value esk)) tno
          "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc value esk)))
            tne tno "false"))) (tran state (hash value state)))
    (uniq-orig tne))
  (defrole tpm-create-key
    (vars (k aik akey) (pcr mesg) (esk skey))
    (trace (recv (enc "create key" pcr esk))
      (send (enc "created" k pcr aik)))
    (non-orig (invk k) aik esk)
    (uniq-orig k))
  (defrole tpm-decrypt
    (vars (m pcr mesg) (k aik akey))
    (trace (recv (cat "decrypt" (enc m k)))
      (recv (enc "created" k pcr aik)) (obsv pcr) (send m))
    (non-orig aik))
  (defrole alice
    (vars (v n tne tno data) (esk1 esk skey) (k aik tpmkey akey)
      (pcr mesg))
    (trace (send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k)))
    (non-orig aik esk1 (invk tpmkey))
    (uniq-orig v n tno esk)
    (neq (tno n))))

(defskeleton envelope
  (vars (pcr mesg) (v n tne tno data) (esk1 esk skey)
    (k aik tpmkey akey))
  (deflistener (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (neq (tno n))
  (non-orig esk1 aik (invk tpmkey))
  (uniq-orig v n tno esk)
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k))))
  (label 9)
  (unrealized (0 0) (1 0) (2 5))
  (preskeleton)
  (origs (esk (2 0)) (n (2 2)) (tno (2 2)) (v (2 6)))
  (comment "Not a skeleton"))

(defskeleton envelope
  (vars (pcr mesg) (v n tne tno data) (esk1 esk skey)
    (k aik tpmkey akey))
  (deflistener (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (precedes ((2 6) (0 0)) ((2 6) (1 0)))
  (neq (tno n))
  (non-orig esk1 aik (invk tpmkey))
  (uniq-orig v n tno esk)
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k))))
  (label 10)
  (parent 9)
  (unrealized (0 0) (2 5))
  (origs (esk (2 0)) (n (2 2)) (tno (2 2)) (v (2 6)))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton envelope
  (vars (pcr nonce mesg) (v n tne tno data) (esk1 esk skey)
    (k aik tpmkey aik-0 akey))
  (deflistener (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (enc "created" k (hash "obtain" (hash n pcr)) aik))
    (aik aik-0))
  (precedes ((2 6) (0 0)) ((2 6) (1 0)) ((3 2) (2 5)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk tpmkey))
  (uniq-orig v n tno esk)
  (operation encryption-test (added-strand tpm-quote 3)
    (enc "created" k (hash "obtain" (hash n pcr)) aik) (2 5))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k)))
    ((recv (cat "quote" nonce))
      (obsv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send
        (enc "quote" (enc "created" k (hash "obtain" (hash n pcr)) aik)
          nonce aik-0))))
  (label 11)
  (parent 10)
  (unrealized (0 0) (3 1))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (pcr mesg) (v n tne tno data) (esk1 esk esk-0 skey)
    (k aik tpmkey akey))
  (deflistener (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n pcr)))
    (esk esk-0) (k k) (aik aik))
  (precedes ((2 6) (0 0)) ((2 6) (1 0)) ((3 1) (2 5)))
  (neq (tno n))
  (non-orig esk1 esk-0 aik (invk k) (invk tpmkey))
  (uniq-orig v n tno esk k)
  (operation encryption-test (added-strand tpm-create-key 2)
    (enc "created" k (hash "obtain" (hash n pcr)) aik) (2 5))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n pcr)) esk-0))
      (send (enc "created" k (hash "obtain" (hash n pcr)) aik))))
  (label 12)
  (parent 10)
  (unrealized (0 0) (1 0) (3 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton envelope
  (vars (pcr nonce mesg) (v n tne tno data) (esk1 esk esk-0 skey)
    (k aik tpmkey aik-0 akey))
  (deflistener (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n pcr)))
    (esk esk-0) (k k) (aik aik))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (enc "create key" (hash "obtain" (hash n pcr)) esk-0))
    (aik aik-0))
  (precedes ((2 6) (0 0)) ((2 6) (1 0)) ((3 1) (2 5)) ((4 2) (3 0)))
  (neq (tno n))
  (non-orig esk1 esk-0 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno esk k)
  (operation encryption-test (added-strand tpm-quote 3)
    (enc "create key" (hash "obtain" (hash n pcr)) esk-0) (3 0))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n pcr)) esk-0))
      (send (enc "created" k (hash "obtain" (hash n pcr)) aik)))
    ((recv (cat "quote" nonce))
      (obsv (enc "create key" (hash "obtain" (hash n pcr)) esk-0))
      (send
        (enc "quote"
          (enc "create key" (hash "obtain" (hash n pcr)) esk-0) nonce
          aik-0))))
  (label 13)
  (parent 12)
  (unrealized (0 0) (1 0) (4 1))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (pcr mesg) (v n tne tno data) (esk1 esk skey)
    (k aik tpmkey akey))
  (deflistener (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n pcr)))
    (esk esk1) (k k) (aik aik))
  (precedes ((2 4) (3 0)) ((2 6) (0 0)) ((2 6) (1 0)) ((3 1) (2 5)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tno esk k)
  (operation encryption-test (displaced 4 2 alice 5)
    (enc "create key" (hash "obtain" (hash n pcr)) esk-0) (3 0))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (send (enc "created" k (hash "obtain" (hash n pcr)) aik))))
  (label 14)
  (parent 12)
  (unrealized (0 0) (1 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton envelope
  (vars (pcr nonce mesg) (v n tne tno data) (esk1 esk skey)
    (k aik tpmkey aik-0 akey))
  (deflistener (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n pcr)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-quote 3 (nonce nonce) (pcr v) (aik aik-0))
  (precedes ((2 4) (3 0)) ((2 6) (0 0)) ((2 6) (4 1)) ((3 1) (2 5))
    ((4 2) (1 0)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno esk k)
  (operation nonce-test (added-strand tpm-quote 3) v (1 0) (enc v k))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (send (enc "created" k (hash "obtain" (hash n pcr)) aik)))
    ((recv (cat "quote" nonce)) (obsv v)
      (send (enc "quote" v nonce aik-0))))
  (label 15)
  (parent 14)
  (unrealized (0 0) (4 1))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (pcr pcr-0 mesg) (v n tne tno data) (esk1 esk skey)
    (k aik tpmkey aik-0 akey))
  (deflistener (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n pcr)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr pcr-0) (k k) (aik aik-0))
  (precedes ((2 4) (3 0)) ((2 6) (0 0)) ((2 6) (4 0)) ((3 1) (2 5))
    ((4 3) (1 0)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno esk k)
  (operation nonce-test (added-strand tpm-decrypt 4) v (1 0) (enc v k))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (send (enc "created" k (hash "obtain" (hash n pcr)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k pcr-0 aik-0)) (obsv pcr-0) (send v)))
  (label 16)
  (parent 14)
  (unrealized (0 0) (4 1))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton envelope
  (vars (pcr pcr-0 nonce mesg) (v n tne tno data) (esk1 esk skey)
    (k aik tpmkey aik-0 aik-1 akey))
  (deflistener (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n pcr)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr pcr-0) (k k) (aik aik-0))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (enc "created" k pcr-0 aik-0)) (aik aik-1))
  (precedes ((2 4) (3 0)) ((2 6) (0 0)) ((2 6) (4 0)) ((3 1) (2 5))
    ((3 1) (5 1)) ((4 3) (1 0)) ((5 2) (4 1)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 aik-1 (invk k) (invk tpmkey))
  (uniq-orig v n tno esk k)
  (operation encryption-test (added-strand tpm-quote 3)
    (enc "created" k pcr-0 aik-0) (4 1))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (send (enc "created" k (hash "obtain" (hash n pcr)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k pcr-0 aik-0)) (obsv pcr-0) (send v))
    ((recv (cat "quote" nonce)) (obsv (enc "created" k pcr-0 aik-0))
      (send (enc "quote" (enc "created" k pcr-0 aik-0) nonce aik-1))))
  (label 17)
  (parent 16)
  (unrealized (0 0) (5 1))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (pcr mesg) (v n tne tno data) (esk1 esk skey)
    (k aik tpmkey akey))
  (deflistener (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n pcr)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n pcr)))
    (k k) (aik aik))
  (precedes ((2 4) (3 0)) ((2 6) (0 0)) ((2 6) (4 0)) ((3 1) (2 5))
    ((4 3) (1 0)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tno esk k)
  (operation encryption-test (displaced 5 3 tpm-create-key 2)
    (enc "created" k pcr-0 aik-0) (4 1))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (send (enc "created" k (hash "obtain" (hash n pcr)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (obsv (hash "obtain" (hash n pcr))) (send v)))
  (label 18)
  (parent 16)
  (unrealized (0 0) (4 2))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton envelope
  (vars (pcr mesg) (v n tne tno tne-0 tno-0 data) (esk1 esk esk-0 skey)
    (k aik tpmkey tpmkey-0 akey))
  (deflistener (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n pcr)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n pcr)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n pcr))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (precedes ((2 4) (3 0)) ((2 6) (0 0)) ((2 6) (4 0)) ((3 1) (2 5))
    ((4 3) (1 0)) ((5 3) (4 2)))
  (leadsto ((5 3) (4 2)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 esk k)
  (operation state-passing-test (added-strand tpm-extend-enc 4)
    (hash "obtain" (hash n pcr)) (4 2))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (send (enc "created" k (hash "obtain" (hash n pcr)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (obsv (hash "obtain" (hash n pcr))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n pcr) (hash "obtain" (hash n pcr)))))
  (label 19)
  (parent 18)
  (unrealized (0 0) (5 3))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton envelope
  (vars (state mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 esk-1 skey) (k aik tpmkey tpmkey-0 tpmkey-1 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (precedes ((2 2) (6 2)) ((2 4) (3 0)) ((2 6) (0 0)) ((2 6) (4 0))
    ((3 1) (2 5)) ((4 3) (1 0)) ((5 3) (4 2)) ((6 3) (5 3)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation state-passing-test (added-strand tpm-extend-enc 4)
    (hash n state) (5 3))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state))))
  (label 20)
  (parent 19)
  (unrealized (0 0) (6 2))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton envelope
  (vars (state nonce mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 esk-1 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr
      (hash esk-1
        (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
        tno-1 "false")) (aik aik-0))
  (precedes ((2 2) (6 2)) ((2 4) (3 0)) ((2 6) (0 0)) ((2 6) (4 0))
    ((3 1) (2 5)) ((4 3) (1 0)) ((5 3) (4 2)) ((6 3) (5 3))
    ((7 2) (6 2)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation encryption-test (added-strand tpm-quote 3)
    (hash esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false") (6 2))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv (cat "quote" nonce))
      (obsv
        (hash esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false"))
      (send
        (enc "quote"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false") nonce aik-0))))
  (label 21)
  (parent 20)
  (unrealized (0 0) (6 2) (7 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton envelope
  (vars (state mesg) (v n tne tno tne-0 tno-0 data)
    (esk1 esk esk-0 skey) (k aik tpmkey tpmkey-0 tpmkey-1 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne)
    (tno tno) (esk esk) (tpmkey tpmkey-1))
  (precedes ((2 0) (6 0)) ((2 2) (6 2)) ((2 4) (3 0)) ((2 6) (0 0))
    ((2 6) (4 0)) ((3 1) (2 5)) ((4 3) (1 0)) ((5 3) (4 2))
    ((6 1) (2 1)) ((6 3) (5 3)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tne tno tne-0 esk k)
  (operation encryption-test (displaced 7 2 alice 3)
    (hash esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false") (6 2))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk tpmkey-1)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (tran state (hash n state))))
  (label 22)
  (parent 20)
  (unrealized (0 0) (6 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton envelope
  (vars (state mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 esk-1 skey) (k aik tpmkey tpmkey-0 tpmkey-1 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (precedes ((2 2) (6 2)) ((2 4) (3 0)) ((2 6) (0 0)) ((2 6) (4 0))
    ((3 1) (2 5)) ((4 3) (1 0)) ((5 3) (4 2)) ((6 1) (7 0))
    ((6 3) (5 3)) ((7 1) (6 2)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation encryption-test
    (added-listener
      (cat esk-1
        (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
        tno-1 "false"))
    (hash esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false") (6 2))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false"))))
  (label 23)
  (parent 20)
  (unrealized (0 0) (6 2) (7 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton envelope
  (vars (state nonce mesg)
    (v n tne tno tne-0 tno-0 tne-1 tno-1 tne-2 tno-2 data)
    (esk1 esk esk-0 esk-1 esk-2 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 tpmkey-2 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr
      (hash esk-1
        (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
        tno-1 "false")) (aik aik-0))
  (defstrand tpm-extend-enc 4 (value esk-1)
    (state
      (cat (hash "execute transport" (hash "extend" (enc n esk-1)))
        tne-1 tno-1 "false")) (tne tne-2) (tno tno-2) (esk esk-2)
    (tpmkey tpmkey-2))
  (precedes ((2 2) (6 2)) ((2 4) (3 0)) ((2 6) (0 0)) ((2 6) (4 0))
    ((3 1) (2 5)) ((4 3) (1 0)) ((5 3) (4 2)) ((6 1) (8 3))
    ((6 3) (5 3)) ((7 2) (6 2)) ((8 3) (7 1)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)) ((8 3) (7 1)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 tne-2 esk k)
  (operation state-passing-test (added-strand tpm-extend-enc 4)
    (hash esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false") (7 1))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv (cat "quote" nonce))
      (obsv
        (hash esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false"))
      (send
        (enc "quote"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false") nonce aik-0)))
    ((recv (cat "establish transport" tpmkey-2 (enc esk-2 tpmkey-2)))
      (send (cat "establish transport" tne-2))
      (recv
        (cat "execute transport" (cat "extend" (enc esk-1 esk-2)) tno-2
          "false"
          (hash esk-2
            (hash "execute transport" (hash "extend" (enc esk-1 esk-2)))
            tne-2 tno-2 "false")))
      (tran
        (cat (hash "execute transport" (hash "extend" (enc n esk-1)))
          tne-1 tno-1 "false")
        (hash esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false"))))
  (label 24)
  (parent 21)
  (unrealized (0 0) (6 2) (8 3))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (state mesg) (v n tne tno tne-0 tno-0 data)
    (esk1 esk esk-0 skey) (k aik tpmkey tpmkey-0 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne)
    (tno tno) (esk esk) (tpmkey tpmkey))
  (precedes ((2 0) (6 0)) ((2 2) (6 2)) ((2 4) (3 0)) ((2 6) (0 0))
    ((2 6) (4 0)) ((3 1) (2 5)) ((4 3) (1 0)) ((5 3) (4 2))
    ((6 1) (2 1)) ((6 3) (5 3)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tne tno tne-0 esk k)
  (operation nonce-test (contracted (tpmkey-1 tpmkey)) esk (6 0)
    (enc esk tpmkey))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey (enc esk tpmkey)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (tran state (hash n state))))
  (label 25)
  (parent 22)
  (unrealized (0 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton envelope
  (vars (state nonce mesg) (v n tne tno tne-0 tno-0 data)
    (esk1 esk esk-0 skey) (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne)
    (tno tno) (esk esk) (tpmkey tpmkey-1))
  (defstrand tpm-quote 3 (nonce nonce) (pcr esk) (aik aik-0))
  (precedes ((2 0) (7 1)) ((2 2) (6 2)) ((2 4) (3 0)) ((2 6) (0 0))
    ((2 6) (4 0)) ((3 1) (2 5)) ((4 3) (1 0)) ((5 3) (4 2))
    ((6 1) (2 1)) ((6 3) (5 3)) ((7 2) (6 0)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tne tno tne-0 esk k)
  (operation nonce-test (added-strand tpm-quote 3) esk (6 0)
    (enc esk tpmkey))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk tpmkey-1)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (tran state (hash n state)))
    ((recv (cat "quote" nonce)) (obsv esk)
      (send (enc "quote" esk nonce aik-0))))
  (label 26)
  (parent 22)
  (unrealized (0 0) (7 1))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (state pcr mesg) (v n tne tno tne-0 tno-0 data)
    (esk1 esk esk-0 skey) (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne)
    (tno tno) (esk esk) (tpmkey tpmkey-1))
  (defstrand tpm-decrypt 4 (m esk) (pcr pcr) (k tpmkey) (aik aik-0))
  (precedes ((2 0) (7 0)) ((2 2) (6 2)) ((2 4) (3 0)) ((2 6) (0 0))
    ((2 6) (4 0)) ((3 1) (2 5)) ((4 3) (1 0)) ((5 3) (4 2))
    ((6 1) (2 1)) ((6 3) (5 3)) ((7 3) (6 0)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tne tno tne-0 esk k)
  (operation nonce-test (added-strand tpm-decrypt 4) esk (6 0)
    (enc esk tpmkey))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk tpmkey-1)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (tran state (hash n state)))
    ((recv (cat "decrypt" (enc esk tpmkey)))
      (recv (enc "created" tpmkey pcr aik-0)) (obsv pcr) (send esk)))
  (label 27)
  (parent 22)
  (unrealized (0 0) (7 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton envelope
  (vars (state nonce mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 esk-1 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (hash "execute transport" (hash "extend" (enc n esk-1))))
    (aik aik-0))
  (precedes ((2 2) (6 2)) ((2 4) (3 0)) ((2 6) (0 0)) ((2 6) (4 0))
    ((3 1) (2 5)) ((4 3) (1 0)) ((5 3) (4 2)) ((6 1) (7 0))
    ((6 3) (5 3)) ((7 1) (6 2)) ((8 2) (7 0)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation encryption-test (added-strand tpm-quote 3)
    (hash "execute transport" (hash "extend" (enc n esk-1))) (7 0))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false")))
    ((recv (cat "quote" nonce))
      (obsv (hash "execute transport" (hash "extend" (enc n esk-1))))
      (send
        (enc "quote"
          (hash "execute transport" (hash "extend" (enc n esk-1))) nonce
          aik-0))))
  (label 28)
  (parent 23)
  (unrealized (0 0) (6 2) (8 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton envelope
  (vars (state mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 esk-1 skey) (k aik tpmkey tpmkey-0 tpmkey-1 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk-1))))
  (precedes ((2 2) (6 2)) ((2 4) (3 0)) ((2 6) (0 0)) ((2 6) (4 0))
    ((3 1) (2 5)) ((4 3) (1 0)) ((5 3) (4 2)) ((6 1) (7 0))
    ((6 3) (5 3)) ((7 1) (6 2)) ((8 1) (7 0)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation encryption-test
    (added-listener
      (cat "execute transport" (hash "extend" (enc n esk-1))))
    (hash "execute transport" (hash "extend" (enc n esk-1))) (7 0))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk-1))))
      (send (cat "execute transport" (hash "extend" (enc n esk-1))))))
  (label 29)
  (parent 23)
  (unrealized (0 0) (6 2) (8 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton envelope
  (vars (state mesg) (v n tne tno tne-0 tno-0 data)
    (esk1 esk esk-0 skey) (k aik tpmkey tpmkey-0 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne)
    (tno tno) (esk esk) (tpmkey tpmkey))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (precedes ((2 0) (6 0)) ((2 2) (6 2)) ((2 4) (3 0)) ((2 6) (4 0))
    ((2 6) (7 0)) ((3 1) (2 5)) ((4 3) (1 0)) ((5 3) (4 2))
    ((6 1) (2 1)) ((6 3) (5 3)) ((7 2) (0 0)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tne tno tne-0 esk k)
  (operation encryption-test (added-strand tpm-quote 3)
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik) (0 0))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey (enc esk tpmkey)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (tran state (hash n state)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send
        (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))))
  (label 30)
  (parent 25)
  (unrealized (7 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton envelope
  (vars (state nonce mesg) (v n tne tno tne-0 tno-0 data)
    (esk1 esk esk-0 skey) (k aik tpmkey tpmkey-0 aik-0 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne)
    (tno tno) (esk esk) (tpmkey tpmkey))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
    (aik aik-0))
  (precedes ((2 0) (6 0)) ((2 2) (6 2)) ((2 4) (3 0)) ((2 6) (4 0))
    ((2 6) (7 1)) ((3 1) (2 5)) ((4 3) (1 0)) ((5 3) (4 2))
    ((6 1) (2 1)) ((6 3) (5 3)) ((7 2) (0 0)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tne tno tne-0 esk k)
  (operation encryption-test (added-strand tpm-quote 3)
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik) (0 0))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey (enc esk tpmkey)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (tran state (hash n state)))
    ((recv (cat "quote" nonce))
      (obsv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send
        (enc "quote"
          (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)
          nonce aik-0))))
  (label 31)
  (parent 25)
  (unrealized (7 1))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (state pcr nonce mesg) (v n tne tno tne-0 tno-0 data)
    (esk1 esk esk-0 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 aik-1 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne)
    (tno tno) (esk esk) (tpmkey tpmkey-1))
  (defstrand tpm-decrypt 4 (m esk) (pcr pcr) (k tpmkey) (aik aik-0))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (enc "created" tpmkey pcr aik-0)) (aik aik-1))
  (precedes ((2 0) (7 0)) ((2 2) (6 2)) ((2 4) (3 0)) ((2 6) (0 0))
    ((2 6) (4 0)) ((3 1) (2 5)) ((4 3) (1 0)) ((5 3) (4 2))
    ((6 1) (2 1)) ((6 3) (5 3)) ((7 3) (6 0)) ((8 2) (7 1)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 aik-1 (invk k) (invk tpmkey))
  (uniq-orig v n tne tno tne-0 esk k)
  (operation encryption-test (added-strand tpm-quote 3)
    (enc "created" tpmkey pcr aik-0) (7 1))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk tpmkey-1)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (tran state (hash n state)))
    ((recv (cat "decrypt" (enc esk tpmkey)))
      (recv (enc "created" tpmkey pcr aik-0)) (obsv pcr) (send esk))
    ((recv (cat "quote" nonce)) (obsv (enc "created" tpmkey pcr aik-0))
      (send
        (enc "quote" (enc "created" tpmkey pcr aik-0) nonce aik-1))))
  (label 32)
  (parent 27)
  (unrealized (0 0) (8 1))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (state nonce mesg)
    (v n tne tno tne-0 tno-0 tne-1 tno-1 tne-2 tno-2 data)
    (esk1 esk esk-0 esk-1 esk-2 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 tpmkey-2 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (hash "execute transport" (hash "extend" (enc n esk-1))))
    (aik aik-0))
  (defstrand tpm-extend-enc 4 (value "execute transport")
    (state (hash "extend" (enc n esk-1))) (tne tne-2) (tno tno-2)
    (esk esk-2) (tpmkey tpmkey-2))
  (precedes ((2 2) (6 2)) ((2 4) (3 0)) ((2 6) (0 0)) ((2 6) (4 0))
    ((3 1) (2 5)) ((4 3) (1 0)) ((5 3) (4 2)) ((6 1) (7 0))
    ((6 3) (5 3)) ((7 1) (6 2)) ((8 2) (7 0)) ((9 3) (8 1)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)) ((9 3) (8 1)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 tne-2 esk k)
  (operation state-passing-test (added-strand tpm-extend-enc 4)
    (hash "execute transport" (hash "extend" (enc n esk-1))) (8 1))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false")))
    ((recv (cat "quote" nonce))
      (obsv (hash "execute transport" (hash "extend" (enc n esk-1))))
      (send
        (enc "quote"
          (hash "execute transport" (hash "extend" (enc n esk-1))) nonce
          aik-0)))
    ((recv (cat "establish transport" tpmkey-2 (enc esk-2 tpmkey-2)))
      (send (cat "establish transport" tne-2))
      (recv
        (cat "execute transport"
          (cat "extend" (enc "execute transport" esk-2)) tno-2 "false"
          (hash esk-2
            (hash "execute transport"
              (hash "extend" (enc "execute transport" esk-2))) tne-2
            tno-2 "false")))
      (tran (hash "extend" (enc n esk-1))
        (hash "execute transport" (hash "extend" (enc n esk-1))))))
  (label 33)
  (parent 28)
  (unrealized (0 0) (6 2) (9 3))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton envelope
  (vars (state nonce mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 esk-1 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk-1))))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (hash "extend" (enc n esk-1))) (aik aik-0))
  (precedes ((2 2) (6 2)) ((2 4) (3 0)) ((2 6) (0 0)) ((2 6) (4 0))
    ((3 1) (2 5)) ((4 3) (1 0)) ((5 3) (4 2)) ((6 1) (7 0))
    ((6 3) (5 3)) ((7 1) (6 2)) ((8 1) (7 0)) ((9 2) (8 0)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation encryption-test (added-strand tpm-quote 3)
    (hash "extend" (enc n esk-1)) (8 0))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk-1))))
      (send (cat "execute transport" (hash "extend" (enc n esk-1)))))
    ((recv (cat "quote" nonce)) (obsv (hash "extend" (enc n esk-1)))
      (send (enc "quote" (hash "extend" (enc n esk-1)) nonce aik-0))))
  (label 34)
  (parent 29)
  (unrealized (0 0) (6 2) (9 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton envelope
  (vars (state mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 esk-1 skey) (k aik tpmkey tpmkey-0 tpmkey-1 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk-1))))
  (deflistener (cat "extend" (enc n esk-1)))
  (precedes ((2 2) (9 0)) ((2 4) (3 0)) ((2 6) (0 0)) ((2 6) (4 0))
    ((3 1) (2 5)) ((4 3) (1 0)) ((5 3) (4 2)) ((6 1) (7 0))
    ((6 3) (5 3)) ((7 1) (6 2)) ((8 1) (7 0)) ((9 1) (8 0)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation encryption-test
    (added-listener (cat "extend" (enc n esk-1)))
    (hash "extend" (enc n esk-1)) (8 0))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk-1))))
      (send (cat "execute transport" (hash "extend" (enc n esk-1)))))
    ((recv (cat "extend" (enc n esk-1)))
      (send (cat "extend" (enc n esk-1)))))
  (label 35)
  (parent 29)
  (unrealized (0 0) (9 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton envelope
  (vars (state mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 esk-1 skey) (k aik tpmkey tpmkey-0 tpmkey-1 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne)
    (tno tno) (esk esk) (tpmkey tpmkey))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-1) (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (precedes ((2 0) (6 0)) ((2 2) (6 2)) ((2 4) (3 0)) ((2 6) (4 0))
    ((2 6) (7 0)) ((3 1) (2 5)) ((4 3) (1 0)) ((5 3) (4 2))
    ((6 1) (2 1)) ((6 3) (5 3)) ((7 2) (0 0)) ((8 3) (7 1)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)) ((8 3) (7 1)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tne tno tne-0 tne-1 esk k)
  (operation state-passing-test (added-strand tpm-extend-enc 4)
    (hash "refuse" (hash n state)) (7 1))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey (enc esk tpmkey)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (tran state (hash n state)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-1))
          tno-1 "false"
          (hash esk-1
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-1))) tne-1 tno-1
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state)))))
  (label 36)
  (parent 30)
  (unrealized (8 3))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton envelope
  (vars (state nonce mesg)
    (v n tne tno tne-0 tno-0 tne-1 tno-1 tne-2 tno-2 tne-3 tno-3 data)
    (esk1 esk esk-0 esk-1 esk-2 esk-3 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 tpmkey-2 tpmkey-3 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (hash "execute transport" (hash "extend" (enc n esk-1))))
    (aik aik-0))
  (defstrand tpm-extend-enc 4 (value "execute transport")
    (state (hash "extend" (enc n esk-1))) (tne tne-2) (tno tno-2)
    (esk esk-2) (tpmkey tpmkey-2))
  (defstrand tpm-extend-enc 4 (value "extend") (state (enc n esk-1))
    (tne tne-3) (tno tno-3) (esk esk-3) (tpmkey tpmkey-3))
  (precedes ((2 2) (10 3)) ((2 4) (3 0)) ((2 6) (0 0)) ((2 6) (4 0))
    ((3 1) (2 5)) ((4 3) (1 0)) ((5 3) (4 2)) ((6 1) (7 0))
    ((6 3) (5 3)) ((7 1) (6 2)) ((8 2) (7 0)) ((9 3) (8 1))
    ((10 3) (9 3)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)) ((9 3) (8 1)) ((10 3) (9 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 tne-2 tne-3 esk k)
  (operation state-passing-test (added-strand tpm-extend-enc 4)
    (hash "extend" (enc n esk-1)) (9 3))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false")))
    ((recv (cat "quote" nonce))
      (obsv (hash "execute transport" (hash "extend" (enc n esk-1))))
      (send
        (enc "quote"
          (hash "execute transport" (hash "extend" (enc n esk-1))) nonce
          aik-0)))
    ((recv (cat "establish transport" tpmkey-2 (enc esk-2 tpmkey-2)))
      (send (cat "establish transport" tne-2))
      (recv
        (cat "execute transport"
          (cat "extend" (enc "execute transport" esk-2)) tno-2 "false"
          (hash esk-2
            (hash "execute transport"
              (hash "extend" (enc "execute transport" esk-2))) tne-2
            tno-2 "false")))
      (tran (hash "extend" (enc n esk-1))
        (hash "execute transport" (hash "extend" (enc n esk-1)))))
    ((recv (cat "establish transport" tpmkey-3 (enc esk-3 tpmkey-3)))
      (send (cat "establish transport" tne-3))
      (recv
        (cat "execute transport" (cat "extend" (enc "extend" esk-3))
          tno-3 "false"
          (hash esk-3
            (hash "execute transport"
              (hash "extend" (enc "extend" esk-3))) tne-3 tno-3
            "false")))
      (tran (enc n esk-1) (hash "extend" (enc n esk-1)))))
  (label 37)
  (parent 33)
  (unrealized (0 0) (6 2) (10 3))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (state nonce mesg)
    (v n tne tno tne-0 tno-0 tne-1 tno-1 tne-2 tno-2 data)
    (esk1 esk esk-0 esk-1 esk-2 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 tpmkey-2 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk-1))))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (hash "extend" (enc n esk-1))) (aik aik-0))
  (defstrand tpm-extend-enc 4 (value "extend") (state (enc n esk-1))
    (tne tne-2) (tno tno-2) (esk esk-2) (tpmkey tpmkey-2))
  (precedes ((2 2) (10 3)) ((2 4) (3 0)) ((2 6) (0 0)) ((2 6) (4 0))
    ((3 1) (2 5)) ((4 3) (1 0)) ((5 3) (4 2)) ((6 1) (7 0))
    ((6 3) (5 3)) ((7 1) (6 2)) ((8 1) (7 0)) ((9 2) (8 0))
    ((10 3) (9 1)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)) ((10 3) (9 1)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 tne-2 esk k)
  (operation state-passing-test (added-strand tpm-extend-enc 4)
    (hash "extend" (enc n esk-1)) (9 1))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk-1))))
      (send (cat "execute transport" (hash "extend" (enc n esk-1)))))
    ((recv (cat "quote" nonce)) (obsv (hash "extend" (enc n esk-1)))
      (send (enc "quote" (hash "extend" (enc n esk-1)) nonce aik-0)))
    ((recv (cat "establish transport" tpmkey-2 (enc esk-2 tpmkey-2)))
      (send (cat "establish transport" tne-2))
      (recv
        (cat "execute transport" (cat "extend" (enc "extend" esk-2))
          tno-2 "false"
          (hash esk-2
            (hash "execute transport"
              (hash "extend" (enc "extend" esk-2))) tne-2 tno-2
            "false")))
      (tran (enc n esk-1) (hash "extend" (enc n esk-1)))))
  (label 38)
  (parent 34)
  (unrealized (0 0) (6 2) (10 3))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (state mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 skey) (k aik tpmkey tpmkey-0 tpmkey-1 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk) (tpmkey tpmkey-1))
  (deflistener
    (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk))))
  (deflistener (cat "extend" (enc n esk)))
  (precedes ((2 0) (6 0)) ((2 2) (9 0)) ((2 4) (3 0)) ((2 6) (0 0))
    ((2 6) (4 0)) ((3 1) (2 5)) ((4 3) (1 0)) ((5 3) (4 2))
    ((6 1) (7 0)) ((6 3) (5 3)) ((7 1) (6 2)) ((8 1) (7 0))
    ((9 1) (8 0)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation nonce-test (contracted (esk-1 esk)) n (9 0) (enc n esk))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno-1
          "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne-1
            tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
         tne-1 tno-1 "false"))
      (send
        (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
          tne-1 tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk))))
      (send (cat "execute transport" (hash "extend" (enc n esk)))))
    ((recv (cat "extend" (enc n esk)))
      (send (cat "extend" (enc n esk)))))
  (label 39)
  (parent 35)
  (unrealized (0 0) (6 0) (7 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton envelope
  (vars (state nonce mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 esk-1 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk-1))))
  (deflistener (cat "extend" (enc n esk-1)))
  (defstrand tpm-quote 3 (nonce nonce) (pcr n) (aik aik-0))
  (precedes ((2 2) (10 1)) ((2 4) (3 0)) ((2 6) (0 0)) ((2 6) (4 0))
    ((3 1) (2 5)) ((4 3) (1 0)) ((5 3) (4 2)) ((6 1) (7 0))
    ((6 3) (5 3)) ((7 1) (6 2)) ((8 1) (7 0)) ((9 1) (8 0))
    ((10 2) (9 0)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation nonce-test (added-strand tpm-quote 3) n (9 0) (enc n esk))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk-1))))
      (send (cat "execute transport" (hash "extend" (enc n esk-1)))))
    ((recv (cat "extend" (enc n esk-1)))
      (send (cat "extend" (enc n esk-1))))
    ((recv (cat "quote" nonce)) (obsv n)
      (send (enc "quote" n nonce aik-0))))
  (label 40)
  (parent 35)
  (unrealized (0 0) (10 1))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (state mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 esk-1 skey) (k aik tpmkey tpmkey-0 tpmkey-1 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk-1))))
  (deflistener (cat "extend" (enc n esk-1)))
  (deflistener esk)
  (precedes ((2 0) (10 0)) ((2 2) (9 0)) ((2 4) (3 0)) ((2 6) (0 0))
    ((2 6) (4 0)) ((3 1) (2 5)) ((4 3) (1 0)) ((5 3) (4 2))
    ((6 1) (7 0)) ((6 3) (5 3)) ((7 1) (6 2)) ((8 1) (7 0))
    ((9 1) (8 0)) ((10 1) (9 0)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation nonce-test (added-listener esk) n (9 0) (enc n esk))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk-1))))
      (send (cat "execute transport" (hash "extend" (enc n esk-1)))))
    ((recv (cat "extend" (enc n esk-1)))
      (send (cat "extend" (enc n esk-1)))) ((recv esk) (send esk)))
  (label 41)
  (parent 35)
  (unrealized (0 0) (10 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton envelope
  (vars (state mesg)
    (v n tne tno tne-0 tno-0 tne-1 tno-1 tne-2 tno-2 data)
    (esk1 esk esk-0 esk-1 esk-2 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 tpmkey-2 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne)
    (tno tno) (esk esk) (tpmkey tpmkey))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-1) (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-2)
    (tno tno-2) (esk esk-2) (tpmkey tpmkey-2))
  (precedes ((2 0) (6 0)) ((2 2) (6 2)) ((2 2) (9 2)) ((2 4) (3 0))
    ((2 6) (4 0)) ((2 6) (7 0)) ((3 1) (2 5)) ((4 3) (1 0))
    ((5 3) (4 2)) ((6 1) (2 1)) ((6 3) (5 3)) ((7 2) (0 0))
    ((8 3) (7 1)) ((9 3) (8 3)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)) ((8 3) (7 1)) ((9 3) (8 3)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tne tno tne-0 tne-1 tne-2 esk k)
  (operation state-passing-test (added-strand tpm-extend-enc 4)
    (hash n state) (8 3))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey (enc esk tpmkey)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (tran state (hash n state)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-1))
          tno-1 "false"
          (hash esk-1
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-1))) tne-1 tno-1
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-2 (enc esk-2 tpmkey-2)))
      (send (cat "establish transport" tne-2))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-2)) tno-2
          "false"
          (hash esk-2
            (hash "execute transport" (hash "extend" (enc n esk-2)))
            tne-2 tno-2 "false"))) (tran state (hash n state))))
  (label 42)
  (parent 36)
  (unrealized (9 2))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton envelope
  (vars (state nonce mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 skey) (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk) (tpmkey tpmkey-1))
  (deflistener
    (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk))))
  (deflistener (cat "extend" (enc n esk)))
  (defstrand tpm-quote 3 (nonce nonce) (pcr esk) (aik aik-0))
  (precedes ((2 0) (6 0)) ((2 0) (10 1)) ((2 2) (9 0)) ((2 4) (3 0))
    ((2 6) (0 0)) ((2 6) (4 0)) ((3 1) (2 5)) ((4 3) (1 0))
    ((5 3) (4 2)) ((6 1) (7 0)) ((6 3) (5 3)) ((7 1) (6 2))
    ((8 1) (7 0)) ((9 1) (8 0)) ((10 2) (7 0)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation nonce-test (added-strand tpm-quote 3) esk (7 0)
    (enc esk tpmkey))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno-1
          "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne-1
            tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
         tne-1 tno-1 "false"))
      (send
        (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
          tne-1 tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk))))
      (send (cat "execute transport" (hash "extend" (enc n esk)))))
    ((recv (cat "extend" (enc n esk)))
      (send (cat "extend" (enc n esk))))
    ((recv (cat "quote" nonce)) (obsv esk)
      (send (enc "quote" esk nonce aik-0))))
  (label 43)
  (parent 39)
  (unrealized (0 0) (6 0) (10 1))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (state pcr mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 skey) (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk) (tpmkey tpmkey-1))
  (deflistener
    (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk))))
  (deflistener (cat "extend" (enc n esk)))
  (defstrand tpm-decrypt 4 (m esk) (pcr pcr) (k tpmkey) (aik aik-0))
  (precedes ((2 0) (6 0)) ((2 0) (10 0)) ((2 2) (9 0)) ((2 4) (3 0))
    ((2 6) (0 0)) ((2 6) (4 0)) ((3 1) (2 5)) ((4 3) (1 0))
    ((5 3) (4 2)) ((6 1) (7 0)) ((6 3) (5 3)) ((7 1) (6 2))
    ((8 1) (7 0)) ((9 1) (8 0)) ((10 3) (7 0)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation nonce-test (added-strand tpm-decrypt 4) esk (7 0)
    (enc esk tpmkey))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno-1
          "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne-1
            tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
         tne-1 tno-1 "false"))
      (send
        (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
          tne-1 tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk))))
      (send (cat "execute transport" (hash "extend" (enc n esk)))))
    ((recv (cat "extend" (enc n esk)))
      (send (cat "extend" (enc n esk))))
    ((recv (cat "decrypt" (enc esk tpmkey)))
      (recv (enc "created" tpmkey pcr aik-0)) (obsv pcr) (send esk)))
  (label 44)
  (parent 39)
  (unrealized (0 0) (6 0) (10 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton envelope
  (vars (state nonce mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 esk-1 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk-1))))
  (deflistener (cat "extend" (enc n esk-1)))
  (deflistener esk)
  (defstrand tpm-quote 3 (nonce nonce) (pcr esk) (aik aik-0))
  (precedes ((2 0) (11 1)) ((2 2) (9 0)) ((2 4) (3 0)) ((2 6) (0 0))
    ((2 6) (4 0)) ((3 1) (2 5)) ((4 3) (1 0)) ((5 3) (4 2))
    ((6 1) (7 0)) ((6 3) (5 3)) ((7 1) (6 2)) ((8 1) (7 0))
    ((9 1) (8 0)) ((10 1) (9 0)) ((11 2) (10 0)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation nonce-test (added-strand tpm-quote 3) esk (10 0)
    (enc esk tpmkey))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk-1))))
      (send (cat "execute transport" (hash "extend" (enc n esk-1)))))
    ((recv (cat "extend" (enc n esk-1)))
      (send (cat "extend" (enc n esk-1)))) ((recv esk) (send esk))
    ((recv (cat "quote" nonce)) (obsv esk)
      (send (enc "quote" esk nonce aik-0))))
  (label 45)
  (parent 41)
  (unrealized (0 0) (11 1))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (state pcr mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 esk-1 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk-1))))
  (deflistener (cat "extend" (enc n esk-1)))
  (deflistener esk)
  (defstrand tpm-decrypt 4 (m esk) (pcr pcr) (k tpmkey) (aik aik-0))
  (precedes ((2 0) (11 0)) ((2 2) (9 0)) ((2 4) (3 0)) ((2 6) (0 0))
    ((2 6) (4 0)) ((3 1) (2 5)) ((4 3) (1 0)) ((5 3) (4 2))
    ((6 1) (7 0)) ((6 3) (5 3)) ((7 1) (6 2)) ((8 1) (7 0))
    ((9 1) (8 0)) ((10 1) (9 0)) ((11 3) (10 0)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation nonce-test (added-strand tpm-decrypt 4) esk (10 0)
    (enc esk tpmkey))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk-1))))
      (send (cat "execute transport" (hash "extend" (enc n esk-1)))))
    ((recv (cat "extend" (enc n esk-1)))
      (send (cat "extend" (enc n esk-1)))) ((recv esk) (send esk))
    ((recv (cat "decrypt" (enc esk tpmkey)))
      (recv (enc "created" tpmkey pcr aik-0)) (obsv pcr) (send esk)))
  (label 46)
  (parent 41)
  (unrealized (0 0) (11 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton envelope
  (vars (state nonce mesg)
    (v n tne tno tne-0 tno-0 tne-1 tno-1 tne-2 tno-2 data)
    (esk1 esk esk-0 esk-1 esk-2 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 tpmkey-2 aik-0 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne)
    (tno tno) (esk esk) (tpmkey tpmkey))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-1) (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-2)
    (tno tno-2) (esk esk-2) (tpmkey tpmkey-2))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr
      (hash esk-2
        (hash "execute transport" (hash "extend" (enc n esk-2))) tne-2
        tno-2 "false")) (aik aik-0))
  (precedes ((2 0) (6 0)) ((2 2) (6 2)) ((2 2) (9 2)) ((2 4) (3 0))
    ((2 6) (4 0)) ((2 6) (7 0)) ((3 1) (2 5)) ((4 3) (1 0))
    ((5 3) (4 2)) ((6 1) (2 1)) ((6 3) (5 3)) ((7 2) (0 0))
    ((8 3) (7 1)) ((9 3) (8 3)) ((10 2) (9 2)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)) ((8 3) (7 1)) ((9 3) (8 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tne tno tne-0 tne-1 tne-2 esk k)
  (operation encryption-test (added-strand tpm-quote 3)
    (hash esk-2 (hash "execute transport" (hash "extend" (enc n esk-2)))
      tne-2 tno-2 "false") (9 2))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey (enc esk tpmkey)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (tran state (hash n state)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-1))
          tno-1 "false"
          (hash esk-1
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-1))) tne-1 tno-1
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-2 (enc esk-2 tpmkey-2)))
      (send (cat "establish transport" tne-2))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-2)) tno-2
          "false"
          (hash esk-2
            (hash "execute transport" (hash "extend" (enc n esk-2)))
            tne-2 tno-2 "false"))) (tran state (hash n state)))
    ((recv (cat "quote" nonce))
      (obsv
        (hash esk-2
          (hash "execute transport" (hash "extend" (enc n esk-2))) tne-2
          tno-2 "false"))
      (send
        (enc "quote"
          (hash esk-2
            (hash "execute transport" (hash "extend" (enc n esk-2)))
            tne-2 tno-2 "false") nonce aik-0))))
  (label 47)
  (parent 42)
  (unrealized (9 2) (10 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton envelope
  (vars (state mesg)
    (v n tne tno tne-0 tno-0 tne-1 tno-1 tne-2 tno-2 data)
    (esk1 esk esk-0 esk-1 esk-2 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 tpmkey-2 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne)
    (tno tno) (esk esk) (tpmkey tpmkey))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-1) (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-2)
    (tno tno-2) (esk esk-2) (tpmkey tpmkey-2))
  (deflistener
    (cat esk-2 (hash "execute transport" (hash "extend" (enc n esk-2)))
      tne-2 tno-2 "false"))
  (precedes ((2 0) (6 0)) ((2 2) (6 2)) ((2 2) (9 2)) ((2 4) (3 0))
    ((2 6) (4 0)) ((2 6) (7 0)) ((3 1) (2 5)) ((4 3) (1 0))
    ((5 3) (4 2)) ((6 1) (2 1)) ((6 3) (5 3)) ((7 2) (0 0))
    ((8 3) (7 1)) ((9 1) (10 0)) ((9 3) (8 3)) ((10 1) (9 2)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)) ((8 3) (7 1)) ((9 3) (8 3)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tne tno tne-0 tne-1 tne-2 esk k)
  (operation encryption-test
    (added-listener
      (cat esk-2
        (hash "execute transport" (hash "extend" (enc n esk-2))) tne-2
        tno-2 "false"))
    (hash esk-2 (hash "execute transport" (hash "extend" (enc n esk-2)))
      tne-2 tno-2 "false") (9 2))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey (enc esk tpmkey)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (tran state (hash n state)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-1))
          tno-1 "false"
          (hash esk-1
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-1))) tne-1 tno-1
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-2 (enc esk-2 tpmkey-2)))
      (send (cat "establish transport" tne-2))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-2)) tno-2
          "false"
          (hash esk-2
            (hash "execute transport" (hash "extend" (enc n esk-2)))
            tne-2 tno-2 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-2
         (hash "execute transport" (hash "extend" (enc n esk-2))) tne-2
         tno-2 "false"))
      (send
        (cat esk-2
          (hash "execute transport" (hash "extend" (enc n esk-2))) tne-2
          tno-2 "false"))))
  (label 48)
  (parent 42)
  (unrealized (9 2) (10 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton envelope
  (vars (state pcr nonce mesg)
    (v n tne tno tne-0 tno-0 tne-1 tno-1 data) (esk1 esk esk-0 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 aik-1 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk) (tpmkey tpmkey-1))
  (deflistener
    (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk))))
  (deflistener (cat "extend" (enc n esk)))
  (defstrand tpm-decrypt 4 (m esk) (pcr pcr) (k tpmkey) (aik aik-0))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (enc "created" tpmkey pcr aik-0)) (aik aik-1))
  (precedes ((2 0) (6 0)) ((2 0) (10 0)) ((2 2) (9 0)) ((2 4) (3 0))
    ((2 6) (0 0)) ((2 6) (4 0)) ((3 1) (2 5)) ((4 3) (1 0))
    ((5 3) (4 2)) ((6 1) (7 0)) ((6 3) (5 3)) ((7 1) (6 2))
    ((8 1) (7 0)) ((9 1) (8 0)) ((10 3) (7 0)) ((11 2) (10 1)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 aik-1 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation encryption-test (added-strand tpm-quote 3)
    (enc "created" tpmkey pcr aik-0) (10 1))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno-1
          "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne-1
            tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
         tne-1 tno-1 "false"))
      (send
        (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
          tne-1 tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk))))
      (send (cat "execute transport" (hash "extend" (enc n esk)))))
    ((recv (cat "extend" (enc n esk)))
      (send (cat "extend" (enc n esk))))
    ((recv (cat "decrypt" (enc esk tpmkey)))
      (recv (enc "created" tpmkey pcr aik-0)) (obsv pcr) (send esk))
    ((recv (cat "quote" nonce)) (obsv (enc "created" tpmkey pcr aik-0))
      (send
        (enc "quote" (enc "created" tpmkey pcr aik-0) nonce aik-1))))
  (label 49)
  (parent 44)
  (unrealized (0 0) (6 0) (11 1))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (state pcr nonce mesg)
    (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 esk-1 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 aik-1 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk-1))))
  (deflistener (cat "extend" (enc n esk-1)))
  (deflistener esk)
  (defstrand tpm-decrypt 4 (m esk) (pcr pcr) (k tpmkey) (aik aik-0))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (enc "created" tpmkey pcr aik-0)) (aik aik-1))
  (precedes ((2 0) (11 0)) ((2 2) (9 0)) ((2 4) (3 0)) ((2 6) (0 0))
    ((2 6) (4 0)) ((3 1) (2 5)) ((4 3) (1 0)) ((5 3) (4 2))
    ((6 1) (7 0)) ((6 3) (5 3)) ((7 1) (6 2)) ((8 1) (7 0))
    ((9 1) (8 0)) ((10 1) (9 0)) ((11 3) (10 0)) ((12 2) (11 1)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 aik-1 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation encryption-test (added-strand tpm-quote 3)
    (enc "created" tpmkey pcr aik-0) (11 1))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk-1))))
      (send (cat "execute transport" (hash "extend" (enc n esk-1)))))
    ((recv (cat "extend" (enc n esk-1)))
      (send (cat "extend" (enc n esk-1)))) ((recv esk) (send esk))
    ((recv (cat "decrypt" (enc esk tpmkey)))
      (recv (enc "created" tpmkey pcr aik-0)) (obsv pcr) (send esk))
    ((recv (cat "quote" nonce)) (obsv (enc "created" tpmkey pcr aik-0))
      (send
        (enc "quote" (enc "created" tpmkey pcr aik-0) nonce aik-1))))
  (label 50)
  (parent 46)
  (unrealized (0 0) (12 1))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (state nonce mesg)
    (v n tne tno tne-0 tno-0 tne-1 tno-1 tne-2 tno-2 tne-3 tno-3 data)
    (esk1 esk esk-0 esk-1 esk-2 esk-3 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 tpmkey-2 aik-0 tpmkey-3 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne)
    (tno tno) (esk esk) (tpmkey tpmkey))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-1) (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-2)
    (tno tno-2) (esk esk-2) (tpmkey tpmkey-2))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr
      (hash esk-2
        (hash "execute transport" (hash "extend" (enc n esk-2))) tne-2
        tno-2 "false")) (aik aik-0))
  (defstrand tpm-extend-enc 4 (value esk-2)
    (state
      (cat (hash "execute transport" (hash "extend" (enc n esk-2)))
        tne-2 tno-2 "false")) (tne tne-3) (tno tno-3) (esk esk-3)
    (tpmkey tpmkey-3))
  (precedes ((2 0) (6 0)) ((2 2) (6 2)) ((2 2) (9 2)) ((2 4) (3 0))
    ((2 6) (4 0)) ((2 6) (7 0)) ((3 1) (2 5)) ((4 3) (1 0))
    ((5 3) (4 2)) ((6 1) (2 1)) ((6 3) (5 3)) ((7 2) (0 0))
    ((8 3) (7 1)) ((9 1) (11 3)) ((9 3) (8 3)) ((10 2) (9 2))
    ((11 3) (10 1)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)) ((8 3) (7 1)) ((9 3) (8 3))
    ((11 3) (10 1)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tne tno tne-0 tne-1 tne-2 tne-3 esk k)
  (operation state-passing-test (added-strand tpm-extend-enc 4)
    (hash esk-2 (hash "execute transport" (hash "extend" (enc n esk-2)))
      tne-2 tno-2 "false") (10 1))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey (enc esk tpmkey)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (tran state (hash n state)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-1))
          tno-1 "false"
          (hash esk-1
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-1))) tne-1 tno-1
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-2 (enc esk-2 tpmkey-2)))
      (send (cat "establish transport" tne-2))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-2)) tno-2
          "false"
          (hash esk-2
            (hash "execute transport" (hash "extend" (enc n esk-2)))
            tne-2 tno-2 "false"))) (tran state (hash n state)))
    ((recv (cat "quote" nonce))
      (obsv
        (hash esk-2
          (hash "execute transport" (hash "extend" (enc n esk-2))) tne-2
          tno-2 "false"))
      (send
        (enc "quote"
          (hash esk-2
            (hash "execute transport" (hash "extend" (enc n esk-2)))
            tne-2 tno-2 "false") nonce aik-0)))
    ((recv (cat "establish transport" tpmkey-3 (enc esk-3 tpmkey-3)))
      (send (cat "establish transport" tne-3))
      (recv
        (cat "execute transport" (cat "extend" (enc esk-2 esk-3)) tno-3
          "false"
          (hash esk-3
            (hash "execute transport" (hash "extend" (enc esk-2 esk-3)))
            tne-3 tno-3 "false")))
      (tran
        (cat (hash "execute transport" (hash "extend" (enc n esk-2)))
          tne-2 tno-2 "false")
        (hash esk-2
          (hash "execute transport" (hash "extend" (enc n esk-2))) tne-2
          tno-2 "false"))))
  (label 51)
  (parent 47)
  (unrealized (9 2) (11 3))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (state nonce mesg)
    (v n tne tno tne-0 tno-0 tne-1 tno-1 tne-2 tno-2 data)
    (esk1 esk esk-0 esk-1 esk-2 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 tpmkey-2 aik-0 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne)
    (tno tno) (esk esk) (tpmkey tpmkey))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-1) (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-2)
    (tno tno-2) (esk esk-2) (tpmkey tpmkey-2))
  (deflistener
    (cat esk-2 (hash "execute transport" (hash "extend" (enc n esk-2)))
      tne-2 tno-2 "false"))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (hash "execute transport" (hash "extend" (enc n esk-2))))
    (aik aik-0))
  (precedes ((2 0) (6 0)) ((2 2) (6 2)) ((2 2) (9 2)) ((2 4) (3 0))
    ((2 6) (4 0)) ((2 6) (7 0)) ((3 1) (2 5)) ((4 3) (1 0))
    ((5 3) (4 2)) ((6 1) (2 1)) ((6 3) (5 3)) ((7 2) (0 0))
    ((8 3) (7 1)) ((9 1) (10 0)) ((9 3) (8 3)) ((10 1) (9 2))
    ((11 2) (10 0)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)) ((8 3) (7 1)) ((9 3) (8 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tne tno tne-0 tne-1 tne-2 esk k)
  (operation encryption-test (added-strand tpm-quote 3)
    (hash "execute transport" (hash "extend" (enc n esk-2))) (10 0))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey (enc esk tpmkey)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (tran state (hash n state)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-1))
          tno-1 "false"
          (hash esk-1
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-1))) tne-1 tno-1
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-2 (enc esk-2 tpmkey-2)))
      (send (cat "establish transport" tne-2))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-2)) tno-2
          "false"
          (hash esk-2
            (hash "execute transport" (hash "extend" (enc n esk-2)))
            tne-2 tno-2 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-2
         (hash "execute transport" (hash "extend" (enc n esk-2))) tne-2
         tno-2 "false"))
      (send
        (cat esk-2
          (hash "execute transport" (hash "extend" (enc n esk-2))) tne-2
          tno-2 "false")))
    ((recv (cat "quote" nonce))
      (obsv (hash "execute transport" (hash "extend" (enc n esk-2))))
      (send
        (enc "quote"
          (hash "execute transport" (hash "extend" (enc n esk-2))) nonce
          aik-0))))
  (label 52)
  (parent 48)
  (unrealized (9 2) (11 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton envelope
  (vars (state mesg)
    (v n tne tno tne-0 tno-0 tne-1 tno-1 tne-2 tno-2 data)
    (esk1 esk esk-0 esk-1 esk-2 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 tpmkey-2 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne)
    (tno tno) (esk esk) (tpmkey tpmkey))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-1) (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-2)
    (tno tno-2) (esk esk-2) (tpmkey tpmkey-2))
  (deflistener
    (cat esk-2 (hash "execute transport" (hash "extend" (enc n esk-2)))
      tne-2 tno-2 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk-2))))
  (precedes ((2 0) (6 0)) ((2 2) (6 2)) ((2 2) (9 2)) ((2 4) (3 0))
    ((2 6) (4 0)) ((2 6) (7 0)) ((3 1) (2 5)) ((4 3) (1 0))
    ((5 3) (4 2)) ((6 1) (2 1)) ((6 3) (5 3)) ((7 2) (0 0))
    ((8 3) (7 1)) ((9 1) (10 0)) ((9 3) (8 3)) ((10 1) (9 2))
    ((11 1) (10 0)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)) ((8 3) (7 1)) ((9 3) (8 3)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tne tno tne-0 tne-1 tne-2 esk k)
  (operation encryption-test
    (added-listener
      (cat "execute transport" (hash "extend" (enc n esk-2))))
    (hash "execute transport" (hash "extend" (enc n esk-2))) (10 0))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey (enc esk tpmkey)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (tran state (hash n state)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-1))
          tno-1 "false"
          (hash esk-1
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-1))) tne-1 tno-1
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-2 (enc esk-2 tpmkey-2)))
      (send (cat "establish transport" tne-2))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-2)) tno-2
          "false"
          (hash esk-2
            (hash "execute transport" (hash "extend" (enc n esk-2)))
            tne-2 tno-2 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-2
         (hash "execute transport" (hash "extend" (enc n esk-2))) tne-2
         tno-2 "false"))
      (send
        (cat esk-2
          (hash "execute transport" (hash "extend" (enc n esk-2))) tne-2
          tno-2 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk-2))))
      (send (cat "execute transport" (hash "extend" (enc n esk-2))))))
  (label 53)
  (parent 48)
  (unrealized (9 2) (11 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton envelope
  (vars (state nonce mesg)
    (v n tne tno tne-0 tno-0 tne-1 tno-1 tne-2 tno-2 tne-3 tno-3 data)
    (esk1 esk esk-0 esk-1 esk-2 esk-3 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 tpmkey-2 aik-0 tpmkey-3 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne)
    (tno tno) (esk esk) (tpmkey tpmkey))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-1) (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-2)
    (tno tno-2) (esk esk-2) (tpmkey tpmkey-2))
  (deflistener
    (cat esk-2 (hash "execute transport" (hash "extend" (enc n esk-2)))
      tne-2 tno-2 "false"))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (hash "execute transport" (hash "extend" (enc n esk-2))))
    (aik aik-0))
  (defstrand tpm-extend-enc 4 (value "execute transport")
    (state (hash "extend" (enc n esk-2))) (tne tne-3) (tno tno-3)
    (esk esk-3) (tpmkey tpmkey-3))
  (precedes ((2 0) (6 0)) ((2 2) (6 2)) ((2 2) (9 2)) ((2 4) (3 0))
    ((2 6) (4 0)) ((2 6) (7 0)) ((3 1) (2 5)) ((4 3) (1 0))
    ((5 3) (4 2)) ((6 1) (2 1)) ((6 3) (5 3)) ((7 2) (0 0))
    ((8 3) (7 1)) ((9 1) (10 0)) ((9 3) (8 3)) ((10 1) (9 2))
    ((11 2) (10 0)) ((12 3) (11 1)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)) ((8 3) (7 1)) ((9 3) (8 3))
    ((12 3) (11 1)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tne tno tne-0 tne-1 tne-2 tne-3 esk k)
  (operation state-passing-test (added-strand tpm-extend-enc 4)
    (hash "execute transport" (hash "extend" (enc n esk-2))) (11 1))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey (enc esk tpmkey)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (tran state (hash n state)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-1))
          tno-1 "false"
          (hash esk-1
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-1))) tne-1 tno-1
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-2 (enc esk-2 tpmkey-2)))
      (send (cat "establish transport" tne-2))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-2)) tno-2
          "false"
          (hash esk-2
            (hash "execute transport" (hash "extend" (enc n esk-2)))
            tne-2 tno-2 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-2
         (hash "execute transport" (hash "extend" (enc n esk-2))) tne-2
         tno-2 "false"))
      (send
        (cat esk-2
          (hash "execute transport" (hash "extend" (enc n esk-2))) tne-2
          tno-2 "false")))
    ((recv (cat "quote" nonce))
      (obsv (hash "execute transport" (hash "extend" (enc n esk-2))))
      (send
        (enc "quote"
          (hash "execute transport" (hash "extend" (enc n esk-2))) nonce
          aik-0)))
    ((recv (cat "establish transport" tpmkey-3 (enc esk-3 tpmkey-3)))
      (send (cat "establish transport" tne-3))
      (recv
        (cat "execute transport"
          (cat "extend" (enc "execute transport" esk-3)) tno-3 "false"
          (hash esk-3
            (hash "execute transport"
              (hash "extend" (enc "execute transport" esk-3))) tne-3
            tno-3 "false")))
      (tran (hash "extend" (enc n esk-2))
        (hash "execute transport" (hash "extend" (enc n esk-2))))))
  (label 54)
  (parent 52)
  (unrealized (9 2) (12 3))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton envelope
  (vars (state nonce mesg)
    (v n tne tno tne-0 tno-0 tne-1 tno-1 tne-2 tno-2 data)
    (esk1 esk esk-0 esk-1 esk-2 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 tpmkey-2 aik-0 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne)
    (tno tno) (esk esk) (tpmkey tpmkey))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-1) (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-2)
    (tno tno-2) (esk esk-2) (tpmkey tpmkey-2))
  (deflistener
    (cat esk-2 (hash "execute transport" (hash "extend" (enc n esk-2)))
      tne-2 tno-2 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk-2))))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (hash "extend" (enc n esk-2))) (aik aik-0))
  (precedes ((2 0) (6 0)) ((2 2) (6 2)) ((2 2) (9 2)) ((2 4) (3 0))
    ((2 6) (4 0)) ((2 6) (7 0)) ((3 1) (2 5)) ((4 3) (1 0))
    ((5 3) (4 2)) ((6 1) (2 1)) ((6 3) (5 3)) ((7 2) (0 0))
    ((8 3) (7 1)) ((9 1) (10 0)) ((9 3) (8 3)) ((10 1) (9 2))
    ((11 1) (10 0)) ((12 2) (11 0)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)) ((8 3) (7 1)) ((9 3) (8 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tne tno tne-0 tne-1 tne-2 esk k)
  (operation encryption-test (added-strand tpm-quote 3)
    (hash "extend" (enc n esk-2)) (11 0))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey (enc esk tpmkey)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (tran state (hash n state)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-1))
          tno-1 "false"
          (hash esk-1
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-1))) tne-1 tno-1
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-2 (enc esk-2 tpmkey-2)))
      (send (cat "establish transport" tne-2))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-2)) tno-2
          "false"
          (hash esk-2
            (hash "execute transport" (hash "extend" (enc n esk-2)))
            tne-2 tno-2 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-2
         (hash "execute transport" (hash "extend" (enc n esk-2))) tne-2
         tno-2 "false"))
      (send
        (cat esk-2
          (hash "execute transport" (hash "extend" (enc n esk-2))) tne-2
          tno-2 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk-2))))
      (send (cat "execute transport" (hash "extend" (enc n esk-2)))))
    ((recv (cat "quote" nonce)) (obsv (hash "extend" (enc n esk-2)))
      (send (enc "quote" (hash "extend" (enc n esk-2)) nonce aik-0))))
  (label 55)
  (parent 53)
  (unrealized (9 2) (12 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton envelope
  (vars (state mesg)
    (v n tne tno tne-0 tno-0 tne-1 tno-1 tne-2 tno-2 data)
    (esk1 esk esk-0 esk-1 esk-2 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 tpmkey-2 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne)
    (tno tno) (esk esk) (tpmkey tpmkey))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-1) (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-2)
    (tno tno-2) (esk esk-2) (tpmkey tpmkey-2))
  (deflistener
    (cat esk-2 (hash "execute transport" (hash "extend" (enc n esk-2)))
      tne-2 tno-2 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk-2))))
  (deflistener (cat "extend" (enc n esk-2)))
  (precedes ((2 0) (6 0)) ((2 2) (6 2)) ((2 2) (12 0)) ((2 4) (3 0))
    ((2 6) (4 0)) ((2 6) (7 0)) ((3 1) (2 5)) ((4 3) (1 0))
    ((5 3) (4 2)) ((6 1) (2 1)) ((6 3) (5 3)) ((7 2) (0 0))
    ((8 3) (7 1)) ((9 1) (10 0)) ((9 3) (8 3)) ((10 1) (9 2))
    ((11 1) (10 0)) ((12 1) (11 0)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)) ((8 3) (7 1)) ((9 3) (8 3)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tne tno tne-0 tne-1 tne-2 esk k)
  (operation encryption-test
    (added-listener (cat "extend" (enc n esk-2)))
    (hash "extend" (enc n esk-2)) (11 0))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey (enc esk tpmkey)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (tran state (hash n state)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-1))
          tno-1 "false"
          (hash esk-1
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-1))) tne-1 tno-1
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-2 (enc esk-2 tpmkey-2)))
      (send (cat "establish transport" tne-2))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-2)) tno-2
          "false"
          (hash esk-2
            (hash "execute transport" (hash "extend" (enc n esk-2)))
            tne-2 tno-2 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-2
         (hash "execute transport" (hash "extend" (enc n esk-2))) tne-2
         tno-2 "false"))
      (send
        (cat esk-2
          (hash "execute transport" (hash "extend" (enc n esk-2))) tne-2
          tno-2 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk-2))))
      (send (cat "execute transport" (hash "extend" (enc n esk-2)))))
    ((recv (cat "extend" (enc n esk-2)))
      (send (cat "extend" (enc n esk-2)))))
  (label 56)
  (parent 53)
  (unrealized (12 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton envelope
  (vars (state nonce mesg)
    (v n tne tno tne-0 tno-0 tne-1 tno-1 tne-2 tno-2 tne-3 tno-3 tne-4
      tno-4 data) (esk1 esk esk-0 esk-1 esk-2 esk-3 esk-4 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 tpmkey-2 aik-0 tpmkey-3 tpmkey-4
      akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne)
    (tno tno) (esk esk) (tpmkey tpmkey))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-1) (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-2)
    (tno tno-2) (esk esk-2) (tpmkey tpmkey-2))
  (deflistener
    (cat esk-2 (hash "execute transport" (hash "extend" (enc n esk-2)))
      tne-2 tno-2 "false"))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (hash "execute transport" (hash "extend" (enc n esk-2))))
    (aik aik-0))
  (defstrand tpm-extend-enc 4 (value "execute transport")
    (state (hash "extend" (enc n esk-2))) (tne tne-3) (tno tno-3)
    (esk esk-3) (tpmkey tpmkey-3))
  (defstrand tpm-extend-enc 4 (value "extend") (state (enc n esk-2))
    (tne tne-4) (tno tno-4) (esk esk-4) (tpmkey tpmkey-4))
  (precedes ((2 0) (6 0)) ((2 2) (6 2)) ((2 2) (13 3)) ((2 4) (3 0))
    ((2 6) (4 0)) ((2 6) (7 0)) ((3 1) (2 5)) ((4 3) (1 0))
    ((5 3) (4 2)) ((6 1) (2 1)) ((6 3) (5 3)) ((7 2) (0 0))
    ((8 3) (7 1)) ((9 1) (10 0)) ((9 3) (8 3)) ((10 1) (9 2))
    ((11 2) (10 0)) ((12 3) (11 1)) ((13 3) (12 3)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)) ((8 3) (7 1)) ((9 3) (8 3))
    ((12 3) (11 1)) ((13 3) (12 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tne tno tne-0 tne-1 tne-2 tne-3 tne-4 esk k)
  (operation state-passing-test (added-strand tpm-extend-enc 4)
    (hash "extend" (enc n esk-2)) (12 3))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey (enc esk tpmkey)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (tran state (hash n state)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-1))
          tno-1 "false"
          (hash esk-1
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-1))) tne-1 tno-1
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-2 (enc esk-2 tpmkey-2)))
      (send (cat "establish transport" tne-2))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-2)) tno-2
          "false"
          (hash esk-2
            (hash "execute transport" (hash "extend" (enc n esk-2)))
            tne-2 tno-2 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-2
         (hash "execute transport" (hash "extend" (enc n esk-2))) tne-2
         tno-2 "false"))
      (send
        (cat esk-2
          (hash "execute transport" (hash "extend" (enc n esk-2))) tne-2
          tno-2 "false")))
    ((recv (cat "quote" nonce))
      (obsv (hash "execute transport" (hash "extend" (enc n esk-2))))
      (send
        (enc "quote"
          (hash "execute transport" (hash "extend" (enc n esk-2))) nonce
          aik-0)))
    ((recv (cat "establish transport" tpmkey-3 (enc esk-3 tpmkey-3)))
      (send (cat "establish transport" tne-3))
      (recv
        (cat "execute transport"
          (cat "extend" (enc "execute transport" esk-3)) tno-3 "false"
          (hash esk-3
            (hash "execute transport"
              (hash "extend" (enc "execute transport" esk-3))) tne-3
            tno-3 "false")))
      (tran (hash "extend" (enc n esk-2))
        (hash "execute transport" (hash "extend" (enc n esk-2)))))
    ((recv (cat "establish transport" tpmkey-4 (enc esk-4 tpmkey-4)))
      (send (cat "establish transport" tne-4))
      (recv
        (cat "execute transport" (cat "extend" (enc "extend" esk-4))
          tno-4 "false"
          (hash esk-4
            (hash "execute transport"
              (hash "extend" (enc "extend" esk-4))) tne-4 tno-4
            "false")))
      (tran (enc n esk-2) (hash "extend" (enc n esk-2)))))
  (label 57)
  (parent 54)
  (unrealized (9 2) (13 3))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (state nonce mesg)
    (v n tne tno tne-0 tno-0 tne-1 tno-1 tne-2 tno-2 tne-3 tno-3 data)
    (esk1 esk esk-0 esk-1 esk-2 esk-3 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 tpmkey-2 aik-0 tpmkey-3 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne)
    (tno tno) (esk esk) (tpmkey tpmkey))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-1) (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-2)
    (tno tno-2) (esk esk-2) (tpmkey tpmkey-2))
  (deflistener
    (cat esk-2 (hash "execute transport" (hash "extend" (enc n esk-2)))
      tne-2 tno-2 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk-2))))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (hash "extend" (enc n esk-2))) (aik aik-0))
  (defstrand tpm-extend-enc 4 (value "extend") (state (enc n esk-2))
    (tne tne-3) (tno tno-3) (esk esk-3) (tpmkey tpmkey-3))
  (precedes ((2 0) (6 0)) ((2 2) (6 2)) ((2 2) (13 3)) ((2 4) (3 0))
    ((2 6) (4 0)) ((2 6) (7 0)) ((3 1) (2 5)) ((4 3) (1 0))
    ((5 3) (4 2)) ((6 1) (2 1)) ((6 3) (5 3)) ((7 2) (0 0))
    ((8 3) (7 1)) ((9 1) (10 0)) ((9 3) (8 3)) ((10 1) (9 2))
    ((11 1) (10 0)) ((12 2) (11 0)) ((13 3) (12 1)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)) ((8 3) (7 1)) ((9 3) (8 3))
    ((13 3) (12 1)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tne tno tne-0 tne-1 tne-2 tne-3 esk k)
  (operation state-passing-test (added-strand tpm-extend-enc 4)
    (hash "extend" (enc n esk-2)) (12 1))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey (enc esk tpmkey)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (tran state (hash n state)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-1))
          tno-1 "false"
          (hash esk-1
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-1))) tne-1 tno-1
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-2 (enc esk-2 tpmkey-2)))
      (send (cat "establish transport" tne-2))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-2)) tno-2
          "false"
          (hash esk-2
            (hash "execute transport" (hash "extend" (enc n esk-2)))
            tne-2 tno-2 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-2
         (hash "execute transport" (hash "extend" (enc n esk-2))) tne-2
         tno-2 "false"))
      (send
        (cat esk-2
          (hash "execute transport" (hash "extend" (enc n esk-2))) tne-2
          tno-2 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk-2))))
      (send (cat "execute transport" (hash "extend" (enc n esk-2)))))
    ((recv (cat "quote" nonce)) (obsv (hash "extend" (enc n esk-2)))
      (send (enc "quote" (hash "extend" (enc n esk-2)) nonce aik-0)))
    ((recv (cat "establish transport" tpmkey-3 (enc esk-3 tpmkey-3)))
      (send (cat "establish transport" tne-3))
      (recv
        (cat "execute transport" (cat "extend" (enc "extend" esk-3))
          tno-3 "false"
          (hash esk-3
            (hash "execute transport"
              (hash "extend" (enc "extend" esk-3))) tne-3 tno-3
            "false")))
      (tran (enc n esk-2) (hash "extend" (enc n esk-2)))))
  (label 58)
  (parent 55)
  (unrealized (9 2) (13 3))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (state mesg)
    (v n tne tno tne-0 tno-0 tne-1 tno-1 tne-2 tno-2 data)
    (esk1 esk esk-0 esk-1 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 tpmkey-2 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne)
    (tno tno) (esk esk) (tpmkey tpmkey))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-1) (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-2)
    (tno tno-2) (esk esk) (tpmkey tpmkey-2))
  (deflistener
    (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
      tne-2 tno-2 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk))))
  (deflistener (cat "extend" (enc n esk)))
  (precedes ((2 0) (6 0)) ((2 0) (9 0)) ((2 2) (6 2)) ((2 2) (12 0))
    ((2 4) (3 0)) ((2 6) (4 0)) ((2 6) (7 0)) ((3 1) (2 5))
    ((4 3) (1 0)) ((5 3) (4 2)) ((6 1) (2 1)) ((6 3) (5 3))
    ((7 2) (0 0)) ((8 3) (7 1)) ((9 1) (10 0)) ((9 3) (8 3))
    ((10 1) (9 2)) ((11 1) (10 0)) ((12 1) (11 0)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)) ((8 3) (7 1)) ((9 3) (8 3)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tne tno tne-0 tne-1 tne-2 esk k)
  (operation nonce-test (contracted (esk-2 esk)) n (12 0) (enc n esk))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey (enc esk tpmkey)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (tran state (hash n state)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-1))
          tno-1 "false"
          (hash esk-1
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-1))) tne-1 tno-1
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-2 (enc esk tpmkey-2)))
      (send (cat "establish transport" tne-2))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno-2
          "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne-2
            tno-2 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
         tne-2 tno-2 "false"))
      (send
        (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
          tne-2 tno-2 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk))))
      (send (cat "execute transport" (hash "extend" (enc n esk)))))
    ((recv (cat "extend" (enc n esk)))
      (send (cat "extend" (enc n esk)))))
  (label 59)
  (parent 56)
  (unrealized (9 0) (10 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton envelope
  (vars (state nonce mesg)
    (v n tne tno tne-0 tno-0 tne-1 tno-1 tne-2 tno-2 data)
    (esk1 esk esk-0 esk-1 esk-2 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 tpmkey-2 aik-0 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne)
    (tno tno) (esk esk) (tpmkey tpmkey))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-1) (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-2)
    (tno tno-2) (esk esk-2) (tpmkey tpmkey-2))
  (deflistener
    (cat esk-2 (hash "execute transport" (hash "extend" (enc n esk-2)))
      tne-2 tno-2 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk-2))))
  (deflistener (cat "extend" (enc n esk-2)))
  (defstrand tpm-quote 3 (nonce nonce) (pcr n) (aik aik-0))
  (precedes ((2 0) (6 0)) ((2 2) (6 2)) ((2 2) (13 1)) ((2 4) (3 0))
    ((2 6) (4 0)) ((2 6) (7 0)) ((3 1) (2 5)) ((4 3) (1 0))
    ((5 3) (4 2)) ((6 1) (2 1)) ((6 3) (5 3)) ((7 2) (0 0))
    ((8 3) (7 1)) ((9 1) (10 0)) ((9 3) (8 3)) ((10 1) (9 2))
    ((11 1) (10 0)) ((12 1) (11 0)) ((13 2) (12 0)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)) ((8 3) (7 1)) ((9 3) (8 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tne tno tne-0 tne-1 tne-2 esk k)
  (operation nonce-test (added-strand tpm-quote 3) n (12 0) (enc n esk))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey (enc esk tpmkey)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (tran state (hash n state)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-1))
          tno-1 "false"
          (hash esk-1
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-1))) tne-1 tno-1
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-2 (enc esk-2 tpmkey-2)))
      (send (cat "establish transport" tne-2))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-2)) tno-2
          "false"
          (hash esk-2
            (hash "execute transport" (hash "extend" (enc n esk-2)))
            tne-2 tno-2 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-2
         (hash "execute transport" (hash "extend" (enc n esk-2))) tne-2
         tno-2 "false"))
      (send
        (cat esk-2
          (hash "execute transport" (hash "extend" (enc n esk-2))) tne-2
          tno-2 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk-2))))
      (send (cat "execute transport" (hash "extend" (enc n esk-2)))))
    ((recv (cat "extend" (enc n esk-2)))
      (send (cat "extend" (enc n esk-2))))
    ((recv (cat "quote" nonce)) (obsv n)
      (send (enc "quote" n nonce aik-0))))
  (label 60)
  (parent 56)
  (unrealized (13 1))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (state mesg)
    (v n tne tno tne-0 tno-0 tne-1 tno-1 tne-2 tno-2 data)
    (esk1 esk esk-0 esk-1 esk-2 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 tpmkey-2 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne)
    (tno tno) (esk esk) (tpmkey tpmkey))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-1) (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-2)
    (tno tno-2) (esk esk-2) (tpmkey tpmkey-2))
  (deflistener
    (cat esk-2 (hash "execute transport" (hash "extend" (enc n esk-2)))
      tne-2 tno-2 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk-2))))
  (deflistener (cat "extend" (enc n esk-2)))
  (deflistener esk)
  (precedes ((2 0) (6 0)) ((2 0) (13 0)) ((2 2) (6 2)) ((2 2) (12 0))
    ((2 4) (3 0)) ((2 6) (4 0)) ((2 6) (7 0)) ((3 1) (2 5))
    ((4 3) (1 0)) ((5 3) (4 2)) ((6 1) (2 1)) ((6 3) (5 3))
    ((7 2) (0 0)) ((8 3) (7 1)) ((9 1) (10 0)) ((9 3) (8 3))
    ((10 1) (9 2)) ((11 1) (10 0)) ((12 1) (11 0)) ((13 1) (12 0)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)) ((8 3) (7 1)) ((9 3) (8 3)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tne tno tne-0 tne-1 tne-2 esk k)
  (operation nonce-test (added-listener esk) n (12 0) (enc n esk))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey (enc esk tpmkey)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (tran state (hash n state)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-1))
          tno-1 "false"
          (hash esk-1
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-1))) tne-1 tno-1
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-2 (enc esk-2 tpmkey-2)))
      (send (cat "establish transport" tne-2))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-2)) tno-2
          "false"
          (hash esk-2
            (hash "execute transport" (hash "extend" (enc n esk-2)))
            tne-2 tno-2 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-2
         (hash "execute transport" (hash "extend" (enc n esk-2))) tne-2
         tno-2 "false"))
      (send
        (cat esk-2
          (hash "execute transport" (hash "extend" (enc n esk-2))) tne-2
          tno-2 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk-2))))
      (send (cat "execute transport" (hash "extend" (enc n esk-2)))))
    ((recv (cat "extend" (enc n esk-2)))
      (send (cat "extend" (enc n esk-2)))) ((recv esk) (send esk)))
  (label 61)
  (parent 56)
  (unrealized (13 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton envelope
  (vars (state nonce mesg)
    (v n tne tno tne-0 tno-0 tne-1 tno-1 tne-2 tno-2 data)
    (esk1 esk esk-0 esk-1 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 tpmkey-2 aik-0 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne)
    (tno tno) (esk esk) (tpmkey tpmkey))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-1) (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-2)
    (tno tno-2) (esk esk) (tpmkey tpmkey-2))
  (deflistener
    (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
      tne-2 tno-2 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk))))
  (deflistener (cat "extend" (enc n esk)))
  (defstrand tpm-quote 3 (nonce nonce) (pcr esk) (aik aik-0))
  (precedes ((2 0) (6 0)) ((2 0) (9 0)) ((2 0) (13 1)) ((2 2) (6 2))
    ((2 2) (12 0)) ((2 4) (3 0)) ((2 6) (4 0)) ((2 6) (7 0))
    ((3 1) (2 5)) ((4 3) (1 0)) ((5 3) (4 2)) ((6 1) (2 1))
    ((6 3) (5 3)) ((7 2) (0 0)) ((8 3) (7 1)) ((9 1) (10 0))
    ((9 3) (8 3)) ((10 1) (9 2)) ((11 1) (10 0)) ((12 1) (11 0))
    ((13 2) (10 0)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)) ((8 3) (7 1)) ((9 3) (8 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tne tno tne-0 tne-1 tne-2 esk k)
  (operation nonce-test (added-strand tpm-quote 3) esk (10 0)
    (enc esk tpmkey))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey (enc esk tpmkey)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (tran state (hash n state)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-1))
          tno-1 "false"
          (hash esk-1
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-1))) tne-1 tno-1
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-2 (enc esk tpmkey-2)))
      (send (cat "establish transport" tne-2))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno-2
          "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne-2
            tno-2 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
         tne-2 tno-2 "false"))
      (send
        (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
          tne-2 tno-2 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk))))
      (send (cat "execute transport" (hash "extend" (enc n esk)))))
    ((recv (cat "extend" (enc n esk)))
      (send (cat "extend" (enc n esk))))
    ((recv (cat "quote" nonce)) (obsv esk)
      (send (enc "quote" esk nonce aik-0))))
  (label 62)
  (parent 59)
  (unrealized (9 0) (13 1))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (state pcr mesg)
    (v n tne tno tne-0 tno-0 tne-1 tno-1 tne-2 tno-2 data)
    (esk1 esk esk-0 esk-1 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 tpmkey-2 aik-0 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne)
    (tno tno) (esk esk) (tpmkey tpmkey))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-1) (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-2)
    (tno tno-2) (esk esk) (tpmkey tpmkey-2))
  (deflistener
    (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
      tne-2 tno-2 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk))))
  (deflistener (cat "extend" (enc n esk)))
  (defstrand tpm-decrypt 4 (m esk) (pcr pcr) (k tpmkey) (aik aik-0))
  (precedes ((2 0) (6 0)) ((2 0) (9 0)) ((2 0) (13 0)) ((2 2) (6 2))
    ((2 2) (12 0)) ((2 4) (3 0)) ((2 6) (4 0)) ((2 6) (7 0))
    ((3 1) (2 5)) ((4 3) (1 0)) ((5 3) (4 2)) ((6 1) (2 1))
    ((6 3) (5 3)) ((7 2) (0 0)) ((8 3) (7 1)) ((9 1) (10 0))
    ((9 3) (8 3)) ((10 1) (9 2)) ((11 1) (10 0)) ((12 1) (11 0))
    ((13 3) (10 0)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)) ((8 3) (7 1)) ((9 3) (8 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tne tno tne-0 tne-1 tne-2 esk k)
  (operation nonce-test (added-strand tpm-decrypt 4) esk (10 0)
    (enc esk tpmkey))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey (enc esk tpmkey)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (tran state (hash n state)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-1))
          tno-1 "false"
          (hash esk-1
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-1))) tne-1 tno-1
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-2 (enc esk tpmkey-2)))
      (send (cat "establish transport" tne-2))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno-2
          "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne-2
            tno-2 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
         tne-2 tno-2 "false"))
      (send
        (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
          tne-2 tno-2 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk))))
      (send (cat "execute transport" (hash "extend" (enc n esk)))))
    ((recv (cat "extend" (enc n esk)))
      (send (cat "extend" (enc n esk))))
    ((recv (cat "decrypt" (enc esk tpmkey)))
      (recv (enc "created" tpmkey pcr aik-0)) (obsv pcr) (send esk)))
  (label 63)
  (parent 59)
  (unrealized (9 0) (13 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton envelope
  (vars (state nonce mesg)
    (v n tne tno tne-0 tno-0 tne-1 tno-1 tne-2 tno-2 data)
    (esk1 esk esk-0 esk-1 esk-2 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 tpmkey-2 aik-0 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne)
    (tno tno) (esk esk) (tpmkey tpmkey))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-1) (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-2)
    (tno tno-2) (esk esk-2) (tpmkey tpmkey-2))
  (deflistener
    (cat esk-2 (hash "execute transport" (hash "extend" (enc n esk-2)))
      tne-2 tno-2 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk-2))))
  (deflistener (cat "extend" (enc n esk-2)))
  (deflistener esk)
  (defstrand tpm-quote 3 (nonce nonce) (pcr esk) (aik aik-0))
  (precedes ((2 0) (6 0)) ((2 0) (14 1)) ((2 2) (6 2)) ((2 2) (12 0))
    ((2 4) (3 0)) ((2 6) (4 0)) ((2 6) (7 0)) ((3 1) (2 5))
    ((4 3) (1 0)) ((5 3) (4 2)) ((6 1) (2 1)) ((6 3) (5 3))
    ((7 2) (0 0)) ((8 3) (7 1)) ((9 1) (10 0)) ((9 3) (8 3))
    ((10 1) (9 2)) ((11 1) (10 0)) ((12 1) (11 0)) ((13 1) (12 0))
    ((14 2) (13 0)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)) ((8 3) (7 1)) ((9 3) (8 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tne tno tne-0 tne-1 tne-2 esk k)
  (operation nonce-test (added-strand tpm-quote 3) esk (13 0)
    (enc esk tpmkey))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey (enc esk tpmkey)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (tran state (hash n state)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-1))
          tno-1 "false"
          (hash esk-1
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-1))) tne-1 tno-1
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-2 (enc esk-2 tpmkey-2)))
      (send (cat "establish transport" tne-2))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-2)) tno-2
          "false"
          (hash esk-2
            (hash "execute transport" (hash "extend" (enc n esk-2)))
            tne-2 tno-2 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-2
         (hash "execute transport" (hash "extend" (enc n esk-2))) tne-2
         tno-2 "false"))
      (send
        (cat esk-2
          (hash "execute transport" (hash "extend" (enc n esk-2))) tne-2
          tno-2 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk-2))))
      (send (cat "execute transport" (hash "extend" (enc n esk-2)))))
    ((recv (cat "extend" (enc n esk-2)))
      (send (cat "extend" (enc n esk-2)))) ((recv esk) (send esk))
    ((recv (cat "quote" nonce)) (obsv esk)
      (send (enc "quote" esk nonce aik-0))))
  (label 64)
  (parent 61)
  (unrealized (14 1))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (state pcr mesg)
    (v n tne tno tne-0 tno-0 tne-1 tno-1 tne-2 tno-2 data)
    (esk1 esk esk-0 esk-1 esk-2 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 tpmkey-2 aik-0 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne)
    (tno tno) (esk esk) (tpmkey tpmkey))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-1) (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-2)
    (tno tno-2) (esk esk-2) (tpmkey tpmkey-2))
  (deflistener
    (cat esk-2 (hash "execute transport" (hash "extend" (enc n esk-2)))
      tne-2 tno-2 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk-2))))
  (deflistener (cat "extend" (enc n esk-2)))
  (deflistener esk)
  (defstrand tpm-decrypt 4 (m esk) (pcr pcr) (k tpmkey) (aik aik-0))
  (precedes ((2 0) (6 0)) ((2 0) (14 0)) ((2 2) (6 2)) ((2 2) (12 0))
    ((2 4) (3 0)) ((2 6) (4 0)) ((2 6) (7 0)) ((3 1) (2 5))
    ((4 3) (1 0)) ((5 3) (4 2)) ((6 1) (2 1)) ((6 3) (5 3))
    ((7 2) (0 0)) ((8 3) (7 1)) ((9 1) (10 0)) ((9 3) (8 3))
    ((10 1) (9 2)) ((11 1) (10 0)) ((12 1) (11 0)) ((13 1) (12 0))
    ((14 3) (13 0)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)) ((8 3) (7 1)) ((9 3) (8 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tne tno tne-0 tne-1 tne-2 esk k)
  (operation nonce-test (added-strand tpm-decrypt 4) esk (13 0)
    (enc esk tpmkey))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey (enc esk tpmkey)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (tran state (hash n state)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-1))
          tno-1 "false"
          (hash esk-1
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-1))) tne-1 tno-1
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-2 (enc esk-2 tpmkey-2)))
      (send (cat "establish transport" tne-2))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-2)) tno-2
          "false"
          (hash esk-2
            (hash "execute transport" (hash "extend" (enc n esk-2)))
            tne-2 tno-2 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-2
         (hash "execute transport" (hash "extend" (enc n esk-2))) tne-2
         tno-2 "false"))
      (send
        (cat esk-2
          (hash "execute transport" (hash "extend" (enc n esk-2))) tne-2
          tno-2 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk-2))))
      (send (cat "execute transport" (hash "extend" (enc n esk-2)))))
    ((recv (cat "extend" (enc n esk-2)))
      (send (cat "extend" (enc n esk-2)))) ((recv esk) (send esk))
    ((recv (cat "decrypt" (enc esk tpmkey)))
      (recv (enc "created" tpmkey pcr aik-0)) (obsv pcr) (send esk)))
  (label 65)
  (parent 61)
  (unrealized (14 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton envelope
  (vars (state pcr nonce mesg)
    (v n tne tno tne-0 tno-0 tne-1 tno-1 tne-2 tno-2 data)
    (esk1 esk esk-0 esk-1 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 tpmkey-2 aik-0 aik-1 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne)
    (tno tno) (esk esk) (tpmkey tpmkey))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-1) (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-2)
    (tno tno-2) (esk esk) (tpmkey tpmkey-2))
  (deflistener
    (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
      tne-2 tno-2 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk))))
  (deflistener (cat "extend" (enc n esk)))
  (defstrand tpm-decrypt 4 (m esk) (pcr pcr) (k tpmkey) (aik aik-0))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (enc "created" tpmkey pcr aik-0)) (aik aik-1))
  (precedes ((2 0) (6 0)) ((2 0) (9 0)) ((2 0) (13 0)) ((2 2) (6 2))
    ((2 2) (12 0)) ((2 4) (3 0)) ((2 6) (4 0)) ((2 6) (7 0))
    ((3 1) (2 5)) ((4 3) (1 0)) ((5 3) (4 2)) ((6 1) (2 1))
    ((6 3) (5 3)) ((7 2) (0 0)) ((8 3) (7 1)) ((9 1) (10 0))
    ((9 3) (8 3)) ((10 1) (9 2)) ((11 1) (10 0)) ((12 1) (11 0))
    ((13 3) (10 0)) ((14 2) (13 1)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)) ((8 3) (7 1)) ((9 3) (8 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 aik-1 (invk k) (invk tpmkey))
  (uniq-orig v n tne tno tne-0 tne-1 tne-2 esk k)
  (operation encryption-test (added-strand tpm-quote 3)
    (enc "created" tpmkey pcr aik-0) (13 1))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey (enc esk tpmkey)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (tran state (hash n state)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-1))
          tno-1 "false"
          (hash esk-1
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-1))) tne-1 tno-1
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-2 (enc esk tpmkey-2)))
      (send (cat "establish transport" tne-2))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno-2
          "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne-2
            tno-2 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
         tne-2 tno-2 "false"))
      (send
        (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
          tne-2 tno-2 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk))))
      (send (cat "execute transport" (hash "extend" (enc n esk)))))
    ((recv (cat "extend" (enc n esk)))
      (send (cat "extend" (enc n esk))))
    ((recv (cat "decrypt" (enc esk tpmkey)))
      (recv (enc "created" tpmkey pcr aik-0)) (obsv pcr) (send esk))
    ((recv (cat "quote" nonce)) (obsv (enc "created" tpmkey pcr aik-0))
      (send
        (enc "quote" (enc "created" tpmkey pcr aik-0) nonce aik-1))))
  (label 66)
  (parent 63)
  (unrealized (9 0) (14 1))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (state pcr nonce mesg)
    (v n tne tno tne-0 tno-0 tne-1 tno-1 tne-2 tno-2 data)
    (esk1 esk esk-0 esk-1 esk-2 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 tpmkey-2 aik-0 aik-1 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne)
    (tno tno) (esk esk) (tpmkey tpmkey))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-1) (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-2)
    (tno tno-2) (esk esk-2) (tpmkey tpmkey-2))
  (deflistener
    (cat esk-2 (hash "execute transport" (hash "extend" (enc n esk-2)))
      tne-2 tno-2 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk-2))))
  (deflistener (cat "extend" (enc n esk-2)))
  (deflistener esk)
  (defstrand tpm-decrypt 4 (m esk) (pcr pcr) (k tpmkey) (aik aik-0))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (enc "created" tpmkey pcr aik-0)) (aik aik-1))
  (precedes ((2 0) (6 0)) ((2 0) (14 0)) ((2 2) (6 2)) ((2 2) (12 0))
    ((2 4) (3 0)) ((2 6) (4 0)) ((2 6) (7 0)) ((3 1) (2 5))
    ((4 3) (1 0)) ((5 3) (4 2)) ((6 1) (2 1)) ((6 3) (5 3))
    ((7 2) (0 0)) ((8 3) (7 1)) ((9 1) (10 0)) ((9 3) (8 3))
    ((10 1) (9 2)) ((11 1) (10 0)) ((12 1) (11 0)) ((13 1) (12 0))
    ((14 3) (13 0)) ((15 2) (14 1)))
  (leadsto ((5 3) (4 2)) ((6 3) (5 3)) ((8 3) (7 1)) ((9 3) (8 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 aik-1 (invk k) (invk tpmkey))
  (uniq-orig v n tne tno tne-0 tne-1 tne-2 esk k)
  (operation encryption-test (added-strand tpm-quote 3)
    (enc "created" tpmkey pcr aik-0) (14 1))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey (enc esk tpmkey)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (tran state (hash n state)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-1))
          tno-1 "false"
          (hash esk-1
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-1))) tne-1 tno-1
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-2 (enc esk-2 tpmkey-2)))
      (send (cat "establish transport" tne-2))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-2)) tno-2
          "false"
          (hash esk-2
            (hash "execute transport" (hash "extend" (enc n esk-2)))
            tne-2 tno-2 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-2
         (hash "execute transport" (hash "extend" (enc n esk-2))) tne-2
         tno-2 "false"))
      (send
        (cat esk-2
          (hash "execute transport" (hash "extend" (enc n esk-2))) tne-2
          tno-2 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk-2))))
      (send (cat "execute transport" (hash "extend" (enc n esk-2)))))
    ((recv (cat "extend" (enc n esk-2)))
      (send (cat "extend" (enc n esk-2)))) ((recv esk) (send esk))
    ((recv (cat "decrypt" (enc esk tpmkey)))
      (recv (enc "created" tpmkey pcr aik-0)) (obsv pcr) (send esk))
    ((recv (cat "quote" nonce)) (obsv (enc "created" tpmkey pcr aik-0))
      (send
        (enc "quote" (enc "created" tpmkey pcr aik-0) nonce aik-1))))
  (label 67)
  (parent 65)
  (unrealized (15 1))
  (dead)
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol envelope basic
  (defrole tpm-power-on (vars) (trace (init "0")))
  (defrole tpm-quote
    (vars (nonce pcr mesg) (aik akey))
    (trace (recv (cat "quote" nonce)) (obsv pcr)
      (send (enc "quote" pcr nonce aik)))
    (non-orig aik))
  (defrole tpm-extend-enc
    (vars (value state mesg) (esk skey) (tne tno data) (tpmkey akey))
    (trace (recv (cat "establish transport" tpmkey (enc esk tpmkey)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc value esk)) tno
          "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc value esk)))
            tne tno "false"))) (tran state (hash value state)))
    (uniq-orig tne))
  (defrole tpm-create-key
    (vars (k aik akey) (pcr mesg) (esk skey))
    (trace (recv (enc "create key" pcr esk))
      (send (enc "created" k pcr aik)))
    (non-orig (invk k) aik esk)
    (uniq-orig k))
  (defrole tpm-decrypt
    (vars (m pcr mesg) (k aik akey))
    (trace (recv (cat "decrypt" (enc m k)))
      (recv (enc "created" k pcr aik)) (obsv pcr) (send m))
    (non-orig aik))
  (defrole alice
    (vars (v n tne tno data) (esk1 esk skey) (k aik tpmkey akey)
      (pcr mesg))
    (trace (send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k)))
    (non-orig aik esk1 (invk tpmkey))
    (uniq-orig v n tno esk)
    (neq (tno n))))

(defskeleton envelope
  (vars (pcr mesg) (v n tne tno data) (esk1 esk skey)
    (k aik tpmkey akey))
  (deflistener v)
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (neq (tno n))
  (non-orig esk1 aik (invk tpmkey))
  (uniq-orig v n tno esk)
  (traces ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k))))
  (label 68)
  (unrealized (0 0) (1 5))
  (preskeleton)
  (origs (esk (1 0)) (n (1 2)) (tno (1 2)) (v (1 6)))
  (comment "Not a skeleton"))

(defskeleton envelope
  (vars (pcr mesg) (v n tne tno data) (esk1 esk skey)
    (k aik tpmkey akey))
  (deflistener v)
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (precedes ((1 6) (0 0)))
  (neq (tno n))
  (non-orig esk1 aik (invk tpmkey))
  (uniq-orig v n tno esk)
  (traces ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k))))
  (label 69)
  (parent 68)
  (unrealized (1 5))
  (origs (esk (1 0)) (n (1 2)) (tno (1 2)) (v (1 6)))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton envelope
  (vars (pcr nonce mesg) (v n tne tno data) (esk1 esk skey)
    (k aik tpmkey aik-0 akey))
  (deflistener v)
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (enc "created" k (hash "obtain" (hash n pcr)) aik))
    (aik aik-0))
  (precedes ((1 6) (0 0)) ((2 2) (1 5)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk tpmkey))
  (uniq-orig v n tno esk)
  (operation encryption-test (added-strand tpm-quote 3)
    (enc "created" k (hash "obtain" (hash n pcr)) aik) (1 5))
  (traces ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k)))
    ((recv (cat "quote" nonce))
      (obsv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send
        (enc "quote" (enc "created" k (hash "obtain" (hash n pcr)) aik)
          nonce aik-0))))
  (label 70)
  (parent 69)
  (unrealized (2 1))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (pcr mesg) (v n tne tno data) (esk1 esk esk-0 skey)
    (k aik tpmkey akey))
  (deflistener v)
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n pcr)))
    (esk esk-0) (k k) (aik aik))
  (precedes ((1 6) (0 0)) ((2 1) (1 5)))
  (neq (tno n))
  (non-orig esk1 esk-0 aik (invk k) (invk tpmkey))
  (uniq-orig v n tno esk k)
  (operation encryption-test (added-strand tpm-create-key 2)
    (enc "created" k (hash "obtain" (hash n pcr)) aik) (1 5))
  (traces ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n pcr)) esk-0))
      (send (enc "created" k (hash "obtain" (hash n pcr)) aik))))
  (label 71)
  (parent 69)
  (unrealized (0 0) (2 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton envelope
  (vars (pcr nonce mesg) (v n tne tno data) (esk1 esk esk-0 skey)
    (k aik tpmkey aik-0 akey))
  (deflistener v)
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n pcr)))
    (esk esk-0) (k k) (aik aik))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (enc "create key" (hash "obtain" (hash n pcr)) esk-0))
    (aik aik-0))
  (precedes ((1 6) (0 0)) ((2 1) (1 5)) ((3 2) (2 0)))
  (neq (tno n))
  (non-orig esk1 esk-0 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno esk k)
  (operation encryption-test (added-strand tpm-quote 3)
    (enc "create key" (hash "obtain" (hash n pcr)) esk-0) (2 0))
  (traces ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n pcr)) esk-0))
      (send (enc "created" k (hash "obtain" (hash n pcr)) aik)))
    ((recv (cat "quote" nonce))
      (obsv (enc "create key" (hash "obtain" (hash n pcr)) esk-0))
      (send
        (enc "quote"
          (enc "create key" (hash "obtain" (hash n pcr)) esk-0) nonce
          aik-0))))
  (label 72)
  (parent 71)
  (unrealized (0 0) (3 1))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (pcr mesg) (v n tne tno data) (esk1 esk skey)
    (k aik tpmkey akey))
  (deflistener v)
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n pcr)))
    (esk esk1) (k k) (aik aik))
  (precedes ((1 4) (2 0)) ((1 6) (0 0)) ((2 1) (1 5)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tno esk k)
  (operation encryption-test (displaced 3 1 alice 5)
    (enc "create key" (hash "obtain" (hash n pcr)) esk-0) (2 0))
  (traces ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (send (enc "created" k (hash "obtain" (hash n pcr)) aik))))
  (label 73)
  (parent 71)
  (unrealized (0 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton envelope
  (vars (pcr nonce mesg) (v n tne tno data) (esk1 esk skey)
    (k aik tpmkey aik-0 akey))
  (deflistener v)
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n pcr)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-quote 3 (nonce nonce) (pcr v) (aik aik-0))
  (precedes ((1 4) (2 0)) ((1 6) (3 1)) ((2 1) (1 5)) ((3 2) (0 0)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno esk k)
  (operation nonce-test (added-strand tpm-quote 3) v (0 0) (enc v k))
  (traces ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (send (enc "created" k (hash "obtain" (hash n pcr)) aik)))
    ((recv (cat "quote" nonce)) (obsv v)
      (send (enc "quote" v nonce aik-0))))
  (label 74)
  (parent 73)
  (unrealized (3 1))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (pcr pcr-0 mesg) (v n tne tno data) (esk1 esk skey)
    (k aik tpmkey aik-0 akey))
  (deflistener v)
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n pcr)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr pcr-0) (k k) (aik aik-0))
  (precedes ((1 4) (2 0)) ((1 6) (3 0)) ((2 1) (1 5)) ((3 3) (0 0)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno esk k)
  (operation nonce-test (added-strand tpm-decrypt 4) v (0 0) (enc v k))
  (traces ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (send (enc "created" k (hash "obtain" (hash n pcr)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k pcr-0 aik-0)) (obsv pcr-0) (send v)))
  (label 75)
  (parent 73)
  (unrealized (3 1))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton envelope
  (vars (pcr pcr-0 nonce mesg) (v n tne tno data) (esk1 esk skey)
    (k aik tpmkey aik-0 aik-1 akey))
  (deflistener v)
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n pcr)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr pcr-0) (k k) (aik aik-0))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (enc "created" k pcr-0 aik-0)) (aik aik-1))
  (precedes ((1 4) (2 0)) ((1 6) (3 0)) ((2 1) (1 5)) ((2 1) (4 1))
    ((3 3) (0 0)) ((4 2) (3 1)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 aik-1 (invk k) (invk tpmkey))
  (uniq-orig v n tno esk k)
  (operation encryption-test (added-strand tpm-quote 3)
    (enc "created" k pcr-0 aik-0) (3 1))
  (traces ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (send (enc "created" k (hash "obtain" (hash n pcr)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k pcr-0 aik-0)) (obsv pcr-0) (send v))
    ((recv (cat "quote" nonce)) (obsv (enc "created" k pcr-0 aik-0))
      (send (enc "quote" (enc "created" k pcr-0 aik-0) nonce aik-1))))
  (label 76)
  (parent 75)
  (unrealized (4 1))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (pcr mesg) (v n tne tno data) (esk1 esk skey)
    (k aik tpmkey akey))
  (deflistener v)
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n pcr)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n pcr)))
    (k k) (aik aik))
  (precedes ((1 4) (2 0)) ((1 6) (3 0)) ((2 1) (1 5)) ((3 3) (0 0)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tno esk k)
  (operation encryption-test (displaced 4 2 tpm-create-key 2)
    (enc "created" k pcr-0 aik-0) (3 1))
  (traces ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (send (enc "created" k (hash "obtain" (hash n pcr)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (obsv (hash "obtain" (hash n pcr))) (send v)))
  (label 77)
  (parent 75)
  (unrealized (3 2))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton envelope
  (vars (pcr mesg) (v n tne tno tne-0 tno-0 data) (esk1 esk esk-0 skey)
    (k aik tpmkey tpmkey-0 akey))
  (deflistener v)
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n pcr)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n pcr)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n pcr))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (precedes ((1 4) (2 0)) ((1 6) (3 0)) ((2 1) (1 5)) ((3 3) (0 0))
    ((4 3) (3 2)))
  (leadsto ((4 3) (3 2)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 esk k)
  (operation state-passing-test (added-strand tpm-extend-enc 4)
    (hash "obtain" (hash n pcr)) (3 2))
  (traces ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (send (enc "created" k (hash "obtain" (hash n pcr)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (obsv (hash "obtain" (hash n pcr))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n pcr) (hash "obtain" (hash n pcr)))))
  (label 78)
  (parent 77)
  (unrealized (4 3))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton envelope
  (vars (state mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 esk-1 skey) (k aik tpmkey tpmkey-0 tpmkey-1 akey))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (precedes ((1 2) (5 2)) ((1 4) (2 0)) ((1 6) (3 0)) ((2 1) (1 5))
    ((3 3) (0 0)) ((4 3) (3 2)) ((5 3) (4 3)))
  (leadsto ((4 3) (3 2)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation state-passing-test (added-strand tpm-extend-enc 4)
    (hash n state) (4 3))
  (traces ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state))))
  (label 79)
  (parent 78)
  (unrealized (5 2))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton envelope
  (vars (state nonce mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 esk-1 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 akey))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr
      (hash esk-1
        (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
        tno-1 "false")) (aik aik-0))
  (precedes ((1 2) (5 2)) ((1 4) (2 0)) ((1 6) (3 0)) ((2 1) (1 5))
    ((3 3) (0 0)) ((4 3) (3 2)) ((5 3) (4 3)) ((6 2) (5 2)))
  (leadsto ((4 3) (3 2)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation encryption-test (added-strand tpm-quote 3)
    (hash esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false") (5 2))
  (traces ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv (cat "quote" nonce))
      (obsv
        (hash esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false"))
      (send
        (enc "quote"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false") nonce aik-0))))
  (label 80)
  (parent 79)
  (unrealized (5 2) (6 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton envelope
  (vars (state mesg) (v n tne tno tne-0 tno-0 data)
    (esk1 esk esk-0 skey) (k aik tpmkey tpmkey-0 tpmkey-1 akey))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne)
    (tno tno) (esk esk) (tpmkey tpmkey-1))
  (precedes ((1 0) (5 0)) ((1 2) (5 2)) ((1 4) (2 0)) ((1 6) (3 0))
    ((2 1) (1 5)) ((3 3) (0 0)) ((4 3) (3 2)) ((5 1) (1 1))
    ((5 3) (4 3)))
  (leadsto ((4 3) (3 2)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tne tno tne-0 esk k)
  (operation encryption-test (displaced 6 1 alice 3)
    (hash esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false") (5 2))
  (traces ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk tpmkey-1)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (tran state (hash n state))))
  (label 81)
  (parent 79)
  (unrealized (5 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton envelope
  (vars (state mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 esk-1 skey) (k aik tpmkey tpmkey-0 tpmkey-1 akey))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (precedes ((1 2) (5 2)) ((1 4) (2 0)) ((1 6) (3 0)) ((2 1) (1 5))
    ((3 3) (0 0)) ((4 3) (3 2)) ((5 1) (6 0)) ((5 3) (4 3))
    ((6 1) (5 2)))
  (leadsto ((4 3) (3 2)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation encryption-test
    (added-listener
      (cat esk-1
        (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
        tno-1 "false"))
    (hash esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false") (5 2))
  (traces ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false"))))
  (label 82)
  (parent 79)
  (unrealized (5 2) (6 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton envelope
  (vars (state nonce mesg)
    (v n tne tno tne-0 tno-0 tne-1 tno-1 tne-2 tno-2 data)
    (esk1 esk esk-0 esk-1 esk-2 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 tpmkey-2 akey))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr
      (hash esk-1
        (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
        tno-1 "false")) (aik aik-0))
  (defstrand tpm-extend-enc 4 (value esk-1)
    (state
      (cat (hash "execute transport" (hash "extend" (enc n esk-1)))
        tne-1 tno-1 "false")) (tne tne-2) (tno tno-2) (esk esk-2)
    (tpmkey tpmkey-2))
  (precedes ((1 2) (5 2)) ((1 4) (2 0)) ((1 6) (3 0)) ((2 1) (1 5))
    ((3 3) (0 0)) ((4 3) (3 2)) ((5 1) (7 3)) ((5 3) (4 3))
    ((6 2) (5 2)) ((7 3) (6 1)))
  (leadsto ((4 3) (3 2)) ((5 3) (4 3)) ((7 3) (6 1)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 tne-2 esk k)
  (operation state-passing-test (added-strand tpm-extend-enc 4)
    (hash esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false") (6 1))
  (traces ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv (cat "quote" nonce))
      (obsv
        (hash esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false"))
      (send
        (enc "quote"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false") nonce aik-0)))
    ((recv (cat "establish transport" tpmkey-2 (enc esk-2 tpmkey-2)))
      (send (cat "establish transport" tne-2))
      (recv
        (cat "execute transport" (cat "extend" (enc esk-1 esk-2)) tno-2
          "false"
          (hash esk-2
            (hash "execute transport" (hash "extend" (enc esk-1 esk-2)))
            tne-2 tno-2 "false")))
      (tran
        (cat (hash "execute transport" (hash "extend" (enc n esk-1)))
          tne-1 tno-1 "false")
        (hash esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false"))))
  (label 83)
  (parent 80)
  (unrealized (5 2) (7 3))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (state mesg) (v n tne tno tne-0 tno-0 data)
    (esk1 esk esk-0 skey) (k aik tpmkey tpmkey-0 akey))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne)
    (tno tno) (esk esk) (tpmkey tpmkey))
  (precedes ((1 0) (5 0)) ((1 2) (5 2)) ((1 4) (2 0)) ((1 6) (3 0))
    ((2 1) (1 5)) ((3 3) (0 0)) ((4 3) (3 2)) ((5 1) (1 1))
    ((5 3) (4 3)))
  (leadsto ((4 3) (3 2)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tne tno tne-0 esk k)
  (operation nonce-test (contracted (tpmkey-1 tpmkey)) esk (5 0)
    (enc esk tpmkey))
  (traces ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey (enc esk tpmkey)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (tran state (hash n state))))
  (label 84)
  (parent 81)
  (unrealized)
  (shape)
  (maps
    ((0 1)
      ((v v) (n n) (tne tne) (tno tno) (esk1 esk1) (esk esk) (k k)
        (aik aik) (tpmkey tpmkey) (pcr state))))
  (origs (n (1 2)) (tno (1 2)) (esk (1 0)) (tne (5 1)) (tne-0 (4 1))
    (k (2 1)) (v (1 6))))

(defskeleton envelope
  (vars (state nonce mesg) (v n tne tno tne-0 tno-0 data)
    (esk1 esk esk-0 skey) (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 akey))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne)
    (tno tno) (esk esk) (tpmkey tpmkey-1))
  (defstrand tpm-quote 3 (nonce nonce) (pcr esk) (aik aik-0))
  (precedes ((1 0) (6 1)) ((1 2) (5 2)) ((1 4) (2 0)) ((1 6) (3 0))
    ((2 1) (1 5)) ((3 3) (0 0)) ((4 3) (3 2)) ((5 1) (1 1))
    ((5 3) (4 3)) ((6 2) (5 0)))
  (leadsto ((4 3) (3 2)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tne tno tne-0 esk k)
  (operation nonce-test (added-strand tpm-quote 3) esk (5 0)
    (enc esk tpmkey))
  (traces ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk tpmkey-1)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (tran state (hash n state)))
    ((recv (cat "quote" nonce)) (obsv esk)
      (send (enc "quote" esk nonce aik-0))))
  (label 85)
  (parent 81)
  (unrealized (6 1))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (state pcr mesg) (v n tne tno tne-0 tno-0 data)
    (esk1 esk esk-0 skey) (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 akey))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne)
    (tno tno) (esk esk) (tpmkey tpmkey-1))
  (defstrand tpm-decrypt 4 (m esk) (pcr pcr) (k tpmkey) (aik aik-0))
  (precedes ((1 0) (6 0)) ((1 2) (5 2)) ((1 4) (2 0)) ((1 6) (3 0))
    ((2 1) (1 5)) ((3 3) (0 0)) ((4 3) (3 2)) ((5 1) (1 1))
    ((5 3) (4 3)) ((6 3) (5 0)))
  (leadsto ((4 3) (3 2)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tne tno tne-0 esk k)
  (operation nonce-test (added-strand tpm-decrypt 4) esk (5 0)
    (enc esk tpmkey))
  (traces ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk tpmkey-1)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (tran state (hash n state)))
    ((recv (cat "decrypt" (enc esk tpmkey)))
      (recv (enc "created" tpmkey pcr aik-0)) (obsv pcr) (send esk)))
  (label 86)
  (parent 81)
  (unrealized (6 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton envelope
  (vars (state nonce mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 esk-1 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 akey))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (hash "execute transport" (hash "extend" (enc n esk-1))))
    (aik aik-0))
  (precedes ((1 2) (5 2)) ((1 4) (2 0)) ((1 6) (3 0)) ((2 1) (1 5))
    ((3 3) (0 0)) ((4 3) (3 2)) ((5 1) (6 0)) ((5 3) (4 3))
    ((6 1) (5 2)) ((7 2) (6 0)))
  (leadsto ((4 3) (3 2)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation encryption-test (added-strand tpm-quote 3)
    (hash "execute transport" (hash "extend" (enc n esk-1))) (6 0))
  (traces ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false")))
    ((recv (cat "quote" nonce))
      (obsv (hash "execute transport" (hash "extend" (enc n esk-1))))
      (send
        (enc "quote"
          (hash "execute transport" (hash "extend" (enc n esk-1))) nonce
          aik-0))))
  (label 87)
  (parent 82)
  (unrealized (5 2) (7 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton envelope
  (vars (state mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 esk-1 skey) (k aik tpmkey tpmkey-0 tpmkey-1 akey))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk-1))))
  (precedes ((1 2) (5 2)) ((1 4) (2 0)) ((1 6) (3 0)) ((2 1) (1 5))
    ((3 3) (0 0)) ((4 3) (3 2)) ((5 1) (6 0)) ((5 3) (4 3))
    ((6 1) (5 2)) ((7 1) (6 0)))
  (leadsto ((4 3) (3 2)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation encryption-test
    (added-listener
      (cat "execute transport" (hash "extend" (enc n esk-1))))
    (hash "execute transport" (hash "extend" (enc n esk-1))) (6 0))
  (traces ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk-1))))
      (send (cat "execute transport" (hash "extend" (enc n esk-1))))))
  (label 88)
  (parent 82)
  (unrealized (5 2) (7 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton envelope
  (vars (state pcr nonce mesg) (v n tne tno tne-0 tno-0 data)
    (esk1 esk esk-0 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 aik-1 akey))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne)
    (tno tno) (esk esk) (tpmkey tpmkey-1))
  (defstrand tpm-decrypt 4 (m esk) (pcr pcr) (k tpmkey) (aik aik-0))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (enc "created" tpmkey pcr aik-0)) (aik aik-1))
  (precedes ((1 0) (6 0)) ((1 2) (5 2)) ((1 4) (2 0)) ((1 6) (3 0))
    ((2 1) (1 5)) ((3 3) (0 0)) ((4 3) (3 2)) ((5 1) (1 1))
    ((5 3) (4 3)) ((6 3) (5 0)) ((7 2) (6 1)))
  (leadsto ((4 3) (3 2)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 aik-1 (invk k) (invk tpmkey))
  (uniq-orig v n tne tno tne-0 esk k)
  (operation encryption-test (added-strand tpm-quote 3)
    (enc "created" tpmkey pcr aik-0) (6 1))
  (traces ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk tpmkey-1)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (tran state (hash n state)))
    ((recv (cat "decrypt" (enc esk tpmkey)))
      (recv (enc "created" tpmkey pcr aik-0)) (obsv pcr) (send esk))
    ((recv (cat "quote" nonce)) (obsv (enc "created" tpmkey pcr aik-0))
      (send
        (enc "quote" (enc "created" tpmkey pcr aik-0) nonce aik-1))))
  (label 89)
  (parent 86)
  (unrealized (7 1))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (state nonce mesg)
    (v n tne tno tne-0 tno-0 tne-1 tno-1 tne-2 tno-2 data)
    (esk1 esk esk-0 esk-1 esk-2 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 tpmkey-2 akey))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (hash "execute transport" (hash "extend" (enc n esk-1))))
    (aik aik-0))
  (defstrand tpm-extend-enc 4 (value "execute transport")
    (state (hash "extend" (enc n esk-1))) (tne tne-2) (tno tno-2)
    (esk esk-2) (tpmkey tpmkey-2))
  (precedes ((1 2) (5 2)) ((1 4) (2 0)) ((1 6) (3 0)) ((2 1) (1 5))
    ((3 3) (0 0)) ((4 3) (3 2)) ((5 1) (6 0)) ((5 3) (4 3))
    ((6 1) (5 2)) ((7 2) (6 0)) ((8 3) (7 1)))
  (leadsto ((4 3) (3 2)) ((5 3) (4 3)) ((8 3) (7 1)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 tne-2 esk k)
  (operation state-passing-test (added-strand tpm-extend-enc 4)
    (hash "execute transport" (hash "extend" (enc n esk-1))) (7 1))
  (traces ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false")))
    ((recv (cat "quote" nonce))
      (obsv (hash "execute transport" (hash "extend" (enc n esk-1))))
      (send
        (enc "quote"
          (hash "execute transport" (hash "extend" (enc n esk-1))) nonce
          aik-0)))
    ((recv (cat "establish transport" tpmkey-2 (enc esk-2 tpmkey-2)))
      (send (cat "establish transport" tne-2))
      (recv
        (cat "execute transport"
          (cat "extend" (enc "execute transport" esk-2)) tno-2 "false"
          (hash esk-2
            (hash "execute transport"
              (hash "extend" (enc "execute transport" esk-2))) tne-2
            tno-2 "false")))
      (tran (hash "extend" (enc n esk-1))
        (hash "execute transport" (hash "extend" (enc n esk-1))))))
  (label 90)
  (parent 87)
  (unrealized (5 2) (8 3))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton envelope
  (vars (state nonce mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 esk-1 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 akey))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk-1))))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (hash "extend" (enc n esk-1))) (aik aik-0))
  (precedes ((1 2) (5 2)) ((1 4) (2 0)) ((1 6) (3 0)) ((2 1) (1 5))
    ((3 3) (0 0)) ((4 3) (3 2)) ((5 1) (6 0)) ((5 3) (4 3))
    ((6 1) (5 2)) ((7 1) (6 0)) ((8 2) (7 0)))
  (leadsto ((4 3) (3 2)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation encryption-test (added-strand tpm-quote 3)
    (hash "extend" (enc n esk-1)) (7 0))
  (traces ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk-1))))
      (send (cat "execute transport" (hash "extend" (enc n esk-1)))))
    ((recv (cat "quote" nonce)) (obsv (hash "extend" (enc n esk-1)))
      (send (enc "quote" (hash "extend" (enc n esk-1)) nonce aik-0))))
  (label 91)
  (parent 88)
  (unrealized (5 2) (8 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton envelope
  (vars (state mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 esk-1 skey) (k aik tpmkey tpmkey-0 tpmkey-1 akey))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk-1))))
  (deflistener (cat "extend" (enc n esk-1)))
  (precedes ((1 2) (8 0)) ((1 4) (2 0)) ((1 6) (3 0)) ((2 1) (1 5))
    ((3 3) (0 0)) ((4 3) (3 2)) ((5 1) (6 0)) ((5 3) (4 3))
    ((6 1) (5 2)) ((7 1) (6 0)) ((8 1) (7 0)))
  (leadsto ((4 3) (3 2)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation encryption-test
    (added-listener (cat "extend" (enc n esk-1)))
    (hash "extend" (enc n esk-1)) (7 0))
  (traces ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk-1))))
      (send (cat "execute transport" (hash "extend" (enc n esk-1)))))
    ((recv (cat "extend" (enc n esk-1)))
      (send (cat "extend" (enc n esk-1)))))
  (label 92)
  (parent 88)
  (unrealized (8 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton envelope
  (vars (state nonce mesg)
    (v n tne tno tne-0 tno-0 tne-1 tno-1 tne-2 tno-2 tne-3 tno-3 data)
    (esk1 esk esk-0 esk-1 esk-2 esk-3 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 tpmkey-2 tpmkey-3 akey))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (hash "execute transport" (hash "extend" (enc n esk-1))))
    (aik aik-0))
  (defstrand tpm-extend-enc 4 (value "execute transport")
    (state (hash "extend" (enc n esk-1))) (tne tne-2) (tno tno-2)
    (esk esk-2) (tpmkey tpmkey-2))
  (defstrand tpm-extend-enc 4 (value "extend") (state (enc n esk-1))
    (tne tne-3) (tno tno-3) (esk esk-3) (tpmkey tpmkey-3))
  (precedes ((1 2) (9 3)) ((1 4) (2 0)) ((1 6) (3 0)) ((2 1) (1 5))
    ((3 3) (0 0)) ((4 3) (3 2)) ((5 1) (6 0)) ((5 3) (4 3))
    ((6 1) (5 2)) ((7 2) (6 0)) ((8 3) (7 1)) ((9 3) (8 3)))
  (leadsto ((4 3) (3 2)) ((5 3) (4 3)) ((8 3) (7 1)) ((9 3) (8 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 tne-2 tne-3 esk k)
  (operation state-passing-test (added-strand tpm-extend-enc 4)
    (hash "extend" (enc n esk-1)) (8 3))
  (traces ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false")))
    ((recv (cat "quote" nonce))
      (obsv (hash "execute transport" (hash "extend" (enc n esk-1))))
      (send
        (enc "quote"
          (hash "execute transport" (hash "extend" (enc n esk-1))) nonce
          aik-0)))
    ((recv (cat "establish transport" tpmkey-2 (enc esk-2 tpmkey-2)))
      (send (cat "establish transport" tne-2))
      (recv
        (cat "execute transport"
          (cat "extend" (enc "execute transport" esk-2)) tno-2 "false"
          (hash esk-2
            (hash "execute transport"
              (hash "extend" (enc "execute transport" esk-2))) tne-2
            tno-2 "false")))
      (tran (hash "extend" (enc n esk-1))
        (hash "execute transport" (hash "extend" (enc n esk-1)))))
    ((recv (cat "establish transport" tpmkey-3 (enc esk-3 tpmkey-3)))
      (send (cat "establish transport" tne-3))
      (recv
        (cat "execute transport" (cat "extend" (enc "extend" esk-3))
          tno-3 "false"
          (hash esk-3
            (hash "execute transport"
              (hash "extend" (enc "extend" esk-3))) tne-3 tno-3
            "false")))
      (tran (enc n esk-1) (hash "extend" (enc n esk-1)))))
  (label 93)
  (parent 90)
  (unrealized (5 2) (9 3))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (state nonce mesg)
    (v n tne tno tne-0 tno-0 tne-1 tno-1 tne-2 tno-2 data)
    (esk1 esk esk-0 esk-1 esk-2 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 tpmkey-2 akey))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk-1))))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (hash "extend" (enc n esk-1))) (aik aik-0))
  (defstrand tpm-extend-enc 4 (value "extend") (state (enc n esk-1))
    (tne tne-2) (tno tno-2) (esk esk-2) (tpmkey tpmkey-2))
  (precedes ((1 2) (9 3)) ((1 4) (2 0)) ((1 6) (3 0)) ((2 1) (1 5))
    ((3 3) (0 0)) ((4 3) (3 2)) ((5 1) (6 0)) ((5 3) (4 3))
    ((6 1) (5 2)) ((7 1) (6 0)) ((8 2) (7 0)) ((9 3) (8 1)))
  (leadsto ((4 3) (3 2)) ((5 3) (4 3)) ((9 3) (8 1)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 tne-2 esk k)
  (operation state-passing-test (added-strand tpm-extend-enc 4)
    (hash "extend" (enc n esk-1)) (8 1))
  (traces ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk-1))))
      (send (cat "execute transport" (hash "extend" (enc n esk-1)))))
    ((recv (cat "quote" nonce)) (obsv (hash "extend" (enc n esk-1)))
      (send (enc "quote" (hash "extend" (enc n esk-1)) nonce aik-0)))
    ((recv (cat "establish transport" tpmkey-2 (enc esk-2 tpmkey-2)))
      (send (cat "establish transport" tne-2))
      (recv
        (cat "execute transport" (cat "extend" (enc "extend" esk-2))
          tno-2 "false"
          (hash esk-2
            (hash "execute transport"
              (hash "extend" (enc "extend" esk-2))) tne-2 tno-2
            "false")))
      (tran (enc n esk-1) (hash "extend" (enc n esk-1)))))
  (label 94)
  (parent 91)
  (unrealized (5 2) (9 3))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (state mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 skey) (k aik tpmkey tpmkey-0 tpmkey-1 akey))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk) (tpmkey tpmkey-1))
  (deflistener
    (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk))))
  (deflistener (cat "extend" (enc n esk)))
  (precedes ((1 0) (5 0)) ((1 2) (8 0)) ((1 4) (2 0)) ((1 6) (3 0))
    ((2 1) (1 5)) ((3 3) (0 0)) ((4 3) (3 2)) ((5 1) (6 0))
    ((5 3) (4 3)) ((6 1) (5 2)) ((7 1) (6 0)) ((8 1) (7 0)))
  (leadsto ((4 3) (3 2)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation nonce-test (contracted (esk-1 esk)) n (8 0) (enc n esk))
  (traces ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno-1
          "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne-1
            tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
         tne-1 tno-1 "false"))
      (send
        (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
          tne-1 tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk))))
      (send (cat "execute transport" (hash "extend" (enc n esk)))))
    ((recv (cat "extend" (enc n esk)))
      (send (cat "extend" (enc n esk)))))
  (label 95)
  (parent 92)
  (unrealized (5 0) (6 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton envelope
  (vars (state nonce mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 esk-1 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 akey))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk-1))))
  (deflistener (cat "extend" (enc n esk-1)))
  (defstrand tpm-quote 3 (nonce nonce) (pcr n) (aik aik-0))
  (precedes ((1 2) (9 1)) ((1 4) (2 0)) ((1 6) (3 0)) ((2 1) (1 5))
    ((3 3) (0 0)) ((4 3) (3 2)) ((5 1) (6 0)) ((5 3) (4 3))
    ((6 1) (5 2)) ((7 1) (6 0)) ((8 1) (7 0)) ((9 2) (8 0)))
  (leadsto ((4 3) (3 2)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation nonce-test (added-strand tpm-quote 3) n (8 0) (enc n esk))
  (traces ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk-1))))
      (send (cat "execute transport" (hash "extend" (enc n esk-1)))))
    ((recv (cat "extend" (enc n esk-1)))
      (send (cat "extend" (enc n esk-1))))
    ((recv (cat "quote" nonce)) (obsv n)
      (send (enc "quote" n nonce aik-0))))
  (label 96)
  (parent 92)
  (unrealized (9 1))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (state mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 esk-1 skey) (k aik tpmkey tpmkey-0 tpmkey-1 akey))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk-1))))
  (deflistener (cat "extend" (enc n esk-1)))
  (deflistener esk)
  (precedes ((1 0) (9 0)) ((1 2) (8 0)) ((1 4) (2 0)) ((1 6) (3 0))
    ((2 1) (1 5)) ((3 3) (0 0)) ((4 3) (3 2)) ((5 1) (6 0))
    ((5 3) (4 3)) ((6 1) (5 2)) ((7 1) (6 0)) ((8 1) (7 0))
    ((9 1) (8 0)))
  (leadsto ((4 3) (3 2)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation nonce-test (added-listener esk) n (8 0) (enc n esk))
  (traces ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk-1))))
      (send (cat "execute transport" (hash "extend" (enc n esk-1)))))
    ((recv (cat "extend" (enc n esk-1)))
      (send (cat "extend" (enc n esk-1)))) ((recv esk) (send esk)))
  (label 97)
  (parent 92)
  (unrealized (9 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton envelope
  (vars (state nonce mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 skey) (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 akey))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk) (tpmkey tpmkey-1))
  (deflistener
    (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk))))
  (deflistener (cat "extend" (enc n esk)))
  (defstrand tpm-quote 3 (nonce nonce) (pcr esk) (aik aik-0))
  (precedes ((1 0) (5 0)) ((1 0) (9 1)) ((1 2) (8 0)) ((1 4) (2 0))
    ((1 6) (3 0)) ((2 1) (1 5)) ((3 3) (0 0)) ((4 3) (3 2))
    ((5 1) (6 0)) ((5 3) (4 3)) ((6 1) (5 2)) ((7 1) (6 0))
    ((8 1) (7 0)) ((9 2) (6 0)))
  (leadsto ((4 3) (3 2)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation nonce-test (added-strand tpm-quote 3) esk (6 0)
    (enc esk tpmkey))
  (traces ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno-1
          "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne-1
            tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
         tne-1 tno-1 "false"))
      (send
        (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
          tne-1 tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk))))
      (send (cat "execute transport" (hash "extend" (enc n esk)))))
    ((recv (cat "extend" (enc n esk)))
      (send (cat "extend" (enc n esk))))
    ((recv (cat "quote" nonce)) (obsv esk)
      (send (enc "quote" esk nonce aik-0))))
  (label 98)
  (parent 95)
  (unrealized (5 0) (9 1))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (state pcr mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 skey) (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 akey))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk) (tpmkey tpmkey-1))
  (deflistener
    (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk))))
  (deflistener (cat "extend" (enc n esk)))
  (defstrand tpm-decrypt 4 (m esk) (pcr pcr) (k tpmkey) (aik aik-0))
  (precedes ((1 0) (5 0)) ((1 0) (9 0)) ((1 2) (8 0)) ((1 4) (2 0))
    ((1 6) (3 0)) ((2 1) (1 5)) ((3 3) (0 0)) ((4 3) (3 2))
    ((5 1) (6 0)) ((5 3) (4 3)) ((6 1) (5 2)) ((7 1) (6 0))
    ((8 1) (7 0)) ((9 3) (6 0)))
  (leadsto ((4 3) (3 2)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation nonce-test (added-strand tpm-decrypt 4) esk (6 0)
    (enc esk tpmkey))
  (traces ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno-1
          "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne-1
            tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
         tne-1 tno-1 "false"))
      (send
        (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
          tne-1 tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk))))
      (send (cat "execute transport" (hash "extend" (enc n esk)))))
    ((recv (cat "extend" (enc n esk)))
      (send (cat "extend" (enc n esk))))
    ((recv (cat "decrypt" (enc esk tpmkey)))
      (recv (enc "created" tpmkey pcr aik-0)) (obsv pcr) (send esk)))
  (label 99)
  (parent 95)
  (unrealized (5 0) (9 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton envelope
  (vars (state nonce mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 esk-1 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 akey))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk-1))))
  (deflistener (cat "extend" (enc n esk-1)))
  (deflistener esk)
  (defstrand tpm-quote 3 (nonce nonce) (pcr esk) (aik aik-0))
  (precedes ((1 0) (10 1)) ((1 2) (8 0)) ((1 4) (2 0)) ((1 6) (3 0))
    ((2 1) (1 5)) ((3 3) (0 0)) ((4 3) (3 2)) ((5 1) (6 0))
    ((5 3) (4 3)) ((6 1) (5 2)) ((7 1) (6 0)) ((8 1) (7 0))
    ((9 1) (8 0)) ((10 2) (9 0)))
  (leadsto ((4 3) (3 2)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation nonce-test (added-strand tpm-quote 3) esk (9 0)
    (enc esk tpmkey))
  (traces ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk-1))))
      (send (cat "execute transport" (hash "extend" (enc n esk-1)))))
    ((recv (cat "extend" (enc n esk-1)))
      (send (cat "extend" (enc n esk-1)))) ((recv esk) (send esk))
    ((recv (cat "quote" nonce)) (obsv esk)
      (send (enc "quote" esk nonce aik-0))))
  (label 100)
  (parent 97)
  (unrealized (10 1))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (state pcr mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 esk-1 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 akey))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk-1))))
  (deflistener (cat "extend" (enc n esk-1)))
  (deflistener esk)
  (defstrand tpm-decrypt 4 (m esk) (pcr pcr) (k tpmkey) (aik aik-0))
  (precedes ((1 0) (10 0)) ((1 2) (8 0)) ((1 4) (2 0)) ((1 6) (3 0))
    ((2 1) (1 5)) ((3 3) (0 0)) ((4 3) (3 2)) ((5 1) (6 0))
    ((5 3) (4 3)) ((6 1) (5 2)) ((7 1) (6 0)) ((8 1) (7 0))
    ((9 1) (8 0)) ((10 3) (9 0)))
  (leadsto ((4 3) (3 2)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation nonce-test (added-strand tpm-decrypt 4) esk (9 0)
    (enc esk tpmkey))
  (traces ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk-1))))
      (send (cat "execute transport" (hash "extend" (enc n esk-1)))))
    ((recv (cat "extend" (enc n esk-1)))
      (send (cat "extend" (enc n esk-1)))) ((recv esk) (send esk))
    ((recv (cat "decrypt" (enc esk tpmkey)))
      (recv (enc "created" tpmkey pcr aik-0)) (obsv pcr) (send esk)))
  (label 101)
  (parent 97)
  (unrealized (10 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton envelope
  (vars (state pcr nonce mesg)
    (v n tne tno tne-0 tno-0 tne-1 tno-1 data) (esk1 esk esk-0 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 aik-1 akey))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk) (tpmkey tpmkey-1))
  (deflistener
    (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk))))
  (deflistener (cat "extend" (enc n esk)))
  (defstrand tpm-decrypt 4 (m esk) (pcr pcr) (k tpmkey) (aik aik-0))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (enc "created" tpmkey pcr aik-0)) (aik aik-1))
  (precedes ((1 0) (5 0)) ((1 0) (9 0)) ((1 2) (8 0)) ((1 4) (2 0))
    ((1 6) (3 0)) ((2 1) (1 5)) ((3 3) (0 0)) ((4 3) (3 2))
    ((5 1) (6 0)) ((5 3) (4 3)) ((6 1) (5 2)) ((7 1) (6 0))
    ((8 1) (7 0)) ((9 3) (6 0)) ((10 2) (9 1)))
  (leadsto ((4 3) (3 2)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 aik-1 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation encryption-test (added-strand tpm-quote 3)
    (enc "created" tpmkey pcr aik-0) (9 1))
  (traces ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno-1
          "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne-1
            tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
         tne-1 tno-1 "false"))
      (send
        (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
          tne-1 tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk))))
      (send (cat "execute transport" (hash "extend" (enc n esk)))))
    ((recv (cat "extend" (enc n esk)))
      (send (cat "extend" (enc n esk))))
    ((recv (cat "decrypt" (enc esk tpmkey)))
      (recv (enc "created" tpmkey pcr aik-0)) (obsv pcr) (send esk))
    ((recv (cat "quote" nonce)) (obsv (enc "created" tpmkey pcr aik-0))
      (send
        (enc "quote" (enc "created" tpmkey pcr aik-0) nonce aik-1))))
  (label 102)
  (parent 99)
  (unrealized (5 0) (10 1))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (state pcr nonce mesg)
    (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 esk-1 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 aik-1 akey))
  (deflistener v)
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-decrypt 4 (m v) (pcr (hash "obtain" (hash n state)))
    (k k) (aik aik))
  (defstrand tpm-extend-enc 4 (value "obtain") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk-1))))
  (deflistener (cat "extend" (enc n esk-1)))
  (deflistener esk)
  (defstrand tpm-decrypt 4 (m esk) (pcr pcr) (k tpmkey) (aik aik-0))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (enc "created" tpmkey pcr aik-0)) (aik aik-1))
  (precedes ((1 0) (10 0)) ((1 2) (8 0)) ((1 4) (2 0)) ((1 6) (3 0))
    ((2 1) (1 5)) ((3 3) (0 0)) ((4 3) (3 2)) ((5 1) (6 0))
    ((5 3) (4 3)) ((6 1) (5 2)) ((7 1) (6 0)) ((8 1) (7 0))
    ((9 1) (8 0)) ((10 3) (9 0)) ((11 2) (10 1)))
  (leadsto ((4 3) (3 2)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 aik-1 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation encryption-test (added-strand tpm-quote 3)
    (enc "created" tpmkey pcr aik-0) (10 1))
  (traces ((recv v) (send v))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "decrypt" (enc v k)))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (obsv (hash "obtain" (hash n state))) (send v))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "obtain" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "obtain" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "obtain" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk-1))))
      (send (cat "execute transport" (hash "extend" (enc n esk-1)))))
    ((recv (cat "extend" (enc n esk-1)))
      (send (cat "extend" (enc n esk-1)))) ((recv esk) (send esk))
    ((recv (cat "decrypt" (enc esk tpmkey)))
      (recv (enc "created" tpmkey pcr aik-0)) (obsv pcr) (send esk))
    ((recv (cat "quote" nonce)) (obsv (enc "created" tpmkey pcr aik-0))
      (send
        (enc "quote" (enc "created" tpmkey pcr aik-0) nonce aik-1))))
  (label 103)
  (parent 101)
  (unrealized (11 1))
  (dead)
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol envelope basic
  (defrole tpm-power-on (vars) (trace (init "0")))
  (defrole tpm-quote
    (vars (nonce pcr mesg) (aik akey))
    (trace (recv (cat "quote" nonce)) (obsv pcr)
      (send (enc "quote" pcr nonce aik)))
    (non-orig aik))
  (defrole tpm-extend-enc
    (vars (value state mesg) (esk skey) (tne tno data) (tpmkey akey))
    (trace (recv (cat "establish transport" tpmkey (enc esk tpmkey)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc value esk)) tno
          "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc value esk)))
            tne tno "false"))) (tran state (hash value state)))
    (uniq-orig tne))
  (defrole tpm-create-key
    (vars (k aik akey) (pcr mesg) (esk skey))
    (trace (recv (enc "create key" pcr esk))
      (send (enc "created" k pcr aik)))
    (non-orig (invk k) aik esk)
    (uniq-orig k))
  (defrole tpm-decrypt
    (vars (m pcr mesg) (k aik akey))
    (trace (recv (cat "decrypt" (enc m k)))
      (recv (enc "created" k pcr aik)) (obsv pcr) (send m))
    (non-orig aik))
  (defrole alice
    (vars (v n tne tno data) (esk1 esk skey) (k aik tpmkey akey)
      (pcr mesg))
    (trace (send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k)))
    (non-orig aik esk1 (invk tpmkey))
    (uniq-orig v n tno esk)
    (neq (tno n))))

(defskeleton envelope
  (vars (pcr mesg) (v n tne tno data) (esk1 esk skey)
    (k aik tpmkey akey))
  (deflistener (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (neq (tno n))
  (non-orig esk1 aik (invk tpmkey))
  (uniq-orig v n tno esk)
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik)))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k))))
  (label 104)
  (unrealized (0 0) (1 5))
  (preskeleton)
  (origs (esk (1 0)) (n (1 2)) (tno (1 2)) (v (1 6)))
  (comment "Not a skeleton"))

(defskeleton envelope
  (vars (pcr mesg) (v n tne tno data) (esk1 esk skey)
    (k aik tpmkey akey))
  (deflistener (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (precedes ((1 6) (0 0)))
  (neq (tno n))
  (non-orig esk1 aik (invk tpmkey))
  (uniq-orig v n tno esk)
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik)))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k))))
  (label 105)
  (parent 104)
  (unrealized (0 0) (1 5))
  (origs (esk (1 0)) (n (1 2)) (tno (1 2)) (v (1 6)))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton envelope
  (vars (pcr nonce mesg) (v n tne tno data) (esk1 esk skey)
    (k aik tpmkey aik-0 akey))
  (deflistener (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (enc "created" k (hash "obtain" (hash n pcr)) aik))
    (aik aik-0))
  (precedes ((1 6) (0 0)) ((2 2) (1 5)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk tpmkey))
  (uniq-orig v n tno esk)
  (operation encryption-test (added-strand tpm-quote 3)
    (enc "created" k (hash "obtain" (hash n pcr)) aik) (1 5))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik)))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k)))
    ((recv (cat "quote" nonce))
      (obsv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send
        (enc "quote" (enc "created" k (hash "obtain" (hash n pcr)) aik)
          nonce aik-0))))
  (label 106)
  (parent 105)
  (unrealized (0 0) (2 1))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (pcr mesg) (v n tne tno data) (esk1 esk esk-0 skey)
    (k aik tpmkey akey))
  (deflistener (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n pcr)))
    (esk esk-0) (k k) (aik aik))
  (precedes ((1 6) (0 0)) ((2 1) (1 5)))
  (neq (tno n))
  (non-orig esk1 esk-0 aik (invk k) (invk tpmkey))
  (uniq-orig v n tno esk k)
  (operation encryption-test (added-strand tpm-create-key 2)
    (enc "created" k (hash "obtain" (hash n pcr)) aik) (1 5))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik)))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n pcr)) esk-0))
      (send (enc "created" k (hash "obtain" (hash n pcr)) aik))))
  (label 107)
  (parent 105)
  (unrealized (0 0) (2 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton envelope
  (vars (pcr nonce mesg) (v n tne tno data) (esk1 esk esk-0 skey)
    (k aik tpmkey aik-0 akey))
  (deflistener (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n pcr)))
    (esk esk-0) (k k) (aik aik))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (enc "create key" (hash "obtain" (hash n pcr)) esk-0))
    (aik aik-0))
  (precedes ((1 6) (0 0)) ((2 1) (1 5)) ((3 2) (2 0)))
  (neq (tno n))
  (non-orig esk1 esk-0 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno esk k)
  (operation encryption-test (added-strand tpm-quote 3)
    (enc "create key" (hash "obtain" (hash n pcr)) esk-0) (2 0))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik)))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n pcr)) esk-0))
      (send (enc "created" k (hash "obtain" (hash n pcr)) aik)))
    ((recv (cat "quote" nonce))
      (obsv (enc "create key" (hash "obtain" (hash n pcr)) esk-0))
      (send
        (enc "quote"
          (enc "create key" (hash "obtain" (hash n pcr)) esk-0) nonce
          aik-0))))
  (label 108)
  (parent 107)
  (unrealized (0 0) (3 1))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (pcr mesg) (v n tne tno data) (esk1 esk skey)
    (k aik tpmkey akey))
  (deflistener (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n pcr)))
    (esk esk1) (k k) (aik aik))
  (precedes ((1 4) (2 0)) ((1 6) (0 0)) ((2 1) (1 5)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tno esk k)
  (operation encryption-test (displaced 3 1 alice 5)
    (enc "create key" (hash "obtain" (hash n pcr)) esk-0) (2 0))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik)))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (send (enc "created" k (hash "obtain" (hash n pcr)) aik))))
  (label 109)
  (parent 107)
  (unrealized (0 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton envelope
  (vars (pcr mesg) (v n tne tno data) (esk1 esk skey)
    (k aik tpmkey akey))
  (deflistener (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n pcr)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n pcr))) (aik aik))
  (precedes ((1 4) (2 0)) ((1 6) (3 0)) ((2 1) (1 5)) ((3 2) (0 0)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tno esk k)
  (operation encryption-test (added-strand tpm-quote 3)
    (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik) (0 0))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik)))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (send (enc "created" k (hash "obtain" (hash n pcr)) aik)))
    ((recv (cat "quote" (enc v k))) (obsv (hash "refuse" (hash n pcr)))
      (send (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))))
  (label 110)
  (parent 109)
  (unrealized (3 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton envelope
  (vars (pcr nonce mesg) (v n tne tno data) (esk1 esk skey)
    (k aik tpmkey aik-0 akey))
  (deflistener (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n pcr)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
    (aik aik-0))
  (precedes ((1 4) (2 0)) ((1 6) (3 1)) ((2 1) (1 5)) ((3 2) (0 0)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno esk k)
  (operation encryption-test (added-strand tpm-quote 3)
    (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik) (0 0))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik)))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (send (enc "created" k (hash "obtain" (hash n pcr)) aik)))
    ((recv (cat "quote" nonce))
      (obsv (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
      (send
        (enc "quote"
          (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik) nonce
          aik-0))))
  (label 111)
  (parent 109)
  (unrealized (3 1))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (pcr mesg) (v n tne tno tne-0 tno-0 data) (esk1 esk esk-0 skey)
    (k aik tpmkey tpmkey-0 akey))
  (deflistener (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
  (defstrand alice 7 (pcr pcr) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n pcr)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n pcr))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n pcr))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (precedes ((1 4) (2 0)) ((1 6) (3 0)) ((2 1) (1 5)) ((3 2) (0 0))
    ((4 3) (3 1)))
  (leadsto ((4 3) (3 1)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 esk k)
  (operation state-passing-test (added-strand tpm-extend-enc 4)
    (hash "refuse" (hash n pcr)) (3 1))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik)))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv pcr)
      (send (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n pcr)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n pcr)) esk1))
      (send (enc "created" k (hash "obtain" (hash n pcr)) aik)))
    ((recv (cat "quote" (enc v k))) (obsv (hash "refuse" (hash n pcr)))
      (send (enc "quote" (hash "refuse" (hash n pcr)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n pcr) (hash "refuse" (hash n pcr)))))
  (label 112)
  (parent 110)
  (unrealized (4 3))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton envelope
  (vars (state mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 esk-1 skey) (k aik tpmkey tpmkey-0 tpmkey-1 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (precedes ((1 2) (5 2)) ((1 4) (2 0)) ((1 6) (3 0)) ((2 1) (1 5))
    ((3 2) (0 0)) ((4 3) (3 1)) ((5 3) (4 3)))
  (leadsto ((4 3) (3 1)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation state-passing-test (added-strand tpm-extend-enc 4)
    (hash n state) (4 3))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state))))
  (label 113)
  (parent 112)
  (unrealized (5 2))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton envelope
  (vars (state nonce mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 esk-1 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr
      (hash esk-1
        (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
        tno-1 "false")) (aik aik-0))
  (precedes ((1 2) (5 2)) ((1 4) (2 0)) ((1 6) (3 0)) ((2 1) (1 5))
    ((3 2) (0 0)) ((4 3) (3 1)) ((5 3) (4 3)) ((6 2) (5 2)))
  (leadsto ((4 3) (3 1)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation encryption-test (added-strand tpm-quote 3)
    (hash esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false") (5 2))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv (cat "quote" nonce))
      (obsv
        (hash esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false"))
      (send
        (enc "quote"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false") nonce aik-0))))
  (label 114)
  (parent 113)
  (unrealized (5 2) (6 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton envelope
  (vars (state mesg) (v n tne tno tne-0 tno-0 data)
    (esk1 esk esk-0 skey) (k aik tpmkey tpmkey-0 tpmkey-1 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne)
    (tno tno) (esk esk) (tpmkey tpmkey-1))
  (precedes ((1 0) (5 0)) ((1 2) (5 2)) ((1 4) (2 0)) ((1 6) (3 0))
    ((2 1) (1 5)) ((3 2) (0 0)) ((4 3) (3 1)) ((5 1) (1 1))
    ((5 3) (4 3)))
  (leadsto ((4 3) (3 1)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tne tno tne-0 esk k)
  (operation encryption-test (displaced 6 1 alice 3)
    (hash esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false") (5 2))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk tpmkey-1)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (tran state (hash n state))))
  (label 115)
  (parent 113)
  (unrealized (5 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton envelope
  (vars (state mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 esk-1 skey) (k aik tpmkey tpmkey-0 tpmkey-1 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (precedes ((1 2) (5 2)) ((1 4) (2 0)) ((1 6) (3 0)) ((2 1) (1 5))
    ((3 2) (0 0)) ((4 3) (3 1)) ((5 1) (6 0)) ((5 3) (4 3))
    ((6 1) (5 2)))
  (leadsto ((4 3) (3 1)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation encryption-test
    (added-listener
      (cat esk-1
        (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
        tno-1 "false"))
    (hash esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false") (5 2))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false"))))
  (label 116)
  (parent 113)
  (unrealized (5 2) (6 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton envelope
  (vars (state nonce mesg)
    (v n tne tno tne-0 tno-0 tne-1 tno-1 tne-2 tno-2 data)
    (esk1 esk esk-0 esk-1 esk-2 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 tpmkey-2 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr
      (hash esk-1
        (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
        tno-1 "false")) (aik aik-0))
  (defstrand tpm-extend-enc 4 (value esk-1)
    (state
      (cat (hash "execute transport" (hash "extend" (enc n esk-1)))
        tne-1 tno-1 "false")) (tne tne-2) (tno tno-2) (esk esk-2)
    (tpmkey tpmkey-2))
  (precedes ((1 2) (5 2)) ((1 4) (2 0)) ((1 6) (3 0)) ((2 1) (1 5))
    ((3 2) (0 0)) ((4 3) (3 1)) ((5 1) (7 3)) ((5 3) (4 3))
    ((6 2) (5 2)) ((7 3) (6 1)))
  (leadsto ((4 3) (3 1)) ((5 3) (4 3)) ((7 3) (6 1)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 tne-2 esk k)
  (operation state-passing-test (added-strand tpm-extend-enc 4)
    (hash esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false") (6 1))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv (cat "quote" nonce))
      (obsv
        (hash esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false"))
      (send
        (enc "quote"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false") nonce aik-0)))
    ((recv (cat "establish transport" tpmkey-2 (enc esk-2 tpmkey-2)))
      (send (cat "establish transport" tne-2))
      (recv
        (cat "execute transport" (cat "extend" (enc esk-1 esk-2)) tno-2
          "false"
          (hash esk-2
            (hash "execute transport" (hash "extend" (enc esk-1 esk-2)))
            tne-2 tno-2 "false")))
      (tran
        (cat (hash "execute transport" (hash "extend" (enc n esk-1)))
          tne-1 tno-1 "false")
        (hash esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false"))))
  (label 117)
  (parent 114)
  (unrealized (5 2) (7 3))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (state mesg) (v n tne tno tne-0 tno-0 data)
    (esk1 esk esk-0 skey) (k aik tpmkey tpmkey-0 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne)
    (tno tno) (esk esk) (tpmkey tpmkey))
  (precedes ((1 0) (5 0)) ((1 2) (5 2)) ((1 4) (2 0)) ((1 6) (3 0))
    ((2 1) (1 5)) ((3 2) (0 0)) ((4 3) (3 1)) ((5 1) (1 1))
    ((5 3) (4 3)))
  (leadsto ((4 3) (3 1)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tne tno tne-0 esk k)
  (operation nonce-test (contracted (tpmkey-1 tpmkey)) esk (5 0)
    (enc esk tpmkey))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey (enc esk tpmkey)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (tran state (hash n state))))
  (label 118)
  (parent 115)
  (unrealized)
  (shape)
  (maps
    ((0 1)
      ((v v) (n n) (k k) (aik aik) (pcr state) (tne tne) (tno tno)
        (esk1 esk1) (esk esk) (tpmkey tpmkey))))
  (origs (n (1 2)) (tno (1 2)) (esk (1 0)) (tne (5 1)) (tne-0 (4 1))
    (k (2 1)) (v (1 6))))

(defskeleton envelope
  (vars (state nonce mesg) (v n tne tno tne-0 tno-0 data)
    (esk1 esk esk-0 skey) (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne)
    (tno tno) (esk esk) (tpmkey tpmkey-1))
  (defstrand tpm-quote 3 (nonce nonce) (pcr esk) (aik aik-0))
  (precedes ((1 0) (6 1)) ((1 2) (5 2)) ((1 4) (2 0)) ((1 6) (3 0))
    ((2 1) (1 5)) ((3 2) (0 0)) ((4 3) (3 1)) ((5 1) (1 1))
    ((5 3) (4 3)) ((6 2) (5 0)))
  (leadsto ((4 3) (3 1)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tne tno tne-0 esk k)
  (operation nonce-test (added-strand tpm-quote 3) esk (5 0)
    (enc esk tpmkey))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk tpmkey-1)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (tran state (hash n state)))
    ((recv (cat "quote" nonce)) (obsv esk)
      (send (enc "quote" esk nonce aik-0))))
  (label 119)
  (parent 115)
  (unrealized (6 1))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (state pcr mesg) (v n tne tno tne-0 tno-0 data)
    (esk1 esk esk-0 skey) (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne)
    (tno tno) (esk esk) (tpmkey tpmkey-1))
  (defstrand tpm-decrypt 4 (m esk) (pcr pcr) (k tpmkey) (aik aik-0))
  (precedes ((1 0) (6 0)) ((1 2) (5 2)) ((1 4) (2 0)) ((1 6) (3 0))
    ((2 1) (1 5)) ((3 2) (0 0)) ((4 3) (3 1)) ((5 1) (1 1))
    ((5 3) (4 3)) ((6 3) (5 0)))
  (leadsto ((4 3) (3 1)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tne tno tne-0 esk k)
  (operation nonce-test (added-strand tpm-decrypt 4) esk (5 0)
    (enc esk tpmkey))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk tpmkey-1)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (tran state (hash n state)))
    ((recv (cat "decrypt" (enc esk tpmkey)))
      (recv (enc "created" tpmkey pcr aik-0)) (obsv pcr) (send esk)))
  (label 120)
  (parent 115)
  (unrealized (6 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton envelope
  (vars (state nonce mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 esk-1 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (hash "execute transport" (hash "extend" (enc n esk-1))))
    (aik aik-0))
  (precedes ((1 2) (5 2)) ((1 4) (2 0)) ((1 6) (3 0)) ((2 1) (1 5))
    ((3 2) (0 0)) ((4 3) (3 1)) ((5 1) (6 0)) ((5 3) (4 3))
    ((6 1) (5 2)) ((7 2) (6 0)))
  (leadsto ((4 3) (3 1)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation encryption-test (added-strand tpm-quote 3)
    (hash "execute transport" (hash "extend" (enc n esk-1))) (6 0))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false")))
    ((recv (cat "quote" nonce))
      (obsv (hash "execute transport" (hash "extend" (enc n esk-1))))
      (send
        (enc "quote"
          (hash "execute transport" (hash "extend" (enc n esk-1))) nonce
          aik-0))))
  (label 121)
  (parent 116)
  (unrealized (5 2) (7 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton envelope
  (vars (state mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 esk-1 skey) (k aik tpmkey tpmkey-0 tpmkey-1 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk-1))))
  (precedes ((1 2) (5 2)) ((1 4) (2 0)) ((1 6) (3 0)) ((2 1) (1 5))
    ((3 2) (0 0)) ((4 3) (3 1)) ((5 1) (6 0)) ((5 3) (4 3))
    ((6 1) (5 2)) ((7 1) (6 0)))
  (leadsto ((4 3) (3 1)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation encryption-test
    (added-listener
      (cat "execute transport" (hash "extend" (enc n esk-1))))
    (hash "execute transport" (hash "extend" (enc n esk-1))) (6 0))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk-1))))
      (send (cat "execute transport" (hash "extend" (enc n esk-1))))))
  (label 122)
  (parent 116)
  (unrealized (5 2) (7 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton envelope
  (vars (state pcr nonce mesg) (v n tne tno tne-0 tno-0 data)
    (esk1 esk esk-0 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 aik-1 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne)
    (tno tno) (esk esk) (tpmkey tpmkey-1))
  (defstrand tpm-decrypt 4 (m esk) (pcr pcr) (k tpmkey) (aik aik-0))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (enc "created" tpmkey pcr aik-0)) (aik aik-1))
  (precedes ((1 0) (6 0)) ((1 2) (5 2)) ((1 4) (2 0)) ((1 6) (3 0))
    ((2 1) (1 5)) ((3 2) (0 0)) ((4 3) (3 1)) ((5 1) (1 1))
    ((5 3) (4 3)) ((6 3) (5 0)) ((7 2) (6 1)))
  (leadsto ((4 3) (3 1)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 aik-1 (invk k) (invk tpmkey))
  (uniq-orig v n tne tno tne-0 esk k)
  (operation encryption-test (added-strand tpm-quote 3)
    (enc "created" tpmkey pcr aik-0) (6 1))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk tpmkey-1)))
      (send (cat "establish transport" tne))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (tran state (hash n state)))
    ((recv (cat "decrypt" (enc esk tpmkey)))
      (recv (enc "created" tpmkey pcr aik-0)) (obsv pcr) (send esk))
    ((recv (cat "quote" nonce)) (obsv (enc "created" tpmkey pcr aik-0))
      (send
        (enc "quote" (enc "created" tpmkey pcr aik-0) nonce aik-1))))
  (label 123)
  (parent 120)
  (unrealized (7 1))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (state nonce mesg)
    (v n tne tno tne-0 tno-0 tne-1 tno-1 tne-2 tno-2 data)
    (esk1 esk esk-0 esk-1 esk-2 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 tpmkey-2 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (hash "execute transport" (hash "extend" (enc n esk-1))))
    (aik aik-0))
  (defstrand tpm-extend-enc 4 (value "execute transport")
    (state (hash "extend" (enc n esk-1))) (tne tne-2) (tno tno-2)
    (esk esk-2) (tpmkey tpmkey-2))
  (precedes ((1 2) (5 2)) ((1 4) (2 0)) ((1 6) (3 0)) ((2 1) (1 5))
    ((3 2) (0 0)) ((4 3) (3 1)) ((5 1) (6 0)) ((5 3) (4 3))
    ((6 1) (5 2)) ((7 2) (6 0)) ((8 3) (7 1)))
  (leadsto ((4 3) (3 1)) ((5 3) (4 3)) ((8 3) (7 1)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 tne-2 esk k)
  (operation state-passing-test (added-strand tpm-extend-enc 4)
    (hash "execute transport" (hash "extend" (enc n esk-1))) (7 1))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false")))
    ((recv (cat "quote" nonce))
      (obsv (hash "execute transport" (hash "extend" (enc n esk-1))))
      (send
        (enc "quote"
          (hash "execute transport" (hash "extend" (enc n esk-1))) nonce
          aik-0)))
    ((recv (cat "establish transport" tpmkey-2 (enc esk-2 tpmkey-2)))
      (send (cat "establish transport" tne-2))
      (recv
        (cat "execute transport"
          (cat "extend" (enc "execute transport" esk-2)) tno-2 "false"
          (hash esk-2
            (hash "execute transport"
              (hash "extend" (enc "execute transport" esk-2))) tne-2
            tno-2 "false")))
      (tran (hash "extend" (enc n esk-1))
        (hash "execute transport" (hash "extend" (enc n esk-1))))))
  (label 124)
  (parent 121)
  (unrealized (5 2) (8 3))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton envelope
  (vars (state nonce mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 esk-1 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk-1))))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (hash "extend" (enc n esk-1))) (aik aik-0))
  (precedes ((1 2) (5 2)) ((1 4) (2 0)) ((1 6) (3 0)) ((2 1) (1 5))
    ((3 2) (0 0)) ((4 3) (3 1)) ((5 1) (6 0)) ((5 3) (4 3))
    ((6 1) (5 2)) ((7 1) (6 0)) ((8 2) (7 0)))
  (leadsto ((4 3) (3 1)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation encryption-test (added-strand tpm-quote 3)
    (hash "extend" (enc n esk-1)) (7 0))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk-1))))
      (send (cat "execute transport" (hash "extend" (enc n esk-1)))))
    ((recv (cat "quote" nonce)) (obsv (hash "extend" (enc n esk-1)))
      (send (enc "quote" (hash "extend" (enc n esk-1)) nonce aik-0))))
  (label 125)
  (parent 122)
  (unrealized (5 2) (8 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton envelope
  (vars (state mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 esk-1 skey) (k aik tpmkey tpmkey-0 tpmkey-1 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk-1))))
  (deflistener (cat "extend" (enc n esk-1)))
  (precedes ((1 2) (8 0)) ((1 4) (2 0)) ((1 6) (3 0)) ((2 1) (1 5))
    ((3 2) (0 0)) ((4 3) (3 1)) ((5 1) (6 0)) ((5 3) (4 3))
    ((6 1) (5 2)) ((7 1) (6 0)) ((8 1) (7 0)))
  (leadsto ((4 3) (3 1)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation encryption-test
    (added-listener (cat "extend" (enc n esk-1)))
    (hash "extend" (enc n esk-1)) (7 0))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk-1))))
      (send (cat "execute transport" (hash "extend" (enc n esk-1)))))
    ((recv (cat "extend" (enc n esk-1)))
      (send (cat "extend" (enc n esk-1)))))
  (label 126)
  (parent 122)
  (unrealized (8 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton envelope
  (vars (state nonce mesg)
    (v n tne tno tne-0 tno-0 tne-1 tno-1 tne-2 tno-2 tne-3 tno-3 data)
    (esk1 esk esk-0 esk-1 esk-2 esk-3 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 tpmkey-2 tpmkey-3 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (hash "execute transport" (hash "extend" (enc n esk-1))))
    (aik aik-0))
  (defstrand tpm-extend-enc 4 (value "execute transport")
    (state (hash "extend" (enc n esk-1))) (tne tne-2) (tno tno-2)
    (esk esk-2) (tpmkey tpmkey-2))
  (defstrand tpm-extend-enc 4 (value "extend") (state (enc n esk-1))
    (tne tne-3) (tno tno-3) (esk esk-3) (tpmkey tpmkey-3))
  (precedes ((1 2) (9 3)) ((1 4) (2 0)) ((1 6) (3 0)) ((2 1) (1 5))
    ((3 2) (0 0)) ((4 3) (3 1)) ((5 1) (6 0)) ((5 3) (4 3))
    ((6 1) (5 2)) ((7 2) (6 0)) ((8 3) (7 1)) ((9 3) (8 3)))
  (leadsto ((4 3) (3 1)) ((5 3) (4 3)) ((8 3) (7 1)) ((9 3) (8 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 tne-2 tne-3 esk k)
  (operation state-passing-test (added-strand tpm-extend-enc 4)
    (hash "extend" (enc n esk-1)) (8 3))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false")))
    ((recv (cat "quote" nonce))
      (obsv (hash "execute transport" (hash "extend" (enc n esk-1))))
      (send
        (enc "quote"
          (hash "execute transport" (hash "extend" (enc n esk-1))) nonce
          aik-0)))
    ((recv (cat "establish transport" tpmkey-2 (enc esk-2 tpmkey-2)))
      (send (cat "establish transport" tne-2))
      (recv
        (cat "execute transport"
          (cat "extend" (enc "execute transport" esk-2)) tno-2 "false"
          (hash esk-2
            (hash "execute transport"
              (hash "extend" (enc "execute transport" esk-2))) tne-2
            tno-2 "false")))
      (tran (hash "extend" (enc n esk-1))
        (hash "execute transport" (hash "extend" (enc n esk-1)))))
    ((recv (cat "establish transport" tpmkey-3 (enc esk-3 tpmkey-3)))
      (send (cat "establish transport" tne-3))
      (recv
        (cat "execute transport" (cat "extend" (enc "extend" esk-3))
          tno-3 "false"
          (hash esk-3
            (hash "execute transport"
              (hash "extend" (enc "extend" esk-3))) tne-3 tno-3
            "false")))
      (tran (enc n esk-1) (hash "extend" (enc n esk-1)))))
  (label 127)
  (parent 124)
  (unrealized (5 2) (9 3))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (state nonce mesg)
    (v n tne tno tne-0 tno-0 tne-1 tno-1 tne-2 tno-2 data)
    (esk1 esk esk-0 esk-1 esk-2 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 tpmkey-2 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk-1))))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (hash "extend" (enc n esk-1))) (aik aik-0))
  (defstrand tpm-extend-enc 4 (value "extend") (state (enc n esk-1))
    (tne tne-2) (tno tno-2) (esk esk-2) (tpmkey tpmkey-2))
  (precedes ((1 2) (9 3)) ((1 4) (2 0)) ((1 6) (3 0)) ((2 1) (1 5))
    ((3 2) (0 0)) ((4 3) (3 1)) ((5 1) (6 0)) ((5 3) (4 3))
    ((6 1) (5 2)) ((7 1) (6 0)) ((8 2) (7 0)) ((9 3) (8 1)))
  (leadsto ((4 3) (3 1)) ((5 3) (4 3)) ((9 3) (8 1)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 tne-2 esk k)
  (operation state-passing-test (added-strand tpm-extend-enc 4)
    (hash "extend" (enc n esk-1)) (8 1))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk-1))))
      (send (cat "execute transport" (hash "extend" (enc n esk-1)))))
    ((recv (cat "quote" nonce)) (obsv (hash "extend" (enc n esk-1)))
      (send (enc "quote" (hash "extend" (enc n esk-1)) nonce aik-0)))
    ((recv (cat "establish transport" tpmkey-2 (enc esk-2 tpmkey-2)))
      (send (cat "establish transport" tne-2))
      (recv
        (cat "execute transport" (cat "extend" (enc "extend" esk-2))
          tno-2 "false"
          (hash esk-2
            (hash "execute transport"
              (hash "extend" (enc "extend" esk-2))) tne-2 tno-2
            "false")))
      (tran (enc n esk-1) (hash "extend" (enc n esk-1)))))
  (label 128)
  (parent 125)
  (unrealized (5 2) (9 3))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (state mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 skey) (k aik tpmkey tpmkey-0 tpmkey-1 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk) (tpmkey tpmkey-1))
  (deflistener
    (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk))))
  (deflistener (cat "extend" (enc n esk)))
  (precedes ((1 0) (5 0)) ((1 2) (8 0)) ((1 4) (2 0)) ((1 6) (3 0))
    ((2 1) (1 5)) ((3 2) (0 0)) ((4 3) (3 1)) ((5 1) (6 0))
    ((5 3) (4 3)) ((6 1) (5 2)) ((7 1) (6 0)) ((8 1) (7 0)))
  (leadsto ((4 3) (3 1)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation nonce-test (contracted (esk-1 esk)) n (8 0) (enc n esk))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno-1
          "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne-1
            tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
         tne-1 tno-1 "false"))
      (send
        (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
          tne-1 tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk))))
      (send (cat "execute transport" (hash "extend" (enc n esk)))))
    ((recv (cat "extend" (enc n esk)))
      (send (cat "extend" (enc n esk)))))
  (label 129)
  (parent 126)
  (unrealized (5 0) (6 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton envelope
  (vars (state nonce mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 esk-1 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk-1))))
  (deflistener (cat "extend" (enc n esk-1)))
  (defstrand tpm-quote 3 (nonce nonce) (pcr n) (aik aik-0))
  (precedes ((1 2) (9 1)) ((1 4) (2 0)) ((1 6) (3 0)) ((2 1) (1 5))
    ((3 2) (0 0)) ((4 3) (3 1)) ((5 1) (6 0)) ((5 3) (4 3))
    ((6 1) (5 2)) ((7 1) (6 0)) ((8 1) (7 0)) ((9 2) (8 0)))
  (leadsto ((4 3) (3 1)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation nonce-test (added-strand tpm-quote 3) n (8 0) (enc n esk))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk-1))))
      (send (cat "execute transport" (hash "extend" (enc n esk-1)))))
    ((recv (cat "extend" (enc n esk-1)))
      (send (cat "extend" (enc n esk-1))))
    ((recv (cat "quote" nonce)) (obsv n)
      (send (enc "quote" n nonce aik-0))))
  (label 130)
  (parent 126)
  (unrealized (9 1))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (state mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 esk-1 skey) (k aik tpmkey tpmkey-0 tpmkey-1 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk-1))))
  (deflistener (cat "extend" (enc n esk-1)))
  (deflistener esk)
  (precedes ((1 0) (9 0)) ((1 2) (8 0)) ((1 4) (2 0)) ((1 6) (3 0))
    ((2 1) (1 5)) ((3 2) (0 0)) ((4 3) (3 1)) ((5 1) (6 0))
    ((5 3) (4 3)) ((6 1) (5 2)) ((7 1) (6 0)) ((8 1) (7 0))
    ((9 1) (8 0)))
  (leadsto ((4 3) (3 1)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation nonce-test (added-listener esk) n (8 0) (enc n esk))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk-1))))
      (send (cat "execute transport" (hash "extend" (enc n esk-1)))))
    ((recv (cat "extend" (enc n esk-1)))
      (send (cat "extend" (enc n esk-1)))) ((recv esk) (send esk)))
  (label 131)
  (parent 126)
  (unrealized (9 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton envelope
  (vars (state nonce mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 skey) (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk) (tpmkey tpmkey-1))
  (deflistener
    (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk))))
  (deflistener (cat "extend" (enc n esk)))
  (defstrand tpm-quote 3 (nonce nonce) (pcr esk) (aik aik-0))
  (precedes ((1 0) (5 0)) ((1 0) (9 1)) ((1 2) (8 0)) ((1 4) (2 0))
    ((1 6) (3 0)) ((2 1) (1 5)) ((3 2) (0 0)) ((4 3) (3 1))
    ((5 1) (6 0)) ((5 3) (4 3)) ((6 1) (5 2)) ((7 1) (6 0))
    ((8 1) (7 0)) ((9 2) (6 0)))
  (leadsto ((4 3) (3 1)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation nonce-test (added-strand tpm-quote 3) esk (6 0)
    (enc esk tpmkey))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno-1
          "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne-1
            tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
         tne-1 tno-1 "false"))
      (send
        (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
          tne-1 tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk))))
      (send (cat "execute transport" (hash "extend" (enc n esk)))))
    ((recv (cat "extend" (enc n esk)))
      (send (cat "extend" (enc n esk))))
    ((recv (cat "quote" nonce)) (obsv esk)
      (send (enc "quote" esk nonce aik-0))))
  (label 132)
  (parent 129)
  (unrealized (5 0) (9 1))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (state pcr mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 skey) (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk) (tpmkey tpmkey-1))
  (deflistener
    (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk))))
  (deflistener (cat "extend" (enc n esk)))
  (defstrand tpm-decrypt 4 (m esk) (pcr pcr) (k tpmkey) (aik aik-0))
  (precedes ((1 0) (5 0)) ((1 0) (9 0)) ((1 2) (8 0)) ((1 4) (2 0))
    ((1 6) (3 0)) ((2 1) (1 5)) ((3 2) (0 0)) ((4 3) (3 1))
    ((5 1) (6 0)) ((5 3) (4 3)) ((6 1) (5 2)) ((7 1) (6 0))
    ((8 1) (7 0)) ((9 3) (6 0)))
  (leadsto ((4 3) (3 1)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation nonce-test (added-strand tpm-decrypt 4) esk (6 0)
    (enc esk tpmkey))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno-1
          "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne-1
            tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
         tne-1 tno-1 "false"))
      (send
        (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
          tne-1 tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk))))
      (send (cat "execute transport" (hash "extend" (enc n esk)))))
    ((recv (cat "extend" (enc n esk)))
      (send (cat "extend" (enc n esk))))
    ((recv (cat "decrypt" (enc esk tpmkey)))
      (recv (enc "created" tpmkey pcr aik-0)) (obsv pcr) (send esk)))
  (label 133)
  (parent 129)
  (unrealized (5 0) (9 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton envelope
  (vars (state nonce mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 esk-1 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk-1))))
  (deflistener (cat "extend" (enc n esk-1)))
  (deflistener esk)
  (defstrand tpm-quote 3 (nonce nonce) (pcr esk) (aik aik-0))
  (precedes ((1 0) (10 1)) ((1 2) (8 0)) ((1 4) (2 0)) ((1 6) (3 0))
    ((2 1) (1 5)) ((3 2) (0 0)) ((4 3) (3 1)) ((5 1) (6 0))
    ((5 3) (4 3)) ((6 1) (5 2)) ((7 1) (6 0)) ((8 1) (7 0))
    ((9 1) (8 0)) ((10 2) (9 0)))
  (leadsto ((4 3) (3 1)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation nonce-test (added-strand tpm-quote 3) esk (9 0)
    (enc esk tpmkey))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk-1))))
      (send (cat "execute transport" (hash "extend" (enc n esk-1)))))
    ((recv (cat "extend" (enc n esk-1)))
      (send (cat "extend" (enc n esk-1)))) ((recv esk) (send esk))
    ((recv (cat "quote" nonce)) (obsv esk)
      (send (enc "quote" esk nonce aik-0))))
  (label 134)
  (parent 131)
  (unrealized (10 1))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (state pcr mesg) (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 esk-1 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk-1))))
  (deflistener (cat "extend" (enc n esk-1)))
  (deflistener esk)
  (defstrand tpm-decrypt 4 (m esk) (pcr pcr) (k tpmkey) (aik aik-0))
  (precedes ((1 0) (10 0)) ((1 2) (8 0)) ((1 4) (2 0)) ((1 6) (3 0))
    ((2 1) (1 5)) ((3 2) (0 0)) ((4 3) (3 1)) ((5 1) (6 0))
    ((5 3) (4 3)) ((6 1) (5 2)) ((7 1) (6 0)) ((8 1) (7 0))
    ((9 1) (8 0)) ((10 3) (9 0)))
  (leadsto ((4 3) (3 1)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation nonce-test (added-strand tpm-decrypt 4) esk (9 0)
    (enc esk tpmkey))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk-1))))
      (send (cat "execute transport" (hash "extend" (enc n esk-1)))))
    ((recv (cat "extend" (enc n esk-1)))
      (send (cat "extend" (enc n esk-1)))) ((recv esk) (send esk))
    ((recv (cat "decrypt" (enc esk tpmkey)))
      (recv (enc "created" tpmkey pcr aik-0)) (obsv pcr) (send esk)))
  (label 135)
  (parent 131)
  (unrealized (10 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton envelope
  (vars (state pcr nonce mesg)
    (v n tne tno tne-0 tno-0 tne-1 tno-1 data) (esk1 esk esk-0 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 aik-1 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk) (tpmkey tpmkey-1))
  (deflistener
    (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk))))
  (deflistener (cat "extend" (enc n esk)))
  (defstrand tpm-decrypt 4 (m esk) (pcr pcr) (k tpmkey) (aik aik-0))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (enc "created" tpmkey pcr aik-0)) (aik aik-1))
  (precedes ((1 0) (5 0)) ((1 0) (9 0)) ((1 2) (8 0)) ((1 4) (2 0))
    ((1 6) (3 0)) ((2 1) (1 5)) ((3 2) (0 0)) ((4 3) (3 1))
    ((5 1) (6 0)) ((5 3) (4 3)) ((6 1) (5 2)) ((7 1) (6 0))
    ((8 1) (7 0)) ((9 3) (6 0)) ((10 2) (9 1)))
  (leadsto ((4 3) (3 1)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 aik-1 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation encryption-test (added-strand tpm-quote 3)
    (enc "created" tpmkey pcr aik-0) (9 1))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk)) tno-1
          "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne-1
            tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
         tne-1 tno-1 "false"))
      (send
        (cat esk (hash "execute transport" (hash "extend" (enc n esk)))
          tne-1 tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk))))
      (send (cat "execute transport" (hash "extend" (enc n esk)))))
    ((recv (cat "extend" (enc n esk)))
      (send (cat "extend" (enc n esk))))
    ((recv (cat "decrypt" (enc esk tpmkey)))
      (recv (enc "created" tpmkey pcr aik-0)) (obsv pcr) (send esk))
    ((recv (cat "quote" nonce)) (obsv (enc "created" tpmkey pcr aik-0))
      (send
        (enc "quote" (enc "created" tpmkey pcr aik-0) nonce aik-1))))
  (label 136)
  (parent 133)
  (unrealized (5 0) (10 1))
  (dead)
  (comment "empty cohort"))

(defskeleton envelope
  (vars (state pcr nonce mesg)
    (v n tne tno tne-0 tno-0 tne-1 tno-1 data)
    (esk1 esk esk-0 esk-1 skey)
    (k aik tpmkey tpmkey-0 tpmkey-1 aik-0 aik-1 akey))
  (deflistener
    (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
  (defstrand alice 7 (pcr state) (v v) (n n) (tne tne) (tno tno)
    (esk1 esk1) (esk esk) (k k) (aik aik) (tpmkey tpmkey))
  (defstrand tpm-create-key 2 (pcr (hash "obtain" (hash n state)))
    (esk esk1) (k k) (aik aik))
  (defstrand tpm-quote 3 (nonce (enc v k))
    (pcr (hash "refuse" (hash n state))) (aik aik))
  (defstrand tpm-extend-enc 4 (value "refuse") (state (hash n state))
    (tne tne-0) (tno tno-0) (esk esk-0) (tpmkey tpmkey-0))
  (defstrand tpm-extend-enc 4 (value n) (state state) (tne tne-1)
    (tno tno-1) (esk esk-1) (tpmkey tpmkey-1))
  (deflistener
    (cat esk-1 (hash "execute transport" (hash "extend" (enc n esk-1)))
      tne-1 tno-1 "false"))
  (deflistener (cat "execute transport" (hash "extend" (enc n esk-1))))
  (deflistener (cat "extend" (enc n esk-1)))
  (deflistener esk)
  (defstrand tpm-decrypt 4 (m esk) (pcr pcr) (k tpmkey) (aik aik-0))
  (defstrand tpm-quote 3 (nonce nonce)
    (pcr (enc "created" tpmkey pcr aik-0)) (aik aik-1))
  (precedes ((1 0) (10 0)) ((1 2) (8 0)) ((1 4) (2 0)) ((1 6) (3 0))
    ((2 1) (1 5)) ((3 2) (0 0)) ((4 3) (3 1)) ((5 1) (6 0))
    ((5 3) (4 3)) ((6 1) (5 2)) ((7 1) (6 0)) ((8 1) (7 0))
    ((9 1) (8 0)) ((10 3) (9 0)) ((11 2) (10 1)))
  (leadsto ((4 3) (3 1)) ((5 3) (4 3)))
  (neq (tno n))
  (non-orig esk1 aik aik-0 aik-1 (invk k) (invk tpmkey))
  (uniq-orig v n tno tne-0 tne-1 esk k)
  (operation encryption-test (added-strand tpm-quote 3)
    (enc "created" tpmkey pcr aik-0) (10 1))
  (traces
    ((recv (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((send (cat "establish transport" tpmkey (enc esk tpmkey)))
      (recv (cat "establish transport" tne))
      (send
        (cat "execute transport" (cat "extend" (enc n esk)) tno "false"
          (hash esk
            (hash "execute transport" (hash "extend" (enc n esk))) tne
            tno "false"))) (recv state)
      (send (enc "create key" (hash "obtain" (hash n state)) esk1))
      (recv (enc "created" k (hash "obtain" (hash n state)) aik))
      (send (enc v k)))
    ((recv (enc "create key" (hash "obtain" (hash n state)) esk1))
      (send (enc "created" k (hash "obtain" (hash n state)) aik)))
    ((recv (cat "quote" (enc v k)))
      (obsv (hash "refuse" (hash n state)))
      (send (enc "quote" (hash "refuse" (hash n state)) (enc v k) aik)))
    ((recv (cat "establish transport" tpmkey-0 (enc esk-0 tpmkey-0)))
      (send (cat "establish transport" tne-0))
      (recv
        (cat "execute transport" (cat "extend" (enc "refuse" esk-0))
          tno-0 "false"
          (hash esk-0
            (hash "execute transport"
              (hash "extend" (enc "refuse" esk-0))) tne-0 tno-0
            "false")))
      (tran (hash n state) (hash "refuse" (hash n state))))
    ((recv (cat "establish transport" tpmkey-1 (enc esk-1 tpmkey-1)))
      (send (cat "establish transport" tne-1))
      (recv
        (cat "execute transport" (cat "extend" (enc n esk-1)) tno-1
          "false"
          (hash esk-1
            (hash "execute transport" (hash "extend" (enc n esk-1)))
            tne-1 tno-1 "false"))) (tran state (hash n state)))
    ((recv
       (cat esk-1
         (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
         tno-1 "false"))
      (send
        (cat esk-1
          (hash "execute transport" (hash "extend" (enc n esk-1))) tne-1
          tno-1 "false")))
    ((recv (cat "execute transport" (hash "extend" (enc n esk-1))))
      (send (cat "execute transport" (hash "extend" (enc n esk-1)))))
    ((recv (cat "extend" (enc n esk-1)))
      (send (cat "extend" (enc n esk-1)))) ((recv esk) (send esk))
    ((recv (cat "decrypt" (enc esk tpmkey)))
      (recv (enc "created" tpmkey pcr aik-0)) (obsv pcr) (send esk))
    ((recv (cat "quote" nonce)) (obsv (enc "created" tpmkey pcr aik-0))
      (send
        (enc "quote" (enc "created" tpmkey pcr aik-0) nonce aik-1))))
  (label 137)
  (parent 135)
  (unrealized (11 1))
  (dead)
  (comment "empty cohort"))

(comment "Nothing left to do")
