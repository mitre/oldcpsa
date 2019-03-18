(comment "CPSA 3.6.2")
(comment "All input read from kerberos++.scm")

(defprotocol kerberos basic
  (defrole client
    (vars (cli tgs as serv name)
      (time t-prime life life-prime tgt st ack nonce text)
      (tgsk servk skey))
    (trace (send (cat cli serv nonce life))
      (recv (cat (enc time nonce life tgsk tgs (ltk cli as)) tgt))
      (send (cat (enc cli time tgsk) tgt))
      (recv (cat (enc serv t-prime life-prime servk tgsk) st))
      (send (cat (enc cli t-prime servk) st)) (recv (enc ack servk)))
    (uniq-orig nonce))
  (defrole tgs
    (vars (cli tgs as serv name) (time t-prime life life-prime text)
      (tgsk servk skey))
    (trace
      (recv
        (cat (enc cli time tgsk) (enc time life tgsk cli (ltk tgs as))))
      (send
        (cat (enc serv t-prime life-prime servk tgsk)
          (enc cli serv t-prime life-prime servk (ltk serv as)))))
    (uniq-orig servk))
  (defrole as
    (vars (cli tgs as serv name) (time life nonce text) (tgsk skey))
    (trace (recv (cat cli serv nonce life))
      (send
        (cat (enc time life nonce tgsk tgs (ltk cli as))
          (enc time life tgsk cli (ltk tgs as)))))
    (uniq-orig tgsk))
  (defrole serv
    (vars (cli as serv name) (t-prime life-prime ack text) (servk skey))
    (trace
      (recv
        (cat (enc cli t-prime servk)
          (enc cli serv t-prime life-prime servk (ltk serv as))))
      (send (enc ack servk)))))

(defskeleton kerberos
  (vars (time t-prime life life-prime tgt st ack nonce text)
    (cli tgs as serv name) (tgsk servk skey))
  (defstrand client 6 (time time) (t-prime t-prime) (life life)
    (life-prime life-prime) (tgt tgt) (st st) (ack ack) (nonce nonce)
    (cli cli) (tgs tgs) (as as) (serv serv) (tgsk tgsk) (servk servk))
  (non-orig (ltk cli as) (ltk tgs as) (ltk serv as))
  (uniq-orig nonce)
  (traces
    ((send (cat cli serv nonce life))
      (recv (cat (enc time nonce life tgsk tgs (ltk cli as)) tgt))
      (send (cat (enc cli time tgsk) tgt))
      (recv (cat (enc serv t-prime life-prime servk tgsk) st))
      (send (cat (enc cli t-prime servk) st)) (recv (enc ack servk))))
  (label 0)
  (unrealized (0 1))
  (origs (nonce (0 0)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton kerberos
  (vars (time t-prime life life-prime tgt st ack nonce text)
    (cli tgs as serv serv-0 name) (tgsk servk skey))
  (defstrand client 6 (time time) (t-prime t-prime) (life life)
    (life-prime life-prime) (tgt tgt) (st st) (ack ack) (nonce nonce)
    (cli cli) (tgs tgs) (as as) (serv serv) (tgsk tgsk) (servk servk))
  (defstrand as 2 (time time) (life nonce) (nonce life) (cli cli)
    (tgs tgs) (as as) (serv serv-0) (tgsk tgsk))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (non-orig (ltk cli as) (ltk tgs as) (ltk serv as))
  (uniq-orig nonce tgsk)
  (operation encryption-test (added-strand as 2)
    (enc time nonce life tgsk tgs (ltk cli as)) (0 1))
  (traces
    ((send (cat cli serv nonce life))
      (recv (cat (enc time nonce life tgsk tgs (ltk cli as)) tgt))
      (send (cat (enc cli time tgsk) tgt))
      (recv (cat (enc serv t-prime life-prime servk tgsk) st))
      (send (cat (enc cli t-prime servk) st)) (recv (enc ack servk)))
    ((recv (cat cli serv-0 life nonce))
      (send
        (cat (enc time nonce life tgsk tgs (ltk cli as))
          (enc time nonce tgsk cli (ltk tgs as))))))
  (label 1)
  (parent 0)
  (unrealized (0 3))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton kerberos
  (vars
    (time t-prime life life-prime tgt st ack nonce time-0 life-0 text)
    (cli tgs as serv serv-0 cli-0 tgs-0 as-0 name) (tgsk servk skey))
  (defstrand client 6 (time time) (t-prime t-prime) (life life)
    (life-prime life-prime) (tgt tgt) (st st) (ack ack) (nonce nonce)
    (cli cli) (tgs tgs) (as as) (serv serv) (tgsk tgsk) (servk servk))
  (defstrand as 2 (time time) (life nonce) (nonce life) (cli cli)
    (tgs tgs) (as as) (serv serv-0) (tgsk tgsk))
  (defstrand tgs 2 (time time-0) (t-prime t-prime) (life life-0)
    (life-prime life-prime) (cli cli-0) (tgs tgs-0) (as as-0)
    (serv serv) (tgsk tgsk) (servk servk))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)) ((1 1) (2 0)) ((2 1) (0 3)))
  (non-orig (ltk cli as) (ltk tgs as) (ltk serv as))
  (uniq-orig nonce tgsk servk)
  (operation encryption-test (added-strand tgs 2)
    (enc serv t-prime life-prime servk tgsk) (0 3))
  (traces
    ((send (cat cli serv nonce life))
      (recv (cat (enc time nonce life tgsk tgs (ltk cli as)) tgt))
      (send (cat (enc cli time tgsk) tgt))
      (recv (cat (enc serv t-prime life-prime servk tgsk) st))
      (send (cat (enc cli t-prime servk) st)) (recv (enc ack servk)))
    ((recv (cat cli serv-0 life nonce))
      (send
        (cat (enc time nonce life tgsk tgs (ltk cli as))
          (enc time nonce tgsk cli (ltk tgs as)))))
    ((recv
       (cat (enc cli-0 time-0 tgsk)
         (enc time-0 life-0 tgsk cli-0 (ltk tgs-0 as-0))))
      (send
        (cat (enc serv t-prime life-prime servk tgsk)
          (enc cli-0 serv t-prime life-prime servk (ltk serv as-0))))))
  (label 2)
  (parent 1)
  (unrealized (2 0))
  (comment "4 in cohort - 4 not yet seen"))

(defskeleton kerberos
  (vars (time t-prime life life-prime tgt st ack nonce text)
    (cli tgs as serv serv-0 name) (tgsk servk skey))
  (defstrand client 6 (time time) (t-prime t-prime) (life life)
    (life-prime life-prime) (tgt tgt) (st st) (ack ack) (nonce nonce)
    (cli cli) (tgs tgs) (as as) (serv serv) (tgsk tgsk) (servk servk))
  (defstrand as 2 (time time) (life nonce) (nonce life) (cli cli)
    (tgs tgs) (as as) (serv serv-0) (tgsk tgsk))
  (deflistener tgsk)
  (precedes ((0 0) (1 0)) ((1 1) (0 1)) ((1 1) (2 0)) ((2 1) (0 3)))
  (non-orig (ltk cli as) (ltk tgs as) (ltk serv as))
  (uniq-orig nonce tgsk)
  (operation encryption-test (added-listener tgsk)
    (enc serv t-prime life-prime servk tgsk) (0 3))
  (traces
    ((send (cat cli serv nonce life))
      (recv (cat (enc time nonce life tgsk tgs (ltk cli as)) tgt))
      (send (cat (enc cli time tgsk) tgt))
      (recv (cat (enc serv t-prime life-prime servk tgsk) st))
      (send (cat (enc cli t-prime servk) st)) (recv (enc ack servk)))
    ((recv (cat cli serv-0 life nonce))
      (send
        (cat (enc time nonce life tgsk tgs (ltk cli as))
          (enc time nonce tgsk cli (ltk tgs as)))))
    ((recv tgsk) (send tgsk)))
  (label 3)
  (parent 1)
  (unrealized (2 0))
  (dead)
  (comment "empty cohort"))

(defskeleton kerberos
  (vars
    (time t-prime life life-prime tgt st ack nonce time-0 life-0 time-1
      life-1 life-prime-0 tgt-0 st-0 nonce-0 text)
    (cli tgs as serv serv-0 cli-0 tgs-0 as-0 tgs-1 as-1 serv-1 name)
    (tgsk servk tgsk-0 skey))
  (defstrand client 6 (time time) (t-prime t-prime) (life life)
    (life-prime life-prime) (tgt tgt) (st st) (ack ack) (nonce nonce)
    (cli cli) (tgs tgs) (as as) (serv serv) (tgsk tgsk) (servk servk))
  (defstrand as 2 (time time) (life nonce) (nonce life) (cli cli)
    (tgs tgs) (as as) (serv serv-0) (tgsk tgsk))
  (defstrand tgs 2 (time time-0) (t-prime t-prime) (life life-0)
    (life-prime life-prime) (cli cli-0) (tgs tgs-0) (as as-0)
    (serv serv) (tgsk tgsk) (servk servk))
  (defstrand client 5 (time time-1) (t-prime time-0) (life life-1)
    (life-prime life-prime-0) (tgt tgt-0) (st st-0) (nonce nonce-0)
    (cli cli-0) (tgs tgs-1) (as as-1) (serv serv-1) (tgsk tgsk-0)
    (servk tgsk))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)) ((1 1) (3 3)) ((2 1) (0 3))
    ((3 4) (2 0)))
  (non-orig (ltk cli as) (ltk tgs as) (ltk serv as))
  (uniq-orig nonce nonce-0 tgsk servk)
  (operation encryption-test (added-strand client 5)
    (enc cli-0 time-0 tgsk) (2 0))
  (traces
    ((send (cat cli serv nonce life))
      (recv (cat (enc time nonce life tgsk tgs (ltk cli as)) tgt))
      (send (cat (enc cli time tgsk) tgt))
      (recv (cat (enc serv t-prime life-prime servk tgsk) st))
      (send (cat (enc cli t-prime servk) st)) (recv (enc ack servk)))
    ((recv (cat cli serv-0 life nonce))
      (send
        (cat (enc time nonce life tgsk tgs (ltk cli as))
          (enc time nonce tgsk cli (ltk tgs as)))))
    ((recv
       (cat (enc cli-0 time-0 tgsk)
         (enc time-0 life-0 tgsk cli-0 (ltk tgs-0 as-0))))
      (send
        (cat (enc serv t-prime life-prime servk tgsk)
          (enc cli-0 serv t-prime life-prime servk (ltk serv as-0)))))
    ((send (cat cli-0 serv-1 nonce-0 life-1))
      (recv
        (cat (enc time-1 nonce-0 life-1 tgsk-0 tgs-1 (ltk cli-0 as-1))
          tgt-0)) (send (cat (enc cli-0 time-1 tgsk-0) tgt-0))
      (recv (cat (enc serv-1 time-0 life-prime-0 tgsk tgsk-0) st-0))
      (send (cat (enc cli-0 time-0 tgsk) st-0))))
  (label 4)
  (parent 2)
  (unrealized (2 0) (3 3))
  (dead)
  (comment "empty cohort"))

(defskeleton kerberos
  (vars (time t-prime life life-prime tgt st ack nonce life-0 text)
    (cli tgs as serv serv-0 tgs-0 as-0 name) (tgsk servk skey))
  (defstrand client 6 (time time) (t-prime t-prime) (life life)
    (life-prime life-prime) (tgt tgt) (st st) (ack ack) (nonce nonce)
    (cli cli) (tgs tgs) (as as) (serv serv) (tgsk tgsk) (servk servk))
  (defstrand as 2 (time time) (life nonce) (nonce life) (cli cli)
    (tgs tgs) (as as) (serv serv-0) (tgsk tgsk))
  (defstrand tgs 2 (time time) (t-prime t-prime) (life life-0)
    (life-prime life-prime) (cli cli) (tgs tgs-0) (as as-0) (serv serv)
    (tgsk tgsk) (servk servk))
  (precedes ((0 0) (1 0)) ((0 2) (2 0)) ((1 1) (0 1)) ((2 1) (0 3)))
  (non-orig (ltk cli as) (ltk tgs as) (ltk serv as))
  (uniq-orig nonce tgsk servk)
  (operation encryption-test (displaced 3 0 client 3)
    (enc cli-0 time-0 tgsk) (2 0))
  (traces
    ((send (cat cli serv nonce life))
      (recv (cat (enc time nonce life tgsk tgs (ltk cli as)) tgt))
      (send (cat (enc cli time tgsk) tgt))
      (recv (cat (enc serv t-prime life-prime servk tgsk) st))
      (send (cat (enc cli t-prime servk) st)) (recv (enc ack servk)))
    ((recv (cat cli serv-0 life nonce))
      (send
        (cat (enc time nonce life tgsk tgs (ltk cli as))
          (enc time nonce tgsk cli (ltk tgs as)))))
    ((recv
       (cat (enc cli time tgsk)
         (enc time life-0 tgsk cli (ltk tgs-0 as-0))))
      (send
        (cat (enc serv t-prime life-prime servk tgsk)
          (enc cli serv t-prime life-prime servk (ltk serv as-0))))))
  (label 5)
  (parent 2)
  (unrealized (2 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton kerberos
  (vars
    (time t-prime life life-prime tgt st ack nonce time-0 life-0 life-1
      tgt-0 nonce-0 text)
    (cli tgs as serv serv-0 cli-0 tgs-0 as-0 tgs-1 as-1 serv-1 name)
    (tgsk servk skey))
  (defstrand client 6 (time time) (t-prime t-prime) (life life)
    (life-prime life-prime) (tgt tgt) (st st) (ack ack) (nonce nonce)
    (cli cli) (tgs tgs) (as as) (serv serv) (tgsk tgsk) (servk servk))
  (defstrand as 2 (time time) (life nonce) (nonce life) (cli cli)
    (tgs tgs) (as as) (serv serv-0) (tgsk tgsk))
  (defstrand tgs 2 (time time-0) (t-prime t-prime) (life life-0)
    (life-prime life-prime) (cli cli-0) (tgs tgs-0) (as as-0)
    (serv serv) (tgsk tgsk) (servk servk))
  (defstrand client 3 (time time-0) (life life-1) (tgt tgt-0)
    (nonce nonce-0) (cli cli-0) (tgs tgs-1) (as as-1) (serv serv-1)
    (tgsk tgsk))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)) ((1 1) (3 1)) ((2 1) (0 3))
    ((3 2) (2 0)))
  (non-orig (ltk cli as) (ltk tgs as) (ltk serv as))
  (uniq-orig nonce nonce-0 tgsk servk)
  (operation encryption-test (added-strand client 3)
    (enc cli-0 time-0 tgsk) (2 0))
  (traces
    ((send (cat cli serv nonce life))
      (recv (cat (enc time nonce life tgsk tgs (ltk cli as)) tgt))
      (send (cat (enc cli time tgsk) tgt))
      (recv (cat (enc serv t-prime life-prime servk tgsk) st))
      (send (cat (enc cli t-prime servk) st)) (recv (enc ack servk)))
    ((recv (cat cli serv-0 life nonce))
      (send
        (cat (enc time nonce life tgsk tgs (ltk cli as))
          (enc time nonce tgsk cli (ltk tgs as)))))
    ((recv
       (cat (enc cli-0 time-0 tgsk)
         (enc time-0 life-0 tgsk cli-0 (ltk tgs-0 as-0))))
      (send
        (cat (enc serv t-prime life-prime servk tgsk)
          (enc cli-0 serv t-prime life-prime servk (ltk serv as-0)))))
    ((send (cat cli-0 serv-1 nonce-0 life-1))
      (recv
        (cat (enc time-0 nonce-0 life-1 tgsk tgs-1 (ltk cli-0 as-1))
          tgt-0)) (send (cat (enc cli-0 time-0 tgsk) tgt-0))))
  (label 6)
  (parent 2)
  (unrealized (2 0) (3 1))
  (dead)
  (comment "empty cohort"))

(defskeleton kerberos
  (vars
    (time t-prime life life-prime tgt st ack nonce time-0 life-0 text)
    (cli tgs as serv serv-0 cli-0 tgs-0 as-0 name) (tgsk servk skey))
  (defstrand client 6 (time time) (t-prime t-prime) (life life)
    (life-prime life-prime) (tgt tgt) (st st) (ack ack) (nonce nonce)
    (cli cli) (tgs tgs) (as as) (serv serv) (tgsk tgsk) (servk servk))
  (defstrand as 2 (time time) (life nonce) (nonce life) (cli cli)
    (tgs tgs) (as as) (serv serv-0) (tgsk tgsk))
  (defstrand tgs 2 (time time-0) (t-prime t-prime) (life life-0)
    (life-prime life-prime) (cli cli-0) (tgs tgs-0) (as as-0)
    (serv serv) (tgsk tgsk) (servk servk))
  (deflistener tgsk)
  (precedes ((0 0) (1 0)) ((1 1) (0 1)) ((1 1) (3 0)) ((2 1) (0 3))
    ((3 1) (2 0)))
  (non-orig (ltk cli as) (ltk tgs as) (ltk serv as))
  (uniq-orig nonce tgsk servk)
  (operation encryption-test (added-listener tgsk)
    (enc cli-0 time-0 tgsk) (2 0))
  (traces
    ((send (cat cli serv nonce life))
      (recv (cat (enc time nonce life tgsk tgs (ltk cli as)) tgt))
      (send (cat (enc cli time tgsk) tgt))
      (recv (cat (enc serv t-prime life-prime servk tgsk) st))
      (send (cat (enc cli t-prime servk) st)) (recv (enc ack servk)))
    ((recv (cat cli serv-0 life nonce))
      (send
        (cat (enc time nonce life tgsk tgs (ltk cli as))
          (enc time nonce tgsk cli (ltk tgs as)))))
    ((recv
       (cat (enc cli-0 time-0 tgsk)
         (enc time-0 life-0 tgsk cli-0 (ltk tgs-0 as-0))))
      (send
        (cat (enc serv t-prime life-prime servk tgsk)
          (enc cli-0 serv t-prime life-prime servk (ltk serv as-0)))))
    ((recv tgsk) (send tgsk)))
  (label 7)
  (parent 2)
  (unrealized (3 0))
  (dead)
  (comment "empty cohort"))

(defskeleton kerberos
  (vars (time t-prime life life-prime tgt st ack nonce text)
    (cli tgs as serv serv-0 name) (tgsk servk skey))
  (defstrand client 6 (time time) (t-prime t-prime) (life life)
    (life-prime life-prime) (tgt tgt) (st st) (ack ack) (nonce nonce)
    (cli cli) (tgs tgs) (as as) (serv serv) (tgsk tgsk) (servk servk))
  (defstrand as 2 (time time) (life nonce) (nonce life) (cli cli)
    (tgs tgs) (as as) (serv serv-0) (tgsk tgsk))
  (defstrand tgs 2 (time time) (t-prime t-prime) (life nonce)
    (life-prime life-prime) (cli cli) (tgs tgs) (as as) (serv serv)
    (tgsk tgsk) (servk servk))
  (precedes ((0 0) (1 0)) ((0 2) (2 0)) ((1 1) (0 1)) ((2 1) (0 3)))
  (non-orig (ltk cli as) (ltk tgs as) (ltk serv as))
  (uniq-orig nonce tgsk servk)
  (operation nonce-test
    (contracted (tgs-0 tgs) (as-0 as) (life-0 nonce)) tgsk (2 0)
    (enc time nonce life tgsk tgs (ltk cli as))
    (enc time nonce tgsk cli (ltk tgs as)))
  (traces
    ((send (cat cli serv nonce life))
      (recv (cat (enc time nonce life tgsk tgs (ltk cli as)) tgt))
      (send (cat (enc cli time tgsk) tgt))
      (recv (cat (enc serv t-prime life-prime servk tgsk) st))
      (send (cat (enc cli t-prime servk) st)) (recv (enc ack servk)))
    ((recv (cat cli serv-0 life nonce))
      (send
        (cat (enc time nonce life tgsk tgs (ltk cli as))
          (enc time nonce tgsk cli (ltk tgs as)))))
    ((recv
       (cat (enc cli time tgsk) (enc time nonce tgsk cli (ltk tgs as))))
      (send
        (cat (enc serv t-prime life-prime servk tgsk)
          (enc cli serv t-prime life-prime servk (ltk serv as))))))
  (label 8)
  (parent 5)
  (unrealized (0 5))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton kerberos
  (vars
    (time t-prime life life-prime tgt st ack nonce t-prime-0
      life-prime-0 text) (cli tgs as serv serv-0 cli-0 as-0 serv-1 name)
    (tgsk servk skey))
  (defstrand client 6 (time time) (t-prime t-prime) (life life)
    (life-prime life-prime) (tgt tgt) (st st) (ack ack) (nonce nonce)
    (cli cli) (tgs tgs) (as as) (serv serv) (tgsk tgsk) (servk servk))
  (defstrand as 2 (time time) (life nonce) (nonce life) (cli cli)
    (tgs tgs) (as as) (serv serv-0) (tgsk tgsk))
  (defstrand tgs 2 (time time) (t-prime t-prime) (life nonce)
    (life-prime life-prime) (cli cli) (tgs tgs) (as as) (serv serv)
    (tgsk tgsk) (servk servk))
  (defstrand serv 2 (t-prime t-prime-0) (life-prime life-prime-0)
    (ack ack) (cli cli-0) (as as-0) (serv serv-1) (servk servk))
  (precedes ((0 0) (1 0)) ((0 2) (2 0)) ((1 1) (0 1)) ((2 1) (0 3))
    ((2 1) (3 0)) ((3 1) (0 5)))
  (non-orig (ltk cli as) (ltk tgs as) (ltk serv as))
  (uniq-orig nonce tgsk servk)
  (operation encryption-test (added-strand serv 2) (enc ack servk)
    (0 5))
  (traces
    ((send (cat cli serv nonce life))
      (recv (cat (enc time nonce life tgsk tgs (ltk cli as)) tgt))
      (send (cat (enc cli time tgsk) tgt))
      (recv (cat (enc serv t-prime life-prime servk tgsk) st))
      (send (cat (enc cli t-prime servk) st)) (recv (enc ack servk)))
    ((recv (cat cli serv-0 life nonce))
      (send
        (cat (enc time nonce life tgsk tgs (ltk cli as))
          (enc time nonce tgsk cli (ltk tgs as)))))
    ((recv
       (cat (enc cli time tgsk) (enc time nonce tgsk cli (ltk tgs as))))
      (send
        (cat (enc serv t-prime life-prime servk tgsk)
          (enc cli serv t-prime life-prime servk (ltk serv as)))))
    ((recv
       (cat (enc cli-0 t-prime-0 servk)
         (enc cli-0 serv-1 t-prime-0 life-prime-0 servk
           (ltk serv-1 as-0)))) (send (enc ack servk))))
  (label 9)
  (parent 8)
  (unrealized (3 0))
  (comment "4 in cohort - 4 not yet seen"))

(defskeleton kerberos
  (vars (time t-prime life life-prime tgt st ack nonce text)
    (cli tgs as serv serv-0 name) (tgsk servk skey))
  (defstrand client 6 (time time) (t-prime t-prime) (life life)
    (life-prime life-prime) (tgt tgt) (st st) (ack ack) (nonce nonce)
    (cli cli) (tgs tgs) (as as) (serv serv) (tgsk tgsk) (servk servk))
  (defstrand as 2 (time time) (life nonce) (nonce life) (cli cli)
    (tgs tgs) (as as) (serv serv-0) (tgsk tgsk))
  (defstrand tgs 2 (time time) (t-prime t-prime) (life nonce)
    (life-prime life-prime) (cli cli) (tgs tgs) (as as) (serv serv)
    (tgsk tgsk) (servk servk))
  (deflistener servk)
  (precedes ((0 0) (1 0)) ((0 2) (2 0)) ((1 1) (0 1)) ((2 1) (0 3))
    ((2 1) (3 0)) ((3 1) (0 5)))
  (non-orig (ltk cli as) (ltk tgs as) (ltk serv as))
  (uniq-orig nonce tgsk servk)
  (operation encryption-test (added-listener servk) (enc ack servk)
    (0 5))
  (traces
    ((send (cat cli serv nonce life))
      (recv (cat (enc time nonce life tgsk tgs (ltk cli as)) tgt))
      (send (cat (enc cli time tgsk) tgt))
      (recv (cat (enc serv t-prime life-prime servk tgsk) st))
      (send (cat (enc cli t-prime servk) st)) (recv (enc ack servk)))
    ((recv (cat cli serv-0 life nonce))
      (send
        (cat (enc time nonce life tgsk tgs (ltk cli as))
          (enc time nonce tgsk cli (ltk tgs as)))))
    ((recv
       (cat (enc cli time tgsk) (enc time nonce tgsk cli (ltk tgs as))))
      (send
        (cat (enc serv t-prime life-prime servk tgsk)
          (enc cli serv t-prime life-prime servk (ltk serv as)))))
    ((recv servk) (send servk)))
  (label 10)
  (parent 8)
  (unrealized (3 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton kerberos
  (vars
    (time t-prime life life-prime tgt st ack nonce life-prime-0 text)
    (cli tgs as serv serv-0 as-0 serv-1 name) (tgsk servk skey))
  (defstrand client 6 (time time) (t-prime t-prime) (life life)
    (life-prime life-prime) (tgt tgt) (st st) (ack ack) (nonce nonce)
    (cli cli) (tgs tgs) (as as) (serv serv) (tgsk tgsk) (servk servk))
  (defstrand as 2 (time time) (life nonce) (nonce life) (cli cli)
    (tgs tgs) (as as) (serv serv-0) (tgsk tgsk))
  (defstrand tgs 2 (time time) (t-prime t-prime) (life nonce)
    (life-prime life-prime) (cli cli) (tgs tgs) (as as) (serv serv)
    (tgsk tgsk) (servk servk))
  (defstrand serv 2 (t-prime t-prime) (life-prime life-prime-0)
    (ack ack) (cli cli) (as as-0) (serv serv-1) (servk servk))
  (precedes ((0 0) (1 0)) ((0 2) (2 0)) ((0 4) (3 0)) ((1 1) (0 1))
    ((2 1) (0 3)) ((3 1) (0 5)))
  (non-orig (ltk cli as) (ltk tgs as) (ltk serv as))
  (uniq-orig nonce tgsk servk)
  (operation encryption-test (displaced 4 0 client 5)
    (enc cli-0 t-prime-0 servk) (3 0))
  (traces
    ((send (cat cli serv nonce life))
      (recv (cat (enc time nonce life tgsk tgs (ltk cli as)) tgt))
      (send (cat (enc cli time tgsk) tgt))
      (recv (cat (enc serv t-prime life-prime servk tgsk) st))
      (send (cat (enc cli t-prime servk) st)) (recv (enc ack servk)))
    ((recv (cat cli serv-0 life nonce))
      (send
        (cat (enc time nonce life tgsk tgs (ltk cli as))
          (enc time nonce tgsk cli (ltk tgs as)))))
    ((recv
       (cat (enc cli time tgsk) (enc time nonce tgsk cli (ltk tgs as))))
      (send
        (cat (enc serv t-prime life-prime servk tgsk)
          (enc cli serv t-prime life-prime servk (ltk serv as)))))
    ((recv
       (cat (enc cli t-prime servk)
         (enc cli serv-1 t-prime life-prime-0 servk (ltk serv-1 as-0))))
      (send (enc ack servk))))
  (label 11)
  (parent 9)
  (unrealized (3 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton kerberos
  (vars
    (time t-prime life life-prime tgt st ack nonce t-prime-0
      life-prime-0 time-0 life-0 life-prime-1 tgt-0 st-0 nonce-0 text)
    (cli tgs as serv serv-0 cli-0 as-0 serv-1 tgs-0 as-1 serv-2 name)
    (tgsk servk tgsk-0 skey))
  (defstrand client 6 (time time) (t-prime t-prime) (life life)
    (life-prime life-prime) (tgt tgt) (st st) (ack ack) (nonce nonce)
    (cli cli) (tgs tgs) (as as) (serv serv) (tgsk tgsk) (servk servk))
  (defstrand as 2 (time time) (life nonce) (nonce life) (cli cli)
    (tgs tgs) (as as) (serv serv-0) (tgsk tgsk))
  (defstrand tgs 2 (time time) (t-prime t-prime) (life nonce)
    (life-prime life-prime) (cli cli) (tgs tgs) (as as) (serv serv)
    (tgsk tgsk) (servk servk))
  (defstrand serv 2 (t-prime t-prime-0) (life-prime life-prime-0)
    (ack ack) (cli cli-0) (as as-0) (serv serv-1) (servk servk))
  (defstrand client 5 (time time-0) (t-prime t-prime-0) (life life-0)
    (life-prime life-prime-1) (tgt tgt-0) (st st-0) (nonce nonce-0)
    (cli cli-0) (tgs tgs-0) (as as-1) (serv serv-2) (tgsk tgsk-0)
    (servk servk))
  (precedes ((0 0) (1 0)) ((0 2) (2 0)) ((1 1) (0 1)) ((2 1) (0 3))
    ((2 1) (4 3)) ((3 1) (0 5)) ((4 4) (3 0)))
  (non-orig (ltk cli as) (ltk tgs as) (ltk serv as))
  (uniq-orig nonce nonce-0 tgsk servk)
  (operation encryption-test (added-strand client 5)
    (enc cli-0 t-prime-0 servk) (3 0))
  (traces
    ((send (cat cli serv nonce life))
      (recv (cat (enc time nonce life tgsk tgs (ltk cli as)) tgt))
      (send (cat (enc cli time tgsk) tgt))
      (recv (cat (enc serv t-prime life-prime servk tgsk) st))
      (send (cat (enc cli t-prime servk) st)) (recv (enc ack servk)))
    ((recv (cat cli serv-0 life nonce))
      (send
        (cat (enc time nonce life tgsk tgs (ltk cli as))
          (enc time nonce tgsk cli (ltk tgs as)))))
    ((recv
       (cat (enc cli time tgsk) (enc time nonce tgsk cli (ltk tgs as))))
      (send
        (cat (enc serv t-prime life-prime servk tgsk)
          (enc cli serv t-prime life-prime servk (ltk serv as)))))
    ((recv
       (cat (enc cli-0 t-prime-0 servk)
         (enc cli-0 serv-1 t-prime-0 life-prime-0 servk
           (ltk serv-1 as-0)))) (send (enc ack servk)))
    ((send (cat cli-0 serv-2 nonce-0 life-0))
      (recv
        (cat (enc time-0 nonce-0 life-0 tgsk-0 tgs-0 (ltk cli-0 as-1))
          tgt-0)) (send (cat (enc cli-0 time-0 tgsk-0) tgt-0))
      (recv (cat (enc serv-2 t-prime-0 life-prime-1 servk tgsk-0) st-0))
      (send (cat (enc cli-0 t-prime-0 servk) st-0))))
  (label 12)
  (parent 9)
  (unrealized (3 0) (4 3))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton kerberos
  (vars
    (time t-prime life life-prime tgt st ack nonce t-prime-0
      life-prime-0 life-0 tgt-0 nonce-0 text)
    (cli tgs as serv serv-0 cli-0 as-0 serv-1 tgs-0 as-1 serv-2 name)
    (tgsk servk skey))
  (defstrand client 6 (time time) (t-prime t-prime) (life life)
    (life-prime life-prime) (tgt tgt) (st st) (ack ack) (nonce nonce)
    (cli cli) (tgs tgs) (as as) (serv serv) (tgsk tgsk) (servk servk))
  (defstrand as 2 (time time) (life nonce) (nonce life) (cli cli)
    (tgs tgs) (as as) (serv serv-0) (tgsk tgsk))
  (defstrand tgs 2 (time time) (t-prime t-prime) (life nonce)
    (life-prime life-prime) (cli cli) (tgs tgs) (as as) (serv serv)
    (tgsk tgsk) (servk servk))
  (defstrand serv 2 (t-prime t-prime-0) (life-prime life-prime-0)
    (ack ack) (cli cli-0) (as as-0) (serv serv-1) (servk servk))
  (defstrand client 3 (time t-prime-0) (life life-0) (tgt tgt-0)
    (nonce nonce-0) (cli cli-0) (tgs tgs-0) (as as-1) (serv serv-2)
    (tgsk servk))
  (precedes ((0 0) (1 0)) ((0 2) (2 0)) ((1 1) (0 1)) ((2 1) (0 3))
    ((2 1) (4 1)) ((3 1) (0 5)) ((4 2) (3 0)))
  (non-orig (ltk cli as) (ltk tgs as) (ltk serv as))
  (uniq-orig nonce nonce-0 tgsk servk)
  (operation encryption-test (added-strand client 3)
    (enc cli-0 t-prime-0 servk) (3 0))
  (traces
    ((send (cat cli serv nonce life))
      (recv (cat (enc time nonce life tgsk tgs (ltk cli as)) tgt))
      (send (cat (enc cli time tgsk) tgt))
      (recv (cat (enc serv t-prime life-prime servk tgsk) st))
      (send (cat (enc cli t-prime servk) st)) (recv (enc ack servk)))
    ((recv (cat cli serv-0 life nonce))
      (send
        (cat (enc time nonce life tgsk tgs (ltk cli as))
          (enc time nonce tgsk cli (ltk tgs as)))))
    ((recv
       (cat (enc cli time tgsk) (enc time nonce tgsk cli (ltk tgs as))))
      (send
        (cat (enc serv t-prime life-prime servk tgsk)
          (enc cli serv t-prime life-prime servk (ltk serv as)))))
    ((recv
       (cat (enc cli-0 t-prime-0 servk)
         (enc cli-0 serv-1 t-prime-0 life-prime-0 servk
           (ltk serv-1 as-0)))) (send (enc ack servk)))
    ((send (cat cli-0 serv-2 nonce-0 life-0))
      (recv
        (cat (enc t-prime-0 nonce-0 life-0 servk tgs-0 (ltk cli-0 as-1))
          tgt-0)) (send (cat (enc cli-0 t-prime-0 servk) tgt-0))))
  (label 13)
  (parent 9)
  (unrealized (3 0) (4 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton kerberos
  (vars
    (time t-prime life life-prime tgt st ack nonce t-prime-0
      life-prime-0 text) (cli tgs as serv serv-0 cli-0 as-0 serv-1 name)
    (tgsk servk skey))
  (defstrand client 6 (time time) (t-prime t-prime) (life life)
    (life-prime life-prime) (tgt tgt) (st st) (ack ack) (nonce nonce)
    (cli cli) (tgs tgs) (as as) (serv serv) (tgsk tgsk) (servk servk))
  (defstrand as 2 (time time) (life nonce) (nonce life) (cli cli)
    (tgs tgs) (as as) (serv serv-0) (tgsk tgsk))
  (defstrand tgs 2 (time time) (t-prime t-prime) (life nonce)
    (life-prime life-prime) (cli cli) (tgs tgs) (as as) (serv serv)
    (tgsk tgsk) (servk servk))
  (defstrand serv 2 (t-prime t-prime-0) (life-prime life-prime-0)
    (ack ack) (cli cli-0) (as as-0) (serv serv-1) (servk servk))
  (deflistener servk)
  (precedes ((0 0) (1 0)) ((0 2) (2 0)) ((1 1) (0 1)) ((2 1) (0 3))
    ((2 1) (4 0)) ((3 1) (0 5)) ((4 1) (3 0)))
  (non-orig (ltk cli as) (ltk tgs as) (ltk serv as))
  (uniq-orig nonce tgsk servk)
  (operation encryption-test (added-listener servk)
    (enc cli-0 t-prime-0 servk) (3 0))
  (traces
    ((send (cat cli serv nonce life))
      (recv (cat (enc time nonce life tgsk tgs (ltk cli as)) tgt))
      (send (cat (enc cli time tgsk) tgt))
      (recv (cat (enc serv t-prime life-prime servk tgsk) st))
      (send (cat (enc cli t-prime servk) st)) (recv (enc ack servk)))
    ((recv (cat cli serv-0 life nonce))
      (send
        (cat (enc time nonce life tgsk tgs (ltk cli as))
          (enc time nonce tgsk cli (ltk tgs as)))))
    ((recv
       (cat (enc cli time tgsk) (enc time nonce tgsk cli (ltk tgs as))))
      (send
        (cat (enc serv t-prime life-prime servk tgsk)
          (enc cli serv t-prime life-prime servk (ltk serv as)))))
    ((recv
       (cat (enc cli-0 t-prime-0 servk)
         (enc cli-0 serv-1 t-prime-0 life-prime-0 servk
           (ltk serv-1 as-0)))) (send (enc ack servk)))
    ((recv servk) (send servk)))
  (label 14)
  (parent 9)
  (unrealized (4 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton kerberos
  (vars (time t-prime life life-prime tgt st ack nonce text)
    (cli tgs as serv serv-0 name) (tgsk servk skey))
  (defstrand client 6 (time time) (t-prime t-prime) (life life)
    (life-prime life-prime) (tgt tgt) (st st) (ack ack) (nonce nonce)
    (cli cli) (tgs tgs) (as as) (serv serv) (tgsk tgsk) (servk servk))
  (defstrand as 2 (time time) (life nonce) (nonce life) (cli cli)
    (tgs tgs) (as as) (serv serv-0) (tgsk tgsk))
  (defstrand tgs 2 (time time) (t-prime t-prime) (life nonce)
    (life-prime life-prime) (cli cli) (tgs tgs) (as as) (serv serv)
    (tgsk tgsk) (servk servk))
  (deflistener servk)
  (deflistener tgsk)
  (precedes ((0 0) (1 0)) ((0 2) (2 0)) ((1 1) (0 1)) ((1 1) (4 0))
    ((2 1) (0 3)) ((2 1) (3 0)) ((3 1) (0 5)) ((4 1) (3 0)))
  (non-orig (ltk cli as) (ltk tgs as) (ltk serv as))
  (uniq-orig nonce tgsk servk)
  (operation nonce-test (added-listener tgsk) servk (3 0)
    (enc cli serv t-prime life-prime servk (ltk serv as))
    (enc serv t-prime life-prime servk tgsk))
  (traces
    ((send (cat cli serv nonce life))
      (recv (cat (enc time nonce life tgsk tgs (ltk cli as)) tgt))
      (send (cat (enc cli time tgsk) tgt))
      (recv (cat (enc serv t-prime life-prime servk tgsk) st))
      (send (cat (enc cli t-prime servk) st)) (recv (enc ack servk)))
    ((recv (cat cli serv-0 life nonce))
      (send
        (cat (enc time nonce life tgsk tgs (ltk cli as))
          (enc time nonce tgsk cli (ltk tgs as)))))
    ((recv
       (cat (enc cli time tgsk) (enc time nonce tgsk cli (ltk tgs as))))
      (send
        (cat (enc serv t-prime life-prime servk tgsk)
          (enc cli serv t-prime life-prime servk (ltk serv as)))))
    ((recv servk) (send servk)) ((recv tgsk) (send tgsk)))
  (label 15)
  (parent 10)
  (unrealized (4 0))
  (dead)
  (comment "empty cohort"))

(defskeleton kerberos
  (vars (time t-prime life life-prime tgt st ack nonce text)
    (cli tgs as serv serv-0 name) (tgsk servk skey))
  (defstrand client 6 (time time) (t-prime t-prime) (life life)
    (life-prime life-prime) (tgt tgt) (st st) (ack ack) (nonce nonce)
    (cli cli) (tgs tgs) (as as) (serv serv) (tgsk tgsk) (servk servk))
  (defstrand as 2 (time time) (life nonce) (nonce life) (cli cli)
    (tgs tgs) (as as) (serv serv-0) (tgsk tgsk))
  (defstrand tgs 2 (time time) (t-prime t-prime) (life nonce)
    (life-prime life-prime) (cli cli) (tgs tgs) (as as) (serv serv)
    (tgsk tgsk) (servk servk))
  (defstrand serv 2 (t-prime t-prime) (life-prime life-prime) (ack ack)
    (cli cli) (as as) (serv serv) (servk servk))
  (precedes ((0 0) (1 0)) ((0 2) (2 0)) ((0 4) (3 0)) ((1 1) (0 1))
    ((2 1) (0 3)) ((3 1) (0 5)))
  (non-orig (ltk cli as) (ltk tgs as) (ltk serv as))
  (uniq-orig nonce tgsk servk)
  (operation nonce-test
    (contracted (as-0 as) (serv-1 serv) (life-prime-0 life-prime)) servk
    (3 0) (enc cli serv t-prime life-prime servk (ltk serv as))
    (enc serv t-prime life-prime servk tgsk))
  (traces
    ((send (cat cli serv nonce life))
      (recv (cat (enc time nonce life tgsk tgs (ltk cli as)) tgt))
      (send (cat (enc cli time tgsk) tgt))
      (recv (cat (enc serv t-prime life-prime servk tgsk) st))
      (send (cat (enc cli t-prime servk) st)) (recv (enc ack servk)))
    ((recv (cat cli serv-0 life nonce))
      (send
        (cat (enc time nonce life tgsk tgs (ltk cli as))
          (enc time nonce tgsk cli (ltk tgs as)))))
    ((recv
       (cat (enc cli time tgsk) (enc time nonce tgsk cli (ltk tgs as))))
      (send
        (cat (enc serv t-prime life-prime servk tgsk)
          (enc cli serv t-prime life-prime servk (ltk serv as)))))
    ((recv
       (cat (enc cli t-prime servk)
         (enc cli serv t-prime life-prime servk (ltk serv as))))
      (send (enc ack servk))))
  (label 16)
  (parent 11)
  (unrealized)
  (shape)
  (maps
    ((0)
      ((cli cli) (tgs tgs) (as as) (serv serv) (time time)
        (t-prime t-prime) (life life) (life-prime life-prime) (tgt tgt)
        (st st) (ack ack) (nonce nonce) (tgsk tgsk) (servk servk))))
  (origs (nonce (0 0)) (servk (2 1)) (tgsk (1 1))))

(defskeleton kerberos
  (vars
    (time t-prime life life-prime tgt st ack nonce life-prime-0 text)
    (cli tgs as serv serv-0 as-0 serv-1 name) (tgsk servk skey))
  (defstrand client 6 (time time) (t-prime t-prime) (life life)
    (life-prime life-prime) (tgt tgt) (st st) (ack ack) (nonce nonce)
    (cli cli) (tgs tgs) (as as) (serv serv) (tgsk tgsk) (servk servk))
  (defstrand as 2 (time time) (life nonce) (nonce life) (cli cli)
    (tgs tgs) (as as) (serv serv-0) (tgsk tgsk))
  (defstrand tgs 2 (time time) (t-prime t-prime) (life nonce)
    (life-prime life-prime) (cli cli) (tgs tgs) (as as) (serv serv)
    (tgsk tgsk) (servk servk))
  (defstrand serv 2 (t-prime t-prime) (life-prime life-prime-0)
    (ack ack) (cli cli) (as as-0) (serv serv-1) (servk servk))
  (deflistener tgsk)
  (precedes ((0 0) (1 0)) ((0 2) (2 0)) ((0 4) (3 0)) ((1 1) (0 1))
    ((1 1) (4 0)) ((2 1) (0 3)) ((3 1) (0 5)) ((4 1) (3 0)))
  (non-orig (ltk cli as) (ltk tgs as) (ltk serv as))
  (uniq-orig nonce tgsk servk)
  (operation nonce-test (added-listener tgsk) servk (3 0)
    (enc cli serv t-prime life-prime servk (ltk serv as))
    (enc serv t-prime life-prime servk tgsk))
  (traces
    ((send (cat cli serv nonce life))
      (recv (cat (enc time nonce life tgsk tgs (ltk cli as)) tgt))
      (send (cat (enc cli time tgsk) tgt))
      (recv (cat (enc serv t-prime life-prime servk tgsk) st))
      (send (cat (enc cli t-prime servk) st)) (recv (enc ack servk)))
    ((recv (cat cli serv-0 life nonce))
      (send
        (cat (enc time nonce life tgsk tgs (ltk cli as))
          (enc time nonce tgsk cli (ltk tgs as)))))
    ((recv
       (cat (enc cli time tgsk) (enc time nonce tgsk cli (ltk tgs as))))
      (send
        (cat (enc serv t-prime life-prime servk tgsk)
          (enc cli serv t-prime life-prime servk (ltk serv as)))))
    ((recv
       (cat (enc cli t-prime servk)
         (enc cli serv-1 t-prime life-prime-0 servk (ltk serv-1 as-0))))
      (send (enc ack servk))) ((recv tgsk) (send tgsk)))
  (label 17)
  (parent 11)
  (unrealized (4 0))
  (dead)
  (comment "empty cohort"))

(defskeleton kerberos
  (vars
    (time t-prime life life-prime tgt st ack nonce life-prime-0 time-0
      life-0 tgt-0 st-0 nonce-0 text)
    (cli tgs as serv serv-0 cli-0 as-0 serv-1 tgs-0 as-1 name)
    (tgsk servk skey))
  (defstrand client 6 (time time) (t-prime t-prime) (life life)
    (life-prime life-prime) (tgt tgt) (st st) (ack ack) (nonce nonce)
    (cli cli) (tgs tgs) (as as) (serv serv) (tgsk tgsk) (servk servk))
  (defstrand as 2 (time time) (life nonce) (nonce life) (cli cli)
    (tgs tgs) (as as) (serv serv-0) (tgsk tgsk))
  (defstrand tgs 2 (time time) (t-prime t-prime) (life nonce)
    (life-prime life-prime) (cli cli) (tgs tgs) (as as) (serv serv)
    (tgsk tgsk) (servk servk))
  (defstrand serv 2 (t-prime t-prime) (life-prime life-prime-0)
    (ack ack) (cli cli-0) (as as-0) (serv serv-1) (servk servk))
  (defstrand client 5 (time time-0) (t-prime t-prime) (life life-0)
    (life-prime life-prime) (tgt tgt-0) (st st-0) (nonce nonce-0)
    (cli cli-0) (tgs tgs-0) (as as-1) (serv serv) (tgsk tgsk)
    (servk servk))
  (precedes ((0 0) (1 0)) ((0 2) (2 0)) ((1 1) (0 1)) ((1 1) (4 1))
    ((2 1) (0 3)) ((2 1) (4 3)) ((3 1) (0 5)) ((4 4) (3 0)))
  (non-orig (ltk cli as) (ltk tgs as) (ltk serv as))
  (uniq-orig nonce nonce-0 tgsk servk)
  (operation nonce-test
    (contracted (t-prime-0 t-prime) (serv-2 serv)
      (life-prime-1 life-prime) (tgsk-0 tgsk)) servk (4 3)
    (enc cli serv t-prime life-prime servk (ltk serv as))
    (enc serv t-prime life-prime servk tgsk))
  (traces
    ((send (cat cli serv nonce life))
      (recv (cat (enc time nonce life tgsk tgs (ltk cli as)) tgt))
      (send (cat (enc cli time tgsk) tgt))
      (recv (cat (enc serv t-prime life-prime servk tgsk) st))
      (send (cat (enc cli t-prime servk) st)) (recv (enc ack servk)))
    ((recv (cat cli serv-0 life nonce))
      (send
        (cat (enc time nonce life tgsk tgs (ltk cli as))
          (enc time nonce tgsk cli (ltk tgs as)))))
    ((recv
       (cat (enc cli time tgsk) (enc time nonce tgsk cli (ltk tgs as))))
      (send
        (cat (enc serv t-prime life-prime servk tgsk)
          (enc cli serv t-prime life-prime servk (ltk serv as)))))
    ((recv
       (cat (enc cli-0 t-prime servk)
         (enc cli-0 serv-1 t-prime life-prime-0 servk
           (ltk serv-1 as-0)))) (send (enc ack servk)))
    ((send (cat cli-0 serv nonce-0 life-0))
      (recv
        (cat (enc time-0 nonce-0 life-0 tgsk tgs-0 (ltk cli-0 as-1))
          tgt-0)) (send (cat (enc cli-0 time-0 tgsk) tgt-0))
      (recv (cat (enc serv t-prime life-prime servk tgsk) st-0))
      (send (cat (enc cli-0 t-prime servk) st-0))))
  (label 18)
  (parent 12)
  (unrealized (3 0) (4 1))
  (dead)
  (comment "empty cohort"))

(defskeleton kerberos
  (vars
    (time t-prime life life-prime tgt st ack nonce t-prime-0
      life-prime-0 time-0 life-0 life-prime-1 tgt-0 st-0 nonce-0 text)
    (cli tgs as serv serv-0 cli-0 as-0 serv-1 tgs-0 as-1 serv-2 name)
    (tgsk servk tgsk-0 skey))
  (defstrand client 6 (time time) (t-prime t-prime) (life life)
    (life-prime life-prime) (tgt tgt) (st st) (ack ack) (nonce nonce)
    (cli cli) (tgs tgs) (as as) (serv serv) (tgsk tgsk) (servk servk))
  (defstrand as 2 (time time) (life nonce) (nonce life) (cli cli)
    (tgs tgs) (as as) (serv serv-0) (tgsk tgsk))
  (defstrand tgs 2 (time time) (t-prime t-prime) (life nonce)
    (life-prime life-prime) (cli cli) (tgs tgs) (as as) (serv serv)
    (tgsk tgsk) (servk servk))
  (defstrand serv 2 (t-prime t-prime-0) (life-prime life-prime-0)
    (ack ack) (cli cli-0) (as as-0) (serv serv-1) (servk servk))
  (defstrand client 5 (time time-0) (t-prime t-prime-0) (life life-0)
    (life-prime life-prime-1) (tgt tgt-0) (st st-0) (nonce nonce-0)
    (cli cli-0) (tgs tgs-0) (as as-1) (serv serv-2) (tgsk tgsk-0)
    (servk servk))
  (deflistener tgsk)
  (precedes ((0 0) (1 0)) ((0 2) (2 0)) ((1 1) (0 1)) ((1 1) (5 0))
    ((2 1) (0 3)) ((2 1) (4 3)) ((3 1) (0 5)) ((4 4) (3 0))
    ((5 1) (4 3)))
  (non-orig (ltk cli as) (ltk tgs as) (ltk serv as))
  (uniq-orig nonce nonce-0 tgsk servk)
  (operation nonce-test (added-listener tgsk) servk (4 3)
    (enc cli serv t-prime life-prime servk (ltk serv as))
    (enc serv t-prime life-prime servk tgsk))
  (traces
    ((send (cat cli serv nonce life))
      (recv (cat (enc time nonce life tgsk tgs (ltk cli as)) tgt))
      (send (cat (enc cli time tgsk) tgt))
      (recv (cat (enc serv t-prime life-prime servk tgsk) st))
      (send (cat (enc cli t-prime servk) st)) (recv (enc ack servk)))
    ((recv (cat cli serv-0 life nonce))
      (send
        (cat (enc time nonce life tgsk tgs (ltk cli as))
          (enc time nonce tgsk cli (ltk tgs as)))))
    ((recv
       (cat (enc cli time tgsk) (enc time nonce tgsk cli (ltk tgs as))))
      (send
        (cat (enc serv t-prime life-prime servk tgsk)
          (enc cli serv t-prime life-prime servk (ltk serv as)))))
    ((recv
       (cat (enc cli-0 t-prime-0 servk)
         (enc cli-0 serv-1 t-prime-0 life-prime-0 servk
           (ltk serv-1 as-0)))) (send (enc ack servk)))
    ((send (cat cli-0 serv-2 nonce-0 life-0))
      (recv
        (cat (enc time-0 nonce-0 life-0 tgsk-0 tgs-0 (ltk cli-0 as-1))
          tgt-0)) (send (cat (enc cli-0 time-0 tgsk-0) tgt-0))
      (recv (cat (enc serv-2 t-prime-0 life-prime-1 servk tgsk-0) st-0))
      (send (cat (enc cli-0 t-prime-0 servk) st-0)))
    ((recv tgsk) (send tgsk)))
  (label 19)
  (parent 12)
  (unrealized (5 0))
  (dead)
  (comment "empty cohort"))

(defskeleton kerberos
  (vars
    (time t-prime life life-prime tgt st ack nonce t-prime-0
      life-prime-0 life-0 tgt-0 nonce-0 text)
    (cli tgs as serv serv-0 cli-0 as-0 serv-1 tgs-0 as-1 serv-2 name)
    (tgsk servk skey))
  (defstrand client 6 (time time) (t-prime t-prime) (life life)
    (life-prime life-prime) (tgt tgt) (st st) (ack ack) (nonce nonce)
    (cli cli) (tgs tgs) (as as) (serv serv) (tgsk tgsk) (servk servk))
  (defstrand as 2 (time time) (life nonce) (nonce life) (cli cli)
    (tgs tgs) (as as) (serv serv-0) (tgsk tgsk))
  (defstrand tgs 2 (time time) (t-prime t-prime) (life nonce)
    (life-prime life-prime) (cli cli) (tgs tgs) (as as) (serv serv)
    (tgsk tgsk) (servk servk))
  (defstrand serv 2 (t-prime t-prime-0) (life-prime life-prime-0)
    (ack ack) (cli cli-0) (as as-0) (serv serv-1) (servk servk))
  (defstrand client 3 (time t-prime-0) (life life-0) (tgt tgt-0)
    (nonce nonce-0) (cli cli-0) (tgs tgs-0) (as as-1) (serv serv-2)
    (tgsk servk))
  (deflistener tgsk)
  (precedes ((0 0) (1 0)) ((0 2) (2 0)) ((1 1) (0 1)) ((1 1) (5 0))
    ((2 1) (0 3)) ((2 1) (4 1)) ((3 1) (0 5)) ((4 2) (3 0))
    ((5 1) (4 1)))
  (non-orig (ltk cli as) (ltk tgs as) (ltk serv as))
  (uniq-orig nonce nonce-0 tgsk servk)
  (operation nonce-test (added-listener tgsk) servk (4 1)
    (enc cli serv t-prime life-prime servk (ltk serv as))
    (enc serv t-prime life-prime servk tgsk))
  (traces
    ((send (cat cli serv nonce life))
      (recv (cat (enc time nonce life tgsk tgs (ltk cli as)) tgt))
      (send (cat (enc cli time tgsk) tgt))
      (recv (cat (enc serv t-prime life-prime servk tgsk) st))
      (send (cat (enc cli t-prime servk) st)) (recv (enc ack servk)))
    ((recv (cat cli serv-0 life nonce))
      (send
        (cat (enc time nonce life tgsk tgs (ltk cli as))
          (enc time nonce tgsk cli (ltk tgs as)))))
    ((recv
       (cat (enc cli time tgsk) (enc time nonce tgsk cli (ltk tgs as))))
      (send
        (cat (enc serv t-prime life-prime servk tgsk)
          (enc cli serv t-prime life-prime servk (ltk serv as)))))
    ((recv
       (cat (enc cli-0 t-prime-0 servk)
         (enc cli-0 serv-1 t-prime-0 life-prime-0 servk
           (ltk serv-1 as-0)))) (send (enc ack servk)))
    ((send (cat cli-0 serv-2 nonce-0 life-0))
      (recv
        (cat (enc t-prime-0 nonce-0 life-0 servk tgs-0 (ltk cli-0 as-1))
          tgt-0)) (send (cat (enc cli-0 t-prime-0 servk) tgt-0)))
    ((recv tgsk) (send tgsk)))
  (label 20)
  (parent 13)
  (unrealized (5 0))
  (dead)
  (comment "empty cohort"))

(defskeleton kerberos
  (vars
    (time t-prime life life-prime tgt st ack nonce t-prime-0
      life-prime-0 text) (cli tgs as serv serv-0 cli-0 as-0 serv-1 name)
    (tgsk servk skey))
  (defstrand client 6 (time time) (t-prime t-prime) (life life)
    (life-prime life-prime) (tgt tgt) (st st) (ack ack) (nonce nonce)
    (cli cli) (tgs tgs) (as as) (serv serv) (tgsk tgsk) (servk servk))
  (defstrand as 2 (time time) (life nonce) (nonce life) (cli cli)
    (tgs tgs) (as as) (serv serv-0) (tgsk tgsk))
  (defstrand tgs 2 (time time) (t-prime t-prime) (life nonce)
    (life-prime life-prime) (cli cli) (tgs tgs) (as as) (serv serv)
    (tgsk tgsk) (servk servk))
  (defstrand serv 2 (t-prime t-prime-0) (life-prime life-prime-0)
    (ack ack) (cli cli-0) (as as-0) (serv serv-1) (servk servk))
  (deflistener servk)
  (deflistener tgsk)
  (precedes ((0 0) (1 0)) ((0 2) (2 0)) ((1 1) (0 1)) ((1 1) (5 0))
    ((2 1) (0 3)) ((2 1) (4 0)) ((3 1) (0 5)) ((4 1) (3 0))
    ((5 1) (4 0)))
  (non-orig (ltk cli as) (ltk tgs as) (ltk serv as))
  (uniq-orig nonce tgsk servk)
  (operation nonce-test (added-listener tgsk) servk (4 0)
    (enc cli serv t-prime life-prime servk (ltk serv as))
    (enc serv t-prime life-prime servk tgsk))
  (traces
    ((send (cat cli serv nonce life))
      (recv (cat (enc time nonce life tgsk tgs (ltk cli as)) tgt))
      (send (cat (enc cli time tgsk) tgt))
      (recv (cat (enc serv t-prime life-prime servk tgsk) st))
      (send (cat (enc cli t-prime servk) st)) (recv (enc ack servk)))
    ((recv (cat cli serv-0 life nonce))
      (send
        (cat (enc time nonce life tgsk tgs (ltk cli as))
          (enc time nonce tgsk cli (ltk tgs as)))))
    ((recv
       (cat (enc cli time tgsk) (enc time nonce tgsk cli (ltk tgs as))))
      (send
        (cat (enc serv t-prime life-prime servk tgsk)
          (enc cli serv t-prime life-prime servk (ltk serv as)))))
    ((recv
       (cat (enc cli-0 t-prime-0 servk)
         (enc cli-0 serv-1 t-prime-0 life-prime-0 servk
           (ltk serv-1 as-0)))) (send (enc ack servk)))
    ((recv servk) (send servk)) ((recv tgsk) (send tgsk)))
  (label 21)
  (parent 14)
  (unrealized (5 0))
  (dead)
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol kerberos basic
  (defrole client
    (vars (cli tgs as serv name)
      (time t-prime life life-prime tgt st ack nonce text)
      (tgsk servk skey))
    (trace (send (cat cli serv nonce life))
      (recv (cat (enc time nonce life tgsk tgs (ltk cli as)) tgt))
      (send (cat (enc cli time tgsk) tgt))
      (recv (cat (enc serv t-prime life-prime servk tgsk) st))
      (send (cat (enc cli t-prime servk) st)) (recv (enc ack servk)))
    (uniq-orig nonce))
  (defrole tgs
    (vars (cli tgs as serv name) (time t-prime life life-prime text)
      (tgsk servk skey))
    (trace
      (recv
        (cat (enc cli time tgsk) (enc time life tgsk cli (ltk tgs as))))
      (send
        (cat (enc serv t-prime life-prime servk tgsk)
          (enc cli serv t-prime life-prime servk (ltk serv as)))))
    (uniq-orig servk))
  (defrole as
    (vars (cli tgs as serv name) (time life nonce text) (tgsk skey))
    (trace (recv (cat cli serv nonce life))
      (send
        (cat (enc time life nonce tgsk tgs (ltk cli as))
          (enc time life tgsk cli (ltk tgs as)))))
    (uniq-orig tgsk))
  (defrole serv
    (vars (cli as serv name) (t-prime life-prime ack text) (servk skey))
    (trace
      (recv
        (cat (enc cli t-prime servk)
          (enc cli serv t-prime life-prime servk (ltk serv as))))
      (send (enc ack servk)))))

(defskeleton kerberos
  (vars (time t-prime life life-prime text) (cli tgs as serv name)
    (tgsk servk skey))
  (defstrand tgs 2 (time time) (t-prime t-prime) (life life)
    (life-prime life-prime) (cli cli) (tgs tgs) (as as) (serv serv)
    (tgsk tgsk) (servk servk))
  (non-orig (ltk cli as) (ltk tgs as) (ltk serv as))
  (uniq-orig servk)
  (traces
    ((recv
       (cat (enc cli time tgsk) (enc time life tgsk cli (ltk tgs as))))
      (send
        (cat (enc serv t-prime life-prime servk tgsk)
          (enc cli serv t-prime life-prime servk (ltk serv as))))))
  (label 22)
  (unrealized (0 0))
  (origs (servk (0 1)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton kerberos
  (vars (time t-prime life life-prime nonce text)
    (cli tgs as serv serv-0 name) (tgsk servk skey))
  (defstrand tgs 2 (time time) (t-prime t-prime) (life life)
    (life-prime life-prime) (cli cli) (tgs tgs) (as as) (serv serv)
    (tgsk tgsk) (servk servk))
  (defstrand as 2 (time time) (life life) (nonce nonce) (cli cli)
    (tgs tgs) (as as) (serv serv-0) (tgsk tgsk))
  (precedes ((1 1) (0 0)))
  (non-orig (ltk cli as) (ltk tgs as) (ltk serv as))
  (uniq-orig tgsk servk)
  (operation encryption-test (added-strand as 2)
    (enc time life tgsk cli (ltk tgs as)) (0 0))
  (traces
    ((recv
       (cat (enc cli time tgsk) (enc time life tgsk cli (ltk tgs as))))
      (send
        (cat (enc serv t-prime life-prime servk tgsk)
          (enc cli serv t-prime life-prime servk (ltk serv as)))))
    ((recv (cat cli serv-0 nonce life))
      (send
        (cat (enc time life nonce tgsk tgs (ltk cli as))
          (enc time life tgsk cli (ltk tgs as))))))
  (label 23)
  (parent 22)
  (unrealized (0 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton kerberos
  (vars
    (time t-prime life life-prime nonce time-0 life-0 life-prime-0 tgt
      st nonce-0 text) (cli tgs as serv serv-0 tgs-0 as-0 serv-1 name)
    (tgsk servk tgsk-0 skey))
  (defstrand tgs 2 (time time) (t-prime t-prime) (life life)
    (life-prime life-prime) (cli cli) (tgs tgs) (as as) (serv serv)
    (tgsk tgsk) (servk servk))
  (defstrand as 2 (time time) (life life) (nonce nonce) (cli cli)
    (tgs tgs) (as as) (serv serv-0) (tgsk tgsk))
  (defstrand client 5 (time time-0) (t-prime time) (life life-0)
    (life-prime life-prime-0) (tgt tgt) (st st) (nonce nonce-0)
    (cli cli) (tgs tgs-0) (as as-0) (serv serv-1) (tgsk tgsk-0)
    (servk tgsk))
  (precedes ((1 1) (2 3)) ((2 4) (0 0)))
  (non-orig (ltk cli as) (ltk tgs as) (ltk serv as))
  (uniq-orig nonce-0 tgsk servk)
  (operation encryption-test (added-strand client 5) (enc cli time tgsk)
    (0 0))
  (traces
    ((recv
       (cat (enc cli time tgsk) (enc time life tgsk cli (ltk tgs as))))
      (send
        (cat (enc serv t-prime life-prime servk tgsk)
          (enc cli serv t-prime life-prime servk (ltk serv as)))))
    ((recv (cat cli serv-0 nonce life))
      (send
        (cat (enc time life nonce tgsk tgs (ltk cli as))
          (enc time life tgsk cli (ltk tgs as)))))
    ((send (cat cli serv-1 nonce-0 life-0))
      (recv
        (cat (enc time-0 nonce-0 life-0 tgsk-0 tgs-0 (ltk cli as-0))
          tgt)) (send (cat (enc cli time-0 tgsk-0) tgt))
      (recv (cat (enc serv-1 time life-prime-0 tgsk tgsk-0) st))
      (send (cat (enc cli time tgsk) st))))
  (label 24)
  (parent 23)
  (unrealized (2 3))
  (dead)
  (comment "empty cohort"))

(defskeleton kerberos
  (vars (time t-prime life life-prime nonce life-0 tgt nonce-0 text)
    (cli tgs as serv serv-0 tgs-0 as-0 serv-1 name) (tgsk servk skey))
  (defstrand tgs 2 (time time) (t-prime t-prime) (life life)
    (life-prime life-prime) (cli cli) (tgs tgs) (as as) (serv serv)
    (tgsk tgsk) (servk servk))
  (defstrand as 2 (time time) (life life) (nonce nonce) (cli cli)
    (tgs tgs) (as as) (serv serv-0) (tgsk tgsk))
  (defstrand client 3 (time time) (life life-0) (tgt tgt)
    (nonce nonce-0) (cli cli) (tgs tgs-0) (as as-0) (serv serv-1)
    (tgsk tgsk))
  (precedes ((1 1) (2 1)) ((2 2) (0 0)))
  (non-orig (ltk cli as) (ltk tgs as) (ltk serv as))
  (uniq-orig nonce-0 tgsk servk)
  (operation encryption-test (added-strand client 3) (enc cli time tgsk)
    (0 0))
  (traces
    ((recv
       (cat (enc cli time tgsk) (enc time life tgsk cli (ltk tgs as))))
      (send
        (cat (enc serv t-prime life-prime servk tgsk)
          (enc cli serv t-prime life-prime servk (ltk serv as)))))
    ((recv (cat cli serv-0 nonce life))
      (send
        (cat (enc time life nonce tgsk tgs (ltk cli as))
          (enc time life tgsk cli (ltk tgs as)))))
    ((send (cat cli serv-1 nonce-0 life-0))
      (recv
        (cat (enc time nonce-0 life-0 tgsk tgs-0 (ltk cli as-0)) tgt))
      (send (cat (enc cli time tgsk) tgt))))
  (label 25)
  (parent 23)
  (unrealized (2 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton kerberos
  (vars (time t-prime life life-prime nonce text)
    (cli tgs as serv serv-0 name) (tgsk servk skey))
  (defstrand tgs 2 (time time) (t-prime t-prime) (life life)
    (life-prime life-prime) (cli cli) (tgs tgs) (as as) (serv serv)
    (tgsk tgsk) (servk servk))
  (defstrand as 2 (time time) (life life) (nonce nonce) (cli cli)
    (tgs tgs) (as as) (serv serv-0) (tgsk tgsk))
  (deflistener tgsk)
  (precedes ((1 1) (2 0)) ((2 1) (0 0)))
  (non-orig (ltk cli as) (ltk tgs as) (ltk serv as))
  (uniq-orig tgsk servk)
  (operation encryption-test (added-listener tgsk) (enc cli time tgsk)
    (0 0))
  (traces
    ((recv
       (cat (enc cli time tgsk) (enc time life tgsk cli (ltk tgs as))))
      (send
        (cat (enc serv t-prime life-prime servk tgsk)
          (enc cli serv t-prime life-prime servk (ltk serv as)))))
    ((recv (cat cli serv-0 nonce life))
      (send
        (cat (enc time life nonce tgsk tgs (ltk cli as))
          (enc time life tgsk cli (ltk tgs as)))))
    ((recv tgsk) (send tgsk)))
  (label 26)
  (parent 23)
  (unrealized (2 0))
  (dead)
  (comment "empty cohort"))

(defskeleton kerberos
  (vars (time t-prime life life-prime nonce tgt text)
    (cli tgs as serv serv-0 serv-1 name) (tgsk servk skey))
  (defstrand tgs 2 (time time) (t-prime t-prime) (life life)
    (life-prime life-prime) (cli cli) (tgs tgs) (as as) (serv serv)
    (tgsk tgsk) (servk servk))
  (defstrand as 2 (time time) (life life) (nonce nonce) (cli cli)
    (tgs tgs) (as as) (serv serv-0) (tgsk tgsk))
  (defstrand client 3 (time time) (life nonce) (tgt tgt) (nonce life)
    (cli cli) (tgs tgs) (as as) (serv serv-1) (tgsk tgsk))
  (precedes ((1 1) (2 1)) ((2 0) (1 0)) ((2 2) (0 0)))
  (non-orig (ltk cli as) (ltk tgs as) (ltk serv as))
  (uniq-orig life tgsk servk)
  (operation nonce-test
    (contracted (tgs-0 tgs) (as-0 as) (life-0 nonce) (nonce-0 life))
    tgsk (2 1) (enc time life nonce tgsk tgs (ltk cli as))
    (enc time life tgsk cli (ltk tgs as)))
  (traces
    ((recv
       (cat (enc cli time tgsk) (enc time life tgsk cli (ltk tgs as))))
      (send
        (cat (enc serv t-prime life-prime servk tgsk)
          (enc cli serv t-prime life-prime servk (ltk serv as)))))
    ((recv (cat cli serv-0 nonce life))
      (send
        (cat (enc time life nonce tgsk tgs (ltk cli as))
          (enc time life tgsk cli (ltk tgs as)))))
    ((send (cat cli serv-1 life nonce))
      (recv (cat (enc time life nonce tgsk tgs (ltk cli as)) tgt))
      (send (cat (enc cli time tgsk) tgt))))
  (label 27)
  (parent 25)
  (unrealized)
  (shape)
  (maps
    ((0)
      ((cli cli) (tgs tgs) (as as) (serv serv) (time time)
        (t-prime t-prime) (life life) (life-prime life-prime)
        (tgsk tgsk) (servk servk))))
  (origs (life (2 0)) (tgsk (1 1)) (servk (0 1))))

(comment "Nothing left to do")

(defprotocol kerberos basic
  (defrole client
    (vars (cli tgs as serv name)
      (time t-prime life life-prime tgt st ack nonce text)
      (tgsk servk skey))
    (trace (send (cat cli serv nonce life))
      (recv (cat (enc time nonce life tgsk tgs (ltk cli as)) tgt))
      (send (cat (enc cli time tgsk) tgt))
      (recv (cat (enc serv t-prime life-prime servk tgsk) st))
      (send (cat (enc cli t-prime servk) st)) (recv (enc ack servk)))
    (uniq-orig nonce))
  (defrole tgs
    (vars (cli tgs as serv name) (time t-prime life life-prime text)
      (tgsk servk skey))
    (trace
      (recv
        (cat (enc cli time tgsk) (enc time life tgsk cli (ltk tgs as))))
      (send
        (cat (enc serv t-prime life-prime servk tgsk)
          (enc cli serv t-prime life-prime servk (ltk serv as)))))
    (uniq-orig servk))
  (defrole as
    (vars (cli tgs as serv name) (time life nonce text) (tgsk skey))
    (trace (recv (cat cli serv nonce life))
      (send
        (cat (enc time life nonce tgsk tgs (ltk cli as))
          (enc time life tgsk cli (ltk tgs as)))))
    (uniq-orig tgsk))
  (defrole serv
    (vars (cli as serv name) (t-prime life-prime ack text) (servk skey))
    (trace
      (recv
        (cat (enc cli t-prime servk)
          (enc cli serv t-prime life-prime servk (ltk serv as))))
      (send (enc ack servk)))))

(defskeleton kerberos
  (vars (time life nonce text) (cli tgs as serv name) (tgsk skey))
  (defstrand as 2 (time time) (life life) (nonce nonce) (cli cli)
    (tgs tgs) (as as) (serv serv) (tgsk tgsk))
  (non-orig (ltk cli as) (ltk tgs as) (ltk serv as))
  (uniq-orig tgsk)
  (traces
    ((recv (cat cli serv nonce life))
      (send
        (cat (enc time life nonce tgsk tgs (ltk cli as))
          (enc time life tgsk cli (ltk tgs as))))))
  (label 28)
  (unrealized)
  (shape)
  (maps
    ((0)
      ((cli cli) (tgs tgs) (as as) (serv serv) (time time) (life life)
        (nonce nonce) (tgsk tgsk))))
  (origs (tgsk (0 1))))

(comment "Nothing left to do")

(defprotocol kerberos basic
  (defrole client
    (vars (cli tgs as serv name)
      (time t-prime life life-prime tgt st ack nonce text)
      (tgsk servk skey))
    (trace (send (cat cli serv nonce life))
      (recv (cat (enc time nonce life tgsk tgs (ltk cli as)) tgt))
      (send (cat (enc cli time tgsk) tgt))
      (recv (cat (enc serv t-prime life-prime servk tgsk) st))
      (send (cat (enc cli t-prime servk) st)) (recv (enc ack servk)))
    (uniq-orig nonce))
  (defrole tgs
    (vars (cli tgs as serv name) (time t-prime life life-prime text)
      (tgsk servk skey))
    (trace
      (recv
        (cat (enc cli time tgsk) (enc time life tgsk cli (ltk tgs as))))
      (send
        (cat (enc serv t-prime life-prime servk tgsk)
          (enc cli serv t-prime life-prime servk (ltk serv as)))))
    (uniq-orig servk))
  (defrole as
    (vars (cli tgs as serv name) (time life nonce text) (tgsk skey))
    (trace (recv (cat cli serv nonce life))
      (send
        (cat (enc time life nonce tgsk tgs (ltk cli as))
          (enc time life tgsk cli (ltk tgs as)))))
    (uniq-orig tgsk))
  (defrole serv
    (vars (cli as serv name) (t-prime life-prime ack text) (servk skey))
    (trace
      (recv
        (cat (enc cli t-prime servk)
          (enc cli serv t-prime life-prime servk (ltk serv as))))
      (send (enc ack servk)))))

(defskeleton kerberos
  (vars (t-prime life-prime ack text) (cli serv as name) (servk skey))
  (defstrand serv 2 (t-prime t-prime) (life-prime life-prime) (ack ack)
    (cli cli) (as as) (serv serv) (servk servk))
  (non-orig (ltk cli as) (ltk serv as))
  (traces
    ((recv
       (cat (enc cli t-prime servk)
         (enc cli serv t-prime life-prime servk (ltk serv as))))
      (send (enc ack servk))))
  (label 29)
  (unrealized (0 0))
  (origs)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton kerberos
  (vars (t-prime life-prime ack time life text) (cli serv as tgs name)
    (servk tgsk skey))
  (defstrand serv 2 (t-prime t-prime) (life-prime life-prime) (ack ack)
    (cli cli) (as as) (serv serv) (servk servk))
  (defstrand tgs 2 (time time) (t-prime t-prime) (life life)
    (life-prime life-prime) (cli cli) (tgs tgs) (as as) (serv serv)
    (tgsk tgsk) (servk servk))
  (precedes ((1 1) (0 0)))
  (non-orig (ltk cli as) (ltk serv as))
  (uniq-orig servk)
  (operation encryption-test (added-strand tgs 2)
    (enc cli serv t-prime life-prime servk (ltk serv as)) (0 0))
  (traces
    ((recv
       (cat (enc cli t-prime servk)
         (enc cli serv t-prime life-prime servk (ltk serv as))))
      (send (enc ack servk)))
    ((recv
       (cat (enc cli time tgsk) (enc time life tgsk cli (ltk tgs as))))
      (send
        (cat (enc serv t-prime life-prime servk tgsk)
          (enc cli serv t-prime life-prime servk (ltk serv as))))))
  (label 30)
  (parent 29)
  (unrealized)
  (shape)
  (maps
    ((0)
      ((cli cli) (serv serv) (as as) (t-prime t-prime)
        (life-prime life-prime) (ack ack) (servk servk))))
  (origs (servk (1 1))))

(comment "Nothing left to do")
