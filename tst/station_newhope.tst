(herald "Station-to-station protocol" (algebra diffie-hellman))

(comment "CPSA 3.6.11")
(comment "All input read from tst/station_newhope.scm")

(defprotocol station-to-station diffie-hellman
  (defrole init
    (vars (xi rndx) (xr expt) (i r name) (hint mesg))
    (trace (send (exp (gen) xi))
      (recv
        (cat (exp (gen) xr) hint
          (enc (enc (exp (gen) xr) (exp (gen) xi) hint (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              hint))))
      (send
        (cat
          (enc (enc (exp (gen) xi) (exp (gen) xr) hint (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xr) hint))
          (enc (enc (exp (gen) xi) (exp (gen) xr) hint (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              hint))))))
  (defrole resp
    (vars (xr rndx) (xi expt) (i r name))
    (trace (recv (exp (gen) xi))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xi)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xi) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
            (hash "hint"
              (hash "share" (exp (gen) (mul xr xi)) xr))))))))

(defskeleton station-to-station
  (vars (hint mesg) (i r name) (xi rndx) (xr expt))
  (defstrand init 3 (hint hint) (i i) (r r) (xi xi) (xr xr))
  (non-orig (privk i) (privk r))
  (uniq-gen xi)
  (traces
    ((send (exp (gen) xi))
      (recv
        (cat (exp (gen) xr) hint
          (enc (enc (exp (gen) xr) (exp (gen) xi) hint (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              hint))))
      (send
        (cat
          (enc (enc (exp (gen) xi) (exp (gen) xr) hint (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xr) hint))
          (enc (enc (exp (gen) xi) (exp (gen) xr) hint (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              hint))))))
  (label 0)
  (unrealized (0 1))
  (origs)
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton station-to-station
  (vars (hint mesg) (i r r-0 name) (xi xi-0 rndx))
  (defstrand init 3 (hint hint) (i i) (r r) (xi xi-0) (xr xi))
  (defstrand init 3 (hint hint) (i r) (r r-0) (xi xi) (xr xi-0))
  (precedes ((0 0) (1 1)) ((1 2) (0 1)))
  (non-orig (privk i) (privk r))
  (uniq-gen xi-0)
  (operation encryption-test (added-strand init 3)
    (enc (enc (exp (gen) xi) (exp (gen) xi-0) hint (privk r))
      (hash "key" (hash "share" (exp (gen) (mul xi xi-0)) xi-0) hint))
    (0 1))
  (traces
    ((send (exp (gen) xi-0))
      (recv
        (cat (exp (gen) xi) hint
          (enc (enc (exp (gen) xi) (exp (gen) xi-0) hint (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xi xi-0)) xi-0)
              hint))))
      (send
        (cat
          (enc (enc (exp (gen) xi-0) (exp (gen) xi) hint (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xi-0)) xi)
              hint))
          (enc (enc (exp (gen) xi-0) (exp (gen) xi) hint (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xi-0)) xi-0)
              hint)))))
    ((send (exp (gen) xi))
      (recv
        (cat (exp (gen) xi-0) hint
          (enc (enc (exp (gen) xi-0) (exp (gen) xi) hint (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xi xi-0)) xi)
              hint))))
      (send
        (cat
          (enc (enc (exp (gen) xi) (exp (gen) xi-0) hint (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xi xi-0)) xi-0)
              hint))
          (enc (enc (exp (gen) xi) (exp (gen) xi-0) hint (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xi xi-0)) xi)
              hint))))))
  (label 1)
  (parent 0)
  (unrealized)
  (shape)
  (maps ((0) ((i i) (r r) (xi xi-0) (xr xi) (hint hint))))
  (origs))

(defskeleton station-to-station
  (vars (i r name) (xr xi rndx))
  (defstrand init 3
    (hint (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))) (i i)
    (r r) (xi xi) (xr xr))
  (defstrand resp 2 (r r) (xr xr) (xi xi))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (non-orig (privk i) (privk r))
  (uniq-gen xi)
  (operation encryption-test (added-strand resp 2)
    (enc
      (enc (exp (gen) xr) (exp (gen) xi)
        (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
        (privk r))
      (hash "key" (hash "share" (exp (gen) (mul xr xi)) xi)
        (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr)))) (0 1))
  (traces
    ((send (exp (gen) xi))
      (recv
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xi)) xr))))))
      (send
        (cat
          (enc
            (enc (exp (gen) xi) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
          (enc
            (enc (exp (gen) xi) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xi)) xr)))))))
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xi)) xr))))))))
  (label 2)
  (parent 0)
  (unrealized)
  (shape)
  (maps
    ((0)
      ((i i) (r r) (xi xi) (xr xr)
        (hint
          (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))))
  (origs))

(defskeleton station-to-station
  (vars (hint mesg) (i r name) (xi rndx) (xr expt))
  (defstrand init 3 (hint hint) (i i) (r r) (xi xi) (xr xr))
  (deflistener
    (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi) hint))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (non-orig (privk i) (privk r))
  (uniq-gen xi)
  (operation encryption-test
    (added-listener
      (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi) hint))
    (enc (enc (exp (gen) xr) (exp (gen) xi) hint (privk r))
      (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi) hint))
    (0 1))
  (traces
    ((send (exp (gen) xi))
      (recv
        (cat (exp (gen) xr) hint
          (enc (enc (exp (gen) xr) (exp (gen) xi) hint (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              hint))))
      (send
        (cat
          (enc (enc (exp (gen) xi) (exp (gen) xr) hint (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xr) hint))
          (enc (enc (exp (gen) xi) (exp (gen) xr) hint (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              hint)))))
    ((recv (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi) hint))
      (send
        (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi) hint))))
  (label 3)
  (parent 0)
  (unrealized (0 1) (1 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton station-to-station
  (vars (hint mesg) (i r name) (xi rndx) (xr expt))
  (defstrand init 3 (hint hint) (i i) (r r) (xi xi) (xr xr))
  (deflistener
    (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi) hint))
  (deflistener
    (cat "key" (hash "share" (exp (gen) (mul xi xr)) xi) hint))
  (precedes ((0 0) (2 0)) ((1 1) (0 1)) ((2 1) (1 0)))
  (non-orig (privk i) (privk r))
  (uniq-gen xi)
  (operation encryption-test
    (added-listener
      (cat "key" (hash "share" (exp (gen) (mul xi xr)) xi) hint))
    (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi) hint) (1 0))
  (traces
    ((send (exp (gen) xi))
      (recv
        (cat (exp (gen) xr) hint
          (enc (enc (exp (gen) xr) (exp (gen) xi) hint (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              hint))))
      (send
        (cat
          (enc (enc (exp (gen) xi) (exp (gen) xr) hint (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xr) hint))
          (enc (enc (exp (gen) xi) (exp (gen) xr) hint (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              hint)))))
    ((recv (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi) hint))
      (send
        (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi) hint)))
    ((recv (cat "key" (hash "share" (exp (gen) (mul xi xr)) xi) hint))
      (send
        (cat "key" (hash "share" (exp (gen) (mul xi xr)) xi) hint))))
  (label 4)
  (parent 3)
  (unrealized (0 1) (2 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton station-to-station
  (vars (hint mesg) (i r name) (xi rndx) (xr expt))
  (defstrand init 3 (hint hint) (i i) (r r) (xi xi) (xr xr))
  (deflistener
    (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi) hint))
  (deflistener
    (cat "key" (hash "share" (exp (gen) (mul xi xr)) xi) hint))
  (deflistener (cat "share" (exp (gen) (mul xi xr)) xi))
  (precedes ((0 0) (3 0)) ((1 1) (0 1)) ((2 1) (1 0)) ((3 1) (2 0)))
  (non-orig (privk i) (privk r))
  (uniq-gen xi)
  (operation encryption-test
    (added-listener (cat "share" (exp (gen) (mul xi xr)) xi))
    (hash "share" (exp (gen) (mul xi xr)) xi) (2 0))
  (traces
    ((send (exp (gen) xi))
      (recv
        (cat (exp (gen) xr) hint
          (enc (enc (exp (gen) xr) (exp (gen) xi) hint (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              hint))))
      (send
        (cat
          (enc (enc (exp (gen) xi) (exp (gen) xr) hint (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xr) hint))
          (enc (enc (exp (gen) xi) (exp (gen) xr) hint (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              hint)))))
    ((recv (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi) hint))
      (send
        (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi) hint)))
    ((recv (cat "key" (hash "share" (exp (gen) (mul xi xr)) xi) hint))
      (send (cat "key" (hash "share" (exp (gen) (mul xi xr)) xi) hint)))
    ((recv (cat "share" (exp (gen) (mul xi xr)) xi))
      (send (cat "share" (exp (gen) (mul xi xr)) xi))))
  (label 5)
  (parent 4)
  (unrealized (0 1) (3 0))
  (dead)
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol station-to-station diffie-hellman
  (defrole init
    (vars (xi rndx) (xr expt) (i r name) (hint mesg))
    (trace (send (exp (gen) xi))
      (recv
        (cat (exp (gen) xr) hint
          (enc (enc (exp (gen) xr) (exp (gen) xi) hint (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              hint))))
      (send
        (cat
          (enc (enc (exp (gen) xi) (exp (gen) xr) hint (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xr) hint))
          (enc (enc (exp (gen) xi) (exp (gen) xr) hint (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              hint))))))
  (defrole resp
    (vars (xr rndx) (xi expt) (i r name))
    (trace (recv (exp (gen) xi))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xi)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xi) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
            (hash "hint"
              (hash "share" (exp (gen) (mul xr xi)) xr))))))))

(defskeleton station-to-station
  (vars (i r name) (xr rndx) (xi expt))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xi))
  (absent (xr xi))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (traces
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xi)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xi) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr)))))))
  (label 6)
  (unrealized (0 2))
  (origs)
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton station-to-station
  (vars (i r r-0 name) (xi xr rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xi))
  (defstrand init 3
    (hint (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))) (i i)
    (r r-0) (xi xi) (xr xr))
  (precedes ((0 1) (1 1)) ((1 2) (0 2)))
  (absent (xr xi))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (operation encryption-test (added-strand init 3)
    (enc
      (enc (exp (gen) xi) (exp (gen) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
        (privk i))
      (hash "key" (hash "share" (exp (gen) (mul xi xr)) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr)))) (0 2))
  (traces
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xi) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xi xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))))))
    ((send (exp (gen) xi))
      (recv
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr)) xr))))))
      (send
        (cat
          (enc
            (enc (exp (gen) xi) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))))
          (enc
            (enc (exp (gen) xi) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr)) xr))))))))
  (label 7)
  (parent 6)
  (unrealized)
  (shape)
  (maps ((0) ((i i) (r r) (xr xr) (xi xi))))
  (origs))

(defskeleton station-to-station
  (vars (i r name) (xr rndx) (xi expt))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xi))
  (deflistener
    (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
      (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
  (precedes ((0 1) (1 0)) ((1 1) (0 2)))
  (absent (xr xi))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (operation encryption-test
    (added-listener
      (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
    (enc
      (enc (exp (gen) xi) (exp (gen) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
        (privk i))
      (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr)))) (0 2))
  (traces
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xi)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xi) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))))
    ((recv
       (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
         (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
      (send
        (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))))
  (label 8)
  (parent 6)
  (unrealized (0 2) (1 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton station-to-station
  (vars (i r name) (xr rndx) (xi expt))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xi))
  (deflistener
    (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
      (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
  (deflistener
    (cat "key" (hash "share" (exp (gen) (mul xr xi)) xr)
      (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
  (precedes ((0 1) (2 0)) ((1 1) (0 2)) ((2 1) (1 0)))
  (absent (xr xi))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (operation encryption-test
    (added-listener
      (cat "key" (hash "share" (exp (gen) (mul xr xi)) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
    (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
      (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))) (1 0))
  (traces
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xi)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xi) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))))
    ((recv
       (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
         (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
      (send
        (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr)))))
    ((recv
       (cat "key" (hash "share" (exp (gen) (mul xr xi)) xr)
         (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
      (send
        (cat "key" (hash "share" (exp (gen) (mul xr xi)) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))))
  (label 9)
  (parent 8)
  (unrealized (0 2) (2 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton station-to-station
  (vars (i r name) (xr rndx) (xi expt))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xi))
  (deflistener
    (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
      (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
  (deflistener
    (cat "key" (hash "share" (exp (gen) (mul xr xi)) xr)
      (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
  (deflistener (cat "share" (exp (gen) (mul xr xi)) xr))
  (precedes ((0 1) (3 0)) ((1 1) (0 2)) ((2 1) (1 0)) ((3 1) (2 0)))
  (absent (xr xi))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (operation encryption-test
    (added-listener (cat "share" (exp (gen) (mul xr xi)) xr))
    (hash "share" (exp (gen) (mul xr xi)) xr) (2 0))
  (traces
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xi)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xi) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))))
    ((recv
       (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
         (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
      (send
        (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr)))))
    ((recv
       (cat "key" (hash "share" (exp (gen) (mul xr xi)) xr)
         (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
      (send
        (cat "key" (hash "share" (exp (gen) (mul xr xi)) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr)))))
    ((recv (cat "share" (exp (gen) (mul xr xi)) xr))
      (send (cat "share" (exp (gen) (mul xr xi)) xr))))
  (label 10)
  (parent 9)
  (unrealized (0 2) (3 0))
  (dead)
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol station-to-station diffie-hellman
  (defrole init
    (vars (xi rndx) (xr expt) (i r name) (hint mesg))
    (trace (send (exp (gen) xi))
      (recv
        (cat (exp (gen) xr) hint
          (enc (enc (exp (gen) xr) (exp (gen) xi) hint (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              hint))))
      (send
        (cat
          (enc (enc (exp (gen) xi) (exp (gen) xr) hint (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xr) hint))
          (enc (enc (exp (gen) xi) (exp (gen) xr) hint (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              hint))))))
  (defrole resp
    (vars (xr rndx) (xi expt) (i r name))
    (trace (recv (exp (gen) xi))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xi)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xi) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
            (hash "hint"
              (hash "share" (exp (gen) (mul xr xi)) xr))))))))

(defskeleton station-to-station
  (vars (i r name) (xr rndx) (xi expt))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xi))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (traces
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xi)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xi) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr)))))))
  (label 11)
  (unrealized (0 2))
  (origs)
  (comment "5 in cohort - 5 not yet seen"))

(defskeleton station-to-station
  (vars (i r r-0 name) (xi xr rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xi))
  (defstrand init 3
    (hint (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))) (i i)
    (r r-0) (xi xi) (xr xr))
  (precedes ((0 1) (1 1)) ((1 2) (0 2)))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (operation encryption-test (added-strand init 3)
    (enc
      (enc (exp (gen) xi) (exp (gen) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
        (privk i))
      (hash "key" (hash "share" (exp (gen) (mul xi xr)) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr)))) (0 2))
  (traces
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xi) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xi xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))))))
    ((send (exp (gen) xi))
      (recv
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr)) xr))))))
      (send
        (cat
          (enc
            (enc (exp (gen) xi) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))))
          (enc
            (enc (exp (gen) xi) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr)) xr))))))))
  (label 12)
  (parent 11)
  (unrealized)
  (shape)
  (maps ((0) ((i i) (r r) (xr xr) (xi xi))))
  (origs))

(defskeleton station-to-station
  (vars (i r r-0 name) (xr rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand init 3
    (hint (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))) (i i)
    (r r-0) (xi xr) (xr xr))
  (precedes ((1 0) (0 0)) ((1 2) (0 2)))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (operation encryption-test (added-strand init 3)
    (enc
      (enc (exp (gen) xr) (exp (gen) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
        (privk i))
      (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr)))) (0 2))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((send (exp (gen) xr))
      (recv
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (send
        (cat
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))))
  (label 13)
  (parent 11)
  (unrealized (1 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton station-to-station
  (vars (r name) (xr rndx))
  (defstrand resp 3 (i r) (r r) (xr xr) (xi xr))
  (non-orig (privk r))
  (uniq-gen xr)
  (operation encryption-test (displaced 1 0 resp 2)
    (enc
      (enc (exp (gen) xr) (exp (gen) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
        (privk i))
      (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr)))) (0 2))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk r))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr)))))))
  (label 14)
  (parent 11)
  (unrealized (0 0))
  (origs)
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton station-to-station
  (vars (i r name) (xr rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand resp 2 (r i) (xr xr) (xi xr))
  (precedes ((1 1) (0 2)))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (operation encryption-test (added-strand resp 2)
    (enc
      (enc (exp (gen) xr) (exp (gen) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
        (privk i))
      (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr)))) (0 2))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))))
  (label 15)
  (parent 11)
  (unrealized (0 0) (1 0))
  (comment "4 in cohort - 4 not yet seen"))

(defskeleton station-to-station
  (vars (i r name) (xr rndx) (xi expt))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xi))
  (deflistener
    (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
      (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
  (precedes ((0 1) (1 0)) ((1 1) (0 2)))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (operation encryption-test
    (added-listener
      (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
    (enc
      (enc (exp (gen) xi) (exp (gen) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
        (privk i))
      (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr)))) (0 2))
  (traces
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xi)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xi) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))))
    ((recv
       (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
         (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
      (send
        (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))))
  (label 16)
  (parent 11)
  (unrealized (0 2) (1 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton station-to-station
  (vars (i r r-0 name) (xr rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand init 3
    (hint (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))) (i i)
    (r r-0) (xi xr) (xr xr))
  (precedes ((0 1) (1 1)) ((1 0) (0 0)) ((1 2) (0 2)))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (operation encryption-test (displaced 2 0 resp 2)
    (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr)) (1 1))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((send (exp (gen) xr))
      (recv
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (send
        (cat
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))))
  (label 17)
  (parent 13)
  (unrealized (1 1))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton station-to-station
  (vars (i r r-0 r-1 name) (xr rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand init 3
    (hint (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))) (i i)
    (r r-0) (xi xr) (xr xr))
  (defstrand resp 2 (r r-1) (xr xr) (xi xr))
  (precedes ((1 0) (0 0)) ((1 0) (2 0)) ((1 2) (0 2)) ((2 1) (1 1)))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (operation encryption-test (added-strand resp 2)
    (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr)) (1 1))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((send (exp (gen) xr))
      (recv
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (send
        (cat
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r-1))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r-1))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))))
  (label 18)
  (parent 13)
  (unrealized (1 1))
  (comment "4 in cohort - 4 not yet seen"))

(defskeleton station-to-station
  (vars (i r r-0 name) (xr rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand init 3
    (hint (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))) (i i)
    (r r-0) (xi xr) (xr xr))
  (deflistener (cat "hint" (hash "share" (exp (gen) (mul xr xr)) xr)))
  (precedes ((1 0) (0 0)) ((1 0) (2 0)) ((1 2) (0 2)) ((2 1) (1 1)))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (operation encryption-test
    (added-listener
      (cat "hint" (hash "share" (exp (gen) (mul xr xr)) xr)))
    (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr)) (1 1))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((send (exp (gen) xr))
      (recv
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (send
        (cat
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv (cat "hint" (hash "share" (exp (gen) (mul xr xr)) xr)))
      (send (cat "hint" (hash "share" (exp (gen) (mul xr xr)) xr)))))
  (label 19)
  (parent 13)
  (unrealized (2 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton station-to-station
  (vars (r name) (xi rndx))
  (defstrand resp 3 (i r) (r r) (xr xi) (xi xi))
  (defstrand init 1 (xi xi))
  (precedes ((1 0) (0 0)))
  (non-orig (privk r))
  (uniq-gen xi)
  (operation nonce-test (added-strand init 1) (exp (gen) xi) (0 0))
  (traces
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xi)
          (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
          (enc
            (enc (exp (gen) xi) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xi xi)) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))))
          (enc
            (enc (exp (gen) xi) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xi xi)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xi)) xi))))))
      (recv
        (enc
          (enc (exp (gen) xi) (exp (gen) xi)
            (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
            (privk r))
          (hash "key" (hash "share" (exp (gen) (mul xi xi)) xi)
            (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))))))
    ((send (exp (gen) xi))))
  (label 20)
  (parent 14)
  (unrealized)
  (shape)
  (maps ((0) ((i r) (r r) (xr xi) (xi xi))))
  (origs))

(defskeleton station-to-station
  (vars (r r-0 name) (xi expt) (xr rndx))
  (defstrand resp 3 (i r) (r r) (xr xr) (xi xr))
  (defstrand resp 2 (r r-0) (xr xr) (xi xi))
  (precedes ((1 1) (0 0)))
  (non-orig (privk r))
  (uniq-gen xr)
  (operation nonce-test (added-strand resp 2) (exp (gen) xr) (0 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk r))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr)) xr))))))))
  (label 21)
  (parent 14)
  (unrealized)
  (shape)
  (maps ((0) ((i r) (r r) (xr xr) (xi xr))))
  (origs))

(defskeleton station-to-station
  (vars (r name) (xr rndx) (w expt))
  (defstrand resp 3 (i r) (r r) (xr xr) (xi xr))
  (deflistener (cat (exp (gen) (mul xr (rec w))) w))
  (precedes ((1 1) (0 0)))
  (non-orig (privk r))
  (uniq-gen xr)
  (precur (1 0))
  (operation nonce-test
    (added-listener (cat (exp (gen) (mul xr (rec w))) w)) (exp (gen) xr)
    (0 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk r))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((recv (cat (exp (gen) (mul xr (rec w))) w))
      (send (cat (exp (gen) (mul xr (rec w))) w))))
  (label 22)
  (parent 14)
  (unrealized (1 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton station-to-station
  (vars (i r name) (xi rndx))
  (defstrand resp 3 (i i) (r r) (xr xi) (xi xi))
  (defstrand resp 2 (r i) (xr xi) (xi xi))
  (defstrand init 1 (xi xi))
  (precedes ((1 1) (0 2)) ((2 0) (0 0)) ((2 0) (1 0)))
  (non-orig (privk i) (privk r))
  (uniq-gen xi)
  (operation nonce-test (added-strand init 1) (exp (gen) xi) (1 0))
  (traces
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xi)
          (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
          (enc
            (enc (exp (gen) xi) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xi xi)) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))))
          (enc
            (enc (exp (gen) xi) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xi xi)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xi)) xi))))))
      (recv
        (enc
          (enc (exp (gen) xi) (exp (gen) xi)
            (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xi xi)) xi)
            (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))))))
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xi)
          (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
          (enc
            (enc (exp (gen) xi) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xi)) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))))
          (enc
            (enc (exp (gen) xi) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xi)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xi)) xi)))))))
    ((send (exp (gen) xi))))
  (label 23)
  (parent 15)
  (unrealized)
  (shape)
  (maps ((0) ((i i) (r r) (xr xi) (xi xi))))
  (origs))

(defskeleton station-to-station
  (vars (i r name) (xr rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand resp 2 (r i) (xr xr) (xi xr))
  (precedes ((0 1) (1 0)) ((1 1) (0 2)))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (operation nonce-test (displaced 2 0 resp 2) (exp (gen) xr-0) (1 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))))
  (label 24)
  (parent 15)
  (unrealized (0 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton station-to-station
  (vars (i r r-0 name) (xi expt) (xr rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand resp 2 (r i) (xr xr) (xi xr))
  (defstrand resp 2 (r r-0) (xr xr) (xi xi))
  (precedes ((1 1) (0 2)) ((2 1) (0 0)) ((2 1) (1 0)))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (operation nonce-test (added-strand resp 2) (exp (gen) xr) (1 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr)) xr))))))))
  (label 25)
  (parent 15)
  (unrealized)
  (shape)
  (maps ((0) ((i i) (r r) (xr xr) (xi xr))))
  (origs))

(defskeleton station-to-station
  (vars (i r name) (xr rndx) (w expt))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand resp 2 (r i) (xr xr) (xi xr))
  (deflistener (cat (exp (gen) (mul xr (rec w))) w))
  (precedes ((1 1) (0 2)) ((2 1) (1 0)))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (precur (2 0))
  (operation nonce-test
    (added-listener (cat (exp (gen) (mul xr (rec w))) w)) (exp (gen) xr)
    (1 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv (cat (exp (gen) (mul xr (rec w))) w))
      (send (cat (exp (gen) (mul xr (rec w))) w))))
  (label 26)
  (parent 15)
  (unrealized (0 0) (2 0))
  (comment "4 in cohort - 4 not yet seen"))

(defskeleton station-to-station
  (vars (i r name) (xr rndx) (xi expt))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xi))
  (deflistener
    (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
      (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
  (deflistener
    (cat "key" (hash "share" (exp (gen) (mul xr xi)) xr)
      (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
  (precedes ((0 1) (2 0)) ((1 1) (0 2)) ((2 1) (1 0)))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (operation encryption-test
    (added-listener
      (cat "key" (hash "share" (exp (gen) (mul xr xi)) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
    (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
      (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))) (1 0))
  (traces
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xi)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xi) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))))
    ((recv
       (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
         (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
      (send
        (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr)))))
    ((recv
       (cat "key" (hash "share" (exp (gen) (mul xr xi)) xr)
         (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
      (send
        (cat "key" (hash "share" (exp (gen) (mul xr xi)) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))))
  (label 27)
  (parent 16)
  (unrealized (0 2) (2 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton station-to-station
  (vars (i r name) (xr rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand init 3
    (hint (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))) (i i)
    (r r) (xi xr) (xr xr))
  (precedes ((0 1) (1 1)) ((1 0) (0 0)) ((1 2) (0 2)))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (operation encryption-test (displaced 2 0 resp 2)
    (enc
      (enc (exp (gen) xr) (exp (gen) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
        (privk r-0))
      (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr)))) (1 1))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((send (exp (gen) xr))
      (recv
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (send
        (cat
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))))
  (label 28)
  (parent 17)
  (unrealized)
  (shape)
  (maps ((0) ((i i) (r r) (xr xr) (xi xr))))
  (origs))

(defskeleton station-to-station
  (vars (i r r-0 name) (xr rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand init 3
    (hint (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))) (i i)
    (r r-0) (xi xr) (xr xr))
  (defstrand resp 2 (r r-0) (xr xr) (xi xr))
  (precedes ((0 1) (1 1)) ((1 0) (0 0)) ((1 0) (2 0)) ((1 2) (0 2))
    ((2 1) (1 1)))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (operation encryption-test (added-strand resp 2)
    (enc
      (enc (exp (gen) xr) (exp (gen) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
        (privk r-0))
      (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr)))) (1 1))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((send (exp (gen) xr))
      (recv
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (send
        (cat
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))))
  (label 29)
  (parent 17)
  (seen 32)
  (unrealized)
  (comment "1 in cohort - 0 not yet seen"))

(defskeleton station-to-station
  (vars (i r r-0 name) (xr rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand init 3
    (hint (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))) (i i)
    (r r-0) (xi xr) (xr xr))
  (deflistener
    (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
      (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
  (precedes ((0 1) (1 1)) ((1 0) (0 0)) ((1 0) (2 0)) ((1 2) (0 2))
    ((2 1) (1 1)))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (operation encryption-test
    (added-listener
      (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
    (enc
      (enc (exp (gen) xr) (exp (gen) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
        (privk r-0))
      (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr)))) (1 1))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((send (exp (gen) xr))
      (recv
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (send
        (cat
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv
       (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
         (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
      (send
        (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
  (label 30)
  (parent 17)
  (unrealized (2 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton station-to-station
  (vars (i r r-0 name) (xr rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand init 3
    (hint (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))) (i i)
    (r r) (xi xr) (xr xr))
  (defstrand resp 2 (r r-0) (xr xr) (xi xr))
  (precedes ((0 1) (1 1)) ((1 0) (0 0)) ((1 0) (2 0)) ((1 2) (0 2))
    ((2 1) (1 1)))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (operation encryption-test (displaced 3 0 resp 2)
    (enc
      (enc (exp (gen) xr) (exp (gen) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
        (privk r-1))
      (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr)))) (1 1))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((send (exp (gen) xr))
      (recv
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (send
        (cat
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))))
  (label 31)
  (parent 18)
  (seen 28)
  (unrealized)
  (comment "1 in cohort - 0 not yet seen"))

(defskeleton station-to-station
  (vars (i r r-0 name) (xr rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand init 3
    (hint (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))) (i i)
    (r r-0) (xi xr) (xr xr))
  (defstrand resp 2 (r r-0) (xr xr) (xi xr))
  (precedes ((1 0) (0 0)) ((1 0) (2 0)) ((1 2) (0 2)) ((2 1) (1 1)))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (operation encryption-test (displaced 3 2 resp 2)
    (enc
      (enc (exp (gen) xr) (exp (gen) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
        (privk r-1))
      (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr)))) (1 1))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((send (exp (gen) xr))
      (recv
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (send
        (cat
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))))
  (label 32)
  (parent 18)
  (unrealized)
  (shape)
  (maps ((0) ((i i) (r r) (xr xr) (xi xr))))
  (origs))

(defskeleton station-to-station
  (vars (i r r-0 r-1 name) (xr rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand init 3
    (hint (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))) (i i)
    (r r-0) (xi xr) (xr xr))
  (defstrand resp 2 (r r-1) (xr xr) (xi xr))
  (defstrand resp 2 (r r-0) (xr xr) (xi xr))
  (precedes ((1 0) (0 0)) ((1 0) (2 0)) ((1 0) (3 0)) ((1 2) (0 2))
    ((2 1) (1 1)) ((3 1) (1 1)))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (operation encryption-test (added-strand resp 2)
    (enc
      (enc (exp (gen) xr) (exp (gen) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
        (privk r-0))
      (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr)))) (1 1))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((send (exp (gen) xr))
      (recv
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (send
        (cat
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r-1))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r-1))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))))
  (label 33)
  (parent 18)
  (seen 32)
  (unrealized)
  (comment "1 in cohort - 0 not yet seen"))

(defskeleton station-to-station
  (vars (i r r-0 r-1 name) (xr rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand init 3
    (hint (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))) (i i)
    (r r-0) (xi xr) (xr xr))
  (defstrand resp 2 (r r-1) (xr xr) (xi xr))
  (deflistener
    (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
      (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
  (precedes ((1 0) (0 0)) ((1 0) (2 0)) ((1 0) (3 0)) ((1 2) (0 2))
    ((2 1) (1 1)) ((3 1) (1 1)))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (operation encryption-test
    (added-listener
      (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
    (enc
      (enc (exp (gen) xr) (exp (gen) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
        (privk r-0))
      (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr)))) (1 1))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((send (exp (gen) xr))
      (recv
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (send
        (cat
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r-1))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r-1))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv
       (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
         (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
      (send
        (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
  (label 34)
  (parent 18)
  (unrealized (3 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton station-to-station
  (vars (i r r-0 name) (xr rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand init 3
    (hint (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))) (i i)
    (r r-0) (xi xr) (xr xr))
  (deflistener (cat "hint" (hash "share" (exp (gen) (mul xr xr)) xr)))
  (deflistener (cat "share" (exp (gen) (mul xr xr)) xr))
  (precedes ((1 0) (0 0)) ((1 0) (3 0)) ((1 2) (0 2)) ((2 1) (1 1))
    ((3 1) (2 0)))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (operation encryption-test
    (added-listener (cat "share" (exp (gen) (mul xr xr)) xr))
    (hash "share" (exp (gen) (mul xr xr)) xr) (2 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((send (exp (gen) xr))
      (recv
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (send
        (cat
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv (cat "hint" (hash "share" (exp (gen) (mul xr xr)) xr)))
      (send (cat "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
    ((recv (cat "share" (exp (gen) (mul xr xr)) xr))
      (send (cat "share" (exp (gen) (mul xr xr)) xr))))
  (label 35)
  (parent 19)
  (unrealized (3 0))
  (dead)
  (comment "empty cohort"))

(defskeleton station-to-station
  (vars (r name) (xr rndx))
  (defstrand resp 3 (i r) (r r) (xr xr) (xi xr))
  (deflistener (cat (gen) xr))
  (precedes ((1 1) (0 0)))
  (non-orig (privk r))
  (uniq-gen xr)
  (precur (1 0))
  (operation nonce-test (contracted (xr-0 xr) (w xr)) (gen) (1 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk r))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((recv (cat (gen) xr)) (send (cat (gen) xr))))
  (label 36)
  (parent 22)
  (unrealized (1 0))
  (dead)
  (comment "empty cohort"))

(defskeleton station-to-station
  (vars (r name) (xr xi rndx))
  (defstrand resp 3 (i r) (r r) (xr xr) (xi xr))
  (deflistener (cat (exp (gen) xi) (mul xr (rec xi))))
  (defstrand init 1 (xi xi))
  (precedes ((1 1) (0 0)) ((2 0) (1 0)))
  (non-orig (privk r))
  (precur (1 0))
  (uniq-gen xr)
  (operation nonce-test (added-strand init 1) (exp (gen) xi) (1 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk r))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((recv (cat (exp (gen) xi) (mul xr (rec xi))))
      (send (cat (exp (gen) xi) (mul xr (rec xi)))))
    ((send (exp (gen) xi))))
  (label 37)
  (parent 22)
  (unrealized (0 0) (1 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton station-to-station
  (vars (r r-0 name) (xi expt) (xr xr-0 rndx))
  (defstrand resp 3 (i r) (r r) (xr xr) (xi xr))
  (deflistener (cat (exp (gen) xr-0) (mul xr (rec xr-0))))
  (defstrand resp 2 (r r-0) (xr xr-0) (xi xi))
  (precedes ((1 1) (0 0)) ((2 1) (1 0)))
  (non-orig (privk r))
  (precur (1 0))
  (uniq-gen xr)
  (operation nonce-test (added-strand resp 2) (exp (gen) xr-0) (1 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk r))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((recv (cat (exp (gen) xr-0) (mul xr (rec xr-0))))
      (send (cat (exp (gen) xr-0) (mul xr (rec xr-0)))))
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xr-0)
          (hash "hint" (hash "share" (exp (gen) (mul xi xr-0)) xr-0))
          (enc
            (enc (exp (gen) xr-0) (exp (gen) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr-0)) xr-0))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xi xr-0)) xr-0)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr-0)) xr-0))))
          (enc
            (enc (exp (gen) xr-0) (exp (gen) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr-0)) xr-0))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xi xr-0)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr-0)) xr-0))))))))
  (label 38)
  (parent 22)
  (unrealized (0 0) (1 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton station-to-station
  (vars (i r name) (xi rndx))
  (defstrand resp 3 (i i) (r r) (xr xi) (xi xi))
  (defstrand resp 2 (r i) (xr xi) (xi xi))
  (defstrand init 1 (xi xi))
  (precedes ((0 1) (1 0)) ((1 1) (0 2)) ((2 0) (0 0)))
  (non-orig (privk i) (privk r))
  (uniq-gen xi)
  (operation nonce-test (added-strand init 1) (exp (gen) xi) (0 0))
  (traces
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xi)
          (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
          (enc
            (enc (exp (gen) xi) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xi xi)) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))))
          (enc
            (enc (exp (gen) xi) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xi xi)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xi)) xi))))))
      (recv
        (enc
          (enc (exp (gen) xi) (exp (gen) xi)
            (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xi xi)) xi)
            (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))))))
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xi)
          (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
          (enc
            (enc (exp (gen) xi) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xi)) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))))
          (enc
            (enc (exp (gen) xi) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xi)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xi)) xi)))))))
    ((send (exp (gen) xi))))
  (label 39)
  (parent 24)
  (seen 23)
  (unrealized)
  (comment "1 in cohort - 0 not yet seen"))

(defskeleton station-to-station
  (vars (i r r-0 name) (xi expt) (xr rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand resp 2 (r i) (xr xr) (xi xr))
  (defstrand resp 2 (r r-0) (xr xr) (xi xi))
  (precedes ((0 1) (1 0)) ((1 1) (0 2)) ((2 1) (0 0)))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (operation nonce-test (added-strand resp 2) (exp (gen) xr) (0 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr)) xr))))))))
  (label 40)
  (parent 24)
  (seen 25)
  (unrealized)
  (comment "1 in cohort - 0 not yet seen"))

(defskeleton station-to-station
  (vars (i r name) (xr rndx) (w expt))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand resp 2 (r i) (xr xr) (xi xr))
  (deflistener (cat (exp (gen) (mul xr (rec w))) w))
  (precedes ((0 1) (1 0)) ((1 1) (0 2)) ((2 1) (0 0)))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (precur (2 0))
  (operation nonce-test
    (added-listener (cat (exp (gen) (mul xr (rec w))) w)) (exp (gen) xr)
    (0 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv (cat (exp (gen) (mul xr (rec w))) w))
      (send (cat (exp (gen) (mul xr (rec w))) w))))
  (label 41)
  (parent 24)
  (unrealized (2 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton station-to-station
  (vars (i r name) (xr rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand resp 2 (r i) (xr xr) (xi xr))
  (deflistener (cat (gen) xr))
  (precedes ((1 1) (0 2)) ((2 1) (1 0)))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (precur (2 0))
  (operation nonce-test (contracted (xr-0 xr) (w xr)) (gen) (2 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv (cat (gen) xr)) (send (cat (gen) xr))))
  (label 42)
  (parent 26)
  (unrealized (0 0) (2 0))
  (dead)
  (comment "empty cohort"))

(defskeleton station-to-station
  (vars (i r name) (xr xi rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand resp 2 (r i) (xr xr) (xi xr))
  (deflistener (cat (exp (gen) xi) (mul xr (rec xi))))
  (defstrand init 1 (xi xi))
  (precedes ((1 1) (0 2)) ((2 1) (1 0)) ((3 0) (2 0)))
  (non-orig (privk i) (privk r))
  (precur (2 0))
  (uniq-gen xr)
  (operation nonce-test (added-strand init 1) (exp (gen) xi) (2 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv (cat (exp (gen) xi) (mul xr (rec xi))))
      (send (cat (exp (gen) xi) (mul xr (rec xi)))))
    ((send (exp (gen) xi))))
  (label 43)
  (parent 26)
  (unrealized (0 0) (1 0) (2 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton station-to-station
  (vars (i r name) (xr rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand resp 2 (r i) (xr xr) (xi xr))
  (deflistener (cat (exp (gen) xr) (one)))
  (precedes ((0 1) (2 0)) ((1 1) (0 2)) ((2 1) (1 0)))
  (non-orig (privk i) (privk r))
  (precur (2 0))
  (uniq-gen xr)
  (operation nonce-test (displaced 3 0 resp 2) (exp (gen) xr-0) (2 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv (cat (exp (gen) xr) (one)))
      (send (cat (exp (gen) xr) (one)))))
  (label 44)
  (parent 26)
  (unrealized (0 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton station-to-station
  (vars (i r r-0 name) (xi expt) (xr xr-0 rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand resp 2 (r i) (xr xr) (xi xr))
  (deflistener (cat (exp (gen) xr-0) (mul xr (rec xr-0))))
  (defstrand resp 2 (r r-0) (xr xr-0) (xi xi))
  (precedes ((1 1) (0 2)) ((2 1) (1 0)) ((3 1) (2 0)))
  (non-orig (privk i) (privk r))
  (precur (2 0))
  (uniq-gen xr)
  (operation nonce-test (added-strand resp 2) (exp (gen) xr-0) (2 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv (cat (exp (gen) xr-0) (mul xr (rec xr-0))))
      (send (cat (exp (gen) xr-0) (mul xr (rec xr-0)))))
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xr-0)
          (hash "hint" (hash "share" (exp (gen) (mul xi xr-0)) xr-0))
          (enc
            (enc (exp (gen) xr-0) (exp (gen) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr-0)) xr-0))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xi xr-0)) xr-0)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr-0)) xr-0))))
          (enc
            (enc (exp (gen) xr-0) (exp (gen) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr-0)) xr-0))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xi xr-0)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr-0)) xr-0))))))))
  (label 45)
  (parent 26)
  (unrealized (0 0) (1 0) (2 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton station-to-station
  (vars (i r name) (xr rndx) (xi expt))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xi))
  (deflistener
    (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
      (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
  (deflistener
    (cat "key" (hash "share" (exp (gen) (mul xr xi)) xr)
      (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
  (deflistener (cat "share" (exp (gen) (mul xr xi)) xr))
  (precedes ((0 1) (3 0)) ((1 1) (0 2)) ((2 1) (1 0)) ((3 1) (2 0)))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (operation encryption-test
    (added-listener (cat "share" (exp (gen) (mul xr xi)) xr))
    (hash "share" (exp (gen) (mul xr xi)) xr) (2 0))
  (traces
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xi)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xi) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))))
    ((recv
       (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
         (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
      (send
        (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr)))))
    ((recv
       (cat "key" (hash "share" (exp (gen) (mul xr xi)) xr)
         (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
      (send
        (cat "key" (hash "share" (exp (gen) (mul xr xi)) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr)))))
    ((recv (cat "share" (exp (gen) (mul xr xi)) xr))
      (send (cat "share" (exp (gen) (mul xr xi)) xr))))
  (label 46)
  (parent 27)
  (unrealized (0 2) (3 0))
  (dead)
  (comment "empty cohort"))

(defskeleton station-to-station
  (vars (i r r-0 name) (xr rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand init 3
    (hint (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))) (i i)
    (r r-0) (xi xr) (xr xr))
  (deflistener
    (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
      (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
  (deflistener
    (cat "key" (hash "share" (exp (gen) (mul xr xr)) xr)
      (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
  (precedes ((0 1) (1 1)) ((1 0) (0 0)) ((1 0) (3 0)) ((1 2) (0 2))
    ((2 1) (1 1)) ((3 1) (2 0)))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (operation encryption-test
    (added-listener
      (cat "key" (hash "share" (exp (gen) (mul xr xr)) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
    (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
      (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))) (2 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((send (exp (gen) xr))
      (recv
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (send
        (cat
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv
       (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
         (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
      (send
        (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr)))))
    ((recv
       (cat "key" (hash "share" (exp (gen) (mul xr xr)) xr)
         (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
      (send
        (cat "key" (hash "share" (exp (gen) (mul xr xr)) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
  (label 47)
  (parent 30)
  (unrealized (3 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton station-to-station
  (vars (i r r-0 r-1 name) (xr rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand init 3
    (hint (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))) (i i)
    (r r-0) (xi xr) (xr xr))
  (defstrand resp 2 (r r-1) (xr xr) (xi xr))
  (deflistener
    (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
      (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
  (deflistener
    (cat "key" (hash "share" (exp (gen) (mul xr xr)) xr)
      (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
  (precedes ((1 0) (0 0)) ((1 0) (2 0)) ((1 0) (4 0)) ((1 2) (0 2))
    ((2 1) (1 1)) ((3 1) (1 1)) ((4 1) (3 0)))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (operation encryption-test
    (added-listener
      (cat "key" (hash "share" (exp (gen) (mul xr xr)) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
    (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
      (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))) (3 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((send (exp (gen) xr))
      (recv
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (send
        (cat
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r-1))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r-1))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv
       (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
         (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
      (send
        (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr)))))
    ((recv
       (cat "key" (hash "share" (exp (gen) (mul xr xr)) xr)
         (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
      (send
        (cat "key" (hash "share" (exp (gen) (mul xr xr)) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
  (label 48)
  (parent 34)
  (unrealized (4 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton station-to-station
  (vars (r name) (xi rndx))
  (defstrand resp 3 (i r) (r r) (xr xi) (xi xi))
  (deflistener (cat (exp (gen) xi) (one)))
  (defstrand init 1 (xi xi))
  (precedes ((1 1) (0 0)) ((2 0) (1 0)))
  (non-orig (privk r))
  (precur (1 0))
  (uniq-gen xi)
  (operation nonce-test (contracted (xr xi) (xi-0 xi)) (one) (1 0))
  (traces
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xi)
          (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
          (enc
            (enc (exp (gen) xi) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xi xi)) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))))
          (enc
            (enc (exp (gen) xi) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xi xi)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xi)) xi))))))
      (recv
        (enc
          (enc (exp (gen) xi) (exp (gen) xi)
            (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
            (privk r))
          (hash "key" (hash "share" (exp (gen) (mul xi xi)) xi)
            (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))))))
    ((recv (cat (exp (gen) xi) (one)))
      (send (cat (exp (gen) xi) (one)))) ((send (exp (gen) xi))))
  (label 49)
  (parent 37)
  (seen 20)
  (unrealized)
  (comment "1 in cohort - 0 not yet seen"))

(defskeleton station-to-station
  (vars (r name) (xr xi rndx))
  (defstrand resp 3 (i r) (r r) (xr xr) (xi xr))
  (deflistener (cat (exp (gen) xi) (mul xr (rec xi))))
  (defstrand init 1 (xi xi))
  (deflistener xr)
  (precedes ((1 1) (0 0)) ((2 0) (1 0)) ((3 1) (1 0)))
  (non-orig (privk r))
  (precur (1 0))
  (uniq-gen xr)
  (operation nonce-test (added-listener xr) (mul xr (rec xi)) (1 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk r))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((recv (cat (exp (gen) xi) (mul xr (rec xi))))
      (send (cat (exp (gen) xi) (mul xr (rec xi)))))
    ((send (exp (gen) xi))) ((recv xr) (send xr)))
  (label 50)
  (parent 37)
  (unrealized (3 0))
  (dead)
  (comment "empty cohort"))

(defskeleton station-to-station
  (vars (r r-0 name) (xi expt) (xr rndx))
  (defstrand resp 3 (i r) (r r) (xr xr) (xi xr))
  (deflistener (cat (exp (gen) xr) (one)))
  (defstrand resp 2 (r r-0) (xr xr) (xi xi))
  (precedes ((1 1) (0 0)) ((2 1) (1 0)))
  (non-orig (privk r))
  (precur (1 0))
  (uniq-gen xr)
  (operation nonce-test (contracted (xr-0 xr) (xr-1 xr)) (one) (1 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk r))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((recv (cat (exp (gen) xr) (one)))
      (send (cat (exp (gen) xr) (one))))
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr)) xr))))))))
  (label 51)
  (parent 38)
  (seen 21)
  (unrealized)
  (comment "1 in cohort - 0 not yet seen"))

(defskeleton station-to-station
  (vars (r r-0 name) (xi expt) (xr xr-0 rndx))
  (defstrand resp 3 (i r) (r r) (xr xr) (xi xr))
  (deflistener (cat (exp (gen) xr-0) (mul xr (rec xr-0))))
  (defstrand resp 2 (r r-0) (xr xr-0) (xi xi))
  (deflistener xr)
  (precedes ((1 1) (0 0)) ((2 1) (1 0)) ((3 1) (1 0)))
  (non-orig (privk r))
  (precur (1 0))
  (uniq-gen xr)
  (operation nonce-test (added-listener xr) (mul xr (rec xr-0)) (1 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk r))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((recv (cat (exp (gen) xr-0) (mul xr (rec xr-0))))
      (send (cat (exp (gen) xr-0) (mul xr (rec xr-0)))))
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xr-0)
          (hash "hint" (hash "share" (exp (gen) (mul xi xr-0)) xr-0))
          (enc
            (enc (exp (gen) xr-0) (exp (gen) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr-0)) xr-0))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xi xr-0)) xr-0)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr-0)) xr-0))))
          (enc
            (enc (exp (gen) xr-0) (exp (gen) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr-0)) xr-0))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xi xr-0)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr-0)) xr-0)))))))
    ((recv xr) (send xr)))
  (label 52)
  (parent 38)
  (unrealized (3 0))
  (dead)
  (comment "empty cohort"))

(defskeleton station-to-station
  (vars (i r name) (xr rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand resp 2 (r i) (xr xr) (xi xr))
  (deflistener (cat (gen) xr))
  (precedes ((0 1) (1 0)) ((1 1) (0 2)) ((2 1) (0 0)))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (precur (2 0))
  (operation nonce-test (contracted (xr-0 xr) (w xr)) (gen) (2 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv (cat (gen) xr)) (send (cat (gen) xr))))
  (label 53)
  (parent 41)
  (unrealized (2 0))
  (dead)
  (comment "empty cohort"))

(defskeleton station-to-station
  (vars (i r name) (xr xi rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand resp 2 (r i) (xr xr) (xi xr))
  (deflistener (cat (exp (gen) xi) (mul xr (rec xi))))
  (defstrand init 1 (xi xi))
  (precedes ((0 1) (1 0)) ((1 1) (0 2)) ((2 1) (0 0)) ((3 0) (2 0)))
  (non-orig (privk i) (privk r))
  (precur (2 0))
  (uniq-gen xr)
  (operation nonce-test (added-strand init 1) (exp (gen) xi) (2 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv (cat (exp (gen) xi) (mul xr (rec xi))))
      (send (cat (exp (gen) xi) (mul xr (rec xi)))))
    ((send (exp (gen) xi))))
  (label 54)
  (parent 41)
  (unrealized (0 0) (2 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton station-to-station
  (vars (i r r-0 name) (xi expt) (xr xr-0 rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand resp 2 (r i) (xr xr) (xi xr))
  (deflistener (cat (exp (gen) xr-0) (mul xr (rec xr-0))))
  (defstrand resp 2 (r r-0) (xr xr-0) (xi xi))
  (precedes ((0 1) (1 0)) ((1 1) (0 2)) ((2 1) (0 0)) ((3 1) (2 0)))
  (non-orig (privk i) (privk r))
  (precur (2 0))
  (uniq-gen xr)
  (operation nonce-test (added-strand resp 2) (exp (gen) xr-0) (2 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv (cat (exp (gen) xr-0) (mul xr (rec xr-0))))
      (send (cat (exp (gen) xr-0) (mul xr (rec xr-0)))))
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xr-0)
          (hash "hint" (hash "share" (exp (gen) (mul xi xr-0)) xr-0))
          (enc
            (enc (exp (gen) xr-0) (exp (gen) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr-0)) xr-0))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xi xr-0)) xr-0)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr-0)) xr-0))))
          (enc
            (enc (exp (gen) xr-0) (exp (gen) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr-0)) xr-0))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xi xr-0)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr-0)) xr-0))))))))
  (label 55)
  (parent 41)
  (unrealized (0 0) (2 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton station-to-station
  (vars (i r name) (xi rndx))
  (defstrand resp 3 (i i) (r r) (xr xi) (xi xi))
  (defstrand resp 2 (r i) (xr xi) (xi xi))
  (deflistener (cat (exp (gen) xi) (one)))
  (defstrand init 1 (xi xi))
  (precedes ((1 1) (0 2)) ((2 1) (1 0)) ((3 0) (0 0)) ((3 0) (2 0)))
  (non-orig (privk i) (privk r))
  (precur (2 0))
  (uniq-gen xi)
  (operation nonce-test (contracted (xr xi) (xi-0 xi)) (one) (2 0))
  (traces
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xi)
          (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
          (enc
            (enc (exp (gen) xi) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xi xi)) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))))
          (enc
            (enc (exp (gen) xi) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xi xi)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xi)) xi))))))
      (recv
        (enc
          (enc (exp (gen) xi) (exp (gen) xi)
            (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xi xi)) xi)
            (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))))))
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xi)
          (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
          (enc
            (enc (exp (gen) xi) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xi)) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))))
          (enc
            (enc (exp (gen) xi) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xi)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xi)) xi)))))))
    ((recv (cat (exp (gen) xi) (one)))
      (send (cat (exp (gen) xi) (one)))) ((send (exp (gen) xi))))
  (label 56)
  (parent 43)
  (seen 23)
  (unrealized)
  (comment "1 in cohort - 0 not yet seen"))

(defskeleton station-to-station
  (vars (i r name) (xr xi rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand resp 2 (r i) (xr xr) (xi xr))
  (deflistener (cat (exp (gen) xi) (mul xr (rec xi))))
  (defstrand init 1 (xi xi))
  (deflistener xr)
  (precedes ((1 1) (0 2)) ((2 1) (1 0)) ((3 0) (2 0)) ((4 1) (2 0)))
  (non-orig (privk i) (privk r))
  (precur (2 0))
  (uniq-gen xr)
  (operation nonce-test (added-listener xr) (mul xr (rec xi)) (2 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv (cat (exp (gen) xi) (mul xr (rec xi))))
      (send (cat (exp (gen) xi) (mul xr (rec xi)))))
    ((send (exp (gen) xi))) ((recv xr) (send xr)))
  (label 57)
  (parent 43)
  (unrealized (0 0) (4 0))
  (dead)
  (comment "empty cohort"))

(defskeleton station-to-station
  (vars (i r name) (xi rndx))
  (defstrand resp 3 (i i) (r r) (xr xi) (xi xi))
  (defstrand resp 2 (r i) (xr xi) (xi xi))
  (deflistener (cat (exp (gen) xi) (one)))
  (defstrand init 1 (xi xi))
  (precedes ((0 1) (2 0)) ((1 1) (0 2)) ((2 1) (1 0)) ((3 0) (0 0)))
  (non-orig (privk i) (privk r))
  (precur (2 0))
  (uniq-gen xi)
  (operation nonce-test (added-strand init 1) (exp (gen) xi) (0 0))
  (traces
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xi)
          (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
          (enc
            (enc (exp (gen) xi) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xi xi)) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))))
          (enc
            (enc (exp (gen) xi) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xi xi)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xi)) xi))))))
      (recv
        (enc
          (enc (exp (gen) xi) (exp (gen) xi)
            (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xi xi)) xi)
            (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))))))
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xi)
          (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
          (enc
            (enc (exp (gen) xi) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xi)) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))))
          (enc
            (enc (exp (gen) xi) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xi)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xi)) xi)))))))
    ((recv (cat (exp (gen) xi) (one)))
      (send (cat (exp (gen) xi) (one)))) ((send (exp (gen) xi))))
  (label 58)
  (parent 44)
  (seen 39)
  (unrealized)
  (comment "1 in cohort - 0 not yet seen"))

(defskeleton station-to-station
  (vars (i r r-0 name) (xi expt) (xr rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand resp 2 (r i) (xr xr) (xi xr))
  (deflistener (cat (exp (gen) xr) (one)))
  (defstrand resp 2 (r r-0) (xr xr) (xi xi))
  (precedes ((0 1) (2 0)) ((1 1) (0 2)) ((2 1) (1 0)) ((3 1) (0 0)))
  (non-orig (privk i) (privk r))
  (precur (2 0))
  (uniq-gen xr)
  (operation nonce-test (added-strand resp 2) (exp (gen) xr) (0 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv (cat (exp (gen) xr) (one)))
      (send (cat (exp (gen) xr) (one))))
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr)) xr))))))))
  (label 59)
  (parent 44)
  (seen 40)
  (unrealized)
  (comment "1 in cohort - 0 not yet seen"))

(defskeleton station-to-station
  (vars (i r name) (xr rndx) (w expt))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand resp 2 (r i) (xr xr) (xi xr))
  (deflistener (cat (exp (gen) xr) (one)))
  (deflistener (cat (exp (gen) (mul xr (rec w))) w))
  (precedes ((0 1) (2 0)) ((1 1) (0 2)) ((2 1) (1 0)) ((3 1) (0 0)))
  (non-orig (privk i) (privk r))
  (precur (3 0) (2 0))
  (uniq-gen xr)
  (operation nonce-test
    (added-listener (cat (exp (gen) (mul xr (rec w))) w)) (exp (gen) xr)
    (0 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv (cat (exp (gen) xr) (one)))
      (send (cat (exp (gen) xr) (one))))
    ((recv (cat (exp (gen) (mul xr (rec w))) w))
      (send (cat (exp (gen) (mul xr (rec w))) w))))
  (label 60)
  (parent 44)
  (unrealized (3 0))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton station-to-station
  (vars (i r r-0 name) (xi expt) (xr rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand resp 2 (r i) (xr xr) (xi xr))
  (deflistener (cat (exp (gen) xr) (one)))
  (defstrand resp 2 (r r-0) (xr xr) (xi xi))
  (precedes ((1 1) (0 2)) ((2 1) (1 0)) ((3 1) (0 0)) ((3 1) (2 0)))
  (non-orig (privk i) (privk r))
  (precur (2 0))
  (uniq-gen xr)
  (operation nonce-test (contracted (xr-0 xr) (xr-1 xr)) (one) (2 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv (cat (exp (gen) xr) (one)))
      (send (cat (exp (gen) xr) (one))))
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr)) xr))))))))
  (label 61)
  (parent 45)
  (seen 25)
  (unrealized)
  (comment "1 in cohort - 0 not yet seen"))

(defskeleton station-to-station
  (vars (i r r-0 name) (xi expt) (xr xr-0 rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand resp 2 (r i) (xr xr) (xi xr))
  (deflistener (cat (exp (gen) xr-0) (mul xr (rec xr-0))))
  (defstrand resp 2 (r r-0) (xr xr-0) (xi xi))
  (deflistener xr)
  (precedes ((1 1) (0 2)) ((2 1) (1 0)) ((3 1) (2 0)) ((4 1) (2 0)))
  (non-orig (privk i) (privk r))
  (precur (2 0))
  (uniq-gen xr)
  (operation nonce-test (added-listener xr) (mul xr (rec xr-0)) (2 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv (cat (exp (gen) xr-0) (mul xr (rec xr-0))))
      (send (cat (exp (gen) xr-0) (mul xr (rec xr-0)))))
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xr-0)
          (hash "hint" (hash "share" (exp (gen) (mul xi xr-0)) xr-0))
          (enc
            (enc (exp (gen) xr-0) (exp (gen) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr-0)) xr-0))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xi xr-0)) xr-0)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr-0)) xr-0))))
          (enc
            (enc (exp (gen) xr-0) (exp (gen) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr-0)) xr-0))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xi xr-0)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr-0)) xr-0)))))))
    ((recv xr) (send xr)))
  (label 62)
  (parent 45)
  (unrealized (0 0) (4 0))
  (dead)
  (comment "empty cohort"))

(defskeleton station-to-station
  (vars (i r r-0 name) (xr rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand init 3
    (hint (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))) (i i)
    (r r-0) (xi xr) (xr xr))
  (deflistener
    (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
      (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
  (deflistener
    (cat "key" (hash "share" (exp (gen) (mul xr xr)) xr)
      (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
  (deflistener (cat "share" (exp (gen) (mul xr xr)) xr))
  (precedes ((0 1) (1 1)) ((1 0) (0 0)) ((1 0) (4 0)) ((1 2) (0 2))
    ((2 1) (1 1)) ((3 1) (2 0)) ((4 1) (3 0)))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (operation encryption-test
    (added-listener (cat "share" (exp (gen) (mul xr xr)) xr))
    (hash "share" (exp (gen) (mul xr xr)) xr) (3 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((send (exp (gen) xr))
      (recv
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (send
        (cat
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv
       (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
         (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
      (send
        (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr)))))
    ((recv
       (cat "key" (hash "share" (exp (gen) (mul xr xr)) xr)
         (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
      (send
        (cat "key" (hash "share" (exp (gen) (mul xr xr)) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr)))))
    ((recv (cat "share" (exp (gen) (mul xr xr)) xr))
      (send (cat "share" (exp (gen) (mul xr xr)) xr))))
  (label 63)
  (parent 47)
  (unrealized (4 0))
  (dead)
  (comment "empty cohort"))

(defskeleton station-to-station
  (vars (i r r-0 r-1 name) (xr rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand init 3
    (hint (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))) (i i)
    (r r-0) (xi xr) (xr xr))
  (defstrand resp 2 (r r-1) (xr xr) (xi xr))
  (deflistener
    (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
      (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
  (deflistener
    (cat "key" (hash "share" (exp (gen) (mul xr xr)) xr)
      (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
  (deflistener (cat "share" (exp (gen) (mul xr xr)) xr))
  (precedes ((1 0) (0 0)) ((1 0) (2 0)) ((1 0) (5 0)) ((1 2) (0 2))
    ((2 1) (1 1)) ((3 1) (1 1)) ((4 1) (3 0)) ((5 1) (4 0)))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (operation encryption-test
    (added-listener (cat "share" (exp (gen) (mul xr xr)) xr))
    (hash "share" (exp (gen) (mul xr xr)) xr) (4 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((send (exp (gen) xr))
      (recv
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (send
        (cat
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r-1))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r-1))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv
       (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
         (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
      (send
        (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr)))))
    ((recv
       (cat "key" (hash "share" (exp (gen) (mul xr xr)) xr)
         (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
      (send
        (cat "key" (hash "share" (exp (gen) (mul xr xr)) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr)))))
    ((recv (cat "share" (exp (gen) (mul xr xr)) xr))
      (send (cat "share" (exp (gen) (mul xr xr)) xr))))
  (label 64)
  (parent 48)
  (unrealized (5 0))
  (dead)
  (comment "empty cohort"))

(defskeleton station-to-station
  (vars (i r name) (xi rndx))
  (defstrand resp 3 (i i) (r r) (xr xi) (xi xi))
  (defstrand resp 2 (r i) (xr xi) (xi xi))
  (deflistener (cat (exp (gen) xi) (one)))
  (defstrand init 1 (xi xi))
  (precedes ((0 1) (1 0)) ((1 1) (0 2)) ((2 1) (0 0)) ((3 0) (2 0)))
  (non-orig (privk i) (privk r))
  (precur (2 0))
  (uniq-gen xi)
  (operation nonce-test (contracted (xr xi) (xi-0 xi)) (one) (2 0))
  (traces
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xi)
          (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
          (enc
            (enc (exp (gen) xi) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xi xi)) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))))
          (enc
            (enc (exp (gen) xi) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xi xi)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xi)) xi))))))
      (recv
        (enc
          (enc (exp (gen) xi) (exp (gen) xi)
            (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xi xi)) xi)
            (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))))))
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xi)
          (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
          (enc
            (enc (exp (gen) xi) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xi)) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))))
          (enc
            (enc (exp (gen) xi) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xi)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xi)) xi)))))))
    ((recv (cat (exp (gen) xi) (one)))
      (send (cat (exp (gen) xi) (one)))) ((send (exp (gen) xi))))
  (label 65)
  (parent 54)
  (seen 39)
  (unrealized)
  (comment "1 in cohort - 0 not yet seen"))

(defskeleton station-to-station
  (vars (i r name) (xr xi rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand resp 2 (r i) (xr xr) (xi xr))
  (deflistener (cat (exp (gen) xi) (mul xr (rec xi))))
  (defstrand init 1 (xi xi))
  (deflistener xr)
  (precedes ((0 1) (1 0)) ((1 1) (0 2)) ((2 1) (0 0)) ((3 0) (2 0))
    ((4 1) (2 0)))
  (non-orig (privk i) (privk r))
  (precur (2 0))
  (uniq-gen xr)
  (operation nonce-test (added-listener xr) (mul xr (rec xi)) (2 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv (cat (exp (gen) xi) (mul xr (rec xi))))
      (send (cat (exp (gen) xi) (mul xr (rec xi)))))
    ((send (exp (gen) xi))) ((recv xr) (send xr)))
  (label 66)
  (parent 54)
  (unrealized (4 0))
  (dead)
  (comment "empty cohort"))

(defskeleton station-to-station
  (vars (i r r-0 name) (xi expt) (xr rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand resp 2 (r i) (xr xr) (xi xr))
  (deflistener (cat (exp (gen) xr) (one)))
  (defstrand resp 2 (r r-0) (xr xr) (xi xi))
  (precedes ((0 1) (1 0)) ((1 1) (0 2)) ((2 1) (0 0)) ((3 1) (2 0)))
  (non-orig (privk i) (privk r))
  (precur (2 0))
  (uniq-gen xr)
  (operation nonce-test (contracted (xr-0 xr) (xr-1 xr)) (one) (2 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv (cat (exp (gen) xr) (one)))
      (send (cat (exp (gen) xr) (one))))
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr)) xr))))))))
  (label 67)
  (parent 55)
  (seen 40)
  (unrealized)
  (comment "1 in cohort - 0 not yet seen"))

(defskeleton station-to-station
  (vars (i r r-0 name) (xi expt) (xr xr-0 rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand resp 2 (r i) (xr xr) (xi xr))
  (deflistener (cat (exp (gen) xr-0) (mul xr (rec xr-0))))
  (defstrand resp 2 (r r-0) (xr xr-0) (xi xi))
  (deflistener xr)
  (precedes ((0 1) (1 0)) ((1 1) (0 2)) ((2 1) (0 0)) ((3 1) (2 0))
    ((4 1) (2 0)))
  (non-orig (privk i) (privk r))
  (precur (2 0))
  (uniq-gen xr)
  (operation nonce-test (added-listener xr) (mul xr (rec xr-0)) (2 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv (cat (exp (gen) xr-0) (mul xr (rec xr-0))))
      (send (cat (exp (gen) xr-0) (mul xr (rec xr-0)))))
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xr-0)
          (hash "hint" (hash "share" (exp (gen) (mul xi xr-0)) xr-0))
          (enc
            (enc (exp (gen) xr-0) (exp (gen) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr-0)) xr-0))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xi xr-0)) xr-0)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr-0)) xr-0))))
          (enc
            (enc (exp (gen) xr-0) (exp (gen) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr-0)) xr-0))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xi xr-0)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr-0)) xr-0)))))))
    ((recv xr) (send xr)))
  (label 68)
  (parent 55)
  (unrealized (4 0))
  (dead)
  (comment "empty cohort"))

(defskeleton station-to-station
  (vars (i r name) (xr rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand resp 2 (r i) (xr xr) (xi xr))
  (deflistener (cat (exp (gen) xr) (one)))
  (deflistener (cat (gen) xr))
  (precedes ((0 1) (2 0)) ((1 1) (0 2)) ((2 1) (1 0)) ((3 1) (0 0)))
  (non-orig (privk i) (privk r))
  (precur (3 0) (2 0))
  (uniq-gen xr)
  (operation nonce-test (contracted (xr-0 xr) (w xr)) (gen) (3 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv (cat (exp (gen) xr) (one)))
      (send (cat (exp (gen) xr) (one))))
    ((recv (cat (gen) xr)) (send (cat (gen) xr))))
  (label 69)
  (parent 60)
  (unrealized (3 0))
  (dead)
  (comment "empty cohort"))

(defskeleton station-to-station
  (vars (i r name) (xr xi rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand resp 2 (r i) (xr xr) (xi xr))
  (deflistener (cat (exp (gen) xr) (one)))
  (deflistener (cat (exp (gen) xi) (mul xr (rec xi))))
  (defstrand init 1 (xi xi))
  (precedes ((0 1) (2 0)) ((1 1) (0 2)) ((2 1) (1 0)) ((3 1) (0 0))
    ((4 0) (3 0)))
  (non-orig (privk i) (privk r))
  (precur (3 0) (2 0))
  (uniq-gen xr)
  (operation nonce-test (added-strand init 1) (exp (gen) xi) (3 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv (cat (exp (gen) xr) (one)))
      (send (cat (exp (gen) xr) (one))))
    ((recv (cat (exp (gen) xi) (mul xr (rec xi))))
      (send (cat (exp (gen) xi) (mul xr (rec xi)))))
    ((send (exp (gen) xi))))
  (label 70)
  (parent 60)
  (unrealized (0 0) (3 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton station-to-station
  (vars (i r r-0 name) (xi expt) (xr xr-0 rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand resp 2 (r i) (xr xr) (xi xr))
  (deflistener (cat (exp (gen) xr) (one)))
  (deflistener (cat (exp (gen) xr-0) (mul xr (rec xr-0))))
  (defstrand resp 2 (r r-0) (xr xr-0) (xi xi))
  (precedes ((0 1) (2 0)) ((1 1) (0 2)) ((2 1) (1 0)) ((3 1) (0 0))
    ((4 1) (3 0)))
  (non-orig (privk i) (privk r))
  (precur (3 0) (2 0))
  (uniq-gen xr)
  (operation nonce-test (added-strand resp 2) (exp (gen) xr-0) (3 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv (cat (exp (gen) xr) (one)))
      (send (cat (exp (gen) xr) (one))))
    ((recv (cat (exp (gen) xr-0) (mul xr (rec xr-0))))
      (send (cat (exp (gen) xr-0) (mul xr (rec xr-0)))))
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xr-0)
          (hash "hint" (hash "share" (exp (gen) (mul xi xr-0)) xr-0))
          (enc
            (enc (exp (gen) xr-0) (exp (gen) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr-0)) xr-0))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xi xr-0)) xr-0)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr-0)) xr-0))))
          (enc
            (enc (exp (gen) xr-0) (exp (gen) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr-0)) xr-0))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xi xr-0)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr-0)) xr-0))))))))
  (label 71)
  (parent 60)
  (unrealized (0 0) (3 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton station-to-station
  (vars (i r name) (xi rndx))
  (defstrand resp 3 (i i) (r r) (xr xi) (xi xi))
  (defstrand resp 2 (r i) (xr xi) (xi xi))
  (deflistener (cat (exp (gen) xi) (one)))
  (deflistener (cat (exp (gen) xi) (one)))
  (defstrand init 1 (xi xi))
  (precedes ((0 1) (2 0)) ((1 1) (0 2)) ((2 1) (1 0)) ((3 1) (0 0))
    ((4 0) (3 0)))
  (non-orig (privk i) (privk r))
  (precur (3 0) (2 0))
  (uniq-gen xi)
  (operation nonce-test (contracted (xr xi) (xi-0 xi)) (one) (3 0))
  (traces
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xi)
          (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
          (enc
            (enc (exp (gen) xi) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xi xi)) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))))
          (enc
            (enc (exp (gen) xi) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xi xi)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xi)) xi))))))
      (recv
        (enc
          (enc (exp (gen) xi) (exp (gen) xi)
            (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xi xi)) xi)
            (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))))))
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xi)
          (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
          (enc
            (enc (exp (gen) xi) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xi)) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))))
          (enc
            (enc (exp (gen) xi) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xi)) xi))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xi)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xi)) xi)))))))
    ((recv (cat (exp (gen) xi) (one)))
      (send (cat (exp (gen) xi) (one))))
    ((recv (cat (exp (gen) xi) (one)))
      (send (cat (exp (gen) xi) (one)))) ((send (exp (gen) xi))))
  (label 72)
  (parent 70)
  (seen 65)
  (unrealized)
  (comment "1 in cohort - 0 not yet seen"))

(defskeleton station-to-station
  (vars (i r name) (xr xi rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand resp 2 (r i) (xr xr) (xi xr))
  (deflistener (cat (exp (gen) xr) (one)))
  (deflistener (cat (exp (gen) xi) (mul xr (rec xi))))
  (defstrand init 1 (xi xi))
  (deflistener xr)
  (precedes ((0 1) (2 0)) ((1 1) (0 2)) ((2 1) (1 0)) ((3 1) (0 0))
    ((4 0) (3 0)) ((5 1) (3 0)))
  (non-orig (privk i) (privk r))
  (precur (3 0) (2 0))
  (uniq-gen xr)
  (operation nonce-test (added-listener xr) (mul xr (rec xi)) (3 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv (cat (exp (gen) xr) (one)))
      (send (cat (exp (gen) xr) (one))))
    ((recv (cat (exp (gen) xi) (mul xr (rec xi))))
      (send (cat (exp (gen) xi) (mul xr (rec xi)))))
    ((send (exp (gen) xi))) ((recv xr) (send xr)))
  (label 73)
  (parent 70)
  (unrealized (5 0))
  (dead)
  (comment "empty cohort"))

(defskeleton station-to-station
  (vars (i r r-0 name) (xi expt) (xr rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand resp 2 (r i) (xr xr) (xi xr))
  (deflistener (cat (exp (gen) xr) (one)))
  (deflistener (cat (exp (gen) xr) (one)))
  (defstrand resp 2 (r r-0) (xr xr) (xi xi))
  (precedes ((0 1) (2 0)) ((1 1) (0 2)) ((2 1) (1 0)) ((3 1) (0 0))
    ((4 1) (3 0)))
  (non-orig (privk i) (privk r))
  (precur (3 0) (2 0))
  (uniq-gen xr)
  (operation nonce-test (contracted (xr-0 xr) (xr-1 xr)) (one) (3 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv (cat (exp (gen) xr) (one)))
      (send (cat (exp (gen) xr) (one))))
    ((recv (cat (exp (gen) xr) (one)))
      (send (cat (exp (gen) xr) (one))))
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xi)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr)) xr))))))))
  (label 74)
  (parent 71)
  (seen 67)
  (unrealized)
  (comment "1 in cohort - 0 not yet seen"))

(defskeleton station-to-station
  (vars (i r r-0 name) (xi expt) (xr xr-0 rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xr))
  (defstrand resp 2 (r i) (xr xr) (xi xr))
  (deflistener (cat (exp (gen) xr) (one)))
  (deflistener (cat (exp (gen) xr-0) (mul xr (rec xr-0))))
  (defstrand resp 2 (r r-0) (xr xr-0) (xi xi))
  (deflistener xr)
  (precedes ((0 1) (2 0)) ((1 1) (0 2)) ((2 1) (1 0)) ((3 1) (0 0))
    ((4 1) (3 0)) ((5 1) (3 0)))
  (non-orig (privk i) (privk r))
  (precur (3 0) (2 0))
  (uniq-gen xr)
  (operation nonce-test (added-listener xr) (mul xr (rec xr-0)) (3 0))
  (traces
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr))))))
      (recv
        (enc
          (enc (exp (gen) xr) (exp (gen) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
            (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))))
    ((recv (exp (gen) xr))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))))
          (enc
            (enc (exp (gen) xr) (exp (gen) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xr)) xr))
              (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xr)) xr)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xr)) xr)))))))
    ((recv (cat (exp (gen) xr) (one)))
      (send (cat (exp (gen) xr) (one))))
    ((recv (cat (exp (gen) xr-0) (mul xr (rec xr-0))))
      (send (cat (exp (gen) xr-0) (mul xr (rec xr-0)))))
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xr-0)
          (hash "hint" (hash "share" (exp (gen) (mul xi xr-0)) xr-0))
          (enc
            (enc (exp (gen) xr-0) (exp (gen) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr-0)) xr-0))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xi xr-0)) xr-0)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr-0)) xr-0))))
          (enc
            (enc (exp (gen) xr-0) (exp (gen) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr-0)) xr-0))
              (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xi xr-0)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr-0)) xr-0)))))))
    ((recv xr) (send xr)))
  (label 75)
  (parent 71)
  (unrealized (5 0))
  (dead)
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol station-nohint diffie-hellman
  (defrole init
    (vars (xi rndx) (xr expt) (i r name) (hint mesg))
    (trace (send (exp (gen) xi))
      (recv
        (cat (exp (gen) xr) hint
          (enc (enc "resp" (exp (gen) xr) (exp (gen) xi) (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              hint))))
      (send
        (cat
          (enc (enc (exp (gen) xi) (exp (gen) xr) (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xr) hint))
          (enc (enc (exp (gen) xi) (exp (gen) xr) (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              hint))))))
  (defrole resp
    (vars (xr rndx) (xi expt) (i r name))
    (trace (recv (exp (gen) xi))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
          (enc (enc "resp" (exp (gen) xr) (exp (gen) xi) (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
          (enc (enc "resp" (exp (gen) xr) (exp (gen) xi) (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xi)) xr))))))
      (recv
        (enc (enc (exp (gen) xi) (exp (gen) xr) (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
            (hash "hint"
              (hash "share" (exp (gen) (mul xr xi)) xr))))))))

(defskeleton station-nohint
  (vars (hint mesg) (i r name) (xi rndx) (xr expt))
  (defstrand init 3 (hint hint) (i i) (r r) (xi xi) (xr xr))
  (non-orig (privk i) (privk r))
  (uniq-gen xi)
  (traces
    ((send (exp (gen) xi))
      (recv
        (cat (exp (gen) xr) hint
          (enc (enc "resp" (exp (gen) xr) (exp (gen) xi) (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              hint))))
      (send
        (cat
          (enc (enc (exp (gen) xi) (exp (gen) xr) (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xr) hint))
          (enc (enc (exp (gen) xi) (exp (gen) xr) (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              hint))))))
  (label 76)
  (unrealized (0 1))
  (origs)
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton station-nohint
  (vars (i r name) (xr xi rndx))
  (defstrand init 3
    (hint (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))) (i i)
    (r r) (xi xi) (xr xr))
  (defstrand resp 2 (r r) (xr xr) (xi xi))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (non-orig (privk i) (privk r))
  (uniq-gen xi)
  (operation encryption-test (added-strand resp 2)
    (enc (enc "resp" (exp (gen) xr) (exp (gen) xi) (privk r))
      (hash "key" (hash "share" (exp (gen) (mul xr xi)) xi)
        (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr)))) (0 1))
  (traces
    ((send (exp (gen) xi))
      (recv
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
          (enc (enc "resp" (exp (gen) xr) (exp (gen) xi) (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xi)) xr))))))
      (send
        (cat
          (enc (enc (exp (gen) xi) (exp (gen) xr) (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
          (enc (enc (exp (gen) xi) (exp (gen) xr) (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xi)) xr)))))))
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
          (enc (enc "resp" (exp (gen) xr) (exp (gen) xi) (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
          (enc (enc "resp" (exp (gen) xr) (exp (gen) xi) (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xi)) xr))))))))
  (label 77)
  (parent 76)
  (unrealized)
  (shape)
  (maps
    ((0)
      ((i i) (r r) (xi xi) (xr xr)
        (hint
          (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))))
  (origs))

(defskeleton station-nohint
  (vars (hint mesg) (i r name) (xi rndx) (xr expt))
  (defstrand init 3 (hint hint) (i i) (r r) (xi xi) (xr xr))
  (deflistener
    (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi) hint))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (non-orig (privk i) (privk r))
  (uniq-gen xi)
  (operation encryption-test
    (added-listener
      (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi) hint))
    (enc (enc "resp" (exp (gen) xr) (exp (gen) xi) (privk r))
      (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi) hint))
    (0 1))
  (traces
    ((send (exp (gen) xi))
      (recv
        (cat (exp (gen) xr) hint
          (enc (enc "resp" (exp (gen) xr) (exp (gen) xi) (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              hint))))
      (send
        (cat
          (enc (enc (exp (gen) xi) (exp (gen) xr) (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xr) hint))
          (enc (enc (exp (gen) xi) (exp (gen) xr) (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              hint)))))
    ((recv (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi) hint))
      (send
        (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi) hint))))
  (label 78)
  (parent 76)
  (unrealized (0 1) (1 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton station-nohint
  (vars (hint mesg) (i r name) (xi rndx) (xr expt))
  (defstrand init 3 (hint hint) (i i) (r r) (xi xi) (xr xr))
  (deflistener
    (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi) hint))
  (deflistener
    (cat "key" (hash "share" (exp (gen) (mul xi xr)) xi) hint))
  (precedes ((0 0) (2 0)) ((1 1) (0 1)) ((2 1) (1 0)))
  (non-orig (privk i) (privk r))
  (uniq-gen xi)
  (operation encryption-test
    (added-listener
      (cat "key" (hash "share" (exp (gen) (mul xi xr)) xi) hint))
    (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi) hint) (1 0))
  (traces
    ((send (exp (gen) xi))
      (recv
        (cat (exp (gen) xr) hint
          (enc (enc "resp" (exp (gen) xr) (exp (gen) xi) (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              hint))))
      (send
        (cat
          (enc (enc (exp (gen) xi) (exp (gen) xr) (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xr) hint))
          (enc (enc (exp (gen) xi) (exp (gen) xr) (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              hint)))))
    ((recv (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi) hint))
      (send
        (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi) hint)))
    ((recv (cat "key" (hash "share" (exp (gen) (mul xi xr)) xi) hint))
      (send
        (cat "key" (hash "share" (exp (gen) (mul xi xr)) xi) hint))))
  (label 79)
  (parent 78)
  (unrealized (0 1) (2 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton station-nohint
  (vars (hint mesg) (i r name) (xi rndx) (xr expt))
  (defstrand init 3 (hint hint) (i i) (r r) (xi xi) (xr xr))
  (deflistener
    (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi) hint))
  (deflistener
    (cat "key" (hash "share" (exp (gen) (mul xi xr)) xi) hint))
  (deflistener (cat "share" (exp (gen) (mul xi xr)) xi))
  (precedes ((0 0) (3 0)) ((1 1) (0 1)) ((2 1) (1 0)) ((3 1) (2 0)))
  (non-orig (privk i) (privk r))
  (uniq-gen xi)
  (operation encryption-test
    (added-listener (cat "share" (exp (gen) (mul xi xr)) xi))
    (hash "share" (exp (gen) (mul xi xr)) xi) (2 0))
  (traces
    ((send (exp (gen) xi))
      (recv
        (cat (exp (gen) xr) hint
          (enc (enc "resp" (exp (gen) xr) (exp (gen) xi) (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              hint))))
      (send
        (cat
          (enc (enc (exp (gen) xi) (exp (gen) xr) (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xr) hint))
          (enc (enc (exp (gen) xi) (exp (gen) xr) (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              hint)))))
    ((recv (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi) hint))
      (send
        (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi) hint)))
    ((recv (cat "key" (hash "share" (exp (gen) (mul xi xr)) xi) hint))
      (send (cat "key" (hash "share" (exp (gen) (mul xi xr)) xi) hint)))
    ((recv (cat "share" (exp (gen) (mul xi xr)) xi))
      (send (cat "share" (exp (gen) (mul xi xr)) xi))))
  (label 80)
  (parent 79)
  (unrealized (0 1) (3 0))
  (dead)
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol station-nohint diffie-hellman
  (defrole init
    (vars (xi rndx) (xr expt) (i r name) (hint mesg))
    (trace (send (exp (gen) xi))
      (recv
        (cat (exp (gen) xr) hint
          (enc (enc "resp" (exp (gen) xr) (exp (gen) xi) (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              hint))))
      (send
        (cat
          (enc (enc (exp (gen) xi) (exp (gen) xr) (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xr) hint))
          (enc (enc (exp (gen) xi) (exp (gen) xr) (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              hint))))))
  (defrole resp
    (vars (xr rndx) (xi expt) (i r name))
    (trace (recv (exp (gen) xi))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
          (enc (enc "resp" (exp (gen) xr) (exp (gen) xi) (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
          (enc (enc "resp" (exp (gen) xr) (exp (gen) xi) (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xi)) xr))))))
      (recv
        (enc (enc (exp (gen) xi) (exp (gen) xr) (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
            (hash "hint"
              (hash "share" (exp (gen) (mul xr xi)) xr))))))))

(defskeleton station-nohint
  (vars (i r name) (xr rndx) (xi expt))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xi))
  (absent (xr xi))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (traces
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
          (enc (enc "resp" (exp (gen) xr) (exp (gen) xi) (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
          (enc (enc "resp" (exp (gen) xr) (exp (gen) xi) (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xi)) xr))))))
      (recv
        (enc (enc (exp (gen) xi) (exp (gen) xr) (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr)))))))
  (label 81)
  (unrealized (0 2))
  (origs)
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton station-nohint
  (vars (i r r-0 name) (xi xr rndx))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xi))
  (defstrand init 3
    (hint (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))) (i i)
    (r r-0) (xi xi) (xr xr))
  (precedes ((0 1) (1 1)) ((1 2) (0 2)))
  (absent (xr xi))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (operation encryption-test (added-strand init 3)
    (enc (enc (exp (gen) xi) (exp (gen) xr) (privk i))
      (hash "key" (hash "share" (exp (gen) (mul xi xr)) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr)))) (0 2))
  (traces
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
          (enc (enc "resp" (exp (gen) xr) (exp (gen) xi) (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))))
          (enc (enc "resp" (exp (gen) xr) (exp (gen) xi) (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr)) xr))))))
      (recv
        (enc (enc (exp (gen) xi) (exp (gen) xr) (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xi xr)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))))))
    ((send (exp (gen) xi))
      (recv
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))
          (enc (enc "resp" (exp (gen) xr) (exp (gen) xi) (privk r-0))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr)) xr))))))
      (send
        (cat
          (enc (enc (exp (gen) xi) (exp (gen) xr) (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xi xr)) xr))))
          (enc (enc (exp (gen) xi) (exp (gen) xr) (privk i))
            (hash "key" (hash "share" (exp (gen) (mul xi xr)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xi xr)) xr))))))))
  (label 82)
  (parent 81)
  (unrealized)
  (shape)
  (maps ((0) ((i i) (r r) (xr xr) (xi xi))))
  (origs))

(defskeleton station-nohint
  (vars (i r name) (xr rndx) (xi expt))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xi))
  (deflistener
    (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
      (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
  (precedes ((0 1) (1 0)) ((1 1) (0 2)))
  (absent (xr xi))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (operation encryption-test
    (added-listener
      (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
    (enc (enc (exp (gen) xi) (exp (gen) xr) (privk i))
      (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr)))) (0 2))
  (traces
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
          (enc (enc "resp" (exp (gen) xr) (exp (gen) xi) (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
          (enc (enc "resp" (exp (gen) xr) (exp (gen) xi) (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xi)) xr))))))
      (recv
        (enc (enc (exp (gen) xi) (exp (gen) xr) (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))))
    ((recv
       (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
         (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
      (send
        (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))))
  (label 83)
  (parent 81)
  (unrealized (0 2) (1 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton station-nohint
  (vars (i r name) (xr rndx) (xi expt))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xi))
  (deflistener
    (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
      (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
  (deflistener
    (cat "key" (hash "share" (exp (gen) (mul xr xi)) xr)
      (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
  (precedes ((0 1) (2 0)) ((1 1) (0 2)) ((2 1) (1 0)))
  (absent (xr xi))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (operation encryption-test
    (added-listener
      (cat "key" (hash "share" (exp (gen) (mul xr xi)) xr)
        (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
    (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
      (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))) (1 0))
  (traces
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
          (enc (enc "resp" (exp (gen) xr) (exp (gen) xi) (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
          (enc (enc "resp" (exp (gen) xr) (exp (gen) xi) (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xi)) xr))))))
      (recv
        (enc (enc (exp (gen) xi) (exp (gen) xr) (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))))
    ((recv
       (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
         (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
      (send
        (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr)))))
    ((recv
       (cat "key" (hash "share" (exp (gen) (mul xr xi)) xr)
         (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
      (send
        (cat "key" (hash "share" (exp (gen) (mul xr xi)) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))))
  (label 84)
  (parent 83)
  (unrealized (0 2) (2 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton station-nohint
  (vars (i r name) (xr rndx) (xi expt))
  (defstrand resp 3 (i i) (r r) (xr xr) (xi xi))
  (deflistener
    (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
      (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
  (deflistener
    (cat "key" (hash "share" (exp (gen) (mul xr xi)) xr)
      (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
  (deflistener (cat "share" (exp (gen) (mul xr xi)) xr))
  (precedes ((0 1) (3 0)) ((1 1) (0 2)) ((2 1) (1 0)) ((3 1) (2 0)))
  (absent (xr xi))
  (non-orig (privk i) (privk r))
  (uniq-gen xr)
  (operation encryption-test
    (added-listener (cat "share" (exp (gen) (mul xr xi)) xr))
    (hash "share" (exp (gen) (mul xr xi)) xr) (2 0))
  (traces
    ((recv (exp (gen) xi))
      (send
        (cat (exp (gen) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))
          (enc (enc "resp" (exp (gen) xr) (exp (gen) xi) (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
              (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
          (enc (enc "resp" (exp (gen) xr) (exp (gen) xi) (privk r))
            (hash "key" (hash "share" (exp (gen) (mul xr xi)) xi)
              (hash "hint"
                (hash "share" (exp (gen) (mul xr xi)) xr))))))
      (recv
        (enc (enc (exp (gen) xi) (exp (gen) xr) (privk i))
          (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
            (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))))
    ((recv
       (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
         (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
      (send
        (hash "key" (hash "share" (exp (gen) (mul xr xi)) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr)))))
    ((recv
       (cat "key" (hash "share" (exp (gen) (mul xr xi)) xr)
         (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr))))
      (send
        (cat "key" (hash "share" (exp (gen) (mul xr xi)) xr)
          (hash "hint" (hash "share" (exp (gen) (mul xr xi)) xr)))))
    ((recv (cat "share" (exp (gen) (mul xr xi)) xr))
      (send (cat "share" (exp (gen) (mul xr xi)) xr))))
  (label 85)
  (parent 84)
  (unrealized (0 2) (3 0))
  (dead)
  (comment "empty cohort"))

(comment "Nothing left to do")
