(herald "NHCR: unified model (UM)" (bound 20) (limit 8000)
  (algebra diffie-hellman))

(comment "CPSA 3.6.11")
(comment "All input read from tst/nhcr_um.scm")
(comment "Step count limited to 8000")
(comment "Strand count bounded at 20")

(defprotocol nhcr-umx diffie-hellman
  (defrole init
    (vars (ltxa ltxb x rndx) (y expt) (a b name) (na nb data)
      (hint1 hint2 mesg))
    (trace (recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    (uniq-orig na)
    (uniq-gen x)
    (fn-of ("ltx-of" (a ltxa) (b ltxb))
      ("principal-of" (ltxa a) (ltxb b)))
    (neq ((exp (gen) y) (gen)))
    (absent (x ltxa) (x ltxb)))
  (defrole resp
    (vars (ltxa ltxb y rndx) (x expt) (a b name) (na nb data))
    (trace (recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na a b (hash "public" x)))
      (send
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb))
    (uniq-orig nb)
    (uniq-gen y)
    (fn-of ("ltx-of" (a ltxa) (b ltxb))
      ("principal-of" (ltxa a) (ltxb b)))
    (neq ((exp (gen) x) (gen)))
    (absent (y ltxa) (y ltxb) (y x)))
  (defrole ltx-gen
    (vars (self name) (l rndx))
    (trace (send (cat self (hash "public" l))) (recv "end-of-protocol")
      (send l))
    (uniq-gen l)
    (fn-of ("ltx-of" (self l)) ("principal-of" (l self)))))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa ltxb x rndx)
    (y expt))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (precedes ((1 0) (0 0)))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (non-orig ltxb)
  (uniq-gen ltxb x)
  (uniq-orig na)
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((send (cat b (hash "public" ltxb)))))
  (label 0)
  (unrealized (0 2))
  (origs (na (0 1)))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (ltxb ltxa y x rndx))
  (defstrand init 4
    (hint1 (hash "hint" (hash "share" (hash "public" ltxa) y)))
    (hint2 (hash "hint" (hash "share" (hash "public" x) ltxb))) (na na)
    (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (defstrand resp 3 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (precedes ((0 1) (2 1)) ((1 0) (0 0)) ((1 0) (2 0)) ((2 2) (0 2)))
  (absent (y ltxa) (y ltxb) (y x) (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq ((exp (gen) x) (gen)) (a b) ((exp (gen) y) (gen)))
  (non-orig ltxb)
  (uniq-gen ltxb y x)
  (uniq-orig na nb)
  (operation encryption-test (added-strand resp 3)
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" y) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb))))) (0 2))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (send nb)) ((send (cat b (hash "public" ltxb))))
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na a b (hash "public" x)))
      (send
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint"
                  (hash "share" (hash "public" x) ltxb)))))))))
  (label 1)
  (parent 0)
  (unrealized)
  (shape)
  (maps
    ((0 1)
      ((ltxa ltxa) (ltxb ltxb) (a a) (b b) (x x) (y y) (na na) (nb nb)
        (hint1 (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hint2 (hash "hint" (hash "share" (hash "public" x) ltxb))))))
  (origs (nb (2 2)) (na (0 1))))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa ltxb x rndx)
    (y expt))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
  (precedes ((0 1) (2 0)) ((1 0) (0 0)) ((2 1) (0 2)))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (non-orig ltxb)
  (uniq-gen ltxb x)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
        (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
    (enc na nb a b hint1 hint2
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
        (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
    (0 2))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
         (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
          (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))))
  (label 2)
  (parent 0)
  (unrealized (2 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa ltxb x rndx)
    (y expt))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
  (precedes ((0 1) (3 0)) ((1 0) (0 0)) ((2 1) (0 2)) ((3 1) (2 0)))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (non-orig ltxb)
  (uniq-gen ltxb x)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (cat "key"
        (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
        (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" y) x) hint2)) (2 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
         (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
          (hash "nh_key" (hash "share" (hash "public" y) x) hint2))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
         (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
          (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))))
  (label 3)
  (parent 2)
  (unrealized (3 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa ltxb x rndx)
    (y expt))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
  (deflistener (cat "nh_key" (hash "share" (hash "public" y) x) hint2))
  (precedes ((0 1) (4 0)) ((1 0) (0 0)) ((2 1) (0 2)) ((3 1) (2 0))
    ((4 1) (3 0)))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (non-orig ltxb)
  (uniq-gen ltxb x)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (cat "nh_key" (hash "share" (hash "public" y) x) hint2))
    (hash "nh_key" (hash "share" (hash "public" y) x) hint2) (3 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
         (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
          (hash "nh_key" (hash "share" (hash "public" y) x) hint2))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
         (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
          (hash "nh_key" (hash "share" (hash "public" y) x) hint2))))
    ((recv (cat "nh_key" (hash "share" (hash "public" y) x) hint2))
      (send (cat "nh_key" (hash "share" (hash "public" y) x) hint2))))
  (label 4)
  (parent 3)
  (unrealized (4 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa ltxb x rndx)
    (y expt))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
  (deflistener (cat "nh_key" (hash "share" (hash "public" y) x) hint2))
  (deflistener (cat "share" (hash "public" y) x))
  (precedes ((0 1) (5 0)) ((1 0) (0 0)) ((2 1) (0 2)) ((3 1) (2 0))
    ((4 1) (3 0)) ((5 1) (4 0)))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (non-orig ltxb)
  (uniq-gen ltxb x)
  (uniq-orig na)
  (operation encryption-test
    (added-listener (cat "share" (hash "public" y) x))
    (hash "share" (hash "public" y) x) (4 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
         (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
          (hash "nh_key" (hash "share" (hash "public" y) x) hint2))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
         (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
          (hash "nh_key" (hash "share" (hash "public" y) x) hint2))))
    ((recv (cat "nh_key" (hash "share" (hash "public" y) x) hint2))
      (send (cat "nh_key" (hash "share" (hash "public" y) x) hint2)))
    ((recv (cat "share" (hash "public" y) x))
      (send (cat "share" (hash "public" y) x))))
  (label 5)
  (parent 4)
  (unrealized (5 0))
  (dead)
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol nhcr-umx diffie-hellman
  (defrole init
    (vars (ltxa ltxb x rndx) (y expt) (a b name) (na nb data)
      (hint1 hint2 mesg))
    (trace (recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    (uniq-orig na)
    (uniq-gen x)
    (fn-of ("ltx-of" (a ltxa) (b ltxb))
      ("principal-of" (ltxa a) (ltxb b)))
    (neq ((exp (gen) y) (gen)))
    (absent (x ltxa) (x ltxb)))
  (defrole resp
    (vars (ltxa ltxb y rndx) (x expt) (a b name) (na nb data))
    (trace (recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na a b (hash "public" x)))
      (send
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb))
    (uniq-orig nb)
    (uniq-gen y)
    (fn-of ("ltx-of" (a ltxa) (b ltxb))
      ("principal-of" (ltxa a) (ltxb b)))
    (neq ((exp (gen) x) (gen)))
    (absent (y ltxa) (y ltxb) (y x)))
  (defrole ltx-gen
    (vars (self name) (l rndx))
    (trace (send (cat self (hash "public" l))) (recv "end-of-protocol")
      (send l))
    (uniq-gen l)
    (fn-of ("ltx-of" (self l)) ("principal-of" (l self)))))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b self self-0 name)
    (ltxa ltxb x rndx) (y expt))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (defstrand ltx-gen 1 (self self) (l ltxa))
  (defstrand ltx-gen 1 (self self-0) (l ltxb))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (self-0 ltxb) (self ltxa) (a ltxa) (b ltxb))
    ("principal-of" (ltxb self-0) (ltxa self) (ltxa a) (ltxb b)))
  (neq (a b) ((exp (gen) y) (gen)))
  (non-orig ltxa ltxb)
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((send (cat self (hash "public" ltxa))))
    ((send (cat self-0 (hash "public" ltxb)))))
  (label 6)
  (unrealized (0 0) (0 2))
  (preskeleton)
  (origs (na (0 1)))
  (comment "Not a skeleton"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa ltxb x rndx)
    (y expt))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (precedes ((1 0) (0 0)) ((2 0) (0 0)))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (non-orig ltxa ltxb)
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((send (cat a (hash "public" ltxa))))
    ((send (cat b (hash "public" ltxb)))))
  (label 7)
  (parent 6)
  (unrealized (0 2))
  (origs (na (0 1)))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (ltxb ltxa y x rndx))
  (defstrand init 4
    (hint1 (hash "hint" (hash "share" (hash "public" ltxa) y)))
    (hint2 (hash "hint" (hash "share" (hash "public" x) ltxb))) (na na)
    (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (defstrand resp 3 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (precedes ((0 1) (3 1)) ((1 0) (0 0)) ((1 0) (3 0)) ((2 0) (0 0))
    ((2 0) (3 0)) ((3 2) (0 2)))
  (absent (y ltxa) (y ltxb) (y x) (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq ((exp (gen) x) (gen)) (a b) ((exp (gen) y) (gen)))
  (non-orig ltxb ltxa)
  (uniq-gen ltxb ltxa y x)
  (uniq-orig na nb)
  (operation encryption-test (added-strand resp 3)
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" y) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb))))) (0 2))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (send nb)) ((send (cat a (hash "public" ltxa))))
    ((send (cat b (hash "public" ltxb))))
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na a b (hash "public" x)))
      (send
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint"
                  (hash "share" (hash "public" x) ltxb)))))))))
  (label 8)
  (parent 7)
  (unrealized)
  (shape)
  (maps
    ((0 1 2)
      ((ltxa ltxa) (ltxb ltxb) (a a) (b b) (x x) (y y) (na na) (nb nb)
        (hint1 (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hint2 (hash "hint" (hash "share" (hash "public" x) ltxb))))))
  (origs (nb (3 2)) (na (0 1))))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa ltxb x rndx)
    (y expt))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
  (precedes ((0 1) (3 0)) ((1 0) (0 0)) ((2 0) (0 0)) ((3 1) (0 2)))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (non-orig ltxa ltxb)
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
        (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
    (enc na nb a b hint1 hint2
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
        (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
    (0 2))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((send (cat a (hash "public" ltxa))))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
         (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
          (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))))
  (label 9)
  (parent 7)
  (unrealized (3 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa ltxb x rndx)
    (y expt))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
  (precedes ((0 1) (4 0)) ((1 0) (0 0)) ((2 0) (0 0)) ((3 1) (0 2))
    ((4 1) (3 0)))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (non-orig ltxa ltxb)
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (cat "key"
        (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
        (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" y) x) hint2)) (3 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((send (cat a (hash "public" ltxa))))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
         (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
          (hash "nh_key" (hash "share" (hash "public" y) x) hint2))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
         (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
          (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))))
  (label 10)
  (parent 9)
  (unrealized (4 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa ltxb x rndx)
    (y expt))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1))
  (precedes ((0 1) (4 0)) ((1 0) (0 0)) ((1 0) (5 0)) ((2 0) (0 0))
    ((2 0) (5 0)) ((3 1) (0 2)) ((4 1) (3 0)) ((5 1) (4 0)))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (non-orig ltxa ltxb)
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1))
    (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
    (4 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((send (cat a (hash "public" ltxa))))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
         (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
          (hash "nh_key" (hash "share" (hash "public" y) x) hint2))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
         (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1)
          (hash "nh_key" (hash "share" (hash "public" y) x) hint2))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa) hint1))))
  (label 11)
  (parent 10)
  (unrealized (4 0) (5 0))
  (dead)
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol nhcr-umx diffie-hellman
  (defrole init
    (vars (ltxa ltxb x rndx) (y expt) (a b name) (na nb data)
      (hint1 hint2 mesg))
    (trace (recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    (uniq-orig na)
    (uniq-gen x)
    (fn-of ("ltx-of" (a ltxa) (b ltxb))
      ("principal-of" (ltxa a) (ltxb b)))
    (neq ((exp (gen) y) (gen)))
    (absent (x ltxa) (x ltxb)))
  (defrole resp
    (vars (ltxa ltxb y rndx) (x expt) (a b name) (na nb data))
    (trace (recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na a b (hash "public" x)))
      (send
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb))
    (uniq-orig nb)
    (uniq-gen y)
    (fn-of ("ltx-of" (a ltxa) (b ltxb))
      ("principal-of" (ltxa a) (ltxb b)))
    (neq ((exp (gen) x) (gen)))
    (absent (y ltxa) (y ltxb) (y x)))
  (defrole ltx-gen
    (vars (self name) (l rndx))
    (trace (send (cat self (hash "public" l))) (recv "end-of-protocol")
      (send l))
    (uniq-gen l)
    (fn-of ("ltx-of" (self l)) ("principal-of" (l self)))))

(defskeleton nhcr-umx
  (vars (na nb data) (a b self self-0 name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self self) (l ltxa))
  (defstrand ltx-gen 1 (self self-0) (l ltxb))
  (absent (y ltxa) (y ltxb) (y x))
  (fn-of ("ltx-of" (self-0 ltxb) (self ltxa) (a ltxa) (b ltxb))
    ("principal-of" (ltxb self-0) (ltxa self) (ltxa a) (ltxb b)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa ltxb)
  (uniq-gen ltxa ltxb y)
  (uniq-orig nb)
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na a b (hash "public" x)))
      (send
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat self (hash "public" ltxa))))
    ((send (cat self-0 (hash "public" ltxb)))))
  (label 12)
  (unrealized (0 0) (0 3))
  (preskeleton)
  (origs (nb (0 2)))
  (comment "Not a skeleton"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (precedes ((1 0) (0 0)) ((2 0) (0 0)))
  (absent (y ltxa) (y ltxb) (y x))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa ltxb)
  (uniq-gen ltxa ltxb y)
  (uniq-orig nb)
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na a b (hash "public" x)))
      (send
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((send (cat b (hash "public" ltxb)))))
  (label 13)
  (parent 12)
  (unrealized (0 3))
  (origs (nb (0 2)))
  (comment "5 in cohort - 5 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (ltxb ltxa y x rndx))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (defstrand init 4
    (hint1 (hash "hint" (hash "share" (hash "public" ltxa) y)))
    (hint2 (hash "hint" (hash "share" (hash "public" x) ltxb))) (na na)
    (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (precedes ((0 2) (3 2)) ((1 0) (0 0)) ((1 0) (3 0)) ((2 0) (0 0))
    ((2 0) (3 0)) ((3 1) (0 1)) ((3 3) (0 3)))
  (absent (x ltxa) (x ltxb) (y ltxa) (y ltxb) (y x))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq ((exp (gen) y) (gen)) (a b) ((exp (gen) x) (gen)))
  (non-orig ltxb ltxa)
  (uniq-gen ltxb ltxa y x)
  (uniq-orig na nb)
  (operation nonce-test (added-strand init 4) nb (0 3)
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" y) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) y)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" y) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) y)
          (hash "hint" (hash "share" (hash "public" x) ltxb))))))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na a b (hash "public" x)))
      (send
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((send (cat b (hash "public" ltxb))))
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (send nb)))
  (label 14)
  (parent 13)
  (unrealized)
  (shape)
  (maps
    ((0 1 2)
      ((ltxa ltxa) (ltxb ltxb) (a a) (b b) (y y) (x x) (na na)
        (nb nb))))
  (origs (na (3 1)) (nb (0 2))))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (precedes ((0 2) (3 0)) ((1 0) (0 0)) ((2 0) (0 0)) ((3 1) (0 3)))
  (absent (y ltxa) (y ltxb) (y x))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa ltxb)
  (uniq-gen ltxa ltxb y)
  (uniq-orig nb)
  (operation nonce-test
    (added-listener
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" y) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb))))) nb
    (0 3)
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" y) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) y)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" y) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) y)
          (hash "hint" (hash "share" (hash "public" x) ltxb))))))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na a b (hash "public" x)))
      (send
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" y) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" y) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
  (label 15)
  (parent 13)
  (unrealized (3 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (precedes ((0 2) (3 0)) ((1 0) (0 0)) ((2 0) (0 0)) ((3 1) (0 3)))
  (absent (y ltxa) (y ltxb) (y x))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa ltxb)
  (uniq-gen ltxa ltxb y)
  (uniq-orig nb)
  (operation nonce-test
    (added-listener
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) y)
          (hash "hint" (hash "share" (hash "public" x) ltxb))))) nb
    (0 3)
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" y) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) y)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" y) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) y)
          (hash "hint" (hash "share" (hash "public" x) ltxb))))))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na a b (hash "public" x)))
      (send
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
  (label 16)
  (parent 13)
  (unrealized (3 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (precedes ((0 2) (3 0)) ((1 0) (0 0)) ((2 0) (0 0)) ((3 1) (0 3)))
  (absent (y ltxa) (y ltxb) (y x))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa ltxb)
  (uniq-gen ltxa ltxb y)
  (uniq-orig nb)
  (operation nonce-test
    (added-listener
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" y) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb))))) nb
    (0 3)
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" y) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) y)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" y) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) y)
          (hash "hint" (hash "share" (hash "public" x) ltxb))))))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na a b (hash "public" x)))
      (send
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" y) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" y) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
  (label 17)
  (parent 13)
  (unrealized (3 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (precedes ((0 2) (3 0)) ((1 0) (0 0)) ((2 0) (0 0)) ((3 1) (0 3)))
  (absent (y ltxa) (y ltxb) (y x))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa ltxb)
  (uniq-gen ltxa ltxb y)
  (uniq-orig nb)
  (operation nonce-test
    (added-listener
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) y)
          (hash "hint" (hash "share" (hash "public" x) ltxb))))) nb
    (0 3)
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" y) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) y)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" y) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) y)
          (hash "hint" (hash "share" (hash "public" x) ltxb))))))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na a b (hash "public" x)))
      (send
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
  (label 18)
  (parent 13)
  (unrealized (3 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (precedes ((0 2) (4 0)) ((1 0) (0 0)) ((2 0) (0 0)) ((3 1) (0 3))
    ((4 1) (3 0)))
  (absent (y ltxa) (y ltxb) (y x))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa ltxb)
  (uniq-gen ltxa ltxb y)
  (uniq-orig nb)
  (operation encryption-test
    (added-listener
      (cat "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" y) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))) (3 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na a b (hash "public" x)))
      (send
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" y) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" y) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" y) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" y) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
  (label 19)
  (parent 15)
  (unrealized (4 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (precedes ((0 2) (4 0)) ((1 0) (0 0)) ((2 0) (0 0)) ((3 1) (0 3))
    ((4 1) (3 0)))
  (absent (y ltxa) (y ltxb) (y x))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa ltxb)
  (uniq-gen ltxa ltxb y)
  (uniq-orig nb)
  (operation encryption-test
    (added-listener
      (cat "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) y)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))) (3 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na a b (hash "public" x)))
      (send
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
  (label 20)
  (parent 16)
  (unrealized (4 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (precedes ((0 2) (4 0)) ((1 0) (0 0)) ((2 0) (0 0)) ((3 1) (0 3))
    ((4 1) (3 0)))
  (absent (y ltxa) (y ltxb) (y x))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa ltxb)
  (uniq-gen ltxa ltxb y)
  (uniq-orig nb)
  (operation encryption-test
    (added-listener
      (cat "key"
        (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" y) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))) (3 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na a b (hash "public" x)))
      (send
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" y) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" y) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" y) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" y) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
  (label 21)
  (parent 17)
  (unrealized (4 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (precedes ((0 2) (4 0)) ((1 0) (0 0)) ((2 0) (0 0)) ((3 1) (0 3))
    ((4 1) (3 0)))
  (absent (y ltxa) (y ltxb) (y x))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa ltxb)
  (uniq-gen ltxa ltxb y)
  (uniq-orig nb)
  (operation encryption-test
    (added-listener
      (cat "key"
        (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) y)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))) (3 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na a b (hash "public" x)))
      (send
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
  (label 22)
  (parent 18)
  (unrealized (4 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxa) ltxb)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (precedes ((0 2) (5 0)) ((1 0) (0 0)) ((2 0) (0 0)) ((3 1) (0 3))
    ((4 1) (3 0)) ((5 1) (4 0)))
  (absent (y ltxa) (y ltxb) (y x))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa ltxb)
  (uniq-gen ltxa ltxb y)
  (uniq-orig nb)
  (operation encryption-test
    (added-listener
      (cat "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y))))
    (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
      (hash "hint" (hash "share" (hash "public" ltxa) y))) (4 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na a b (hash "public" x)))
      (send
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" y) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" y) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" y) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" y) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxa) ltxb)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxa) ltxb)
          (hash "hint" (hash "share" (hash "public" ltxa) y))))))
  (label 23)
  (parent 19)
  (unrealized (5 0))
  (dead)
  (comment "empty cohort"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxa) ltxb)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (precedes ((0 2) (5 0)) ((1 0) (0 0)) ((2 0) (0 0)) ((3 1) (0 3))
    ((4 1) (3 0)) ((5 1) (4 0)))
  (absent (y ltxa) (y ltxb) (y x))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa ltxb)
  (uniq-gen ltxa ltxb y)
  (uniq-orig nb)
  (operation encryption-test
    (added-listener
      (cat "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y))))
    (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
      (hash "hint" (hash "share" (hash "public" ltxa) y))) (4 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na a b (hash "public" x)))
      (send
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxa) ltxb)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxa) ltxb)
          (hash "hint" (hash "share" (hash "public" ltxa) y))))))
  (label 24)
  (parent 20)
  (unrealized (4 0) (5 0))
  (dead)
  (comment "empty cohort"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (precedes ((0 2) (5 0)) ((1 0) (0 0)) ((2 0) (0 0)) ((3 1) (0 3))
    ((4 1) (3 0)) ((5 1) (4 0)))
  (absent (y ltxa) (y ltxb) (y x))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa ltxb)
  (uniq-gen ltxa ltxb y)
  (uniq-orig nb)
  (operation encryption-test
    (added-listener
      (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y))))
    (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
      (hash "hint" (hash "share" (hash "public" ltxa) y))) (4 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na a b (hash "public" x)))
      (send
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" y) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" y) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" y) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" y) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y))))))
  (label 25)
  (parent 21)
  (unrealized (5 0))
  (dead)
  (comment "empty cohort"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (precedes ((0 2) (5 0)) ((1 0) (0 0)) ((2 0) (0 0)) ((3 1) (0 3))
    ((4 1) (3 0)) ((5 1) (4 0)))
  (absent (y ltxa) (y ltxb) (y x))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa ltxb)
  (uniq-gen ltxa ltxb y)
  (uniq-orig nb)
  (operation encryption-test
    (added-listener
      (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y))))
    (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
      (hash "hint" (hash "share" (hash "public" ltxa) y))) (4 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na a b (hash "public" x)))
      (send
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y))))))
  (label 26)
  (parent 22)
  (unrealized (4 0) (5 0))
  (dead)
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol nhcr-umx diffie-hellman
  (defrole init
    (vars (ltxa ltxb x rndx) (y expt) (a b name) (na nb data)
      (hint1 hint2 mesg))
    (trace (recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    (uniq-orig na)
    (uniq-gen x)
    (fn-of ("ltx-of" (a ltxa) (b ltxb))
      ("principal-of" (ltxa a) (ltxb b)))
    (neq ((exp (gen) y) (gen)))
    (absent (x ltxa) (x ltxb)))
  (defrole resp
    (vars (ltxa ltxb y rndx) (x expt) (a b name) (na nb data))
    (trace (recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na a b (hash "public" x)))
      (send
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb))
    (uniq-orig nb)
    (uniq-gen y)
    (fn-of ("ltx-of" (a ltxa) (b ltxb))
      ("principal-of" (ltxa a) (ltxb b)))
    (neq ((exp (gen) x) (gen)))
    (absent (y ltxa) (y ltxb) (y x)))
  (defrole ltx-gen
    (vars (self name) (l rndx))
    (trace (send (cat self (hash "public" l))) (recv "end-of-protocol")
      (send l))
    (uniq-gen l)
    (fn-of ("ltx-of" (self l)) ("principal-of" (l self)))))

(defskeleton nhcr-umx
  (vars (na nb data) (a b self name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self self) (l ltxa))
  (absent (y ltxa) (y ltxb) (y x))
  (fn-of ("ltx-of" (self ltxa) (a ltxa) (b ltxb))
    ("principal-of" (ltxa self) (ltxa a) (ltxb b)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa)
  (uniq-gen ltxa y)
  (uniq-orig nb)
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na a b (hash "public" x)))
      (send
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat self (hash "public" ltxa)))))
  (label 27)
  (unrealized (0 0) (0 3))
  (preskeleton)
  (origs (nb (0 2)))
  (comment "Not a skeleton"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (precedes ((1 0) (0 0)))
  (absent (y ltxa) (y ltxb) (y x))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa)
  (uniq-gen ltxa y)
  (uniq-orig nb)
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na a b (hash "public" x)))
      (send
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa)))))
  (label 28)
  (parent 27)
  (unrealized)
  (shape)
  (maps
    ((0 1)
      ((ltxa ltxa) (ltxb ltxb) (a a) (b b) (y y) (x x) (na na)
        (nb nb))))
  (origs (nb (0 2))))

(comment "Nothing left to do")

(defprotocol nhcr-umx diffie-hellman
  (defrole init
    (vars (ltxa ltxb x rndx) (y expt) (a b name) (na nb data)
      (hint1 hint2 mesg))
    (trace (recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    (uniq-orig na)
    (uniq-gen x)
    (fn-of ("ltx-of" (a ltxa) (b ltxb))
      ("principal-of" (ltxa a) (ltxb b)))
    (neq ((exp (gen) y) (gen)))
    (absent (x ltxa) (x ltxb)))
  (defrole resp
    (vars (ltxa ltxb y rndx) (x expt) (a b name) (na nb data))
    (trace (recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na a b (hash "public" x)))
      (send
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb))
    (uniq-orig nb)
    (uniq-gen y)
    (fn-of ("ltx-of" (a ltxa) (b ltxb))
      ("principal-of" (ltxa a) (ltxb b)))
    (neq ((exp (gen) x) (gen)))
    (absent (y ltxa) (y ltxb) (y x)))
  (defrole ltx-gen
    (vars (self name) (l rndx))
    (trace (send (cat self (hash "public" l))) (recv "end-of-protocol")
      (send l))
    (uniq-gen l)
    (fn-of ("ltx-of" (self l)) ("principal-of" (l self)))))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b self self-0 name)
    (ltxa ltxb x rndx) (y expt))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self self) (l ltxa))
  (defstrand ltx-gen 3 (self self-0) (l ltxb))
  (precedes ((0 3) (2 1)) ((0 3) (3 1)))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (self-0 ltxb) (self ltxa) (a ltxa) (b ltxb))
    ("principal-of" (ltxb self-0) (ltxa self) (ltxa a) (ltxb b)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat self (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat self-0 (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb)))
  (label 29)
  (unrealized (0 0) (0 2) (1 0))
  (preskeleton)
  (origs (na (0 1)))
  (comment "Not a skeleton"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa ltxb x rndx)
    (y expt))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (precedes ((0 1) (1 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((3 0) (0 0)))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb)))
  (label 30)
  (parent 29)
  (unrealized (0 2) (1 0))
  (origs (na (0 1)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa ltxb x rndx)
    (y expt))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (precedes ((0 1) (4 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((3 0) (0 0)) ((4 1) (1 0)))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (cat "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) y)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))) (1 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
  (label 31)
  (parent 30)
  (unrealized (0 2) (4 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa ltxb x rndx)
    (y expt))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxa) ltxb)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (precedes ((0 1) (4 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (5 0)) ((3 0) (0 0)) ((3 0) (5 0)) ((4 1) (1 0))
    ((5 1) (4 0)))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (cat "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y))))
    (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
      (hash "hint" (hash "share" (hash "public" ltxa) y))) (4 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxa) ltxb)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxa) ltxb)
          (hash "hint" (hash "share" (hash "public" ltxa) y))))))
  (label 32)
  (parent 31)
  (unrealized (0 2) (4 0) (5 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa ltxb x rndx)
    (y expt))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxa) ltxb)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (deflistener (cat "share" (hash "public" ltxa) ltxb))
  (precedes ((0 1) (4 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (6 0)) ((3 0) (0 0)) ((3 0) (6 0)) ((4 1) (1 0))
    ((5 1) (4 0)) ((6 1) (5 0)))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (operation encryption-test
    (added-listener (cat "share" (hash "public" ltxa) ltxb))
    (hash "share" (hash "public" ltxa) ltxb) (5 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxa) ltxb)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxa) ltxb)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))))
    ((recv (cat "share" (hash "public" ltxa) ltxb))
      (send (cat "share" (hash "public" ltxa) ltxb))))
  (label 33)
  (parent 32)
  (unrealized (0 2) (6 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa x rndx)
    (y expt) (l rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb l) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) l)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) l)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l l))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) l)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) l)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxa) l)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (deflistener (cat "share" (hash "public" ltxa) l))
  (precedes ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0)) ((3 0) (0 0))
    ((3 2) (6 0)) ((4 1) (1 0)) ((5 1) (4 0)) ((6 1) (5 0)))
  (absent (x ltxa) (x l))
  (fn-of ("ltx-of" (b l) (a ltxa)) ("principal-of" (l b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa x l)
  (uniq-orig na)
  (operation nonce-test (displaced 7 3 ltx-gen 3) l (6 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" l)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" l) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) l)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) l)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) l)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) l))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" l))) (recv "end-of-protocol") (send l))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) l)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) l)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) l)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) l))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxa) l)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxa) l)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))))
    ((recv (cat "share" (hash "public" ltxa) l))
      (send (cat "share" (hash "public" ltxa) l))))
  (label 34)
  (parent 33)
  (unrealized (0 2))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (ltxb ltxa y x rndx))
  (defstrand init 4
    (hint1 (hash "hint" (hash "share" (hash "public" ltxa) y)))
    (hint2 (hash "hint" (hash "share" (hash "public" x) ltxb))) (na na)
    (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxa) ltxb)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (deflistener (cat "share" (hash "public" ltxa) ltxb))
  (defstrand resp 3 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (precedes ((0 1) (7 1)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (7 0)) ((3 0) (0 0)) ((3 0) (7 0)) ((3 2) (6 0))
    ((4 1) (1 0)) ((5 1) (4 0)) ((6 1) (5 0)) ((7 2) (0 2)))
  (absent (y ltxa) (y ltxb) (y x) (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq ((exp (gen) x) (gen)) (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxb ltxa y x)
  (uniq-orig na nb)
  (operation encryption-test (added-strand resp 3)
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" y) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb))))) (0 2))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxa) ltxb)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxa) ltxb)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))))
    ((recv (cat "share" (hash "public" ltxa) ltxb))
      (send (cat "share" (hash "public" ltxa) ltxb)))
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na a b (hash "public" x)))
      (send
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint"
                  (hash "share" (hash "public" x) ltxb)))))))))
  (label 35)
  (parent 34)
  (unrealized (4 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa x rndx)
    (y expt) (l rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb l) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) l)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) l)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l l))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) l)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) l)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxa) l)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (deflistener (cat "share" (hash "public" ltxa) l))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" l) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
  (precedes ((0 1) (7 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((3 0) (0 0)) ((3 2) (6 0)) ((4 1) (1 0)) ((5 1) (4 0))
    ((6 1) (5 0)) ((7 1) (0 2)))
  (absent (x ltxa) (x l))
  (fn-of ("ltx-of" (b l) (a ltxa)) ("principal-of" (l b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa x l)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" l) ltxa) hint1)
        (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
    (enc na nb a b hint1 hint2
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" l) ltxa) hint1)
        (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
    (0 2))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" l)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" l) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) l)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) l)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) l)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) l))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" l))) (recv "end-of-protocol") (send l))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) l)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) l)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) l)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) l))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxa) l)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxa) l)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))))
    ((recv (cat "share" (hash "public" ltxa) l))
      (send (cat "share" (hash "public" ltxa) l)))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" l) ltxa) hint1)
         (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" l) ltxa) hint1)
          (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))))
  (label 36)
  (parent 34)
  (unrealized (7 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (ltxb ltxa y x rndx))
  (defstrand init 4
    (hint1 (hash "hint" (hash "share" (hash "public" ltxa) y)))
    (hint2 (hash "hint" (hash "share" (hash "public" x) ltxb))) (na na)
    (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxa) ltxb)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (deflistener (cat "share" (hash "public" ltxa) ltxb))
  (defstrand resp 3 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) y)
      (hash "hint" (hash "share" (hash "public" x) ltxb))))
  (precedes ((0 1) (7 1)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (7 0)) ((3 0) (0 0)) ((3 0) (7 0)) ((3 2) (6 0))
    ((4 1) (1 0)) ((5 1) (4 0)) ((6 1) (5 0)) ((7 2) (0 2))
    ((7 2) (8 0)) ((8 1) (4 0)))
  (absent (y ltxa) (y ltxb) (y x) (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq ((exp (gen) x) (gen)) (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxb ltxa y x)
  (uniq-orig na nb)
  (operation encryption-test
    (added-listener
      (cat "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb))))
    (hash "nh_key" (hash "share" (hash "public" x) y)
      (hash "hint" (hash "share" (hash "public" x) ltxb))) (4 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxa) ltxb)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxa) ltxb)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))))
    ((recv (cat "share" (hash "public" ltxa) ltxb))
      (send (cat "share" (hash "public" ltxa) ltxb)))
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na a b (hash "public" x)))
      (send
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint"
                  (hash "share" (hash "public" x) ltxb))))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) y)
         (hash "hint" (hash "share" (hash "public" x) ltxb))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) y)
          (hash "hint" (hash "share" (hash "public" x) ltxb))))))
  (label 37)
  (parent 35)
  (unrealized (8 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa x rndx)
    (y expt) (l rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb l) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) l)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) l)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l l))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) l)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) l)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxa) l)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (deflistener (cat "share" (hash "public" ltxa) l))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" l) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" l) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
  (precedes ((0 1) (8 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((3 0) (0 0)) ((3 2) (6 0)) ((4 1) (1 0)) ((5 1) (4 0))
    ((6 1) (5 0)) ((7 1) (0 2)) ((8 1) (7 0)))
  (absent (x ltxa) (x l))
  (fn-of ("ltx-of" (b l) (a ltxa)) ("principal-of" (l b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa x l)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (cat "key"
        (hash "nh_key" (hash "share" (hash "public" l) ltxa) hint1)
        (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" l) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" y) x) hint2)) (7 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" l)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" l) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) l)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) l)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) l)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) l))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" l))) (recv "end-of-protocol") (send l))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) l)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) l)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) l)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) l))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxa) l)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxa) l)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))))
    ((recv (cat "share" (hash "public" ltxa) l))
      (send (cat "share" (hash "public" ltxa) l)))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" l) ltxa) hint1)
         (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" l) ltxa) hint1)
          (hash "nh_key" (hash "share" (hash "public" y) x) hint2))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" l) ltxa) hint1)
         (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" l) ltxa) hint1)
          (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))))
  (label 38)
  (parent 36)
  (unrealized (8 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (ltxb ltxa y x rndx))
  (defstrand init 4
    (hint1 (hash "hint" (hash "share" (hash "public" ltxa) y)))
    (hint2 (hash "hint" (hash "share" (hash "public" x) ltxb))) (na na)
    (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxa) ltxb)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (deflistener (cat "share" (hash "public" ltxa) ltxb))
  (defstrand resp 3 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) y)
      (hash "hint" (hash "share" (hash "public" x) ltxb))))
  (deflistener (cat "share" (hash "public" x) y))
  (precedes ((0 1) (7 1)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (7 0)) ((3 0) (0 0)) ((3 0) (7 0)) ((3 2) (6 0))
    ((4 1) (1 0)) ((5 1) (4 0)) ((6 1) (5 0)) ((7 2) (0 2))
    ((7 2) (9 0)) ((8 1) (4 0)) ((9 1) (8 0)))
  (absent (y ltxa) (y ltxb) (y x) (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq ((exp (gen) x) (gen)) (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxb ltxa y x)
  (uniq-orig na nb)
  (operation encryption-test
    (added-listener (cat "share" (hash "public" x) y))
    (hash "share" (hash "public" x) y) (8 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxa) ltxb)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxa) ltxb)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))))
    ((recv (cat "share" (hash "public" ltxa) ltxb))
      (send (cat "share" (hash "public" ltxa) ltxb)))
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na a b (hash "public" x)))
      (send
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint"
                  (hash "share" (hash "public" x) ltxb))))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) y)
         (hash "hint" (hash "share" (hash "public" x) ltxb))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) y)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    ((recv (cat "share" (hash "public" x) y))
      (send (cat "share" (hash "public" x) y))))
  (label 39)
  (parent 37)
  (unrealized (9 0))
  (dead)
  (comment "empty cohort"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa x rndx)
    (y expt) (l rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb l) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) l)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) l)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l l))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) l)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) l)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxa) l)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (deflistener (cat "share" (hash "public" ltxa) l))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" l) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" l) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" l) ltxa) hint1))
  (precedes ((0 1) (8 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (9 0)) ((3 0) (0 0)) ((3 0) (9 0)) ((3 2) (6 0))
    ((4 1) (1 0)) ((5 1) (4 0)) ((6 1) (5 0)) ((7 1) (0 2))
    ((8 1) (7 0)) ((9 1) (8 0)))
  (absent (x ltxa) (x l))
  (fn-of ("ltx-of" (b l) (a ltxa)) ("principal-of" (l b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa x l)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (cat "nh_key" (hash "share" (hash "public" l) ltxa) hint1))
    (hash "nh_key" (hash "share" (hash "public" l) ltxa) hint1) (8 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" l)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" l) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) l)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) l)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) l)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) l))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" l))) (recv "end-of-protocol") (send l))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) l)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) l)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) l)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) l))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxa) l)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxa) l)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))))
    ((recv (cat "share" (hash "public" ltxa) l))
      (send (cat "share" (hash "public" ltxa) l)))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" l) ltxa) hint1)
         (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" l) ltxa) hint1)
          (hash "nh_key" (hash "share" (hash "public" y) x) hint2))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" l) ltxa) hint1)
         (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" l) ltxa) hint1)
          (hash "nh_key" (hash "share" (hash "public" y) x) hint2))))
    ((recv (cat "nh_key" (hash "share" (hash "public" l) ltxa) hint1))
      (send
        (cat "nh_key" (hash "share" (hash "public" l) ltxa) hint1))))
  (label 40)
  (parent 38)
  (unrealized (8 0) (9 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa x rndx)
    (y expt) (l rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb l) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) l)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) l)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l l))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) l)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) l)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxa) l)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (deflistener (cat "share" (hash "public" ltxa) l))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" l) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" l) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" l) ltxa) hint1))
  (deflistener (cat "share" (hash "public" l) ltxa))
  (precedes ((0 1) (8 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (10 0)) ((3 0) (0 0)) ((3 0) (10 0)) ((3 2) (6 0))
    ((4 1) (1 0)) ((5 1) (4 0)) ((6 1) (5 0)) ((7 1) (0 2))
    ((8 1) (7 0)) ((9 1) (8 0)) ((10 1) (9 0)))
  (absent (x ltxa) (x l))
  (fn-of ("ltx-of" (b l) (a ltxa)) ("principal-of" (l b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa x l)
  (uniq-orig na)
  (operation encryption-test
    (added-listener (cat "share" (hash "public" l) ltxa))
    (hash "share" (hash "public" l) ltxa) (9 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" l)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" l) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) l)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) l)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) l)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) l))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" l))) (recv "end-of-protocol") (send l))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) l)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) l)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) l)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) l))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxa) l)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxa) l)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))))
    ((recv (cat "share" (hash "public" ltxa) l))
      (send (cat "share" (hash "public" ltxa) l)))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" l) ltxa) hint1)
         (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" l) ltxa) hint1)
          (hash "nh_key" (hash "share" (hash "public" y) x) hint2))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" l) ltxa) hint1)
         (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" l) ltxa) hint1)
          (hash "nh_key" (hash "share" (hash "public" y) x) hint2))))
    ((recv (cat "nh_key" (hash "share" (hash "public" l) ltxa) hint1))
      (send (cat "nh_key" (hash "share" (hash "public" l) ltxa) hint1)))
    ((recv (cat "share" (hash "public" l) ltxa))
      (send (cat "share" (hash "public" l) ltxa))))
  (label 41)
  (parent 40)
  (unrealized (8 0) (10 0))
  (dead)
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol nhcr-umx diffie-hellman
  (defrole init
    (vars (ltxa ltxb x rndx) (y expt) (a b name) (na nb data)
      (hint1 hint2 mesg))
    (trace (recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    (uniq-orig na)
    (uniq-gen x)
    (fn-of ("ltx-of" (a ltxa) (b ltxb))
      ("principal-of" (ltxa a) (ltxb b)))
    (neq ((exp (gen) y) (gen)))
    (absent (x ltxa) (x ltxb)))
  (defrole resp
    (vars (ltxa ltxb y rndx) (x expt) (a b name) (na nb data))
    (trace (recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na a b (hash "public" x)))
      (send
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb))
    (uniq-orig nb)
    (uniq-gen y)
    (fn-of ("ltx-of" (a ltxa) (b ltxb))
      ("principal-of" (ltxa a) (ltxb b)))
    (neq ((exp (gen) x) (gen)))
    (absent (y ltxa) (y ltxb) (y x)))
  (defrole ltx-gen
    (vars (self name) (l rndx))
    (trace (send (cat self (hash "public" l))) (recv "end-of-protocol")
      (send l))
    (uniq-gen l)
    (fn-of ("ltx-of" (self l)) ("principal-of" (l self)))))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b self self-0 name)
    (ltxa ltxb x rndx) (y expt))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self self) (l ltxa))
  (defstrand ltx-gen 3 (self self-0) (l ltxb))
  (precedes ((0 3) (2 1)) ((0 3) (3 1)))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (self-0 ltxb) (self ltxa) (a ltxa) (b ltxb))
    ("principal-of" (ltxb self-0) (ltxa self) (ltxa a) (ltxb b)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" y) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" y) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat self (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat self-0 (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb)))
  (label 42)
  (unrealized (0 0) (0 2) (1 0))
  (preskeleton)
  (origs (na (0 1)))
  (comment "Not a skeleton"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa ltxb x rndx)
    (y expt))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (precedes ((0 1) (1 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((3 0) (0 0)))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" y) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" y) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb)))
  (label 43)
  (parent 42)
  (unrealized (0 2) (1 0))
  (origs (na (0 1)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa ltxb x rndx)
    (y expt))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (precedes ((0 1) (4 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((3 0) (0 0)) ((4 1) (1 0)))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (cat "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" y) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))) (1 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" y) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" y) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" y) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" y) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
  (label 44)
  (parent 43)
  (unrealized (0 2) (4 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa ltxb x rndx)
    (y expt))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxa) ltxb)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (precedes ((0 1) (4 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (5 0)) ((3 0) (0 0)) ((3 0) (5 0)) ((4 1) (1 0))
    ((5 1) (4 0)))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (cat "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y))))
    (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
      (hash "hint" (hash "share" (hash "public" ltxa) y))) (4 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" y) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" y) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" y) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" y) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxa) ltxb)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxa) ltxb)
          (hash "hint" (hash "share" (hash "public" ltxa) y))))))
  (label 45)
  (parent 44)
  (unrealized (0 2) (4 0) (5 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa ltxb x rndx)
    (y expt))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxa) ltxb)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (deflistener (cat "share" (hash "public" ltxa) ltxb))
  (precedes ((0 1) (4 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (6 0)) ((3 0) (0 0)) ((3 0) (6 0)) ((4 1) (1 0))
    ((5 1) (4 0)) ((6 1) (5 0)))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (operation encryption-test
    (added-listener (cat "share" (hash "public" ltxa) ltxb))
    (hash "share" (hash "public" ltxa) ltxb) (5 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" y) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" y) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" y) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" y) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxa) ltxb)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxa) ltxb)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))))
    ((recv (cat "share" (hash "public" ltxa) ltxb))
      (send (cat "share" (hash "public" ltxa) ltxb))))
  (label 46)
  (parent 45)
  (unrealized (0 2) (4 0) (6 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa x rndx)
    (y expt) (l rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb l) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) l)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) l)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l l))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) l)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) l)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxa) l)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (deflistener (cat "share" (hash "public" ltxa) l))
  (precedes ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0)) ((3 0) (0 0))
    ((3 2) (6 0)) ((4 1) (1 0)) ((5 1) (4 0)) ((6 1) (5 0)))
  (absent (x ltxa) (x l))
  (fn-of ("ltx-of" (b l) (a ltxa)) ("principal-of" (l b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa x l)
  (uniq-orig na)
  (operation nonce-test (displaced 7 3 ltx-gen 3) l (6 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" l)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" l) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) l)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" y) x)
           (hash "hint" (hash "share" (hash "public" x) l)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) l)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" y) x)
            (hash "hint" (hash "share" (hash "public" x) l))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" l))) (recv "end-of-protocol") (send l))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) l)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" y) x)
           (hash "hint" (hash "share" (hash "public" x) l)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) l)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" y) x)
            (hash "hint" (hash "share" (hash "public" x) l))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxa) l)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxa) l)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))))
    ((recv (cat "share" (hash "public" ltxa) l))
      (send (cat "share" (hash "public" ltxa) l))))
  (label 47)
  (parent 46)
  (unrealized (0 2) (4 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa x rndx)
    (y expt) (l rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb l) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) l)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) l)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l l))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) l)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) l)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxa) l)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (deflistener (cat "share" (hash "public" ltxa) l))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" y) x)
      (hash "hint" (hash "share" (hash "public" x) l))))
  (precedes ((0 1) (7 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((3 0) (0 0)) ((3 2) (6 0)) ((4 1) (1 0)) ((5 1) (4 0))
    ((6 1) (5 0)) ((7 1) (4 0)))
  (absent (x ltxa) (x l))
  (fn-of ("ltx-of" (b l) (a ltxa)) ("principal-of" (l b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa x l)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (cat "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) l))))
    (hash "nh_key" (hash "share" (hash "public" y) x)
      (hash "hint" (hash "share" (hash "public" x) l))) (4 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" l)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" l) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) l)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" y) x)
           (hash "hint" (hash "share" (hash "public" x) l)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) l)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" y) x)
            (hash "hint" (hash "share" (hash "public" x) l))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" l))) (recv "end-of-protocol") (send l))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) l)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" y) x)
           (hash "hint" (hash "share" (hash "public" x) l)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) l)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" y) x)
            (hash "hint" (hash "share" (hash "public" x) l))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxa) l)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxa) l)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))))
    ((recv (cat "share" (hash "public" ltxa) l))
      (send (cat "share" (hash "public" ltxa) l)))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" y) x)
         (hash "hint" (hash "share" (hash "public" x) l))))
      (send
        (cat "nh_key" (hash "share" (hash "public" y) x)
          (hash "hint" (hash "share" (hash "public" x) l))))))
  (label 48)
  (parent 47)
  (unrealized (0 2) (7 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa x rndx)
    (y expt) (l rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb l) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) l)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) l)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l l))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) l)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) l)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxa) l)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (deflistener (cat "share" (hash "public" ltxa) l))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" y) x)
      (hash "hint" (hash "share" (hash "public" x) l))))
  (deflistener (cat "share" (hash "public" y) x))
  (precedes ((0 1) (8 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((3 0) (0 0)) ((3 2) (6 0)) ((4 1) (1 0)) ((5 1) (4 0))
    ((6 1) (5 0)) ((7 1) (4 0)) ((8 1) (7 0)))
  (absent (x ltxa) (x l))
  (fn-of ("ltx-of" (b l) (a ltxa)) ("principal-of" (l b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa x l)
  (uniq-orig na)
  (operation encryption-test
    (added-listener (cat "share" (hash "public" y) x))
    (hash "share" (hash "public" y) x) (7 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" l)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" l) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) l)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" y) x)
           (hash "hint" (hash "share" (hash "public" x) l)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) l)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" y) x)
            (hash "hint" (hash "share" (hash "public" x) l))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" l))) (recv "end-of-protocol") (send l))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) l)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" y) x)
           (hash "hint" (hash "share" (hash "public" x) l)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) l)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" y) x)
            (hash "hint" (hash "share" (hash "public" x) l))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxa) l)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxa) l)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))))
    ((recv (cat "share" (hash "public" ltxa) l))
      (send (cat "share" (hash "public" ltxa) l)))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" y) x)
         (hash "hint" (hash "share" (hash "public" x) l))))
      (send
        (cat "nh_key" (hash "share" (hash "public" y) x)
          (hash "hint" (hash "share" (hash "public" x) l)))))
    ((recv (cat "share" (hash "public" y) x))
      (send (cat "share" (hash "public" y) x))))
  (label 49)
  (parent 48)
  (unrealized (0 2) (7 0) (8 0))
  (dead)
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol nhcr-umx diffie-hellman
  (defrole init
    (vars (ltxa ltxb x rndx) (y expt) (a b name) (na nb data)
      (hint1 hint2 mesg))
    (trace (recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    (uniq-orig na)
    (uniq-gen x)
    (fn-of ("ltx-of" (a ltxa) (b ltxb))
      ("principal-of" (ltxa a) (ltxb b)))
    (neq ((exp (gen) y) (gen)))
    (absent (x ltxa) (x ltxb)))
  (defrole resp
    (vars (ltxa ltxb y rndx) (x expt) (a b name) (na nb data))
    (trace (recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na a b (hash "public" x)))
      (send
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb))
    (uniq-orig nb)
    (uniq-gen y)
    (fn-of ("ltx-of" (a ltxa) (b ltxb))
      ("principal-of" (ltxa a) (ltxb b)))
    (neq ((exp (gen) x) (gen)))
    (absent (y ltxa) (y ltxb) (y x)))
  (defrole ltx-gen
    (vars (self name) (l rndx))
    (trace (send (cat self (hash "public" l))) (recv "end-of-protocol")
      (send l))
    (uniq-gen l)
    (fn-of ("ltx-of" (self l)) ("principal-of" (l self)))))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b self self-0 name)
    (ltxa ltxb x rndx) (y expt))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self self) (l ltxa))
  (defstrand ltx-gen 3 (self self-0) (l ltxb))
  (precedes ((0 3) (2 1)) ((0 3) (3 1)))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (self-0 ltxb) (self ltxa) (a ltxa) (b ltxb))
    ("principal-of" (ltxb self-0) (ltxa self) (ltxa a) (ltxb b)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat self (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat self-0 (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb)))
  (label 50)
  (unrealized (0 0) (0 2) (1 0))
  (preskeleton)
  (origs (na (0 1)))
  (comment "Not a skeleton"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa ltxb x rndx)
    (y expt))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (precedes ((0 1) (1 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((3 0) (0 0)))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb)))
  (label 51)
  (parent 50)
  (unrealized (0 2) (1 0))
  (origs (na (0 1)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa ltxb x rndx)
    (y expt))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (precedes ((0 1) (4 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((3 0) (0 0)) ((4 1) (1 0)))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (cat "key"
        (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) y)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))) (1 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
  (label 52)
  (parent 51)
  (unrealized (0 2) (4 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa ltxb x rndx)
    (y expt))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (precedes ((0 1) (4 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (5 0)) ((3 0) (0 0)) ((3 0) (5 0)) ((4 1) (1 0))
    ((5 1) (4 0)))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y))))
    (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
      (hash "hint" (hash "share" (hash "public" ltxa) y))) (4 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y))))))
  (label 53)
  (parent 52)
  (unrealized (0 2) (4 0) (5 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa ltxb x rndx)
    (y expt))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (deflistener (cat "share" (hash "public" ltxb) ltxa))
  (precedes ((0 1) (4 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (6 0)) ((3 0) (0 0)) ((3 0) (6 0)) ((4 1) (1 0))
    ((5 1) (4 0)) ((6 1) (5 0)))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (operation encryption-test
    (added-listener (cat "share" (hash "public" ltxb) ltxa))
    (hash "share" (hash "public" ltxb) ltxa) (5 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))))
    ((recv (cat "share" (hash "public" ltxb) ltxa))
      (send (cat "share" (hash "public" ltxb) ltxa))))
  (label 54)
  (parent 53)
  (unrealized (0 2) (4 0) (6 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxb x rndx)
    (y expt) (l rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa l) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l l))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxb) l)
      (hash "hint" (hash "share" (hash "public" l) y))))
  (deflistener (cat "share" (hash "public" ltxb) l))
  (precedes ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0)) ((2 2) (6 0))
    ((3 0) (0 0)) ((4 1) (1 0)) ((5 1) (4 0)) ((6 1) (5 0)))
  (absent (x l) (x ltxb))
  (fn-of ("ltx-of" (a l) (b ltxb)) ("principal-of" (l a) (ltxb b)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxb x l)
  (uniq-orig na)
  (operation nonce-test (displaced 7 2 ltx-gen 3) l (6 0))
  (traces
    ((recv (cat (hash "public" l) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) l)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" l))) (recv "end-of-protocol") (send l))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxb) l)
         (hash "hint" (hash "share" (hash "public" l) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxb) l)
          (hash "hint" (hash "share" (hash "public" l) y)))))
    ((recv (cat "share" (hash "public" ltxb) l))
      (send (cat "share" (hash "public" ltxb) l))))
  (label 55)
  (parent 54)
  (unrealized (0 2) (4 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxb x rndx)
    (y expt) (l rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa l) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l l))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxb) l)
      (hash "hint" (hash "share" (hash "public" l) y))))
  (deflistener (cat "share" (hash "public" ltxb) l))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) y)
      (hash "hint" (hash "share" (hash "public" x) ltxb))))
  (precedes ((0 1) (7 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 2) (6 0)) ((3 0) (0 0)) ((4 1) (1 0)) ((5 1) (4 0))
    ((6 1) (5 0)) ((7 1) (4 0)))
  (absent (x l) (x ltxb))
  (fn-of ("ltx-of" (a l) (b ltxb)) ("principal-of" (l a) (ltxb b)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxb x l)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (cat "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb))))
    (hash "nh_key" (hash "share" (hash "public" x) y)
      (hash "hint" (hash "share" (hash "public" x) ltxb))) (4 0))
  (traces
    ((recv (cat (hash "public" l) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) l)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" l))) (recv "end-of-protocol") (send l))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxb) l)
         (hash "hint" (hash "share" (hash "public" l) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxb) l)
          (hash "hint" (hash "share" (hash "public" l) y)))))
    ((recv (cat "share" (hash "public" ltxb) l))
      (send (cat "share" (hash "public" ltxb) l)))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) y)
         (hash "hint" (hash "share" (hash "public" x) ltxb))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) y)
          (hash "hint" (hash "share" (hash "public" x) ltxb))))))
  (label 56)
  (parent 55)
  (unrealized (0 2) (7 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb na-0 nb-0 data) (a b a-0 name)
    (y expt) (l ltxa y-0 x ltxb rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa l) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l l))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxb) l)
      (hash "hint" (hash "share" (hash "public" l) y))))
  (deflistener (cat "share" (hash "public" ltxb) l))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) y)
      (hash "hint" (hash "share" (hash "public" x) ltxb))))
  (defstrand resp 3 (na na-0) (nb nb-0) (a a-0) (b b) (ltxa ltxa)
    (ltxb ltxb) (y y-0) (x x))
  (precedes ((0 1) (8 1)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 2) (6 0)) ((3 0) (0 0)) ((3 0) (8 0)) ((4 1) (1 0))
    ((5 1) (4 0)) ((6 1) (5 0)) ((7 1) (4 0)) ((8 2) (7 0)))
  (absent (y-0 ltxa) (y-0 ltxb) (y-0 x) (x l) (x ltxb))
  (fn-of ("ltx-of" (a-0 ltxa) (b ltxb) (a l))
    ("principal-of" (ltxa a-0) (ltxb b) (l a)))
  (neq ((exp (gen) x) (gen)) (a b) ((exp (gen) y) (gen)))
  (uniq-gen l y-0 x ltxb)
  (uniq-orig na nb-0)
  (operation encryption-test (added-strand resp 3)
    (hash "hint" (hash "share" (hash "public" x) ltxb)) (7 0))
  (traces
    ((recv (cat (hash "public" l) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) l)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" l))) (recv "end-of-protocol") (send l))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxb) l)
         (hash "hint" (hash "share" (hash "public" l) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxb) l)
          (hash "hint" (hash "share" (hash "public" l) y)))))
    ((recv (cat "share" (hash "public" ltxb) l))
      (send (cat "share" (hash "public" ltxb) l)))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) y)
         (hash "hint" (hash "share" (hash "public" x) ltxb))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) y)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na-0 a-0 b (hash "public" x)))
      (send
        (cat (hash "public" y-0)
          (hash "hint" (hash "share" (hash "public" ltxa) y-0))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na-0 nb-0 a-0 b
            (hash "hint" (hash "share" (hash "public" ltxa) y-0))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y-0)))
              (hash "nh_key" (hash "share" (hash "public" x) y-0)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na-0 nb-0 a-0 b
            (hash "hint" (hash "share" (hash "public" ltxa) y-0))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y-0)))
              (hash "nh_key" (hash "share" (hash "public" y-0) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na-0 nb-0 a-0 b
            (hash "hint" (hash "share" (hash "public" ltxa) y-0))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y-0)))
              (hash "nh_key" (hash "share" (hash "public" x) y-0)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na-0 nb-0 a-0 b
            (hash "hint" (hash "share" (hash "public" ltxa) y-0))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y-0)))
              (hash "nh_key" (hash "share" (hash "public" y-0) x)
                (hash "hint"
                  (hash "share" (hash "public" x) ltxb)))))))))
  (label 57)
  (parent 56)
  (unrealized (0 2))
  (comment "3 in cohort - 3 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxb x rndx)
    (y expt) (l rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa l) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l l))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxb) l)
      (hash "hint" (hash "share" (hash "public" l) y))))
  (deflistener (cat "share" (hash "public" ltxb) l))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) y)
      (hash "hint" (hash "share" (hash "public" x) ltxb))))
  (deflistener (cat "hint" (hash "share" (hash "public" x) ltxb)))
  (precedes ((0 1) (8 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 2) (6 0)) ((3 0) (0 0)) ((4 1) (1 0)) ((5 1) (4 0))
    ((6 1) (5 0)) ((7 1) (4 0)) ((8 1) (7 0)))
  (absent (x l) (x ltxb))
  (fn-of ("ltx-of" (a l) (b ltxb)) ("principal-of" (l a) (ltxb b)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxb x l)
  (uniq-orig na)
  (operation encryption-test
    (added-listener (cat "hint" (hash "share" (hash "public" x) ltxb)))
    (hash "hint" (hash "share" (hash "public" x) ltxb)) (7 0))
  (traces
    ((recv (cat (hash "public" l) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) l)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" l))) (recv "end-of-protocol") (send l))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxb) l)
         (hash "hint" (hash "share" (hash "public" l) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxb) l)
          (hash "hint" (hash "share" (hash "public" l) y)))))
    ((recv (cat "share" (hash "public" ltxb) l))
      (send (cat "share" (hash "public" ltxb) l)))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) y)
         (hash "hint" (hash "share" (hash "public" x) ltxb))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) y)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    ((recv (cat "hint" (hash "share" (hash "public" x) ltxb)))
      (send (cat "hint" (hash "share" (hash "public" x) ltxb)))))
  (label 58)
  (parent 56)
  (unrealized (0 2) (8 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (b a name) (ltxb x ltxa y rndx))
  (defstrand init 4
    (hint1 (hash "hint" (hash "share" (hash "public" ltxa) y)))
    (hint2 (hash "hint" (hash "share" (hash "public" x) ltxb))) (na na)
    (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (deflistener (cat "share" (hash "public" ltxb) ltxa))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) y)
      (hash "hint" (hash "share" (hash "public" x) ltxb))))
  (defstrand resp 3 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (precedes ((0 1) (8 1)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (8 0)) ((2 2) (6 0)) ((3 0) (0 0)) ((3 0) (8 0))
    ((4 1) (1 0)) ((5 1) (4 0)) ((6 1) (5 0)) ((7 1) (4 0))
    ((8 2) (0 2)) ((8 2) (7 0)))
  (absent (y ltxa) (y ltxb) (y x) (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq ((exp (gen) x) (gen)) (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxb x ltxa y)
  (uniq-orig na nb)
  (operation encryption-test (displaced 9 8 resp 3)
    (enc na-0 nb-0 a-0 b
      (hash "hint" (hash "share" (hash "public" ltxa-0) y-0))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa-0)
          (hash "hint" (hash "share" (hash "public" ltxa-0) y-0)))
        (hash "nh_key" (hash "share" (hash "public" y-0) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb))))) (0 2))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))))
    ((recv (cat "share" (hash "public" ltxb) ltxa))
      (send (cat "share" (hash "public" ltxb) ltxa)))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) y)
         (hash "hint" (hash "share" (hash "public" x) ltxb))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) y)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na a b (hash "public" x)))
      (send
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint"
                  (hash "share" (hash "public" x) ltxb)))))))))
  (label 59)
  (parent 57)
  (unrealized (7 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb na-0 nb-0 data) (a b a-0 name)
    (ltxa y ltxb ltxa-0 y-0 x rndx))
  (defstrand init 4
    (hint1 (hash "hint" (hash "share" (hash "public" ltxa-0) y-0)))
    (hint2 (hash "hint" (hash "share" (hash "public" x) ltxb))) (na na)
    (nb nb) (a a) (b b) (ltxa ltxa-0) (ltxb ltxb) (x x) (y y-0))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa-0)
        (hash "hint" (hash "share" (hash "public" ltxa-0) y-0)))
      (hash "nh_key" (hash "share" (hash "public" x) y-0)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa-0))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa-0)
        (hash "hint" (hash "share" (hash "public" ltxa-0) y-0)))
      (hash "nh_key" (hash "share" (hash "public" x) y-0)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa-0)
      (hash "hint" (hash "share" (hash "public" ltxa-0) y-0))))
  (deflistener (cat "share" (hash "public" ltxb) ltxa-0))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) y-0)
      (hash "hint" (hash "share" (hash "public" x) ltxb))))
  (defstrand resp 3 (na na-0) (nb nb-0) (a a-0) (b b) (ltxa ltxa)
    (ltxb ltxb) (y y) (x x))
  (defstrand resp 3 (na na) (nb nb) (a a) (b b) (ltxa ltxa-0)
    (ltxb ltxb) (y y-0) (x x))
  (precedes ((0 1) (8 1)) ((0 1) (9 1)) ((0 3) (2 1)) ((0 3) (3 1))
    ((2 0) (0 0)) ((2 0) (9 0)) ((2 2) (6 0)) ((3 0) (0 0))
    ((3 0) (8 0)) ((3 0) (9 0)) ((4 1) (1 0)) ((5 1) (4 0))
    ((6 1) (5 0)) ((7 1) (4 0)) ((8 2) (7 0)) ((9 2) (0 2))
    ((9 2) (7 0)))
  (absent (y-0 ltxa-0) (y-0 ltxb) (y-0 x) (y ltxa) (y ltxb) (y x)
    (x ltxa-0) (x ltxb))
  (fn-of ("ltx-of" (a ltxa-0) (b ltxb) (a-0 ltxa))
    ("principal-of" (ltxa-0 a) (ltxb b) (ltxa a-0)))
  (neq ((exp (gen) x) (gen)) (a b) ((exp (gen) y-0) (gen)))
  (uniq-gen y ltxb ltxa-0 y-0 x)
  (uniq-orig na nb nb-0)
  (operation encryption-test (added-strand resp 3)
    (enc na nb a b
      (hash "hint" (hash "share" (hash "public" ltxa-0) y-0))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa-0)
          (hash "hint" (hash "share" (hash "public" ltxa-0) y-0)))
        (hash "nh_key" (hash "share" (hash "public" y-0) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb))))) (0 2))
  (traces
    ((recv (cat (hash "public" ltxa-0) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y-0)
          (hash "hint" (hash "share" (hash "public" ltxa-0) y-0))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa-0) y-0))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa-0)
                (hash "hint" (hash "share" (hash "public" ltxa-0) y-0)))
              (hash "nh_key" (hash "share" (hash "public" y-0) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa-0)
           (hash "hint" (hash "share" (hash "public" ltxa-0) y-0)))
         (hash "nh_key" (hash "share" (hash "public" x) y-0)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa-0)
            (hash "hint" (hash "share" (hash "public" ltxa-0) y-0)))
          (hash "nh_key" (hash "share" (hash "public" x) y-0)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa-0))) (recv "end-of-protocol")
      (send ltxa-0))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa-0)
           (hash "hint" (hash "share" (hash "public" ltxa-0) y-0)))
         (hash "nh_key" (hash "share" (hash "public" x) y-0)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa-0)
            (hash "hint" (hash "share" (hash "public" ltxa-0) y-0)))
          (hash "nh_key" (hash "share" (hash "public" x) y-0)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa-0)
         (hash "hint" (hash "share" (hash "public" ltxa-0) y-0))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa-0)
          (hash "hint" (hash "share" (hash "public" ltxa-0) y-0)))))
    ((recv (cat "share" (hash "public" ltxb) ltxa-0))
      (send (cat "share" (hash "public" ltxb) ltxa-0)))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) y-0)
         (hash "hint" (hash "share" (hash "public" x) ltxb))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) y-0)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na-0 a-0 b (hash "public" x)))
      (send
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na-0 nb-0 a-0 b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na-0 nb-0 a-0 b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na-0 nb-0 a-0 b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na-0 nb-0 a-0 b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint"
                  (hash "share" (hash "public" x) ltxb))))))))
    ((recv (cat (hash "public" ltxa-0) (hash "public" ltxb)))
      (recv (cat na a b (hash "public" x)))
      (send
        (cat (hash "public" y-0)
          (hash "hint" (hash "share" (hash "public" ltxa-0) y-0))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa-0) y-0))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa-0) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa-0) y-0)))
              (hash "nh_key" (hash "share" (hash "public" x) y-0)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa-0) y-0))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa-0) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa-0) y-0)))
              (hash "nh_key" (hash "share" (hash "public" y-0) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa-0) y-0))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa-0)
                (hash "hint" (hash "share" (hash "public" ltxa-0) y-0)))
              (hash "nh_key" (hash "share" (hash "public" x) y-0)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa-0) y-0))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa-0)
                (hash "hint" (hash "share" (hash "public" ltxa-0) y-0)))
              (hash "nh_key" (hash "share" (hash "public" y-0) x)
                (hash "hint"
                  (hash "share" (hash "public" x) ltxb)))))))))
  (label 60)
  (parent 57)
  (unrealized (7 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb na-0 nb-0 data) (a b a-0 name)
    (y expt) (l ltxa y-0 x ltxb rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa l) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l l))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxb) l)
      (hash "hint" (hash "share" (hash "public" l) y))))
  (deflistener (cat "share" (hash "public" ltxb) l))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) y)
      (hash "hint" (hash "share" (hash "public" x) ltxb))))
  (defstrand resp 3 (na na-0) (nb nb-0) (a a-0) (b b) (ltxa ltxa)
    (ltxb ltxb) (y y-0) (x x))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) l) hint1)
      (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
  (precedes ((0 1) (8 1)) ((0 1) (9 0)) ((0 3) (2 1)) ((0 3) (3 1))
    ((2 0) (0 0)) ((2 2) (6 0)) ((3 0) (0 0)) ((3 0) (8 0))
    ((4 1) (1 0)) ((5 1) (4 0)) ((6 1) (5 0)) ((7 1) (4 0))
    ((8 2) (7 0)) ((9 1) (0 2)))
  (absent (y-0 ltxa) (y-0 ltxb) (y-0 x) (x l) (x ltxb))
  (fn-of ("ltx-of" (a-0 ltxa) (b ltxb) (a l))
    ("principal-of" (ltxa a-0) (ltxb b) (l a)))
  (neq ((exp (gen) x) (gen)) (a b) ((exp (gen) y) (gen)))
  (uniq-gen l y-0 x ltxb)
  (uniq-orig na nb-0)
  (operation encryption-test
    (added-listener
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxb) l) hint1)
        (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
    (enc na nb a b hint1 hint2
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxb) l) hint1)
        (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
    (0 2))
  (traces
    ((recv (cat (hash "public" l) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) l)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" l))) (recv "end-of-protocol") (send l))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxb) l)
         (hash "hint" (hash "share" (hash "public" l) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxb) l)
          (hash "hint" (hash "share" (hash "public" l) y)))))
    ((recv (cat "share" (hash "public" ltxb) l))
      (send (cat "share" (hash "public" ltxb) l)))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) y)
         (hash "hint" (hash "share" (hash "public" x) ltxb))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) y)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na-0 a-0 b (hash "public" x)))
      (send
        (cat (hash "public" y-0)
          (hash "hint" (hash "share" (hash "public" ltxa) y-0))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na-0 nb-0 a-0 b
            (hash "hint" (hash "share" (hash "public" ltxa) y-0))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y-0)))
              (hash "nh_key" (hash "share" (hash "public" x) y-0)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na-0 nb-0 a-0 b
            (hash "hint" (hash "share" (hash "public" ltxa) y-0))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y-0)))
              (hash "nh_key" (hash "share" (hash "public" y-0) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na-0 nb-0 a-0 b
            (hash "hint" (hash "share" (hash "public" ltxa) y-0))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y-0)))
              (hash "nh_key" (hash "share" (hash "public" x) y-0)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na-0 nb-0 a-0 b
            (hash "hint" (hash "share" (hash "public" ltxa) y-0))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y-0)))
              (hash "nh_key" (hash "share" (hash "public" y-0) x)
                (hash "hint"
                  (hash "share" (hash "public" x) ltxb))))))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) l) hint1)
         (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) l) hint1)
          (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))))
  (label 61)
  (parent 57)
  (unrealized (9 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxb x rndx)
    (y expt) (l rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa l) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l l))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxb) l)
      (hash "hint" (hash "share" (hash "public" l) y))))
  (deflistener (cat "share" (hash "public" ltxb) l))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) y)
      (hash "hint" (hash "share" (hash "public" x) ltxb))))
  (deflistener (cat "hint" (hash "share" (hash "public" x) ltxb)))
  (deflistener (cat "share" (hash "public" x) ltxb))
  (precedes ((0 1) (9 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 2) (6 0)) ((3 0) (0 0)) ((4 1) (1 0)) ((5 1) (4 0))
    ((6 1) (5 0)) ((7 1) (4 0)) ((8 1) (7 0)) ((9 1) (8 0)))
  (absent (x l) (x ltxb))
  (fn-of ("ltx-of" (a l) (b ltxb)) ("principal-of" (l a) (ltxb b)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxb x l)
  (uniq-orig na)
  (operation encryption-test
    (added-listener (cat "share" (hash "public" x) ltxb))
    (hash "share" (hash "public" x) ltxb) (8 0))
  (traces
    ((recv (cat (hash "public" l) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) l)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" l))) (recv "end-of-protocol") (send l))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxb) l)
         (hash "hint" (hash "share" (hash "public" l) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxb) l)
          (hash "hint" (hash "share" (hash "public" l) y)))))
    ((recv (cat "share" (hash "public" ltxb) l))
      (send (cat "share" (hash "public" ltxb) l)))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) y)
         (hash "hint" (hash "share" (hash "public" x) ltxb))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) y)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    ((recv (cat "hint" (hash "share" (hash "public" x) ltxb)))
      (send (cat "hint" (hash "share" (hash "public" x) ltxb))))
    ((recv (cat "share" (hash "public" x) ltxb))
      (send (cat "share" (hash "public" x) ltxb))))
  (label 62)
  (parent 58)
  (unrealized (0 2) (9 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (b a name) (ltxb x ltxa y rndx))
  (defstrand init 4
    (hint1 (hash "hint" (hash "share" (hash "public" ltxa) y)))
    (hint2 (hash "hint" (hash "share" (hash "public" x) ltxb))) (na na)
    (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (deflistener (cat "share" (hash "public" ltxb) ltxa))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) y)
      (hash "hint" (hash "share" (hash "public" x) ltxb))))
  (defstrand resp 3 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (deflistener (cat "share" (hash "public" x) y))
  (precedes ((0 1) (8 1)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (8 0)) ((2 2) (6 0)) ((3 0) (0 0)) ((3 0) (8 0))
    ((4 1) (1 0)) ((5 1) (4 0)) ((6 1) (5 0)) ((7 1) (4 0))
    ((8 2) (0 2)) ((8 2) (9 0)) ((9 1) (7 0)))
  (absent (y ltxa) (y ltxb) (y x) (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq ((exp (gen) x) (gen)) (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxb x ltxa y)
  (uniq-orig na nb)
  (operation encryption-test
    (added-listener (cat "share" (hash "public" x) y))
    (hash "share" (hash "public" x) y) (7 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))))
    ((recv (cat "share" (hash "public" ltxb) ltxa))
      (send (cat "share" (hash "public" ltxb) ltxa)))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) y)
         (hash "hint" (hash "share" (hash "public" x) ltxb))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) y)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na a b (hash "public" x)))
      (send
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint"
                  (hash "share" (hash "public" x) ltxb))))))))
    ((recv (cat "share" (hash "public" x) y))
      (send (cat "share" (hash "public" x) y))))
  (label 63)
  (parent 59)
  (unrealized (9 0))
  (dead)
  (comment "empty cohort"))

(defskeleton nhcr-umx
  (vars (na nb na-0 nb-0 data) (a b a-0 name)
    (ltxa y ltxb ltxa-0 y-0 x rndx))
  (defstrand init 4
    (hint1 (hash "hint" (hash "share" (hash "public" ltxa-0) y-0)))
    (hint2 (hash "hint" (hash "share" (hash "public" x) ltxb))) (na na)
    (nb nb) (a a) (b b) (ltxa ltxa-0) (ltxb ltxb) (x x) (y y-0))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa-0)
        (hash "hint" (hash "share" (hash "public" ltxa-0) y-0)))
      (hash "nh_key" (hash "share" (hash "public" x) y-0)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa-0))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa-0)
        (hash "hint" (hash "share" (hash "public" ltxa-0) y-0)))
      (hash "nh_key" (hash "share" (hash "public" x) y-0)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa-0)
      (hash "hint" (hash "share" (hash "public" ltxa-0) y-0))))
  (deflistener (cat "share" (hash "public" ltxb) ltxa-0))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) y-0)
      (hash "hint" (hash "share" (hash "public" x) ltxb))))
  (defstrand resp 3 (na na-0) (nb nb-0) (a a-0) (b b) (ltxa ltxa)
    (ltxb ltxb) (y y) (x x))
  (defstrand resp 3 (na na) (nb nb) (a a) (b b) (ltxa ltxa-0)
    (ltxb ltxb) (y y-0) (x x))
  (deflistener (cat "share" (hash "public" x) y-0))
  (precedes ((0 1) (8 1)) ((0 1) (9 1)) ((0 3) (2 1)) ((0 3) (3 1))
    ((2 0) (0 0)) ((2 0) (9 0)) ((2 2) (6 0)) ((3 0) (0 0))
    ((3 0) (8 0)) ((3 0) (9 0)) ((4 1) (1 0)) ((5 1) (4 0))
    ((6 1) (5 0)) ((7 1) (4 0)) ((8 2) (7 0)) ((9 2) (0 2))
    ((9 2) (10 0)) ((10 1) (7 0)))
  (absent (y-0 ltxa-0) (y-0 ltxb) (y-0 x) (y ltxa) (y ltxb) (y x)
    (x ltxa-0) (x ltxb))
  (fn-of ("ltx-of" (a ltxa-0) (b ltxb) (a-0 ltxa))
    ("principal-of" (ltxa-0 a) (ltxb b) (ltxa a-0)))
  (neq ((exp (gen) x) (gen)) (a b) ((exp (gen) y-0) (gen)))
  (uniq-gen y ltxb ltxa-0 y-0 x)
  (uniq-orig na nb nb-0)
  (operation encryption-test
    (added-listener (cat "share" (hash "public" x) y-0))
    (hash "share" (hash "public" x) y-0) (7 0))
  (traces
    ((recv (cat (hash "public" ltxa-0) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y-0)
          (hash "hint" (hash "share" (hash "public" ltxa-0) y-0))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa-0) y-0))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa-0)
                (hash "hint" (hash "share" (hash "public" ltxa-0) y-0)))
              (hash "nh_key" (hash "share" (hash "public" y-0) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa-0)
           (hash "hint" (hash "share" (hash "public" ltxa-0) y-0)))
         (hash "nh_key" (hash "share" (hash "public" x) y-0)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa-0)
            (hash "hint" (hash "share" (hash "public" ltxa-0) y-0)))
          (hash "nh_key" (hash "share" (hash "public" x) y-0)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa-0))) (recv "end-of-protocol")
      (send ltxa-0))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa-0)
           (hash "hint" (hash "share" (hash "public" ltxa-0) y-0)))
         (hash "nh_key" (hash "share" (hash "public" x) y-0)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa-0)
            (hash "hint" (hash "share" (hash "public" ltxa-0) y-0)))
          (hash "nh_key" (hash "share" (hash "public" x) y-0)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa-0)
         (hash "hint" (hash "share" (hash "public" ltxa-0) y-0))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa-0)
          (hash "hint" (hash "share" (hash "public" ltxa-0) y-0)))))
    ((recv (cat "share" (hash "public" ltxb) ltxa-0))
      (send (cat "share" (hash "public" ltxb) ltxa-0)))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) y-0)
         (hash "hint" (hash "share" (hash "public" x) ltxb))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) y-0)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na-0 a-0 b (hash "public" x)))
      (send
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na-0 nb-0 a-0 b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na-0 nb-0 a-0 b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na-0 nb-0 a-0 b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na-0 nb-0 a-0 b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint"
                  (hash "share" (hash "public" x) ltxb))))))))
    ((recv (cat (hash "public" ltxa-0) (hash "public" ltxb)))
      (recv (cat na a b (hash "public" x)))
      (send
        (cat (hash "public" y-0)
          (hash "hint" (hash "share" (hash "public" ltxa-0) y-0))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa-0) y-0))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa-0) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa-0) y-0)))
              (hash "nh_key" (hash "share" (hash "public" x) y-0)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa-0) y-0))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa-0) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa-0) y-0)))
              (hash "nh_key" (hash "share" (hash "public" y-0) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa-0) y-0))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa-0)
                (hash "hint" (hash "share" (hash "public" ltxa-0) y-0)))
              (hash "nh_key" (hash "share" (hash "public" x) y-0)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa-0) y-0))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa-0)
                (hash "hint" (hash "share" (hash "public" ltxa-0) y-0)))
              (hash "nh_key" (hash "share" (hash "public" y-0) x)
                (hash "hint"
                  (hash "share" (hash "public" x) ltxb))))))))
    ((recv (cat "share" (hash "public" x) y-0))
      (send (cat "share" (hash "public" x) y-0))))
  (label 64)
  (parent 60)
  (unrealized (10 0))
  (dead)
  (comment "empty cohort"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb na-0 nb-0 data) (a b a-0 name)
    (y expt) (l ltxa y-0 x ltxb rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa l) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l l))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxb) l)
      (hash "hint" (hash "share" (hash "public" l) y))))
  (deflistener (cat "share" (hash "public" ltxb) l))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) y)
      (hash "hint" (hash "share" (hash "public" x) ltxb))))
  (defstrand resp 3 (na na-0) (nb nb-0) (a a-0) (b b) (ltxa ltxa)
    (ltxb ltxb) (y y-0) (x x))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) l) hint1)
      (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) l) hint1)
      (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
  (precedes ((0 1) (8 1)) ((0 1) (10 0)) ((0 3) (2 1)) ((0 3) (3 1))
    ((2 0) (0 0)) ((2 2) (6 0)) ((3 0) (0 0)) ((3 0) (8 0))
    ((4 1) (1 0)) ((5 1) (4 0)) ((6 1) (5 0)) ((7 1) (4 0))
    ((8 2) (7 0)) ((9 1) (0 2)) ((10 1) (9 0)))
  (absent (y-0 ltxa) (y-0 ltxb) (y-0 x) (x l) (x ltxb))
  (fn-of ("ltx-of" (a-0 ltxa) (b ltxb) (a l))
    ("principal-of" (ltxa a-0) (ltxb b) (l a)))
  (neq ((exp (gen) x) (gen)) (a b) ((exp (gen) y) (gen)))
  (uniq-gen l y-0 x ltxb)
  (uniq-orig na nb-0)
  (operation encryption-test
    (added-listener
      (cat "key"
        (hash "nh_key" (hash "share" (hash "public" ltxb) l) hint1)
        (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) l) hint1)
      (hash "nh_key" (hash "share" (hash "public" y) x) hint2)) (9 0))
  (traces
    ((recv (cat (hash "public" l) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) l)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" l))) (recv "end-of-protocol") (send l))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxb) l)
         (hash "hint" (hash "share" (hash "public" l) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxb) l)
          (hash "hint" (hash "share" (hash "public" l) y)))))
    ((recv (cat "share" (hash "public" ltxb) l))
      (send (cat "share" (hash "public" ltxb) l)))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) y)
         (hash "hint" (hash "share" (hash "public" x) ltxb))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) y)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na-0 a-0 b (hash "public" x)))
      (send
        (cat (hash "public" y-0)
          (hash "hint" (hash "share" (hash "public" ltxa) y-0))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na-0 nb-0 a-0 b
            (hash "hint" (hash "share" (hash "public" ltxa) y-0))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y-0)))
              (hash "nh_key" (hash "share" (hash "public" x) y-0)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na-0 nb-0 a-0 b
            (hash "hint" (hash "share" (hash "public" ltxa) y-0))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y-0)))
              (hash "nh_key" (hash "share" (hash "public" y-0) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na-0 nb-0 a-0 b
            (hash "hint" (hash "share" (hash "public" ltxa) y-0))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y-0)))
              (hash "nh_key" (hash "share" (hash "public" x) y-0)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na-0 nb-0 a-0 b
            (hash "hint" (hash "share" (hash "public" ltxa) y-0))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y-0)))
              (hash "nh_key" (hash "share" (hash "public" y-0) x)
                (hash "hint"
                  (hash "share" (hash "public" x) ltxb))))))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) l) hint1)
         (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) l) hint1)
          (hash "nh_key" (hash "share" (hash "public" y) x) hint2))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) l) hint1)
         (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) l) hint1)
          (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))))
  (label 65)
  (parent 61)
  (unrealized (10 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (x rndx) (y expt)
    (l l-0 rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa l) (ltxb l-0) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" l-0) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) l-0)))))
  (defstrand ltx-gen 3 (self a) (l l))
  (defstrand ltx-gen 3 (self b) (l l-0))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" l-0) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) l-0)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" l-0) l)
      (hash "hint" (hash "share" (hash "public" l) y))))
  (deflistener (cat "share" (hash "public" l-0) l))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) y)
      (hash "hint" (hash "share" (hash "public" x) l-0))))
  (deflistener (cat "hint" (hash "share" (hash "public" x) l-0)))
  (deflistener (cat "share" (hash "public" x) l-0))
  (precedes ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0)) ((2 2) (6 0))
    ((3 0) (0 0)) ((3 2) (9 0)) ((4 1) (1 0)) ((5 1) (4 0))
    ((6 1) (5 0)) ((7 1) (4 0)) ((8 1) (7 0)) ((9 1) (8 0)))
  (absent (x l) (x l-0))
  (fn-of ("ltx-of" (b l-0) (a l)) ("principal-of" (l-0 b) (l a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen x l l-0)
  (uniq-orig na)
  (operation nonce-test (displaced 10 3 ltx-gen 3) l-0 (9 0))
  (traces
    ((recv (cat (hash "public" l) (hash "public" l-0)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" l-0) l) hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" l-0) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) l-0)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" l-0) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) l-0))))))
    ((send (cat a (hash "public" l))) (recv "end-of-protocol") (send l))
    ((send (cat b (hash "public" l-0))) (recv "end-of-protocol")
      (send l-0))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" l-0) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) l-0)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" l-0) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) l-0))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" l-0) l)
         (hash "hint" (hash "share" (hash "public" l) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" l-0) l)
          (hash "hint" (hash "share" (hash "public" l) y)))))
    ((recv (cat "share" (hash "public" l-0) l))
      (send (cat "share" (hash "public" l-0) l)))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) y)
         (hash "hint" (hash "share" (hash "public" x) l-0))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) y)
          (hash "hint" (hash "share" (hash "public" x) l-0)))))
    ((recv (cat "hint" (hash "share" (hash "public" x) l-0)))
      (send (cat "hint" (hash "share" (hash "public" x) l-0))))
    ((recv (cat "share" (hash "public" x) l-0))
      (send (cat "share" (hash "public" x) l-0))))
  (label 66)
  (parent 62)
  (unrealized (0 2))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb na-0 nb-0 data) (a b a-0 name)
    (y expt) (l ltxa y-0 x ltxb rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa l) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l l))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxb) l)
      (hash "hint" (hash "share" (hash "public" l) y))))
  (deflistener (cat "share" (hash "public" ltxb) l))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) y)
      (hash "hint" (hash "share" (hash "public" x) ltxb))))
  (defstrand resp 3 (na na-0) (nb nb-0) (a a-0) (b b) (ltxa ltxa)
    (ltxb ltxb) (y y-0) (x x))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) l) hint1)
      (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) l) hint1)
      (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxb) l) hint1))
  (precedes ((0 1) (8 1)) ((0 1) (10 0)) ((0 3) (2 1)) ((0 3) (3 1))
    ((2 0) (0 0)) ((2 0) (11 0)) ((2 2) (6 0)) ((3 0) (0 0))
    ((3 0) (8 0)) ((3 0) (11 0)) ((4 1) (1 0)) ((5 1) (4 0))
    ((6 1) (5 0)) ((7 1) (4 0)) ((8 2) (7 0)) ((9 1) (0 2))
    ((10 1) (9 0)) ((11 1) (10 0)))
  (absent (y-0 ltxa) (y-0 ltxb) (y-0 x) (x l) (x ltxb))
  (fn-of ("ltx-of" (a-0 ltxa) (b ltxb) (a l))
    ("principal-of" (ltxa a-0) (ltxb b) (l a)))
  (neq ((exp (gen) x) (gen)) (a b) ((exp (gen) y) (gen)))
  (uniq-gen l y-0 x ltxb)
  (uniq-orig na nb-0)
  (operation encryption-test
    (added-listener
      (cat "nh_key" (hash "share" (hash "public" ltxb) l) hint1))
    (hash "nh_key" (hash "share" (hash "public" ltxb) l) hint1) (10 0))
  (traces
    ((recv (cat (hash "public" l) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) l)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" l))) (recv "end-of-protocol") (send l))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxb) l)
         (hash "hint" (hash "share" (hash "public" l) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxb) l)
          (hash "hint" (hash "share" (hash "public" l) y)))))
    ((recv (cat "share" (hash "public" ltxb) l))
      (send (cat "share" (hash "public" ltxb) l)))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) y)
         (hash "hint" (hash "share" (hash "public" x) ltxb))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) y)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na-0 a-0 b (hash "public" x)))
      (send
        (cat (hash "public" y-0)
          (hash "hint" (hash "share" (hash "public" ltxa) y-0))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na-0 nb-0 a-0 b
            (hash "hint" (hash "share" (hash "public" ltxa) y-0))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y-0)))
              (hash "nh_key" (hash "share" (hash "public" x) y-0)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na-0 nb-0 a-0 b
            (hash "hint" (hash "share" (hash "public" ltxa) y-0))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y-0)))
              (hash "nh_key" (hash "share" (hash "public" y-0) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na-0 nb-0 a-0 b
            (hash "hint" (hash "share" (hash "public" ltxa) y-0))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y-0)))
              (hash "nh_key" (hash "share" (hash "public" x) y-0)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na-0 nb-0 a-0 b
            (hash "hint" (hash "share" (hash "public" ltxa) y-0))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y-0)))
              (hash "nh_key" (hash "share" (hash "public" y-0) x)
                (hash "hint"
                  (hash "share" (hash "public" x) ltxb))))))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) l) hint1)
         (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) l) hint1)
          (hash "nh_key" (hash "share" (hash "public" y) x) hint2))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) l) hint1)
         (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) l) hint1)
          (hash "nh_key" (hash "share" (hash "public" y) x) hint2))))
    ((recv (cat "nh_key" (hash "share" (hash "public" ltxb) l) hint1))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxb) l) hint1))))
  (label 67)
  (parent 65)
  (unrealized (10 0) (11 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (ltxb ltxa y x rndx))
  (defstrand init 4
    (hint1 (hash "hint" (hash "share" (hash "public" ltxa) y)))
    (hint2 (hash "hint" (hash "share" (hash "public" x) ltxb))) (na na)
    (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (deflistener (cat "share" (hash "public" ltxb) ltxa))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) y)
      (hash "hint" (hash "share" (hash "public" x) ltxb))))
  (deflistener (cat "hint" (hash "share" (hash "public" x) ltxb)))
  (deflistener (cat "share" (hash "public" x) ltxb))
  (defstrand resp 3 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (precedes ((0 1) (10 1)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (10 0)) ((2 2) (6 0)) ((3 0) (0 0)) ((3 0) (10 0))
    ((3 2) (9 0)) ((4 1) (1 0)) ((5 1) (4 0)) ((6 1) (5 0))
    ((7 1) (4 0)) ((8 1) (7 0)) ((9 1) (8 0)) ((10 2) (0 2)))
  (absent (y ltxa) (y ltxb) (y x) (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq ((exp (gen) x) (gen)) (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxb ltxa y x)
  (uniq-orig na nb)
  (operation encryption-test (added-strand resp 3)
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" y) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb))))) (0 2))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))))
    ((recv (cat "share" (hash "public" ltxb) ltxa))
      (send (cat "share" (hash "public" ltxb) ltxa)))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) y)
         (hash "hint" (hash "share" (hash "public" x) ltxb))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) y)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    ((recv (cat "hint" (hash "share" (hash "public" x) ltxb)))
      (send (cat "hint" (hash "share" (hash "public" x) ltxb))))
    ((recv (cat "share" (hash "public" x) ltxb))
      (send (cat "share" (hash "public" x) ltxb)))
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na a b (hash "public" x)))
      (send
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint"
                  (hash "share" (hash "public" x) ltxb)))))))))
  (label 68)
  (parent 66)
  (unrealized (7 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (x rndx) (y expt)
    (l l-0 rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa l) (ltxb l-0) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" l-0) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) l-0)))))
  (defstrand ltx-gen 3 (self a) (l l))
  (defstrand ltx-gen 3 (self b) (l l-0))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" l-0) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) l-0)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" l-0) l)
      (hash "hint" (hash "share" (hash "public" l) y))))
  (deflistener (cat "share" (hash "public" l-0) l))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) y)
      (hash "hint" (hash "share" (hash "public" x) l-0))))
  (deflistener (cat "hint" (hash "share" (hash "public" x) l-0)))
  (deflistener (cat "share" (hash "public" x) l-0))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" l-0) l) hint1)
      (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
  (precedes ((0 1) (10 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 2) (6 0)) ((3 0) (0 0)) ((3 2) (9 0)) ((4 1) (1 0))
    ((5 1) (4 0)) ((6 1) (5 0)) ((7 1) (4 0)) ((8 1) (7 0))
    ((9 1) (8 0)) ((10 1) (0 2)))
  (absent (x l) (x l-0))
  (fn-of ("ltx-of" (b l-0) (a l)) ("principal-of" (l-0 b) (l a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen x l l-0)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" l-0) l) hint1)
        (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
    (enc na nb a b hint1 hint2
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" l-0) l) hint1)
        (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
    (0 2))
  (traces
    ((recv (cat (hash "public" l) (hash "public" l-0)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" l-0) l) hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" l-0) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) l-0)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" l-0) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) l-0))))))
    ((send (cat a (hash "public" l))) (recv "end-of-protocol") (send l))
    ((send (cat b (hash "public" l-0))) (recv "end-of-protocol")
      (send l-0))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" l-0) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) l-0)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" l-0) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) l-0))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" l-0) l)
         (hash "hint" (hash "share" (hash "public" l) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" l-0) l)
          (hash "hint" (hash "share" (hash "public" l) y)))))
    ((recv (cat "share" (hash "public" l-0) l))
      (send (cat "share" (hash "public" l-0) l)))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) y)
         (hash "hint" (hash "share" (hash "public" x) l-0))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) y)
          (hash "hint" (hash "share" (hash "public" x) l-0)))))
    ((recv (cat "hint" (hash "share" (hash "public" x) l-0)))
      (send (cat "hint" (hash "share" (hash "public" x) l-0))))
    ((recv (cat "share" (hash "public" x) l-0))
      (send (cat "share" (hash "public" x) l-0)))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" l-0) l) hint1)
         (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" l-0) l) hint1)
          (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))))
  (label 69)
  (parent 66)
  (unrealized (10 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb na-0 nb-0 data) (a b a-0 name)
    (y expt) (l ltxa y-0 x ltxb rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa l) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l l))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxb) l)
      (hash "hint" (hash "share" (hash "public" l) y))))
  (deflistener (cat "share" (hash "public" ltxb) l))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) y)
      (hash "hint" (hash "share" (hash "public" x) ltxb))))
  (defstrand resp 3 (na na-0) (nb nb-0) (a a-0) (b b) (ltxa ltxa)
    (ltxb ltxb) (y y-0) (x x))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) l) hint1)
      (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) l) hint1)
      (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxb) l) hint1))
  (deflistener (cat "share" (hash "public" ltxb) l))
  (precedes ((0 1) (8 1)) ((0 1) (10 0)) ((0 3) (2 1)) ((0 3) (3 1))
    ((2 0) (0 0)) ((2 0) (12 0)) ((2 2) (6 0)) ((3 0) (0 0))
    ((3 0) (8 0)) ((3 0) (12 0)) ((4 1) (1 0)) ((5 1) (4 0))
    ((6 1) (5 0)) ((7 1) (4 0)) ((8 2) (7 0)) ((9 1) (0 2))
    ((10 1) (9 0)) ((11 1) (10 0)) ((12 1) (11 0)))
  (absent (y-0 ltxa) (y-0 ltxb) (y-0 x) (x l) (x ltxb))
  (fn-of ("ltx-of" (a-0 ltxa) (b ltxb) (a l))
    ("principal-of" (ltxa a-0) (ltxb b) (l a)))
  (neq ((exp (gen) x) (gen)) (a b) ((exp (gen) y) (gen)))
  (uniq-gen l y-0 x ltxb)
  (uniq-orig na nb-0)
  (operation encryption-test
    (added-listener (cat "share" (hash "public" ltxb) l))
    (hash "share" (hash "public" ltxb) l) (11 0))
  (traces
    ((recv (cat (hash "public" l) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) l)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" l))) (recv "end-of-protocol") (send l))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxb) l)
         (hash "hint" (hash "share" (hash "public" l) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxb) l)
          (hash "hint" (hash "share" (hash "public" l) y)))))
    ((recv (cat "share" (hash "public" ltxb) l))
      (send (cat "share" (hash "public" ltxb) l)))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) y)
         (hash "hint" (hash "share" (hash "public" x) ltxb))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) y)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na-0 a-0 b (hash "public" x)))
      (send
        (cat (hash "public" y-0)
          (hash "hint" (hash "share" (hash "public" ltxa) y-0))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na-0 nb-0 a-0 b
            (hash "hint" (hash "share" (hash "public" ltxa) y-0))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y-0)))
              (hash "nh_key" (hash "share" (hash "public" x) y-0)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na-0 nb-0 a-0 b
            (hash "hint" (hash "share" (hash "public" ltxa) y-0))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y-0)))
              (hash "nh_key" (hash "share" (hash "public" y-0) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na-0 nb-0 a-0 b
            (hash "hint" (hash "share" (hash "public" ltxa) y-0))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y-0)))
              (hash "nh_key" (hash "share" (hash "public" x) y-0)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na-0 nb-0 a-0 b
            (hash "hint" (hash "share" (hash "public" ltxa) y-0))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y-0)))
              (hash "nh_key" (hash "share" (hash "public" y-0) x)
                (hash "hint"
                  (hash "share" (hash "public" x) ltxb))))))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) l) hint1)
         (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) l) hint1)
          (hash "nh_key" (hash "share" (hash "public" y) x) hint2))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) l) hint1)
         (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) l) hint1)
          (hash "nh_key" (hash "share" (hash "public" y) x) hint2))))
    ((recv (cat "nh_key" (hash "share" (hash "public" ltxb) l) hint1))
      (send (cat "nh_key" (hash "share" (hash "public" ltxb) l) hint1)))
    ((recv (cat "share" (hash "public" ltxb) l))
      (send (cat "share" (hash "public" ltxb) l))))
  (label 70)
  (parent 67)
  (unrealized (10 0) (12 0))
  (dead)
  (comment "empty cohort"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (ltxb ltxa y x rndx))
  (defstrand init 4
    (hint1 (hash "hint" (hash "share" (hash "public" ltxa) y)))
    (hint2 (hash "hint" (hash "share" (hash "public" x) ltxb))) (na na)
    (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (deflistener (cat "share" (hash "public" ltxb) ltxa))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) y)
      (hash "hint" (hash "share" (hash "public" x) ltxb))))
  (deflistener (cat "hint" (hash "share" (hash "public" x) ltxb)))
  (deflistener (cat "share" (hash "public" x) ltxb))
  (defstrand resp 3 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (deflistener (cat "share" (hash "public" x) y))
  (precedes ((0 1) (10 1)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (10 0)) ((2 2) (6 0)) ((3 0) (0 0)) ((3 0) (10 0))
    ((3 2) (9 0)) ((4 1) (1 0)) ((5 1) (4 0)) ((6 1) (5 0))
    ((7 1) (4 0)) ((8 1) (7 0)) ((9 1) (8 0)) ((10 2) (0 2))
    ((10 2) (11 0)) ((11 1) (7 0)))
  (absent (y ltxa) (y ltxb) (y x) (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq ((exp (gen) x) (gen)) (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxb ltxa y x)
  (uniq-orig na nb)
  (operation encryption-test
    (added-listener (cat "share" (hash "public" x) y))
    (hash "share" (hash "public" x) y) (7 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))))
    ((recv (cat "share" (hash "public" ltxb) ltxa))
      (send (cat "share" (hash "public" ltxb) ltxa)))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) y)
         (hash "hint" (hash "share" (hash "public" x) ltxb))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) y)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    ((recv (cat "hint" (hash "share" (hash "public" x) ltxb)))
      (send (cat "hint" (hash "share" (hash "public" x) ltxb))))
    ((recv (cat "share" (hash "public" x) ltxb))
      (send (cat "share" (hash "public" x) ltxb)))
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na a b (hash "public" x)))
      (send
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint"
                  (hash "share" (hash "public" x) ltxb))))))))
    ((recv (cat "share" (hash "public" x) y))
      (send (cat "share" (hash "public" x) y))))
  (label 71)
  (parent 68)
  (unrealized (11 0))
  (dead)
  (comment "empty cohort"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (x rndx) (y expt)
    (l l-0 rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa l) (ltxb l-0) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" l-0) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) l-0)))))
  (defstrand ltx-gen 3 (self a) (l l))
  (defstrand ltx-gen 3 (self b) (l l-0))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" l-0) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) l-0)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" l-0) l)
      (hash "hint" (hash "share" (hash "public" l) y))))
  (deflistener (cat "share" (hash "public" l-0) l))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) y)
      (hash "hint" (hash "share" (hash "public" x) l-0))))
  (deflistener (cat "hint" (hash "share" (hash "public" x) l-0)))
  (deflistener (cat "share" (hash "public" x) l-0))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" l-0) l) hint1)
      (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" l-0) l) hint1)
      (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
  (precedes ((0 1) (11 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 2) (6 0)) ((3 0) (0 0)) ((3 2) (9 0)) ((4 1) (1 0))
    ((5 1) (4 0)) ((6 1) (5 0)) ((7 1) (4 0)) ((8 1) (7 0))
    ((9 1) (8 0)) ((10 1) (0 2)) ((11 1) (10 0)))
  (absent (x l) (x l-0))
  (fn-of ("ltx-of" (b l-0) (a l)) ("principal-of" (l-0 b) (l a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen x l l-0)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (cat "key"
        (hash "nh_key" (hash "share" (hash "public" l-0) l) hint1)
        (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" l-0) l) hint1)
      (hash "nh_key" (hash "share" (hash "public" y) x) hint2)) (10 0))
  (traces
    ((recv (cat (hash "public" l) (hash "public" l-0)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" l-0) l) hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" l-0) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) l-0)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" l-0) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) l-0))))))
    ((send (cat a (hash "public" l))) (recv "end-of-protocol") (send l))
    ((send (cat b (hash "public" l-0))) (recv "end-of-protocol")
      (send l-0))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" l-0) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) l-0)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" l-0) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) l-0))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" l-0) l)
         (hash "hint" (hash "share" (hash "public" l) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" l-0) l)
          (hash "hint" (hash "share" (hash "public" l) y)))))
    ((recv (cat "share" (hash "public" l-0) l))
      (send (cat "share" (hash "public" l-0) l)))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) y)
         (hash "hint" (hash "share" (hash "public" x) l-0))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) y)
          (hash "hint" (hash "share" (hash "public" x) l-0)))))
    ((recv (cat "hint" (hash "share" (hash "public" x) l-0)))
      (send (cat "hint" (hash "share" (hash "public" x) l-0))))
    ((recv (cat "share" (hash "public" x) l-0))
      (send (cat "share" (hash "public" x) l-0)))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" l-0) l) hint1)
         (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" l-0) l) hint1)
          (hash "nh_key" (hash "share" (hash "public" y) x) hint2))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" l-0) l) hint1)
         (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" l-0) l) hint1)
          (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))))
  (label 72)
  (parent 69)
  (unrealized (11 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (x rndx) (y expt)
    (l l-0 rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa l) (ltxb l-0) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" l-0) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) l-0)))))
  (defstrand ltx-gen 3 (self a) (l l))
  (defstrand ltx-gen 3 (self b) (l l-0))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" l-0) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) l-0)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" l-0) l)
      (hash "hint" (hash "share" (hash "public" l) y))))
  (deflistener (cat "share" (hash "public" l-0) l))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) y)
      (hash "hint" (hash "share" (hash "public" x) l-0))))
  (deflistener (cat "hint" (hash "share" (hash "public" x) l-0)))
  (deflistener (cat "share" (hash "public" x) l-0))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" l-0) l) hint1)
      (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" l-0) l) hint1)
      (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" l-0) l) hint1))
  (precedes ((0 1) (11 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (12 0)) ((2 2) (6 0)) ((3 0) (0 0)) ((3 0) (12 0))
    ((3 2) (9 0)) ((4 1) (1 0)) ((5 1) (4 0)) ((6 1) (5 0))
    ((7 1) (4 0)) ((8 1) (7 0)) ((9 1) (8 0)) ((10 1) (0 2))
    ((11 1) (10 0)) ((12 1) (11 0)))
  (absent (x l) (x l-0))
  (fn-of ("ltx-of" (b l-0) (a l)) ("principal-of" (l-0 b) (l a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen x l l-0)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (cat "nh_key" (hash "share" (hash "public" l-0) l) hint1))
    (hash "nh_key" (hash "share" (hash "public" l-0) l) hint1) (11 0))
  (traces
    ((recv (cat (hash "public" l) (hash "public" l-0)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" l-0) l) hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" l-0) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) l-0)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" l-0) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) l-0))))))
    ((send (cat a (hash "public" l))) (recv "end-of-protocol") (send l))
    ((send (cat b (hash "public" l-0))) (recv "end-of-protocol")
      (send l-0))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" l-0) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) l-0)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" l-0) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) l-0))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" l-0) l)
         (hash "hint" (hash "share" (hash "public" l) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" l-0) l)
          (hash "hint" (hash "share" (hash "public" l) y)))))
    ((recv (cat "share" (hash "public" l-0) l))
      (send (cat "share" (hash "public" l-0) l)))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) y)
         (hash "hint" (hash "share" (hash "public" x) l-0))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) y)
          (hash "hint" (hash "share" (hash "public" x) l-0)))))
    ((recv (cat "hint" (hash "share" (hash "public" x) l-0)))
      (send (cat "hint" (hash "share" (hash "public" x) l-0))))
    ((recv (cat "share" (hash "public" x) l-0))
      (send (cat "share" (hash "public" x) l-0)))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" l-0) l) hint1)
         (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" l-0) l) hint1)
          (hash "nh_key" (hash "share" (hash "public" y) x) hint2))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" l-0) l) hint1)
         (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" l-0) l) hint1)
          (hash "nh_key" (hash "share" (hash "public" y) x) hint2))))
    ((recv (cat "nh_key" (hash "share" (hash "public" l-0) l) hint1))
      (send (cat "nh_key" (hash "share" (hash "public" l-0) l) hint1))))
  (label 73)
  (parent 72)
  (unrealized (11 0) (12 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (x rndx) (y expt)
    (l l-0 rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa l) (ltxb l-0) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" l-0) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) l-0)))))
  (defstrand ltx-gen 3 (self a) (l l))
  (defstrand ltx-gen 3 (self b) (l l-0))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" l-0) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) y)
        (hash "hint" (hash "share" (hash "public" x) l-0)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" l-0) l)
      (hash "hint" (hash "share" (hash "public" l) y))))
  (deflistener (cat "share" (hash "public" l-0) l))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) y)
      (hash "hint" (hash "share" (hash "public" x) l-0))))
  (deflistener (cat "hint" (hash "share" (hash "public" x) l-0)))
  (deflistener (cat "share" (hash "public" x) l-0))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" l-0) l) hint1)
      (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" l-0) l) hint1)
      (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" l-0) l) hint1))
  (deflistener (cat "share" (hash "public" l-0) l))
  (precedes ((0 1) (11 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (13 0)) ((2 2) (6 0)) ((3 0) (0 0)) ((3 0) (13 0))
    ((3 2) (9 0)) ((4 1) (1 0)) ((5 1) (4 0)) ((6 1) (5 0))
    ((7 1) (4 0)) ((8 1) (7 0)) ((9 1) (8 0)) ((10 1) (0 2))
    ((11 1) (10 0)) ((12 1) (11 0)) ((13 1) (12 0)))
  (absent (x l) (x l-0))
  (fn-of ("ltx-of" (b l-0) (a l)) ("principal-of" (l-0 b) (l a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen x l l-0)
  (uniq-orig na)
  (operation encryption-test
    (added-listener (cat "share" (hash "public" l-0) l))
    (hash "share" (hash "public" l-0) l) (12 0))
  (traces
    ((recv (cat (hash "public" l) (hash "public" l-0)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" l-0) l) hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" l-0) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) l-0)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" l-0) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) l-0))))))
    ((send (cat a (hash "public" l))) (recv "end-of-protocol") (send l))
    ((send (cat b (hash "public" l-0))) (recv "end-of-protocol")
      (send l-0))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" l-0) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) y)
           (hash "hint" (hash "share" (hash "public" x) l-0)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" l-0) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) y)
            (hash "hint" (hash "share" (hash "public" x) l-0))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" l-0) l)
         (hash "hint" (hash "share" (hash "public" l) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" l-0) l)
          (hash "hint" (hash "share" (hash "public" l) y)))))
    ((recv (cat "share" (hash "public" l-0) l))
      (send (cat "share" (hash "public" l-0) l)))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) y)
         (hash "hint" (hash "share" (hash "public" x) l-0))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) y)
          (hash "hint" (hash "share" (hash "public" x) l-0)))))
    ((recv (cat "hint" (hash "share" (hash "public" x) l-0)))
      (send (cat "hint" (hash "share" (hash "public" x) l-0))))
    ((recv (cat "share" (hash "public" x) l-0))
      (send (cat "share" (hash "public" x) l-0)))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" l-0) l) hint1)
         (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" l-0) l) hint1)
          (hash "nh_key" (hash "share" (hash "public" y) x) hint2))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" l-0) l) hint1)
         (hash "nh_key" (hash "share" (hash "public" y) x) hint2)))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" l-0) l) hint1)
          (hash "nh_key" (hash "share" (hash "public" y) x) hint2))))
    ((recv (cat "nh_key" (hash "share" (hash "public" l-0) l) hint1))
      (send (cat "nh_key" (hash "share" (hash "public" l-0) l) hint1)))
    ((recv (cat "share" (hash "public" l-0) l))
      (send (cat "share" (hash "public" l-0) l))))
  (label 74)
  (parent 73)
  (unrealized (11 0) (13 0))
  (dead)
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol nhcr-umx diffie-hellman
  (defrole init
    (vars (ltxa ltxb x rndx) (y expt) (a b name) (na nb data)
      (hint1 hint2 mesg))
    (trace (recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    (uniq-orig na)
    (uniq-gen x)
    (fn-of ("ltx-of" (a ltxa) (b ltxb))
      ("principal-of" (ltxa a) (ltxb b)))
    (neq ((exp (gen) y) (gen)))
    (absent (x ltxa) (x ltxb)))
  (defrole resp
    (vars (ltxa ltxb y rndx) (x expt) (a b name) (na nb data))
    (trace (recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (recv (cat na a b (hash "public" x)))
      (send
        (cat (hash "public" y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))
          (hash "hint" (hash "share" (hash "public" x) ltxb))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) ltxb)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) y)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" y) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb))
    (uniq-orig nb)
    (uniq-gen y)
    (fn-of ("ltx-of" (a ltxa) (b ltxb))
      ("principal-of" (ltxa a) (ltxb b)))
    (neq ((exp (gen) x) (gen)))
    (absent (y ltxa) (y ltxb) (y x)))
  (defrole ltx-gen
    (vars (self name) (l rndx))
    (trace (send (cat self (hash "public" l))) (recv "end-of-protocol")
      (send l))
    (uniq-gen l)
    (fn-of ("ltx-of" (self l)) ("principal-of" (l self)))))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b self self-0 name)
    (ltxa ltxb x rndx) (y expt))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self self) (l ltxa))
  (defstrand ltx-gen 3 (self self-0) (l ltxb))
  (precedes ((0 3) (2 1)) ((0 3) (3 1)))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (self-0 ltxb) (self ltxa) (a ltxa) (b ltxb))
    ("principal-of" (ltxb self-0) (ltxa self) (ltxa a) (ltxb b)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" y) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" y) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat self (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat self-0 (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb)))
  (label 75)
  (unrealized (0 0) (0 2) (1 0))
  (preskeleton)
  (origs (na (0 1)))
  (comment "Not a skeleton"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa ltxb x rndx)
    (y expt))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (precedes ((0 1) (1 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((3 0) (0 0)))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" y) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" y) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb)))
  (label 76)
  (parent 75)
  (unrealized (0 2) (1 0))
  (origs (na (0 1)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa ltxb x rndx)
    (y expt))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (precedes ((0 1) (4 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((3 0) (0 0)) ((4 1) (1 0)))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (cat "key"
        (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" y) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))) (1 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" y) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" y) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" y) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" y) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
  (label 77)
  (parent 76)
  (unrealized (0 2) (4 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa ltxb x rndx)
    (y expt))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (precedes ((0 1) (4 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (5 0)) ((3 0) (0 0)) ((3 0) (5 0)) ((4 1) (1 0))
    ((5 1) (4 0)))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y))))
    (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
      (hash "hint" (hash "share" (hash "public" ltxa) y))) (4 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" y) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" y) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" y) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" y) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y))))))
  (label 78)
  (parent 77)
  (unrealized (0 2) (4 0) (5 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa ltxb x rndx)
    (y expt))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (deflistener (cat "share" (hash "public" ltxb) ltxa))
  (precedes ((0 1) (4 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (6 0)) ((3 0) (0 0)) ((3 0) (6 0)) ((4 1) (1 0))
    ((5 1) (4 0)) ((6 1) (5 0)))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (operation encryption-test
    (added-listener (cat "share" (hash "public" ltxb) ltxa))
    (hash "share" (hash "public" ltxb) ltxa) (5 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" y) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" y) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" y) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" y) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxb) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))))
    ((recv (cat "share" (hash "public" ltxb) ltxa))
      (send (cat "share" (hash "public" ltxb) ltxa))))
  (label 79)
  (parent 78)
  (unrealized (0 2) (4 0) (6 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxb x rndx)
    (y expt) (l rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa l) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l l))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxb) l)
      (hash "hint" (hash "share" (hash "public" l) y))))
  (deflistener (cat "share" (hash "public" ltxb) l))
  (precedes ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0)) ((2 2) (6 0))
    ((3 0) (0 0)) ((4 1) (1 0)) ((5 1) (4 0)) ((6 1) (5 0)))
  (absent (x l) (x ltxb))
  (fn-of ("ltx-of" (a l) (b ltxb)) ("principal-of" (l a) (ltxb b)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxb x l)
  (uniq-orig na)
  (operation nonce-test (displaced 7 2 ltx-gen 3) l (6 0))
  (traces
    ((recv (cat (hash "public" l) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) l)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" y) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" y) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" l))) (recv "end-of-protocol") (send l))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" y) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" y) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxb) l)
         (hash "hint" (hash "share" (hash "public" l) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxb) l)
          (hash "hint" (hash "share" (hash "public" l) y)))))
    ((recv (cat "share" (hash "public" ltxb) l))
      (send (cat "share" (hash "public" ltxb) l))))
  (label 80)
  (parent 79)
  (unrealized (0 2) (4 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxb x rndx)
    (y expt) (l rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa l) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l l))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxb) l)
      (hash "hint" (hash "share" (hash "public" l) y))))
  (deflistener (cat "share" (hash "public" ltxb) l))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" y) x)
      (hash "hint" (hash "share" (hash "public" x) ltxb))))
  (precedes ((0 1) (7 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 2) (6 0)) ((3 0) (0 0)) ((4 1) (1 0)) ((5 1) (4 0))
    ((6 1) (5 0)) ((7 1) (4 0)))
  (absent (x l) (x ltxb))
  (fn-of ("ltx-of" (a l) (b ltxb)) ("principal-of" (l a) (ltxb b)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxb x l)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (cat "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb))))
    (hash "nh_key" (hash "share" (hash "public" y) x)
      (hash "hint" (hash "share" (hash "public" x) ltxb))) (4 0))
  (traces
    ((recv (cat (hash "public" l) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) l)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" y) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" y) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" l))) (recv "end-of-protocol") (send l))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" y) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" y) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxb) l)
         (hash "hint" (hash "share" (hash "public" l) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxb) l)
          (hash "hint" (hash "share" (hash "public" l) y)))))
    ((recv (cat "share" (hash "public" ltxb) l))
      (send (cat "share" (hash "public" ltxb) l)))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" y) x)
         (hash "hint" (hash "share" (hash "public" x) ltxb))))
      (send
        (cat "nh_key" (hash "share" (hash "public" y) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb))))))
  (label 81)
  (parent 80)
  (unrealized (0 2) (7 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxb x rndx)
    (y expt) (l rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa l) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l l))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxb) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" y) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxb) l)
      (hash "hint" (hash "share" (hash "public" l) y))))
  (deflistener (cat "share" (hash "public" ltxb) l))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" y) x)
      (hash "hint" (hash "share" (hash "public" x) ltxb))))
  (deflistener (cat "share" (hash "public" y) x))
  (precedes ((0 1) (8 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 2) (6 0)) ((3 0) (0 0)) ((4 1) (1 0)) ((5 1) (4 0))
    ((6 1) (5 0)) ((7 1) (4 0)) ((8 1) (7 0)))
  (absent (x l) (x ltxb))
  (fn-of ("ltx-of" (a l) (b ltxb)) ("principal-of" (l a) (ltxb b)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxb x l)
  (uniq-orig na)
  (operation encryption-test
    (added-listener (cat "share" (hash "public" y) x))
    (hash "share" (hash "public" y) x) (7 0))
  (traces
    ((recv (cat (hash "public" l) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxb) l)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" y) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" y) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" y) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" l))) (recv "end-of-protocol") (send l))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxb) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" y) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxb) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" y) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxb) l)
         (hash "hint" (hash "share" (hash "public" l) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxb) l)
          (hash "hint" (hash "share" (hash "public" l) y)))))
    ((recv (cat "share" (hash "public" ltxb) l))
      (send (cat "share" (hash "public" ltxb) l)))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" y) x)
         (hash "hint" (hash "share" (hash "public" x) ltxb))))
      (send
        (cat "nh_key" (hash "share" (hash "public" y) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    ((recv (cat "share" (hash "public" y) x))
      (send (cat "share" (hash "public" y) x))))
  (label 82)
  (parent 81)
  (unrealized (0 2) (7 0) (8 0))
  (dead)
  (comment "empty cohort"))

(comment "Nothing left to do")
