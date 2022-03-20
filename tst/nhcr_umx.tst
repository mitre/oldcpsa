(herald "NHCR: unified model (UMX) criss-cross" (bound 20) (limit 8000)
  (algebra diffie-hellman))

(comment "CPSA 3.6.10")
(comment "All input read from tst/nhcr_umx.scm")
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
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                hint2))))) (send nb))
    ((send (cat b (hash "public" ltxb)))))
  (label 0)
  (unrealized (0 2))
  (origs (na (0 1)))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (y ltxa ltxb x rndx))
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
  (uniq-gen y ltxb x)
  (uniq-orig na nb)
  (operation encryption-test (added-strand resp 3)
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
      (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2)))
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
        (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
        (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2)))
    (enc na nb a b hint1 hint2
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
        (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2)))
    (0 2))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                hint2))))) (send nb))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
         (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2)))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            hint2)))))
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
      (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2)))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2)))
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
        (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
        (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2)))
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2))
    (2 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                hint2))))) (send nb))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
         (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2)))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
          (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
         (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2)))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            hint2)))))
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
      (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2)))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2)))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxb) x) hint2))
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
      (cat "nh_key" (hash "share" (hash "public" ltxb) x) hint2))
    (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2) (3 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                hint2))))) (send nb))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
         (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2)))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
          (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
         (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2)))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
          (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2))))
    ((recv (cat "nh_key" (hash "share" (hash "public" ltxb) x) hint2))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxb) x) hint2))))
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
      (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2)))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2)))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxb) x) hint2))
  (deflistener (cat "share" (hash "public" ltxb) x))
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
    (added-listener (cat "share" (hash "public" ltxb) x))
    (hash "share" (hash "public" ltxb) x) (4 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                hint2))))) (send nb))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
         (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2)))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
          (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
         (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2)))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
          (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2))))
    ((recv (cat "nh_key" (hash "share" (hash "public" ltxb) x) hint2))
      (send (cat "nh_key" (hash "share" (hash "public" ltxb) x) hint2)))
    ((recv (cat "share" (hash "public" ltxb) x))
      (send (cat "share" (hash "public" ltxb) x))))
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
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                hint2))))) (send nb))
    ((send (cat a (hash "public" ltxa))))
    ((send (cat b (hash "public" ltxb)))))
  (label 7)
  (parent 6)
  (unrealized (0 2))
  (origs (na (0 1)))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (y ltxa ltxb x rndx))
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
  (non-orig ltxa ltxb)
  (uniq-gen y ltxa ltxb x)
  (uniq-orig na nb)
  (operation encryption-test (added-strand resp 3)
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
      (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2)))
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
        (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
        (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2)))
    (enc na nb a b hint1 hint2
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
        (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2)))
    (0 2))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                hint2))))) (send nb))
    ((send (cat a (hash "public" ltxa))))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
         (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2)))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            hint2)))))
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
      (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2)))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2)))
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
        (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
        (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2)))
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2))
    (3 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                hint2))))) (send nb))
    ((send (cat a (hash "public" ltxa))))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
         (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2)))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
          (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
         (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2)))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            hint2)))))
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
      (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2)))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2)))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" y) ltxa) hint1))
  (precedes ((0 1) (4 0)) ((1 0) (0 0)) ((1 0) (5 0)) ((2 0) (0 0))
    ((3 1) (0 2)) ((4 1) (3 0)) ((5 1) (4 0)))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (non-orig ltxa ltxb)
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (cat "nh_key" (hash "share" (hash "public" y) ltxa) hint1))
    (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1) (4 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                hint2))))) (send nb))
    ((send (cat a (hash "public" ltxa))))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
         (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2)))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
          (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
         (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2)))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
          (hash "nh_key" (hash "share" (hash "public" ltxb) x) hint2))))
    ((recv (cat "nh_key" (hash "share" (hash "public" y) ltxa) hint1))
      (send
        (cat "nh_key" (hash "share" (hash "public" y) ltxa) hint1))))
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
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((send (cat b (hash "public" ltxb)))))
  (label 13)
  (parent 12)
  (unrealized (0 3))
  (origs (nb (0 2)))
  (comment "5 in cohort - 5 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (y ltxa ltxb x rndx))
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
  (non-orig ltxa ltxb)
  (uniq-gen y ltxa ltxb x)
  (uniq-orig na nb)
  (operation nonce-test (added-strand init 4) nb (0 3)
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" ltxb) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) ltxb)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" ltxb) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) ltxb)
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
        (hash "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" ltxb) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb))))) nb
    (0 3)
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" ltxb) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) ltxb)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" ltxb) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) ltxb)
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
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
        (hash "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) ltxb)
          (hash "hint" (hash "share" (hash "public" x) ltxb))))) nb
    (0 3)
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" ltxb) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) ltxb)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" ltxb) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) ltxb)
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
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
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
        (hash "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" ltxb) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb))))) nb
    (0 3)
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" ltxb) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) ltxb)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" ltxb) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) ltxb)
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
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
        (hash "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) ltxb)
          (hash "hint" (hash "share" (hash "public" x) ltxb))))) nb
    (0 3)
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" ltxb) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) ltxb)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" ltxb) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) ltxb)
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
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
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
        (hash "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" ltxb) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
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
        (hash "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) ltxb)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
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
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
        (hash "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" ltxb) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
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
        (hash "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) ltxb)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
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
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxa) y)
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
      (cat "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y))))
    (hash "nh_key" (hash "share" (hash "public" ltxa) y)
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxa) y)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))))))
  (label 23)
  (parent 19)
  (unrealized (5 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxa) y)
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
      (cat "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y))))
    (hash "nh_key" (hash "share" (hash "public" ltxa) y)
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxa) y)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))))))
  (label 24)
  (parent 20)
  (unrealized (4 0) (5 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" y) ltxa)
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
      (cat "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y))))
    (hash "nh_key" (hash "share" (hash "public" y) ltxa)
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" y) ltxa)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" y) ltxa)
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
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" y) ltxa)
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
      (cat "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y))))
    (hash "nh_key" (hash "share" (hash "public" y) ltxa)
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" y) ltxa)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y))))))
  (label 26)
  (parent 22)
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
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxa) y)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (deflistener (cat "share" (hash "public" ltxa) y))
  (precedes ((0 2) (6 0)) ((1 0) (0 0)) ((2 0) (0 0)) ((3 1) (0 3))
    ((4 1) (3 0)) ((5 1) (4 0)) ((6 1) (5 0)))
  (absent (y ltxa) (y ltxb) (y x))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa ltxb)
  (uniq-gen ltxa ltxb y)
  (uniq-orig nb)
  (operation encryption-test
    (added-listener (cat "share" (hash "public" ltxa) y))
    (hash "share" (hash "public" ltxa) y) (5 0))
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxa) y)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))))
    ((recv (cat "share" (hash "public" ltxa) y))
      (send (cat "share" (hash "public" ltxa) y))))
  (label 27)
  (parent 23)
  (unrealized (6 0))
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
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxa) y)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (deflistener (cat "share" (hash "public" ltxa) y))
  (precedes ((0 2) (6 0)) ((1 0) (0 0)) ((2 0) (0 0)) ((3 1) (0 3))
    ((4 1) (3 0)) ((5 1) (4 0)) ((6 1) (5 0)))
  (absent (y ltxa) (y ltxb) (y x))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa ltxb)
  (uniq-gen ltxa ltxb y)
  (uniq-orig nb)
  (operation encryption-test
    (added-listener (cat "share" (hash "public" ltxa) y))
    (hash "share" (hash "public" ltxa) y) (5 0))
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((send (cat b (hash "public" ltxb))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxa) y)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))))
    ((recv (cat "share" (hash "public" ltxa) y))
      (send (cat "share" (hash "public" ltxa) y))))
  (label 28)
  (parent 24)
  (unrealized (4 0) (6 0))
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
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat self (hash "public" ltxa)))))
  (label 29)
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa)))))
  (label 30)
  (parent 29)
  (unrealized (0 3))
  (origs (nb (0 2)))
  (comment "5 in cohort - 5 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (y ltxa ltxb x rndx))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (defstrand init 4
    (hint1 (hash "hint" (hash "share" (hash "public" ltxa) y)))
    (hint2 (hash "hint" (hash "share" (hash "public" x) ltxb))) (na na)
    (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (precedes ((0 2) (2 2)) ((1 0) (0 0)) ((1 0) (2 0)) ((2 1) (0 1))
    ((2 3) (0 3)))
  (absent (x ltxa) (x ltxb) (y ltxa) (y ltxb) (y x))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq ((exp (gen) y) (gen)) (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa)
  (uniq-gen y ltxa x)
  (uniq-orig na nb)
  (operation nonce-test (added-strand init 4) nb (0 3)
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" ltxb) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) ltxb)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" ltxb) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) ltxb)
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
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
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (send nb)))
  (label 31)
  (parent 30)
  (unrealized)
  (shape)
  (maps
    ((0 1)
      ((ltxa ltxa) (ltxb ltxb) (a a) (b b) (y y) (x x) (na na)
        (nb nb))))
  (origs (na (2 1)) (nb (0 2))))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (precedes ((0 2) (2 0)) ((1 0) (0 0)) ((2 1) (0 3)))
  (absent (y ltxa) (y ltxb) (y x))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa)
  (uniq-gen ltxa y)
  (uniq-orig nb)
  (operation nonce-test
    (added-listener
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" ltxb) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb))))) nb
    (0 3)
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" ltxb) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) ltxb)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" ltxb) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) ltxb)
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
  (label 32)
  (parent 30)
  (unrealized (2 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (precedes ((0 2) (2 0)) ((1 0) (0 0)) ((2 1) (0 3)))
  (absent (y ltxa) (y ltxb) (y x))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa)
  (uniq-gen ltxa y)
  (uniq-orig nb)
  (operation nonce-test
    (added-listener
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) ltxb)
          (hash "hint" (hash "share" (hash "public" x) ltxb))))) nb
    (0 3)
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" ltxb) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) ltxb)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" ltxb) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) ltxb)
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
  (label 33)
  (parent 30)
  (unrealized (2 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (precedes ((0 2) (2 0)) ((1 0) (0 0)) ((2 1) (0 3)))
  (absent (y ltxa) (y ltxb) (y x))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa)
  (uniq-gen ltxa y)
  (uniq-orig nb)
  (operation nonce-test
    (added-listener
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" ltxb) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb))))) nb
    (0 3)
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" ltxb) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) ltxb)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" ltxb) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) ltxb)
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
  (label 34)
  (parent 30)
  (unrealized (2 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (precedes ((0 2) (2 0)) ((1 0) (0 0)) ((2 1) (0 3)))
  (absent (y ltxa) (y ltxb) (y x))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa)
  (uniq-gen ltxa y)
  (uniq-orig nb)
  (operation nonce-test
    (added-listener
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) ltxb)
          (hash "hint" (hash "share" (hash "public" x) ltxb))))) nb
    (0 3)
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" ltxb) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) ltxb)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" ltxb) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) ltxb)
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
  (label 35)
  (parent 30)
  (unrealized (2 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (precedes ((0 2) (3 0)) ((1 0) (0 0)) ((2 1) (0 3)) ((3 1) (2 0)))
  (absent (y ltxa) (y ltxb) (y x))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa)
  (uniq-gen ltxa y)
  (uniq-orig nb)
  (operation encryption-test
    (added-listener
      (cat "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" ltxb) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))) (2 0))
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
  (label 36)
  (parent 32)
  (unrealized (3 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (precedes ((0 2) (3 0)) ((1 0) (0 0)) ((2 1) (0 3)) ((3 1) (2 0)))
  (absent (y ltxa) (y ltxb) (y x))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa)
  (uniq-gen ltxa y)
  (uniq-orig nb)
  (operation encryption-test
    (added-listener
      (cat "key"
        (hash "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) ltxb)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))) (2 0))
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
  (label 37)
  (parent 33)
  (unrealized (3 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (precedes ((0 2) (3 0)) ((1 0) (0 0)) ((2 1) (0 3)) ((3 1) (2 0)))
  (absent (y ltxa) (y ltxb) (y x))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa)
  (uniq-gen ltxa y)
  (uniq-orig nb)
  (operation encryption-test
    (added-listener
      (cat "key"
        (hash "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" ltxb) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))) (2 0))
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
  (label 38)
  (parent 34)
  (unrealized (3 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (precedes ((0 2) (3 0)) ((1 0) (0 0)) ((2 1) (0 3)) ((3 1) (2 0)))
  (absent (y ltxa) (y ltxb) (y x))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa)
  (uniq-gen ltxa y)
  (uniq-orig nb)
  (operation encryption-test
    (added-listener
      (cat "key"
        (hash "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) ltxb)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))) (2 0))
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
  (label 39)
  (parent 35)
  (unrealized (3 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxa) y)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (precedes ((0 2) (4 0)) ((1 0) (0 0)) ((2 1) (0 3)) ((3 1) (2 0))
    ((4 1) (3 0)))
  (absent (y ltxa) (y ltxb) (y x))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa)
  (uniq-gen ltxa y)
  (uniq-orig nb)
  (operation encryption-test
    (added-listener
      (cat "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y))))
    (hash "nh_key" (hash "share" (hash "public" ltxa) y)
      (hash "hint" (hash "share" (hash "public" ltxa) y))) (3 0))
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxa) y)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))))))
  (label 40)
  (parent 36)
  (unrealized (4 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxa) y)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (precedes ((0 2) (4 0)) ((1 0) (0 0)) ((2 1) (0 3)) ((3 1) (2 0))
    ((4 1) (3 0)))
  (absent (y ltxa) (y ltxb) (y x))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa)
  (uniq-gen ltxa y)
  (uniq-orig nb)
  (operation encryption-test
    (added-listener
      (cat "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y))))
    (hash "nh_key" (hash "share" (hash "public" ltxa) y)
      (hash "hint" (hash "share" (hash "public" ltxa) y))) (3 0))
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxa) y)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))))))
  (label 41)
  (parent 37)
  (unrealized (4 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" y) ltxa)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (precedes ((0 2) (4 0)) ((1 0) (0 0)) ((2 1) (0 3)) ((3 1) (2 0))
    ((4 1) (3 0)))
  (absent (y ltxa) (y ltxb) (y x))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa)
  (uniq-gen ltxa y)
  (uniq-orig nb)
  (operation encryption-test
    (added-listener
      (cat "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y))))
    (hash "nh_key" (hash "share" (hash "public" y) ltxa)
      (hash "hint" (hash "share" (hash "public" ltxa) y))) (3 0))
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" y) ltxa)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y))))))
  (label 42)
  (parent 38)
  (unrealized (4 0))
  (dead)
  (comment "empty cohort"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" y) ltxa)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (precedes ((0 2) (4 0)) ((1 0) (0 0)) ((2 1) (0 3)) ((3 1) (2 0))
    ((4 1) (3 0)))
  (absent (y ltxa) (y ltxb) (y x))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa)
  (uniq-gen ltxa y)
  (uniq-orig nb)
  (operation encryption-test
    (added-listener
      (cat "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y))))
    (hash "nh_key" (hash "share" (hash "public" y) ltxa)
      (hash "hint" (hash "share" (hash "public" ltxa) y))) (3 0))
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" y) ltxa)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y))))))
  (label 43)
  (parent 39)
  (unrealized (4 0))
  (dead)
  (comment "empty cohort"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxa) y)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (deflistener (cat "share" (hash "public" ltxa) y))
  (precedes ((0 2) (5 0)) ((1 0) (0 0)) ((2 1) (0 3)) ((3 1) (2 0))
    ((4 1) (3 0)) ((5 1) (4 0)))
  (absent (y ltxa) (y ltxb) (y x))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa)
  (uniq-gen ltxa y)
  (uniq-orig nb)
  (operation encryption-test
    (added-listener (cat "share" (hash "public" ltxa) y))
    (hash "share" (hash "public" ltxa) y) (4 0))
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxa) y)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))))
    ((recv (cat "share" (hash "public" ltxa) y))
      (send (cat "share" (hash "public" ltxa) y))))
  (label 44)
  (parent 40)
  (unrealized (5 0))
  (dead)
  (comment "empty cohort"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxa) y)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (deflistener (cat "share" (hash "public" ltxa) y))
  (precedes ((0 2) (5 0)) ((1 0) (0 0)) ((2 1) (0 3)) ((3 1) (2 0))
    ((4 1) (3 0)) ((5 1) (4 0)))
  (absent (y ltxa) (y ltxb) (y x))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa)
  (uniq-gen ltxa y)
  (uniq-orig nb)
  (operation encryption-test
    (added-listener (cat "share" (hash "public" ltxa) y))
    (hash "share" (hash "public" ltxa) y) (4 0))
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (recv nb)) ((send (cat a (hash "public" ltxa))))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxa) y)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))))
    ((recv (cat "share" (hash "public" ltxa) y))
      (send (cat "share" (hash "public" ltxa) y))))
  (label 45)
  (parent 41)
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
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
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
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat self (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat self-0 (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb)))
  (label 46)
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
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
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
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb)))
  (label 47)
  (parent 46)
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
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
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
        (hash "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) ltxb)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))) (1 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
  (label 48)
  (parent 47)
  (unrealized (0 2) (4 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa ltxb x rndx)
    (y expt))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) ltxb)
      (hash "hint" (hash "share" (hash "public" x) ltxb))))
  (precedes ((0 1) (5 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((3 0) (0 0)) ((4 1) (1 0)) ((5 1) (4 0)))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (cat "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb))))
    (hash "nh_key" (hash "share" (hash "public" x) ltxb)
      (hash "hint" (hash "share" (hash "public" x) ltxb))) (4 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) ltxb)
         (hash "hint" (hash "share" (hash "public" x) ltxb))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) ltxb)
          (hash "hint" (hash "share" (hash "public" x) ltxb))))))
  (label 49)
  (parent 48)
  (unrealized (0 2) (5 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa ltxb x rndx)
    (y expt))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) ltxb)
      (hash "hint" (hash "share" (hash "public" x) ltxb))))
  (deflistener (cat "share" (hash "public" x) ltxb))
  (precedes ((0 1) (6 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((3 0) (0 0)) ((4 1) (1 0)) ((5 1) (4 0)) ((6 1) (5 0)))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (operation encryption-test
    (added-listener (cat "share" (hash "public" x) ltxb))
    (hash "share" (hash "public" x) ltxb) (5 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) ltxb)
         (hash "hint" (hash "share" (hash "public" x) ltxb))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) ltxb)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    ((recv (cat "share" (hash "public" x) ltxb))
      (send (cat "share" (hash "public" x) ltxb))))
  (label 50)
  (parent 49)
  (unrealized (0 2) (6 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa x rndx)
    (y expt) (l rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb l) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) l)
        (hash "hint" (hash "share" (hash "public" x) l)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l l))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) l)
        (hash "hint" (hash "share" (hash "public" x) l)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) l)
      (hash "hint" (hash "share" (hash "public" x) l))))
  (deflistener (cat "share" (hash "public" x) l))
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
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" l) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) l)
           (hash "hint" (hash "share" (hash "public" x) l)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) l)
            (hash "hint" (hash "share" (hash "public" x) l))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" l))) (recv "end-of-protocol") (send l))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) l)
           (hash "hint" (hash "share" (hash "public" x) l)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) l)
            (hash "hint" (hash "share" (hash "public" x) l))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) l)
         (hash "hint" (hash "share" (hash "public" x) l))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) l)
          (hash "hint" (hash "share" (hash "public" x) l)))))
    ((recv (cat "share" (hash "public" x) l))
      (send (cat "share" (hash "public" x) l))))
  (label 51)
  (parent 50)
  (unrealized (0 2))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (y ltxa ltxb x rndx))
  (defstrand init 4
    (hint1 (hash "hint" (hash "share" (hash "public" ltxa) y)))
    (hint2 (hash "hint" (hash "share" (hash "public" x) ltxb))) (na na)
    (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) ltxb)
      (hash "hint" (hash "share" (hash "public" x) ltxb))))
  (deflistener (cat "share" (hash "public" x) ltxb))
  (defstrand resp 3 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (precedes ((0 1) (7 1)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (7 0)) ((3 0) (0 0)) ((3 0) (7 0)) ((3 2) (6 0))
    ((4 1) (1 0)) ((5 1) (4 0)) ((6 1) (5 0)) ((7 2) (0 2)))
  (absent (y ltxa) (y ltxb) (y x) (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq ((exp (gen) x) (gen)) (a b) ((exp (gen) y) (gen)))
  (uniq-gen y ltxa ltxb x)
  (uniq-orig na nb)
  (operation encryption-test (added-strand resp 3)
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) ltxb)
         (hash "hint" (hash "share" (hash "public" x) ltxb))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) ltxb)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint"
                  (hash "share" (hash "public" x) ltxb)))))))))
  (label 52)
  (parent 51)
  (unrealized (4 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa x rndx)
    (y expt) (l rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb l) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) l)
        (hash "hint" (hash "share" (hash "public" x) l)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l l))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) l)
        (hash "hint" (hash "share" (hash "public" x) l)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) l)
      (hash "hint" (hash "share" (hash "public" x) l))))
  (deflistener (cat "share" (hash "public" x) l))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" l) x) hint2)))
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
        (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
        (hash "nh_key" (hash "share" (hash "public" l) x) hint2)))
    (enc na nb a b hint1 hint2
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
        (hash "nh_key" (hash "share" (hash "public" l) x) hint2)))
    (0 2))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" l)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" l) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) l)
           (hash "hint" (hash "share" (hash "public" x) l)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) l)
            (hash "hint" (hash "share" (hash "public" x) l))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" l))) (recv "end-of-protocol") (send l))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) l)
           (hash "hint" (hash "share" (hash "public" x) l)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) l)
            (hash "hint" (hash "share" (hash "public" x) l))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) l)
         (hash "hint" (hash "share" (hash "public" x) l))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) l)
          (hash "hint" (hash "share" (hash "public" x) l)))))
    ((recv (cat "share" (hash "public" x) l))
      (send (cat "share" (hash "public" x) l)))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
         (hash "nh_key" (hash "share" (hash "public" l) x) hint2)))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
          (hash "nh_key" (hash "share" (hash "public" l) x) hint2)))))
  (label 53)
  (parent 51)
  (unrealized (7 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (y ltxa ltxb x rndx))
  (defstrand init 4
    (hint1 (hash "hint" (hash "share" (hash "public" ltxa) y)))
    (hint2 (hash "hint" (hash "share" (hash "public" x) ltxb))) (na na)
    (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) ltxb)
      (hash "hint" (hash "share" (hash "public" x) ltxb))))
  (deflistener (cat "share" (hash "public" x) ltxb))
  (defstrand resp 3 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxa) y)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (precedes ((0 1) (7 1)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (7 0)) ((3 0) (0 0)) ((3 0) (7 0)) ((3 2) (6 0))
    ((4 1) (1 0)) ((5 1) (4 0)) ((6 1) (5 0)) ((7 2) (0 2))
    ((7 2) (8 0)) ((8 1) (4 0)))
  (absent (y ltxa) (y ltxb) (y x) (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq ((exp (gen) x) (gen)) (a b) ((exp (gen) y) (gen)))
  (uniq-gen y ltxa ltxb x)
  (uniq-orig na nb)
  (operation encryption-test
    (added-listener
      (cat "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y))))
    (hash "nh_key" (hash "share" (hash "public" ltxa) y)
      (hash "hint" (hash "share" (hash "public" ltxa) y))) (4 0))
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
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) ltxb)
         (hash "hint" (hash "share" (hash "public" x) ltxb))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) ltxb)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint"
                  (hash "share" (hash "public" x) ltxb))))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxa) y)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y))))))
  (label 54)
  (parent 52)
  (unrealized (8 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa x rndx)
    (y expt) (l rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb l) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) l)
        (hash "hint" (hash "share" (hash "public" x) l)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l l))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) l)
        (hash "hint" (hash "share" (hash "public" x) l)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) l)
      (hash "hint" (hash "share" (hash "public" x) l))))
  (deflistener (cat "share" (hash "public" x) l))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" l) x) hint2)))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" l) x) hint2)))
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
        (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
        (hash "nh_key" (hash "share" (hash "public" l) x) hint2)))
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" l) x) hint2)) (7 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" l)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" l) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) l)
           (hash "hint" (hash "share" (hash "public" x) l)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) l)
            (hash "hint" (hash "share" (hash "public" x) l))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" l))) (recv "end-of-protocol") (send l))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) l)
           (hash "hint" (hash "share" (hash "public" x) l)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) l)
            (hash "hint" (hash "share" (hash "public" x) l))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) l)
         (hash "hint" (hash "share" (hash "public" x) l))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) l)
          (hash "hint" (hash "share" (hash "public" x) l)))))
    ((recv (cat "share" (hash "public" x) l))
      (send (cat "share" (hash "public" x) l)))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
         (hash "nh_key" (hash "share" (hash "public" l) x) hint2)))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
          (hash "nh_key" (hash "share" (hash "public" l) x) hint2))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
         (hash "nh_key" (hash "share" (hash "public" l) x) hint2)))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
          (hash "nh_key" (hash "share" (hash "public" l) x) hint2)))))
  (label 55)
  (parent 53)
  (unrealized (8 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (y ltxa ltxb x rndx))
  (defstrand init 4
    (hint1 (hash "hint" (hash "share" (hash "public" ltxa) y)))
    (hint2 (hash "hint" (hash "share" (hash "public" x) ltxb))) (na na)
    (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) ltxb)
      (hash "hint" (hash "share" (hash "public" x) ltxb))))
  (deflistener (cat "share" (hash "public" x) ltxb))
  (defstrand resp 3 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxa) y)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (deflistener (cat "share" (hash "public" ltxa) y))
  (precedes ((0 1) (7 1)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (7 0)) ((3 0) (0 0)) ((3 0) (7 0)) ((3 2) (6 0))
    ((4 1) (1 0)) ((5 1) (4 0)) ((6 1) (5 0)) ((7 2) (0 2))
    ((7 2) (9 0)) ((8 1) (4 0)) ((9 1) (8 0)))
  (absent (y ltxa) (y ltxb) (y x) (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq ((exp (gen) x) (gen)) (a b) ((exp (gen) y) (gen)))
  (uniq-gen y ltxa ltxb x)
  (uniq-orig na nb)
  (operation encryption-test
    (added-listener (cat "share" (hash "public" ltxa) y))
    (hash "share" (hash "public" ltxa) y) (8 0))
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
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) ltxb)
         (hash "hint" (hash "share" (hash "public" x) ltxb))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) ltxb)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint"
                  (hash "share" (hash "public" x) ltxb))))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxa) y)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))))
    ((recv (cat "share" (hash "public" ltxa) y))
      (send (cat "share" (hash "public" ltxa) y))))
  (label 56)
  (parent 54)
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
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) l)
        (hash "hint" (hash "share" (hash "public" x) l)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l l))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) l)
        (hash "hint" (hash "share" (hash "public" x) l)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) l)
      (hash "hint" (hash "share" (hash "public" x) l))))
  (deflistener (cat "share" (hash "public" x) l))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" l) x) hint2)))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" l) x) hint2)))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" y) ltxa) hint1))
  (precedes ((0 1) (8 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (9 0)) ((3 0) (0 0)) ((3 2) (6 0)) ((4 1) (1 0))
    ((5 1) (4 0)) ((6 1) (5 0)) ((7 1) (0 2)) ((8 1) (7 0))
    ((9 1) (8 0)))
  (absent (x ltxa) (x l))
  (fn-of ("ltx-of" (b l) (a ltxa)) ("principal-of" (l b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa x l)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (cat "nh_key" (hash "share" (hash "public" y) ltxa) hint1))
    (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1) (8 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" l)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" l) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) l)
           (hash "hint" (hash "share" (hash "public" x) l)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) l)
            (hash "hint" (hash "share" (hash "public" x) l))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" l))) (recv "end-of-protocol") (send l))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) l)
           (hash "hint" (hash "share" (hash "public" x) l)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) l)
            (hash "hint" (hash "share" (hash "public" x) l))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) l)
         (hash "hint" (hash "share" (hash "public" x) l))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) l)
          (hash "hint" (hash "share" (hash "public" x) l)))))
    ((recv (cat "share" (hash "public" x) l))
      (send (cat "share" (hash "public" x) l)))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
         (hash "nh_key" (hash "share" (hash "public" l) x) hint2)))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
          (hash "nh_key" (hash "share" (hash "public" l) x) hint2))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
         (hash "nh_key" (hash "share" (hash "public" l) x) hint2)))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
          (hash "nh_key" (hash "share" (hash "public" l) x) hint2))))
    ((recv (cat "nh_key" (hash "share" (hash "public" y) ltxa) hint1))
      (send
        (cat "nh_key" (hash "share" (hash "public" y) ltxa) hint1))))
  (label 57)
  (parent 55)
  (unrealized (8 0) (9 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa x rndx)
    (y expt) (l rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb l) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) l)
        (hash "hint" (hash "share" (hash "public" x) l)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l l))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) l)
        (hash "hint" (hash "share" (hash "public" x) l)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) l)
      (hash "hint" (hash "share" (hash "public" x) l))))
  (deflistener (cat "share" (hash "public" x) l))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" l) x) hint2)))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
      (hash "nh_key" (hash "share" (hash "public" l) x) hint2)))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" y) ltxa) hint1))
  (deflistener (cat "share" (hash "public" y) ltxa))
  (precedes ((0 1) (8 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (10 0)) ((3 0) (0 0)) ((3 2) (6 0)) ((4 1) (1 0))
    ((5 1) (4 0)) ((6 1) (5 0)) ((7 1) (0 2)) ((8 1) (7 0))
    ((9 1) (8 0)) ((10 1) (9 0)))
  (absent (x ltxa) (x l))
  (fn-of ("ltx-of" (b l) (a ltxa)) ("principal-of" (l b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa x l)
  (uniq-orig na)
  (operation encryption-test
    (added-listener (cat "share" (hash "public" y) ltxa))
    (hash "share" (hash "public" y) ltxa) (9 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" l)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" l) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) l)
           (hash "hint" (hash "share" (hash "public" x) l)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) l)
            (hash "hint" (hash "share" (hash "public" x) l))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" l))) (recv "end-of-protocol") (send l))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) l)
           (hash "hint" (hash "share" (hash "public" x) l)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) l)
            (hash "hint" (hash "share" (hash "public" x) l))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) l)
         (hash "hint" (hash "share" (hash "public" x) l))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) l)
          (hash "hint" (hash "share" (hash "public" x) l)))))
    ((recv (cat "share" (hash "public" x) l))
      (send (cat "share" (hash "public" x) l)))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
         (hash "nh_key" (hash "share" (hash "public" l) x) hint2)))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
          (hash "nh_key" (hash "share" (hash "public" l) x) hint2))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
         (hash "nh_key" (hash "share" (hash "public" l) x) hint2)))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa) hint1)
          (hash "nh_key" (hash "share" (hash "public" l) x) hint2))))
    ((recv (cat "nh_key" (hash "share" (hash "public" y) ltxa) hint1))
      (send (cat "nh_key" (hash "share" (hash "public" y) ltxa) hint1)))
    ((recv (cat "share" (hash "public" y) ltxa))
      (send (cat "share" (hash "public" y) ltxa))))
  (label 58)
  (parent 57)
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
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat self (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat self-0 (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb)))
  (label 59)
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
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb)))
  (label 60)
  (parent 59)
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
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
        (hash "nh_key" (hash "share" (hash "public" ltxa) y)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" ltxb) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))) (1 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
  (label 61)
  (parent 60)
  (unrealized (0 2) (4 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa ltxb x rndx)
    (y expt))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxb) x)
      (hash "hint" (hash "share" (hash "public" x) ltxb))))
  (precedes ((0 1) (5 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((3 0) (0 0)) ((4 1) (1 0)) ((5 1) (4 0)))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (cat "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb))))
    (hash "nh_key" (hash "share" (hash "public" ltxb) x)
      (hash "hint" (hash "share" (hash "public" x) ltxb))) (4 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxb) x)
         (hash "hint" (hash "share" (hash "public" x) ltxb))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxb) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb))))))
  (label 62)
  (parent 61)
  (unrealized (0 2) (5 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa ltxb x rndx)
    (y expt))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" ltxa) y)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxb) x)
      (hash "hint" (hash "share" (hash "public" x) ltxb))))
  (deflistener (cat "share" (hash "public" ltxb) x))
  (precedes ((0 1) (6 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((3 0) (0 0)) ((4 1) (1 0)) ((5 1) (4 0)) ((6 1) (5 0)))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (operation encryption-test
    (added-listener (cat "share" (hash "public" ltxb) x))
    (hash "share" (hash "public" ltxb) x) (5 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" ltxa) y)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" ltxa) y)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxb) x)
         (hash "hint" (hash "share" (hash "public" x) ltxb))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxb) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    ((recv (cat "share" (hash "public" ltxb) x))
      (send (cat "share" (hash "public" ltxb) x))))
  (label 63)
  (parent 62)
  (unrealized (0 2) (5 0) (6 0))
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
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
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
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat self (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat self-0 (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb)))
  (label 64)
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
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
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
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb)))
  (label 65)
  (parent 64)
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
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
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
        (hash "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" x) ltxb)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))) (1 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
  (label 66)
  (parent 65)
  (unrealized (0 2) (4 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa ltxb x rndx)
    (y expt))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" y) ltxa)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (precedes ((0 1) (4 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (5 0)) ((3 0) (0 0)) ((4 1) (1 0)) ((5 1) (4 0)))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (cat "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y))))
    (hash "nh_key" (hash "share" (hash "public" y) ltxa)
      (hash "hint" (hash "share" (hash "public" ltxa) y))) (4 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" y) ltxa)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y))))))
  (label 67)
  (parent 66)
  (unrealized (0 2) (4 0) (5 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa ltxb x rndx)
    (y expt))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" y) ltxa)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (deflistener (cat "share" (hash "public" y) ltxa))
  (precedes ((0 1) (4 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (6 0)) ((3 0) (0 0)) ((4 1) (1 0)) ((5 1) (4 0))
    ((6 1) (5 0)))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (operation encryption-test
    (added-listener (cat "share" (hash "public" y) ltxa))
    (hash "share" (hash "public" y) ltxa) (5 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" y) ltxa)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))))
    ((recv (cat "share" (hash "public" y) ltxa))
      (send (cat "share" (hash "public" y) ltxa))))
  (label 68)
  (parent 67)
  (unrealized (0 2) (4 0) (6 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxb x rndx)
    (y expt) (l rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa l) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l l))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" y) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" y) l)
      (hash "hint" (hash "share" (hash "public" l) y))))
  (deflistener (cat "share" (hash "public" y) l))
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
              (hash "nh_key" (hash "share" (hash "public" y) l) hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" l))) (recv "end-of-protocol") (send l))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" y) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" y) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" y) l)
         (hash "hint" (hash "share" (hash "public" l) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" y) l)
          (hash "hint" (hash "share" (hash "public" l) y)))))
    ((recv (cat "share" (hash "public" y) l))
      (send (cat "share" (hash "public" y) l))))
  (label 69)
  (parent 68)
  (unrealized (0 2) (4 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxb x rndx)
    (y expt) (l rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa l) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l l))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" y) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" y) l)
      (hash "hint" (hash "share" (hash "public" l) y))))
  (deflistener (cat "share" (hash "public" y) l))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) ltxb)
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
      (cat "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb))))
    (hash "nh_key" (hash "share" (hash "public" x) ltxb)
      (hash "hint" (hash "share" (hash "public" x) ltxb))) (4 0))
  (traces
    ((recv (cat (hash "public" l) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) l) hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" l))) (recv "end-of-protocol") (send l))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" y) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" y) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" y) l)
         (hash "hint" (hash "share" (hash "public" l) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" y) l)
          (hash "hint" (hash "share" (hash "public" l) y)))))
    ((recv (cat "share" (hash "public" y) l))
      (send (cat "share" (hash "public" y) l)))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) ltxb)
         (hash "hint" (hash "share" (hash "public" x) ltxb))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) ltxb)
          (hash "hint" (hash "share" (hash "public" x) ltxb))))))
  (label 70)
  (parent 69)
  (unrealized (0 2) (7 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxb x rndx)
    (y expt) (l rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa l) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l l))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" y) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" y) l)
      (hash "hint" (hash "share" (hash "public" l) y))))
  (deflistener (cat "share" (hash "public" y) l))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) ltxb)
      (hash "hint" (hash "share" (hash "public" x) ltxb))))
  (deflistener (cat "share" (hash "public" x) ltxb))
  (precedes ((0 1) (8 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 2) (6 0)) ((3 0) (0 0)) ((4 1) (1 0)) ((5 1) (4 0))
    ((6 1) (5 0)) ((7 1) (4 0)) ((8 1) (7 0)))
  (absent (x l) (x ltxb))
  (fn-of ("ltx-of" (a l) (b ltxb)) ("principal-of" (l a) (ltxb b)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxb x l)
  (uniq-orig na)
  (operation encryption-test
    (added-listener (cat "share" (hash "public" x) ltxb))
    (hash "share" (hash "public" x) ltxb) (7 0))
  (traces
    ((recv (cat (hash "public" l) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) l) hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" l))) (recv "end-of-protocol") (send l))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" y) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" y) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" y) l)
         (hash "hint" (hash "share" (hash "public" l) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" y) l)
          (hash "hint" (hash "share" (hash "public" l) y)))))
    ((recv (cat "share" (hash "public" y) l))
      (send (cat "share" (hash "public" y) l)))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) ltxb)
         (hash "hint" (hash "share" (hash "public" x) ltxb))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) ltxb)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    ((recv (cat "share" (hash "public" x) ltxb))
      (send (cat "share" (hash "public" x) ltxb))))
  (label 71)
  (parent 70)
  (unrealized (0 2) (8 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (x rndx) (y expt)
    (l l-0 rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa l) (ltxb l-0) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) l-0)
        (hash "hint" (hash "share" (hash "public" x) l-0)))))
  (defstrand ltx-gen 3 (self a) (l l))
  (defstrand ltx-gen 3 (self b) (l l-0))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" y) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) l-0)
        (hash "hint" (hash "share" (hash "public" x) l-0)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" y) l)
      (hash "hint" (hash "share" (hash "public" l) y))))
  (deflistener (cat "share" (hash "public" y) l))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) l-0)
      (hash "hint" (hash "share" (hash "public" x) l-0))))
  (deflistener (cat "share" (hash "public" x) l-0))
  (precedes ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0)) ((2 2) (6 0))
    ((3 0) (0 0)) ((3 2) (8 0)) ((4 1) (1 0)) ((5 1) (4 0))
    ((6 1) (5 0)) ((7 1) (4 0)) ((8 1) (7 0)))
  (absent (x l) (x l-0))
  (fn-of ("ltx-of" (b l-0) (a l)) ("principal-of" (l-0 b) (l a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen x l l-0)
  (uniq-orig na)
  (operation nonce-test (displaced 9 3 ltx-gen 3) l-0 (8 0))
  (traces
    ((recv (cat (hash "public" l) (hash "public" l-0)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) l) hint1)
              (hash "nh_key" (hash "share" (hash "public" l-0) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) l-0)
           (hash "hint" (hash "share" (hash "public" x) l-0)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) l-0)
            (hash "hint" (hash "share" (hash "public" x) l-0))))))
    ((send (cat a (hash "public" l))) (recv "end-of-protocol") (send l))
    ((send (cat b (hash "public" l-0))) (recv "end-of-protocol")
      (send l-0))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" y) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) l-0)
           (hash "hint" (hash "share" (hash "public" x) l-0)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" y) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) l-0)
            (hash "hint" (hash "share" (hash "public" x) l-0))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" y) l)
         (hash "hint" (hash "share" (hash "public" l) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" y) l)
          (hash "hint" (hash "share" (hash "public" l) y)))))
    ((recv (cat "share" (hash "public" y) l))
      (send (cat "share" (hash "public" y) l)))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) l-0)
         (hash "hint" (hash "share" (hash "public" x) l-0))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) l-0)
          (hash "hint" (hash "share" (hash "public" x) l-0)))))
    ((recv (cat "share" (hash "public" x) l-0))
      (send (cat "share" (hash "public" x) l-0))))
  (label 72)
  (parent 71)
  (unrealized (0 2))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (y ltxa ltxb x rndx))
  (defstrand init 4
    (hint1 (hash "hint" (hash "share" (hash "public" ltxa) y)))
    (hint2 (hash "hint" (hash "share" (hash "public" x) ltxb))) (na na)
    (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" y) ltxa)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (deflistener (cat "share" (hash "public" y) ltxa))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) ltxb)
      (hash "hint" (hash "share" (hash "public" x) ltxb))))
  (deflistener (cat "share" (hash "public" x) ltxb))
  (defstrand resp 3 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (precedes ((0 1) (9 1)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (9 0)) ((2 2) (6 0)) ((3 0) (0 0)) ((3 0) (9 0))
    ((3 2) (8 0)) ((4 1) (1 0)) ((5 1) (4 0)) ((6 1) (5 0))
    ((7 1) (4 0)) ((8 1) (7 0)) ((9 2) (0 2)))
  (absent (y ltxa) (y ltxb) (y x) (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq ((exp (gen) x) (gen)) (a b) ((exp (gen) y) (gen)))
  (uniq-gen y ltxa ltxb x)
  (uniq-orig na nb)
  (operation encryption-test (added-strand resp 3)
    (enc na nb a b (hash "hint" (hash "share" (hash "public" ltxa) y))
      (hash "hint" (hash "share" (hash "public" x) ltxb))
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" y) ltxa)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))))
    ((recv (cat "share" (hash "public" y) ltxa))
      (send (cat "share" (hash "public" y) ltxa)))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) ltxb)
         (hash "hint" (hash "share" (hash "public" x) ltxb))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) ltxb)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint"
                  (hash "share" (hash "public" x) ltxb)))))))))
  (label 73)
  (parent 72)
  (unrealized)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (x rndx) (y expt)
    (l l-0 rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa l) (ltxb l-0) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) l-0)
        (hash "hint" (hash "share" (hash "public" x) l-0)))))
  (defstrand ltx-gen 3 (self a) (l l))
  (defstrand ltx-gen 3 (self b) (l l-0))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" y) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) l-0)
        (hash "hint" (hash "share" (hash "public" x) l-0)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" y) l)
      (hash "hint" (hash "share" (hash "public" l) y))))
  (deflistener (cat "share" (hash "public" y) l))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) l-0)
      (hash "hint" (hash "share" (hash "public" x) l-0))))
  (deflistener (cat "share" (hash "public" x) l-0))
  (deflistener
    (hash "key" (hash "nh_key" (hash "share" (hash "public" y) l) hint1)
      (hash "nh_key" (hash "share" (hash "public" l-0) x) hint2)))
  (precedes ((0 1) (9 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 2) (6 0)) ((3 0) (0 0)) ((3 2) (8 0)) ((4 1) (1 0))
    ((5 1) (4 0)) ((6 1) (5 0)) ((7 1) (4 0)) ((8 1) (7 0))
    ((9 1) (0 2)))
  (absent (x l) (x l-0))
  (fn-of ("ltx-of" (b l-0) (a l)) ("principal-of" (l-0 b) (l a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen x l l-0)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" y) l) hint1)
        (hash "nh_key" (hash "share" (hash "public" l-0) x) hint2)))
    (enc na nb a b hint1 hint2
      (hash "key"
        (hash "nh_key" (hash "share" (hash "public" y) l) hint1)
        (hash "nh_key" (hash "share" (hash "public" l-0) x) hint2)))
    (0 2))
  (traces
    ((recv (cat (hash "public" l) (hash "public" l-0)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) l) hint1)
              (hash "nh_key" (hash "share" (hash "public" l-0) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) l-0)
           (hash "hint" (hash "share" (hash "public" x) l-0)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) l-0)
            (hash "hint" (hash "share" (hash "public" x) l-0))))))
    ((send (cat a (hash "public" l))) (recv "end-of-protocol") (send l))
    ((send (cat b (hash "public" l-0))) (recv "end-of-protocol")
      (send l-0))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" y) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) l-0)
           (hash "hint" (hash "share" (hash "public" x) l-0)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" y) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) l-0)
            (hash "hint" (hash "share" (hash "public" x) l-0))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" y) l)
         (hash "hint" (hash "share" (hash "public" l) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" y) l)
          (hash "hint" (hash "share" (hash "public" l) y)))))
    ((recv (cat "share" (hash "public" y) l))
      (send (cat "share" (hash "public" y) l)))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) l-0)
         (hash "hint" (hash "share" (hash "public" x) l-0))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) l-0)
          (hash "hint" (hash "share" (hash "public" x) l-0)))))
    ((recv (cat "share" (hash "public" x) l-0))
      (send (cat "share" (hash "public" x) l-0)))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) l) hint1)
         (hash "nh_key" (hash "share" (hash "public" l-0) x) hint2)))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) l) hint1)
          (hash "nh_key" (hash "share" (hash "public" l-0) x) hint2)))))
  (label 74)
  (parent 72)
  (unrealized (9 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (y ltxa ltxb x rndx))
  (defstrand init 4
    (hint1 (hash "hint" (hash "share" (hash "public" ltxa) y)))
    (hint2 (hash "hint" (hash "share" (hash "public" x) ltxb))) (na na)
    (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" y) ltxa)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (deflistener (cat "share" (hash "public" y) ltxa))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) ltxb)
      (hash "hint" (hash "share" (hash "public" x) ltxb))))
  (deflistener (cat "share" (hash "public" x) ltxb))
  (defstrand resp 3 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (precedes ((0 1) (8 1)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (8 0)) ((2 2) (5 0)) ((3 0) (0 0)) ((3 0) (8 0))
    ((3 2) (7 0)) ((4 1) (1 0)) ((5 1) (4 0)) ((6 1) (1 0))
    ((7 1) (6 0)) ((8 2) (0 2)))
  (absent (y ltxa) (y ltxb) (y x) (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq ((exp (gen) x) (gen)) (a b) ((exp (gen) y) (gen)))
  (uniq-gen y ltxa ltxb x)
  (uniq-orig na nb)
  (operation generalization deleted (4 0))
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
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" y) ltxa)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))))
    ((recv (cat "share" (hash "public" y) ltxa))
      (send (cat "share" (hash "public" y) ltxa)))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) ltxb)
         (hash "hint" (hash "share" (hash "public" x) ltxb))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) ltxb)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint"
                  (hash "share" (hash "public" x) ltxb)))))))))
  (label 75)
  (parent 73)
  (unrealized)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (x rndx) (y expt)
    (l l-0 rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa l) (ltxb l-0) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) l-0)
        (hash "hint" (hash "share" (hash "public" x) l-0)))))
  (defstrand ltx-gen 3 (self a) (l l))
  (defstrand ltx-gen 3 (self b) (l l-0))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" y) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) l-0)
        (hash "hint" (hash "share" (hash "public" x) l-0)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" y) l)
      (hash "hint" (hash "share" (hash "public" l) y))))
  (deflistener (cat "share" (hash "public" y) l))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) l-0)
      (hash "hint" (hash "share" (hash "public" x) l-0))))
  (deflistener (cat "share" (hash "public" x) l-0))
  (deflistener
    (hash "key" (hash "nh_key" (hash "share" (hash "public" y) l) hint1)
      (hash "nh_key" (hash "share" (hash "public" l-0) x) hint2)))
  (deflistener
    (cat "key" (hash "nh_key" (hash "share" (hash "public" y) l) hint1)
      (hash "nh_key" (hash "share" (hash "public" l-0) x) hint2)))
  (precedes ((0 1) (10 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 2) (6 0)) ((3 0) (0 0)) ((3 2) (8 0)) ((4 1) (1 0))
    ((5 1) (4 0)) ((6 1) (5 0)) ((7 1) (4 0)) ((8 1) (7 0))
    ((9 1) (0 2)) ((10 1) (9 0)))
  (absent (x l) (x l-0))
  (fn-of ("ltx-of" (b l-0) (a l)) ("principal-of" (l-0 b) (l a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen x l l-0)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (cat "key"
        (hash "nh_key" (hash "share" (hash "public" y) l) hint1)
        (hash "nh_key" (hash "share" (hash "public" l-0) x) hint2)))
    (hash "key" (hash "nh_key" (hash "share" (hash "public" y) l) hint1)
      (hash "nh_key" (hash "share" (hash "public" l-0) x) hint2)) (9 0))
  (traces
    ((recv (cat (hash "public" l) (hash "public" l-0)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) l) hint1)
              (hash "nh_key" (hash "share" (hash "public" l-0) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) l-0)
           (hash "hint" (hash "share" (hash "public" x) l-0)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) l-0)
            (hash "hint" (hash "share" (hash "public" x) l-0))))))
    ((send (cat a (hash "public" l))) (recv "end-of-protocol") (send l))
    ((send (cat b (hash "public" l-0))) (recv "end-of-protocol")
      (send l-0))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" y) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) l-0)
           (hash "hint" (hash "share" (hash "public" x) l-0)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" y) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) l-0)
            (hash "hint" (hash "share" (hash "public" x) l-0))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" y) l)
         (hash "hint" (hash "share" (hash "public" l) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" y) l)
          (hash "hint" (hash "share" (hash "public" l) y)))))
    ((recv (cat "share" (hash "public" y) l))
      (send (cat "share" (hash "public" y) l)))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) l-0)
         (hash "hint" (hash "share" (hash "public" x) l-0))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) l-0)
          (hash "hint" (hash "share" (hash "public" x) l-0)))))
    ((recv (cat "share" (hash "public" x) l-0))
      (send (cat "share" (hash "public" x) l-0)))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) l) hint1)
         (hash "nh_key" (hash "share" (hash "public" l-0) x) hint2)))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) l) hint1)
          (hash "nh_key" (hash "share" (hash "public" l-0) x) hint2))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" y) l) hint1)
         (hash "nh_key" (hash "share" (hash "public" l-0) x) hint2)))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" y) l) hint1)
          (hash "nh_key" (hash "share" (hash "public" l-0) x) hint2)))))
  (label 76)
  (parent 74)
  (unrealized (10 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (y ltxa ltxb x rndx))
  (defstrand init 4
    (hint1 (hash "hint" (hash "share" (hash "public" ltxa) y)))
    (hint2 (hash "hint" (hash "share" (hash "public" x) ltxb))) (na na)
    (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener (cat "share" (hash "public" y) ltxa))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) ltxb)
      (hash "hint" (hash "share" (hash "public" x) ltxb))))
  (deflistener (cat "share" (hash "public" x) ltxb))
  (defstrand resp 3 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (precedes ((0 1) (7 1)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (7 0)) ((2 2) (4 0)) ((3 0) (0 0)) ((3 0) (7 0))
    ((3 2) (6 0)) ((4 1) (1 0)) ((5 1) (1 0)) ((6 1) (5 0))
    ((7 2) (0 2)))
  (absent (y ltxa) (y ltxb) (y x) (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq ((exp (gen) x) (gen)) (a b) ((exp (gen) y) (gen)))
  (uniq-gen y ltxa ltxb x)
  (uniq-orig na nb)
  (operation generalization deleted (4 0))
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
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv (cat "share" (hash "public" y) ltxa))
      (send (cat "share" (hash "public" y) ltxa)))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) ltxb)
         (hash "hint" (hash "share" (hash "public" x) ltxb))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) ltxb)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint"
                  (hash "share" (hash "public" x) ltxb)))))))))
  (label 77)
  (parent 75)
  (unrealized)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (x rndx) (y expt)
    (l l-0 rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa l) (ltxb l-0) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) l-0)
        (hash "hint" (hash "share" (hash "public" x) l-0)))))
  (defstrand ltx-gen 3 (self a) (l l))
  (defstrand ltx-gen 3 (self b) (l l-0))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" y) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) l-0)
        (hash "hint" (hash "share" (hash "public" x) l-0)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" y) l)
      (hash "hint" (hash "share" (hash "public" l) y))))
  (deflistener (cat "share" (hash "public" y) l))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) l-0)
      (hash "hint" (hash "share" (hash "public" x) l-0))))
  (deflistener (cat "share" (hash "public" x) l-0))
  (deflistener
    (hash "key" (hash "nh_key" (hash "share" (hash "public" y) l) hint1)
      (hash "nh_key" (hash "share" (hash "public" l-0) x) hint2)))
  (deflistener
    (cat "key" (hash "nh_key" (hash "share" (hash "public" y) l) hint1)
      (hash "nh_key" (hash "share" (hash "public" l-0) x) hint2)))
  (deflistener (cat "nh_key" (hash "share" (hash "public" y) l) hint1))
  (precedes ((0 1) (10 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (11 0)) ((2 2) (6 0)) ((3 0) (0 0)) ((3 2) (8 0))
    ((4 1) (1 0)) ((5 1) (4 0)) ((6 1) (5 0)) ((7 1) (4 0))
    ((8 1) (7 0)) ((9 1) (0 2)) ((10 1) (9 0)) ((11 1) (10 0)))
  (absent (x l) (x l-0))
  (fn-of ("ltx-of" (b l-0) (a l)) ("principal-of" (l-0 b) (l a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen x l l-0)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (cat "nh_key" (hash "share" (hash "public" y) l) hint1))
    (hash "nh_key" (hash "share" (hash "public" y) l) hint1) (10 0))
  (traces
    ((recv (cat (hash "public" l) (hash "public" l-0)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) l) hint1)
              (hash "nh_key" (hash "share" (hash "public" l-0) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) l-0)
           (hash "hint" (hash "share" (hash "public" x) l-0)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) l-0)
            (hash "hint" (hash "share" (hash "public" x) l-0))))))
    ((send (cat a (hash "public" l))) (recv "end-of-protocol") (send l))
    ((send (cat b (hash "public" l-0))) (recv "end-of-protocol")
      (send l-0))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" y) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) l-0)
           (hash "hint" (hash "share" (hash "public" x) l-0)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" y) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) l-0)
            (hash "hint" (hash "share" (hash "public" x) l-0))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" y) l)
         (hash "hint" (hash "share" (hash "public" l) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" y) l)
          (hash "hint" (hash "share" (hash "public" l) y)))))
    ((recv (cat "share" (hash "public" y) l))
      (send (cat "share" (hash "public" y) l)))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) l-0)
         (hash "hint" (hash "share" (hash "public" x) l-0))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) l-0)
          (hash "hint" (hash "share" (hash "public" x) l-0)))))
    ((recv (cat "share" (hash "public" x) l-0))
      (send (cat "share" (hash "public" x) l-0)))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) l) hint1)
         (hash "nh_key" (hash "share" (hash "public" l-0) x) hint2)))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) l) hint1)
          (hash "nh_key" (hash "share" (hash "public" l-0) x) hint2))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" y) l) hint1)
         (hash "nh_key" (hash "share" (hash "public" l-0) x) hint2)))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" y) l) hint1)
          (hash "nh_key" (hash "share" (hash "public" l-0) x) hint2))))
    ((recv (cat "nh_key" (hash "share" (hash "public" y) l) hint1))
      (send (cat "nh_key" (hash "share" (hash "public" y) l) hint1))))
  (label 78)
  (parent 76)
  (unrealized (10 0) (11 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (y ltxa ltxb x rndx))
  (defstrand init 4
    (hint1 (hash "hint" (hash "share" (hash "public" ltxa) y)))
    (hint2 (hash "hint" (hash "share" (hash "public" x) ltxb))) (na na)
    (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) ltxb)
      (hash "hint" (hash "share" (hash "public" x) ltxb))))
  (deflistener (cat "share" (hash "public" x) ltxb))
  (defstrand resp 3 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (precedes ((0 1) (6 1)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (6 0)) ((2 2) (1 0)) ((3 0) (0 0)) ((3 0) (6 0))
    ((3 2) (5 0)) ((4 1) (1 0)) ((5 1) (4 0)) ((6 2) (0 2)))
  (absent (y ltxa) (y ltxb) (y x) (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq ((exp (gen) x) (gen)) (a b) ((exp (gen) y) (gen)))
  (uniq-gen y ltxa ltxb x)
  (uniq-orig na nb)
  (operation generalization deleted (4 0))
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
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) ltxb)
         (hash "hint" (hash "share" (hash "public" x) ltxb))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) ltxb)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint"
                  (hash "share" (hash "public" x) ltxb)))))))))
  (label 79)
  (parent 77)
  (unrealized)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (x rndx) (y expt)
    (l l-0 rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa l) (ltxb l-0) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) l-0)
        (hash "hint" (hash "share" (hash "public" x) l-0)))))
  (defstrand ltx-gen 3 (self a) (l l))
  (defstrand ltx-gen 3 (self b) (l l-0))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" y) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" x) l-0)
        (hash "hint" (hash "share" (hash "public" x) l-0)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" y) l)
      (hash "hint" (hash "share" (hash "public" l) y))))
  (deflistener (cat "share" (hash "public" y) l))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" x) l-0)
      (hash "hint" (hash "share" (hash "public" x) l-0))))
  (deflistener (cat "share" (hash "public" x) l-0))
  (deflistener
    (hash "key" (hash "nh_key" (hash "share" (hash "public" y) l) hint1)
      (hash "nh_key" (hash "share" (hash "public" l-0) x) hint2)))
  (deflistener
    (cat "key" (hash "nh_key" (hash "share" (hash "public" y) l) hint1)
      (hash "nh_key" (hash "share" (hash "public" l-0) x) hint2)))
  (deflistener (cat "nh_key" (hash "share" (hash "public" y) l) hint1))
  (deflistener (cat "share" (hash "public" y) l))
  (precedes ((0 1) (10 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (12 0)) ((2 2) (6 0)) ((3 0) (0 0)) ((3 2) (8 0))
    ((4 1) (1 0)) ((5 1) (4 0)) ((6 1) (5 0)) ((7 1) (4 0))
    ((8 1) (7 0)) ((9 1) (0 2)) ((10 1) (9 0)) ((11 1) (10 0))
    ((12 1) (11 0)))
  (absent (x l) (x l-0))
  (fn-of ("ltx-of" (b l-0) (a l)) ("principal-of" (l-0 b) (l a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen x l l-0)
  (uniq-orig na)
  (operation encryption-test
    (added-listener (cat "share" (hash "public" y) l))
    (hash "share" (hash "public" y) l) (11 0))
  (traces
    ((recv (cat (hash "public" l) (hash "public" l-0)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) l) hint1)
              (hash "nh_key" (hash "share" (hash "public" l-0) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) l-0)
           (hash "hint" (hash "share" (hash "public" x) l-0)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) l-0)
            (hash "hint" (hash "share" (hash "public" x) l-0))))))
    ((send (cat a (hash "public" l))) (recv "end-of-protocol") (send l))
    ((send (cat b (hash "public" l-0))) (recv "end-of-protocol")
      (send l-0))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" y) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" x) l-0)
           (hash "hint" (hash "share" (hash "public" x) l-0)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" y) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" x) l-0)
            (hash "hint" (hash "share" (hash "public" x) l-0))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" y) l)
         (hash "hint" (hash "share" (hash "public" l) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" y) l)
          (hash "hint" (hash "share" (hash "public" l) y)))))
    ((recv (cat "share" (hash "public" y) l))
      (send (cat "share" (hash "public" y) l)))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" x) l-0)
         (hash "hint" (hash "share" (hash "public" x) l-0))))
      (send
        (cat "nh_key" (hash "share" (hash "public" x) l-0)
          (hash "hint" (hash "share" (hash "public" x) l-0)))))
    ((recv (cat "share" (hash "public" x) l-0))
      (send (cat "share" (hash "public" x) l-0)))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) l) hint1)
         (hash "nh_key" (hash "share" (hash "public" l-0) x) hint2)))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) l) hint1)
          (hash "nh_key" (hash "share" (hash "public" l-0) x) hint2))))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" y) l) hint1)
         (hash "nh_key" (hash "share" (hash "public" l-0) x) hint2)))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" y) l) hint1)
          (hash "nh_key" (hash "share" (hash "public" l-0) x) hint2))))
    ((recv (cat "nh_key" (hash "share" (hash "public" y) l) hint1))
      (send (cat "nh_key" (hash "share" (hash "public" y) l) hint1)))
    ((recv (cat "share" (hash "public" y) l))
      (send (cat "share" (hash "public" y) l))))
  (label 80)
  (parent 78)
  (unrealized (10 0) (12 0))
  (dead)
  (comment "empty cohort"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (y ltxa ltxb x rndx))
  (defstrand init 4
    (hint1 (hash "hint" (hash "share" (hash "public" ltxa) y)))
    (hint2 (hash "hint" (hash "share" (hash "public" x) ltxb))) (na na)
    (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener (cat "share" (hash "public" x) ltxb))
  (defstrand resp 3 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (precedes ((0 1) (5 1)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (5 0)) ((2 2) (1 0)) ((3 0) (0 0)) ((3 0) (5 0))
    ((3 2) (4 0)) ((4 1) (1 0)) ((5 2) (0 2)))
  (absent (y ltxa) (y ltxb) (y x) (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq ((exp (gen) x) (gen)) (a b) ((exp (gen) y) (gen)))
  (uniq-gen y ltxa ltxb x)
  (uniq-orig na nb)
  (operation generalization deleted (4 0))
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
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint"
                  (hash "share" (hash "public" x) ltxb)))))))))
  (label 81)
  (parent 79)
  (unrealized)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (na nb data) (a b name) (y ltxa ltxb x rndx))
  (defstrand init 4
    (hint1 (hash "hint" (hash "share" (hash "public" ltxa) y)))
    (hint2 (hash "hint" (hash "share" (hash "public" x) ltxb))) (na na)
    (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" x) ltxb)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (defstrand resp 3 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (precedes ((0 1) (4 1)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (4 0)) ((2 2) (1 0)) ((3 0) (0 0)) ((3 0) (4 0))
    ((3 2) (1 0)) ((4 2) (0 2)))
  (absent (y ltxa) (y ltxb) (y x) (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq ((exp (gen) x) (gen)) (a b) ((exp (gen) y) (gen)))
  (uniq-gen y ltxa ltxb x)
  (uniq-orig na nb)
  (operation generalization deleted (4 0))
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
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
      (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" x) ltxb)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" x) ltxb)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint"
                  (hash "share" (hash "public" x) ltxb)))))))))
  (label 82)
  (parent 81)
  (unrealized)
  (shape)
  (maps
    ((0 1 2 3)
      ((ltxa ltxa) (ltxb ltxb) (x x) (y y) (a a) (b b) (na na) (nb nb)
        (hint1 (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hint2 (hash "hint" (hash "share" (hash "public" x) ltxb))))))
  (origs (nb (4 2)) (na (0 1))))

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
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" ltxa) y)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" x) ltxb)
                (hash "hint" (hash "share" (hash "public" x) ltxb)))))
          (enc na nb a b
            (hash "hint" (hash "share" (hash "public" ltxa) y))
            (hash "hint" (hash "share" (hash "public" x) ltxb))
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                (hash "hint" (hash "share" (hash "public" ltxa) y)))
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat self (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat self-0 (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb)))
  (label 83)
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
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb)))
  (label 84)
  (parent 83)
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
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
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
        (hash "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))
        (hash "nh_key" (hash "share" (hash "public" ltxb) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))) (1 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb)))))))
  (label 85)
  (parent 84)
  (unrealized (0 2) (4 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa ltxb x rndx)
    (y expt))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" y) ltxa)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (precedes ((0 1) (4 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (5 0)) ((3 0) (0 0)) ((4 1) (1 0)) ((5 1) (4 0)))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (cat "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y))))
    (hash "nh_key" (hash "share" (hash "public" y) ltxa)
      (hash "hint" (hash "share" (hash "public" ltxa) y))) (4 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" y) ltxa)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y))))))
  (label 86)
  (parent 85)
  (unrealized (0 2) (4 0) (5 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxa ltxb x rndx)
    (y expt))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa ltxa) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" y) ltxa)
        (hash "hint" (hash "share" (hash "public" ltxa) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" y) ltxa)
      (hash "hint" (hash "share" (hash "public" ltxa) y))))
  (deflistener (cat "share" (hash "public" y) ltxa))
  (precedes ((0 1) (4 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (6 0)) ((3 0) (0 0)) ((4 1) (1 0)) ((5 1) (4 0))
    ((6 1) (5 0)))
  (absent (x ltxa) (x ltxb))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (operation encryption-test
    (added-listener (cat "share" (hash "public" y) ltxa))
    (hash "share" (hash "public" y) ltxa) (5 0))
  (traces
    ((recv (cat (hash "public" ltxa) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) ltxa)
                hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" y) ltxa)
           (hash "hint" (hash "share" (hash "public" ltxa) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" y) ltxa)
            (hash "hint" (hash "share" (hash "public" ltxa) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" y) ltxa)
         (hash "hint" (hash "share" (hash "public" ltxa) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" y) ltxa)
          (hash "hint" (hash "share" (hash "public" ltxa) y)))))
    ((recv (cat "share" (hash "public" y) ltxa))
      (send (cat "share" (hash "public" y) ltxa))))
  (label 87)
  (parent 86)
  (unrealized (0 2) (4 0) (6 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxb x rndx)
    (y expt) (l rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa l) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l l))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" y) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" y) l)
      (hash "hint" (hash "share" (hash "public" l) y))))
  (deflistener (cat "share" (hash "public" y) l))
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
              (hash "nh_key" (hash "share" (hash "public" y) l) hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" l))) (recv "end-of-protocol") (send l))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" y) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" y) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" y) l)
         (hash "hint" (hash "share" (hash "public" l) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" y) l)
          (hash "hint" (hash "share" (hash "public" l) y)))))
    ((recv (cat "share" (hash "public" y) l))
      (send (cat "share" (hash "public" y) l))))
  (label 88)
  (parent 87)
  (unrealized (0 2) (4 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxb x rndx)
    (y expt) (l rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa l) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l l))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" y) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" y) l)
      (hash "hint" (hash "share" (hash "public" l) y))))
  (deflistener (cat "share" (hash "public" y) l))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxb) x)
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
      (cat "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb))))
    (hash "nh_key" (hash "share" (hash "public" ltxb) x)
      (hash "hint" (hash "share" (hash "public" x) ltxb))) (4 0))
  (traces
    ((recv (cat (hash "public" l) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) l) hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" l))) (recv "end-of-protocol") (send l))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" y) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" y) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" y) l)
         (hash "hint" (hash "share" (hash "public" l) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" y) l)
          (hash "hint" (hash "share" (hash "public" l) y)))))
    ((recv (cat "share" (hash "public" y) l))
      (send (cat "share" (hash "public" y) l)))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxb) x)
         (hash "hint" (hash "share" (hash "public" x) ltxb))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxb) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb))))))
  (label 89)
  (parent 88)
  (unrealized (0 2) (7 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton nhcr-umx
  (vars (hint1 hint2 mesg) (na nb data) (a b name) (ltxb x rndx)
    (y expt) (l rndx))
  (defstrand init 4 (hint1 hint1) (hint2 hint2) (na na) (nb nb) (a a)
    (b b) (ltxa l) (ltxb ltxb) (x x) (y y))
  (deflistener
    (hash "key"
      (hash "nh_key" (hash "share" (hash "public" y) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (defstrand ltx-gen 3 (self a) (l l))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat "key"
      (hash "nh_key" (hash "share" (hash "public" y) l)
        (hash "hint" (hash "share" (hash "public" l) y)))
      (hash "nh_key" (hash "share" (hash "public" ltxb) x)
        (hash "hint" (hash "share" (hash "public" x) ltxb)))))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" y) l)
      (hash "hint" (hash "share" (hash "public" l) y))))
  (deflistener (cat "share" (hash "public" y) l))
  (deflistener
    (cat "nh_key" (hash "share" (hash "public" ltxb) x)
      (hash "hint" (hash "share" (hash "public" x) ltxb))))
  (deflistener (cat "share" (hash "public" ltxb) x))
  (precedes ((0 1) (8 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 2) (6 0)) ((3 0) (0 0)) ((4 1) (1 0)) ((5 1) (4 0))
    ((6 1) (5 0)) ((7 1) (4 0)) ((8 1) (7 0)))
  (absent (x l) (x ltxb))
  (fn-of ("ltx-of" (a l) (b ltxb)) ("principal-of" (l a) (ltxb b)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxb x l)
  (uniq-orig na)
  (operation encryption-test
    (added-listener (cat "share" (hash "public" ltxb) x))
    (hash "share" (hash "public" ltxb) x) (7 0))
  (traces
    ((recv (cat (hash "public" l) (hash "public" ltxb)))
      (send (cat na a b (hash "public" x)))
      (recv
        (cat (hash "public" y) hint1 hint2
          (enc na nb a b hint1 hint2
            (hash "key"
              (hash "nh_key" (hash "share" (hash "public" y) l) hint1)
              (hash "nh_key" (hash "share" (hash "public" ltxb) x)
                hint2))))) (send nb))
    ((recv
       (hash "key"
         (hash "nh_key" (hash "share" (hash "public" y) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (hash "key"
          (hash "nh_key" (hash "share" (hash "public" y) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((send (cat a (hash "public" l))) (recv "end-of-protocol") (send l))
    ((send (cat b (hash "public" ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat "key"
         (hash "nh_key" (hash "share" (hash "public" y) l)
           (hash "hint" (hash "share" (hash "public" l) y)))
         (hash "nh_key" (hash "share" (hash "public" ltxb) x)
           (hash "hint" (hash "share" (hash "public" x) ltxb)))))
      (send
        (cat "key"
          (hash "nh_key" (hash "share" (hash "public" y) l)
            (hash "hint" (hash "share" (hash "public" l) y)))
          (hash "nh_key" (hash "share" (hash "public" ltxb) x)
            (hash "hint" (hash "share" (hash "public" x) ltxb))))))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" y) l)
         (hash "hint" (hash "share" (hash "public" l) y))))
      (send
        (cat "nh_key" (hash "share" (hash "public" y) l)
          (hash "hint" (hash "share" (hash "public" l) y)))))
    ((recv (cat "share" (hash "public" y) l))
      (send (cat "share" (hash "public" y) l)))
    ((recv
       (cat "nh_key" (hash "share" (hash "public" ltxb) x)
         (hash "hint" (hash "share" (hash "public" x) ltxb))))
      (send
        (cat "nh_key" (hash "share" (hash "public" ltxb) x)
          (hash "hint" (hash "share" (hash "public" x) ltxb)))))
    ((recv (cat "share" (hash "public" ltxb) x))
      (send (cat "share" (hash "public" ltxb) x))))
  (label 90)
  (parent 89)
  (unrealized (0 2) (7 0) (8 0))
  (dead)
  (comment "empty cohort"))

(comment "Nothing left to do")
