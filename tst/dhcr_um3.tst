(herald "DHCR: unified model (UM) three-part" (bound 20) (limit 8000)
  (algebra diffie-hellman))

(comment "CPSA 3.6.10")
(comment "All input read from tst/dhcr_um3.scm")
(comment "Step count limited to 8000")
(comment "Strand count bounded at 20")

(defprotocol dhcr-um3 diffie-hellman
  (defrole init
    (vars (ltxa ltxb x rndx) (y expt) (a b name) (na nb data))
    (trace (recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul x y)))))) (send nb))
    (uniq-orig na)
    (uniq-gen x)
    (fn-of ("ltx-of" (a ltxa) (b ltxb))
      ("principal-of" (ltxa a) (ltxb b)))
    (neq ((exp (gen) y) (gen)))
    (absent (x (exp (gen) ltxa)) (x (exp (gen) ltxb))))
  (defrole resp
    (vars (ltxa ltxb y rndx) (x expt) (a b name) (na nb data))
    (trace (recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (recv (cat na a b (exp (gen) x)))
      (send
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul y x)))))) (recv nb))
    (uniq-orig nb)
    (uniq-gen y)
    (fn-of ("ltx-of" (a ltxa) (b ltxb))
      ("principal-of" (ltxa a) (ltxb b)))
    (neq ((exp (gen) x) (gen)))
    (absent (y (exp (gen) ltxa)) (y (exp (gen) ltxb))
      (y (exp (gen) x))))
  (defrole ltx-gen
    (vars (self name) (l rndx))
    (trace (send (cat self (exp (gen) l))) (recv "end-of-protocol")
      (send l))
    (uniq-gen l)
    (fn-of ("ltx-of" (self l)) ("principal-of" (l self)))))

(defskeleton dhcr-um3
  (vars (na nb data) (a b self self-0 name) (ltxa ltxb x rndx) (y expt))
  (defstrand init 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (x x) (y y))
  (defstrand ltx-gen 1 (self self) (l ltxa))
  (defstrand ltx-gen 1 (self self-0) (l ltxb))
  (absent (x (exp (gen) ltxa)) (x (exp (gen) ltxb)))
  (fn-of ("ltx-of" (self-0 ltxb) (self ltxa) (a ltxa) (b ltxb))
    ("principal-of" (ltxb self-0) (ltxa self) (ltxa a) (ltxb b)))
  (neq (a b) ((exp (gen) y) (gen)))
  (non-orig ltxa ltxb)
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul x y)))))) (send nb))
    ((send (cat self (exp (gen) ltxa))))
    ((send (cat self-0 (exp (gen) ltxb)))))
  (label 0)
  (unrealized (0 0) (0 2))
  (preskeleton)
  (origs (na (0 1)))
  (comment "Not a skeleton"))

(defskeleton dhcr-um3
  (vars (na nb data) (a b name) (ltxa ltxb x rndx) (y expt))
  (defstrand init 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (x x) (y y))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (precedes ((1 0) (0 0)) ((2 0) (0 0)))
  (absent (x (exp (gen) ltxa)) (x (exp (gen) ltxb)))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (non-orig ltxa ltxb)
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul x y)))))) (send nb))
    ((send (cat a (exp (gen) ltxa)))) ((send (cat b (exp (gen) ltxb)))))
  (label 1)
  (parent 0)
  (unrealized (0 2))
  (origs (na (0 1)))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton dhcr-um3
  (vars (na nb data) (a b name) (y x ltxa ltxb rndx))
  (defstrand init 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (x x) (y y))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (defstrand resp 3 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (precedes ((0 1) (3 1)) ((1 0) (0 0)) ((1 0) (3 0)) ((2 0) (0 0))
    ((2 0) (3 0)) ((3 2) (0 2)))
  (absent (y (exp (gen) ltxa)) (y (exp (gen) ltxb)) (y (exp (gen) x))
    (x (exp (gen) ltxa)) (x (exp (gen) ltxb)))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq ((exp (gen) x) (gen)) (a b) ((exp (gen) y) (gen)))
  (non-orig ltxa ltxb)
  (uniq-gen y x ltxa ltxb)
  (uniq-orig na nb)
  (operation encryption-test (added-strand resp 3)
    (enc na nb a b
      (hash (exp (gen) (mul y ltxa)) (exp (gen) (mul x ltxb))
        (exp (gen) (mul y x)))) (0 2))
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul y ltxa)) (exp (gen) (mul x ltxb))
              (exp (gen) (mul y x)))))) (send nb))
    ((send (cat a (exp (gen) ltxa)))) ((send (cat b (exp (gen) ltxb))))
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (recv (cat na a b (exp (gen) x)))
      (send
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul y ltxa)) (exp (gen) (mul x ltxb))
              (exp (gen) (mul y x))))))))
  (label 2)
  (parent 1)
  (unrealized)
  (shape)
  (maps
    ((0 1 2)
      ((ltxa ltxa) (ltxb ltxb) (a a) (b b) (x x) (y y) (na na)
        (nb nb))))
  (origs (nb (3 2)) (na (0 1))))

(defskeleton dhcr-um3
  (vars (na nb data) (a b name) (ltxa ltxb x rndx) (y expt))
  (defstrand init 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (x x) (y y))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (deflistener
    (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
      (exp (gen) (mul x y))))
  (precedes ((0 1) (3 0)) ((1 0) (0 0)) ((2 0) (0 0)) ((3 1) (0 2)))
  (absent (x (exp (gen) ltxa)) (x (exp (gen) ltxb)))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (non-orig ltxa ltxb)
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
        (exp (gen) (mul x y))))
    (enc na nb a b
      (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
        (exp (gen) (mul x y)))) (0 2))
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul x y)))))) (send nb))
    ((send (cat a (exp (gen) ltxa)))) ((send (cat b (exp (gen) ltxb))))
    ((recv
       (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
         (exp (gen) (mul x y))))
      (send
        (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
          (exp (gen) (mul x y))))))
  (label 3)
  (parent 1)
  (unrealized (3 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhcr-um3
  (vars (na nb data) (a b name) (ltxa ltxb x rndx) (y expt))
  (defstrand init 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (x x) (y y))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (deflistener
    (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
      (exp (gen) (mul x y))))
  (deflistener
    (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
      (exp (gen) (mul x y))))
  (precedes ((0 1) (4 0)) ((1 0) (0 0)) ((2 0) (0 0)) ((3 1) (0 2))
    ((4 1) (3 0)))
  (absent (x (exp (gen) ltxa)) (x (exp (gen) ltxb)))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (non-orig ltxa ltxb)
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
        (exp (gen) (mul x y))))
    (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
      (exp (gen) (mul x y))) (3 0))
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul x y)))))) (send nb))
    ((send (cat a (exp (gen) ltxa)))) ((send (cat b (exp (gen) ltxb))))
    ((recv
       (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
         (exp (gen) (mul x y))))
      (send
        (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
          (exp (gen) (mul x y)))))
    ((recv
       (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
         (exp (gen) (mul x y))))
      (send
        (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
          (exp (gen) (mul x y))))))
  (label 4)
  (parent 3)
  (unrealized (4 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhcr-um3
  (vars (na nb data) (a b name) (ltxa ltxb x rndx) (y expt))
  (defstrand init 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (x x) (y y))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (deflistener
    (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
      (exp (gen) (mul x y))))
  (deflistener
    (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
      (exp (gen) (mul x y))))
  (deflistener (cat (exp (gen) ltxb) x))
  (precedes ((0 1) (5 0)) ((1 0) (0 0)) ((2 0) (0 0)) ((3 1) (0 2))
    ((4 1) (3 0)) ((5 1) (4 0)))
  (absent (x (exp (gen) ltxa)) (x (exp (gen) ltxb)))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (non-orig ltxa ltxb)
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (operation nonce-test (added-listener (cat (exp (gen) ltxb) x))
    (exp (gen) (mul ltxb x)) (4 0))
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul x y)))))) (send nb))
    ((send (cat a (exp (gen) ltxa)))) ((send (cat b (exp (gen) ltxb))))
    ((recv
       (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
         (exp (gen) (mul x y))))
      (send
        (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
          (exp (gen) (mul x y)))))
    ((recv
       (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
         (exp (gen) (mul x y))))
      (send
        (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
          (exp (gen) (mul x y)))))
    ((recv (cat (exp (gen) ltxb) x)) (send (cat (exp (gen) ltxb) x))))
  (label 5)
  (parent 4)
  (unrealized (5 0))
  (dead)
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol dhcr-um3 diffie-hellman
  (defrole init
    (vars (ltxa ltxb x rndx) (y expt) (a b name) (na nb data))
    (trace (recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul x y)))))) (send nb))
    (uniq-orig na)
    (uniq-gen x)
    (fn-of ("ltx-of" (a ltxa) (b ltxb))
      ("principal-of" (ltxa a) (ltxb b)))
    (neq ((exp (gen) y) (gen)))
    (absent (x (exp (gen) ltxa)) (x (exp (gen) ltxb))))
  (defrole resp
    (vars (ltxa ltxb y rndx) (x expt) (a b name) (na nb data))
    (trace (recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (recv (cat na a b (exp (gen) x)))
      (send
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul y x)))))) (recv nb))
    (uniq-orig nb)
    (uniq-gen y)
    (fn-of ("ltx-of" (a ltxa) (b ltxb))
      ("principal-of" (ltxa a) (ltxb b)))
    (neq ((exp (gen) x) (gen)))
    (absent (y (exp (gen) ltxa)) (y (exp (gen) ltxb))
      (y (exp (gen) x))))
  (defrole ltx-gen
    (vars (self name) (l rndx))
    (trace (send (cat self (exp (gen) l))) (recv "end-of-protocol")
      (send l))
    (uniq-gen l)
    (fn-of ("ltx-of" (self l)) ("principal-of" (l self)))))

(defskeleton dhcr-um3
  (vars (na nb data) (a b self name) (ltxa ltxb x rndx) (y expt))
  (defstrand init 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (x x) (y y))
  (defstrand ltx-gen 1 (self self) (l ltxb))
  (absent (x (exp (gen) ltxa)) (x (exp (gen) ltxb)))
  (fn-of ("ltx-of" (self ltxb) (a ltxa) (b ltxb))
    ("principal-of" (ltxb self) (ltxa a) (ltxb b)))
  (neq (a b) ((exp (gen) y) (gen)))
  (non-orig ltxb)
  (uniq-gen ltxb x)
  (uniq-orig na)
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul x y)))))) (send nb))
    ((send (cat self (exp (gen) ltxb)))))
  (label 6)
  (unrealized (0 0) (0 2))
  (preskeleton)
  (origs (na (0 1)))
  (comment "Not a skeleton"))

(defskeleton dhcr-um3
  (vars (na nb data) (a b name) (ltxa ltxb x rndx) (y expt))
  (defstrand init 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (x x) (y y))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (precedes ((1 0) (0 0)))
  (absent (x (exp (gen) ltxa)) (x (exp (gen) ltxb)))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (non-orig ltxb)
  (uniq-gen ltxb x)
  (uniq-orig na)
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul x y)))))) (send nb))
    ((send (cat b (exp (gen) ltxb)))))
  (label 7)
  (parent 6)
  (unrealized (0 2))
  (origs (na (0 1)))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton dhcr-um3
  (vars (na nb data) (a b name) (y x ltxa ltxb rndx))
  (defstrand init 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (x x) (y y))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (defstrand resp 3 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (precedes ((0 1) (2 1)) ((1 0) (0 0)) ((1 0) (2 0)) ((2 2) (0 2)))
  (absent (y (exp (gen) ltxa)) (y (exp (gen) ltxb)) (y (exp (gen) x))
    (x (exp (gen) ltxa)) (x (exp (gen) ltxb)))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq ((exp (gen) x) (gen)) (a b) ((exp (gen) y) (gen)))
  (non-orig ltxb)
  (uniq-gen y x ltxb)
  (uniq-orig na nb)
  (operation encryption-test (added-strand resp 3)
    (enc na nb a b
      (hash (exp (gen) (mul y ltxa)) (exp (gen) (mul x ltxb))
        (exp (gen) (mul y x)))) (0 2))
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul y ltxa)) (exp (gen) (mul x ltxb))
              (exp (gen) (mul y x)))))) (send nb))
    ((send (cat b (exp (gen) ltxb))))
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (recv (cat na a b (exp (gen) x)))
      (send
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul y ltxa)) (exp (gen) (mul x ltxb))
              (exp (gen) (mul y x))))))))
  (label 8)
  (parent 7)
  (unrealized)
  (shape)
  (maps
    ((0 1)
      ((ltxa ltxa) (ltxb ltxb) (a a) (b b) (x x) (y y) (na na)
        (nb nb))))
  (origs (nb (2 2)) (na (0 1))))

(defskeleton dhcr-um3
  (vars (na nb data) (a b name) (ltxa ltxb x rndx) (y expt))
  (defstrand init 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (x x) (y y))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (deflistener
    (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
      (exp (gen) (mul x y))))
  (precedes ((0 1) (2 0)) ((1 0) (0 0)) ((2 1) (0 2)))
  (absent (x (exp (gen) ltxa)) (x (exp (gen) ltxb)))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (non-orig ltxb)
  (uniq-gen ltxb x)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
        (exp (gen) (mul x y))))
    (enc na nb a b
      (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
        (exp (gen) (mul x y)))) (0 2))
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul x y)))))) (send nb))
    ((send (cat b (exp (gen) ltxb))))
    ((recv
       (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
         (exp (gen) (mul x y))))
      (send
        (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
          (exp (gen) (mul x y))))))
  (label 9)
  (parent 7)
  (unrealized (2 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhcr-um3
  (vars (na nb data) (a b name) (ltxa ltxb x rndx) (y expt))
  (defstrand init 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (x x) (y y))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (deflistener
    (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
      (exp (gen) (mul x y))))
  (deflistener
    (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
      (exp (gen) (mul x y))))
  (precedes ((0 1) (3 0)) ((1 0) (0 0)) ((2 1) (0 2)) ((3 1) (2 0)))
  (absent (x (exp (gen) ltxa)) (x (exp (gen) ltxb)))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (non-orig ltxb)
  (uniq-gen ltxb x)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
        (exp (gen) (mul x y))))
    (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
      (exp (gen) (mul x y))) (2 0))
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul x y)))))) (send nb))
    ((send (cat b (exp (gen) ltxb))))
    ((recv
       (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
         (exp (gen) (mul x y))))
      (send
        (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
          (exp (gen) (mul x y)))))
    ((recv
       (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
         (exp (gen) (mul x y))))
      (send
        (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
          (exp (gen) (mul x y))))))
  (label 10)
  (parent 9)
  (unrealized (3 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhcr-um3
  (vars (na nb data) (a b name) (ltxa ltxb x rndx) (y expt))
  (defstrand init 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (x x) (y y))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (deflistener
    (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
      (exp (gen) (mul x y))))
  (deflistener
    (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
      (exp (gen) (mul x y))))
  (deflistener (cat (exp (gen) ltxb) x))
  (precedes ((0 1) (4 0)) ((1 0) (0 0)) ((2 1) (0 2)) ((3 1) (2 0))
    ((4 1) (3 0)))
  (absent (x (exp (gen) ltxa)) (x (exp (gen) ltxb)))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (non-orig ltxb)
  (uniq-gen ltxb x)
  (uniq-orig na)
  (operation nonce-test (added-listener (cat (exp (gen) ltxb) x))
    (exp (gen) (mul ltxb x)) (3 0))
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul x y)))))) (send nb))
    ((send (cat b (exp (gen) ltxb))))
    ((recv
       (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
         (exp (gen) (mul x y))))
      (send
        (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
          (exp (gen) (mul x y)))))
    ((recv
       (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
         (exp (gen) (mul x y))))
      (send
        (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
          (exp (gen) (mul x y)))))
    ((recv (cat (exp (gen) ltxb) x)) (send (cat (exp (gen) ltxb) x))))
  (label 11)
  (parent 10)
  (unrealized (4 0))
  (dead)
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol dhcr-um3 diffie-hellman
  (defrole init
    (vars (ltxa ltxb x rndx) (y expt) (a b name) (na nb data))
    (trace (recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul x y)))))) (send nb))
    (uniq-orig na)
    (uniq-gen x)
    (fn-of ("ltx-of" (a ltxa) (b ltxb))
      ("principal-of" (ltxa a) (ltxb b)))
    (neq ((exp (gen) y) (gen)))
    (absent (x (exp (gen) ltxa)) (x (exp (gen) ltxb))))
  (defrole resp
    (vars (ltxa ltxb y rndx) (x expt) (a b name) (na nb data))
    (trace (recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (recv (cat na a b (exp (gen) x)))
      (send
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul y x)))))) (recv nb))
    (uniq-orig nb)
    (uniq-gen y)
    (fn-of ("ltx-of" (a ltxa) (b ltxb))
      ("principal-of" (ltxa a) (ltxb b)))
    (neq ((exp (gen) x) (gen)))
    (absent (y (exp (gen) ltxa)) (y (exp (gen) ltxb))
      (y (exp (gen) x))))
  (defrole ltx-gen
    (vars (self name) (l rndx))
    (trace (send (cat self (exp (gen) l))) (recv "end-of-protocol")
      (send l))
    (uniq-gen l)
    (fn-of ("ltx-of" (self l)) ("principal-of" (l self)))))

(defskeleton dhcr-um3
  (vars (na nb data) (a b self name) (ltxa ltxb x y rndx))
  (defstrand init 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (x x) (y y))
  (defstrand ltx-gen 1 (self self) (l ltxb))
  (deflistener (mul x y))
  (absent (x (exp (gen) ltxa)) (x (exp (gen) ltxb)))
  (fn-of ("ltx-of" (self ltxb) (a ltxa) (b ltxb))
    ("principal-of" (ltxb self) (ltxa a) (ltxb b)))
  (neq (a b) ((exp (gen) y) (gen)))
  (non-orig ltxb)
  (uniq-gen ltxb x)
  (uniq-orig na)
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul x y)))))) (send nb))
    ((send (cat self (exp (gen) ltxb))))
    ((recv (mul x y)) (send (mul x y))))
  (label 12)
  (unrealized (0 0) (0 2) (2 0))
  (preskeleton)
  (origs (na (0 1)))
  (comment "Not a skeleton"))

(defskeleton dhcr-um3
  (vars (na nb data) (a b name) (ltxa ltxb x y rndx))
  (defstrand init 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (x x) (y y))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (deflistener (mul x y))
  (precedes ((0 1) (2 0)) ((1 0) (0 0)))
  (absent (x (exp (gen) ltxa)) (x (exp (gen) ltxb)))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (non-orig ltxb)
  (uniq-gen ltxb x)
  (uniq-orig na)
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul x y)))))) (send nb))
    ((send (cat b (exp (gen) ltxb))))
    ((recv (mul x y)) (send (mul x y))))
  (label 13)
  (parent 12)
  (unrealized (0 2) (2 0))
  (origs (na (0 1)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhcr-um3
  (vars (na nb data) (a b name) (ltxa ltxb x y rndx))
  (defstrand init 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (x x) (y y))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (deflistener (mul x y))
  (deflistener x)
  (precedes ((0 1) (3 0)) ((1 0) (0 0)) ((3 1) (2 0)))
  (absent (x (exp (gen) ltxa)) (x (exp (gen) ltxb)))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (non-orig ltxb)
  (uniq-gen ltxb x)
  (uniq-orig na)
  (operation nonce-test (added-listener x) (mul x y) (2 0))
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul x y)))))) (send nb))
    ((send (cat b (exp (gen) ltxb))))
    ((recv (mul x y)) (send (mul x y))) ((recv x) (send x)))
  (label 14)
  (parent 13)
  (unrealized (0 2) (3 0))
  (dead)
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol dhcr-um3 diffie-hellman
  (defrole init
    (vars (ltxa ltxb x rndx) (y expt) (a b name) (na nb data))
    (trace (recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul x y)))))) (send nb))
    (uniq-orig na)
    (uniq-gen x)
    (fn-of ("ltx-of" (a ltxa) (b ltxb))
      ("principal-of" (ltxa a) (ltxb b)))
    (neq ((exp (gen) y) (gen)))
    (absent (x (exp (gen) ltxa)) (x (exp (gen) ltxb))))
  (defrole resp
    (vars (ltxa ltxb y rndx) (x expt) (a b name) (na nb data))
    (trace (recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (recv (cat na a b (exp (gen) x)))
      (send
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul y x)))))) (recv nb))
    (uniq-orig nb)
    (uniq-gen y)
    (fn-of ("ltx-of" (a ltxa) (b ltxb))
      ("principal-of" (ltxa a) (ltxb b)))
    (neq ((exp (gen) x) (gen)))
    (absent (y (exp (gen) ltxa)) (y (exp (gen) ltxb))
      (y (exp (gen) x))))
  (defrole ltx-gen
    (vars (self name) (l rndx))
    (trace (send (cat self (exp (gen) l))) (recv "end-of-protocol")
      (send l))
    (uniq-gen l)
    (fn-of ("ltx-of" (self l)) ("principal-of" (l self)))))

(defskeleton dhcr-um3
  (vars (na nb data) (a b self self-0 name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self self) (l ltxa))
  (defstrand ltx-gen 1 (self self-0) (l ltxb))
  (absent (y (exp (gen) ltxa)) (y (exp (gen) ltxb)) (y (exp (gen) x)))
  (fn-of ("ltx-of" (self-0 ltxb) (self ltxa) (a ltxa) (b ltxb))
    ("principal-of" (ltxb self-0) (ltxa self) (ltxa a) (ltxb b)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa ltxb)
  (uniq-gen ltxa ltxb y)
  (uniq-orig nb)
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (recv (cat na a b (exp (gen) x)))
      (send
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul y x)))))) (recv nb))
    ((send (cat self (exp (gen) ltxa))))
    ((send (cat self-0 (exp (gen) ltxb)))))
  (label 15)
  (unrealized (0 0) (0 3))
  (preskeleton)
  (origs (nb (0 2)))
  (comment "Not a skeleton"))

(defskeleton dhcr-um3
  (vars (na nb data) (a b name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (precedes ((1 0) (0 0)) ((2 0) (0 0)))
  (absent (y (exp (gen) ltxa)) (y (exp (gen) ltxb)) (y (exp (gen) x)))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa ltxb)
  (uniq-gen ltxa ltxb y)
  (uniq-orig nb)
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (recv (cat na a b (exp (gen) x)))
      (send
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul y x)))))) (recv nb))
    ((send (cat a (exp (gen) ltxa)))) ((send (cat b (exp (gen) ltxb)))))
  (label 16)
  (parent 15)
  (unrealized (0 3))
  (origs (nb (0 2)))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton dhcr-um3
  (vars (na nb data) (a b name) (y x ltxa ltxb rndx))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (defstrand init 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (x x) (y y))
  (precedes ((0 2) (3 2)) ((1 0) (0 0)) ((1 0) (3 0)) ((2 0) (0 0))
    ((2 0) (3 0)) ((3 1) (0 1)) ((3 3) (0 3)))
  (absent (x (exp (gen) ltxa)) (x (exp (gen) ltxb)) (y (exp (gen) ltxa))
    (y (exp (gen) ltxb)) (y (exp (gen) x)))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq ((exp (gen) y) (gen)) (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa ltxb)
  (uniq-gen y x ltxa ltxb)
  (uniq-orig na nb)
  (operation nonce-test (added-strand init 4) nb (0 3)
    (enc na nb a b
      (hash (exp (gen) (mul y ltxa)) (exp (gen) (mul x ltxb))
        (exp (gen) (mul y x)))))
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (recv (cat na a b (exp (gen) x)))
      (send
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul y ltxa)) (exp (gen) (mul x ltxb))
              (exp (gen) (mul y x)))))) (recv nb))
    ((send (cat a (exp (gen) ltxa)))) ((send (cat b (exp (gen) ltxb))))
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul y ltxa)) (exp (gen) (mul x ltxb))
              (exp (gen) (mul y x)))))) (send nb)))
  (label 17)
  (parent 16)
  (unrealized)
  (shape)
  (maps
    ((0 1 2)
      ((ltxa ltxa) (ltxb ltxb) (a a) (b b) (y y) (x x) (na na)
        (nb nb))))
  (origs (na (3 1)) (nb (0 2))))

(defskeleton dhcr-um3
  (vars (na nb data) (a b name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (deflistener
    (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
      (exp (gen) (mul y x))))
  (precedes ((0 2) (3 0)) ((1 0) (0 0)) ((2 0) (0 0)) ((3 1) (0 3)))
  (absent (y (exp (gen) ltxa)) (y (exp (gen) ltxb)) (y (exp (gen) x)))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa ltxb)
  (uniq-gen ltxa ltxb y)
  (uniq-orig nb)
  (operation nonce-test
    (added-listener
      (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
        (exp (gen) (mul y x)))) nb (0 3)
    (enc na nb a b
      (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
        (exp (gen) (mul y x)))))
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (recv (cat na a b (exp (gen) x)))
      (send
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul y x)))))) (recv nb))
    ((send (cat a (exp (gen) ltxa)))) ((send (cat b (exp (gen) ltxb))))
    ((recv
       (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
         (exp (gen) (mul y x))))
      (send
        (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
          (exp (gen) (mul y x))))))
  (label 18)
  (parent 16)
  (unrealized (3 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhcr-um3
  (vars (na nb data) (a b name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (deflistener
    (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
      (exp (gen) (mul y x))))
  (deflistener
    (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
      (exp (gen) (mul y x))))
  (precedes ((0 2) (4 0)) ((1 0) (0 0)) ((2 0) (0 0)) ((3 1) (0 3))
    ((4 1) (3 0)))
  (absent (y (exp (gen) ltxa)) (y (exp (gen) ltxb)) (y (exp (gen) x)))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa ltxb)
  (uniq-gen ltxa ltxb y)
  (uniq-orig nb)
  (operation encryption-test
    (added-listener
      (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
        (exp (gen) (mul y x))))
    (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
      (exp (gen) (mul y x))) (3 0))
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (recv (cat na a b (exp (gen) x)))
      (send
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul y x)))))) (recv nb))
    ((send (cat a (exp (gen) ltxa)))) ((send (cat b (exp (gen) ltxb))))
    ((recv
       (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
         (exp (gen) (mul y x))))
      (send
        (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
          (exp (gen) (mul y x)))))
    ((recv
       (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
         (exp (gen) (mul y x))))
      (send
        (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
          (exp (gen) (mul y x))))))
  (label 19)
  (parent 18)
  (unrealized (4 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhcr-um3
  (vars (na nb data) (a b name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (defstrand ltx-gen 1 (self b) (l ltxb))
  (deflistener
    (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
      (exp (gen) (mul y x))))
  (deflistener
    (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
      (exp (gen) (mul y x))))
  (deflistener (cat (exp (gen) ltxa) y))
  (precedes ((0 2) (5 0)) ((1 0) (0 0)) ((2 0) (0 0)) ((3 1) (0 3))
    ((4 1) (3 0)) ((5 1) (4 0)))
  (absent (y (exp (gen) ltxa)) (y (exp (gen) ltxb)) (y (exp (gen) x)))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa ltxb)
  (uniq-gen ltxa ltxb y)
  (uniq-orig nb)
  (operation nonce-test (added-listener (cat (exp (gen) ltxa) y))
    (exp (gen) (mul ltxa y)) (4 0))
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (recv (cat na a b (exp (gen) x)))
      (send
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul y x)))))) (recv nb))
    ((send (cat a (exp (gen) ltxa)))) ((send (cat b (exp (gen) ltxb))))
    ((recv
       (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
         (exp (gen) (mul y x))))
      (send
        (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
          (exp (gen) (mul y x)))))
    ((recv
       (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
         (exp (gen) (mul y x))))
      (send
        (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
          (exp (gen) (mul y x)))))
    ((recv (cat (exp (gen) ltxa) y)) (send (cat (exp (gen) ltxa) y))))
  (label 20)
  (parent 19)
  (unrealized (5 0))
  (dead)
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol dhcr-um3 diffie-hellman
  (defrole init
    (vars (ltxa ltxb x rndx) (y expt) (a b name) (na nb data))
    (trace (recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul x y)))))) (send nb))
    (uniq-orig na)
    (uniq-gen x)
    (fn-of ("ltx-of" (a ltxa) (b ltxb))
      ("principal-of" (ltxa a) (ltxb b)))
    (neq ((exp (gen) y) (gen)))
    (absent (x (exp (gen) ltxa)) (x (exp (gen) ltxb))))
  (defrole resp
    (vars (ltxa ltxb y rndx) (x expt) (a b name) (na nb data))
    (trace (recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (recv (cat na a b (exp (gen) x)))
      (send
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul y x)))))) (recv nb))
    (uniq-orig nb)
    (uniq-gen y)
    (fn-of ("ltx-of" (a ltxa) (b ltxb))
      ("principal-of" (ltxa a) (ltxb b)))
    (neq ((exp (gen) x) (gen)))
    (absent (y (exp (gen) ltxa)) (y (exp (gen) ltxb))
      (y (exp (gen) x))))
  (defrole ltx-gen
    (vars (self name) (l rndx))
    (trace (send (cat self (exp (gen) l))) (recv "end-of-protocol")
      (send l))
    (uniq-gen l)
    (fn-of ("ltx-of" (self l)) ("principal-of" (l self)))))

(defskeleton dhcr-um3
  (vars (na nb data) (a b self name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self self) (l ltxa))
  (absent (y (exp (gen) ltxa)) (y (exp (gen) ltxb)) (y (exp (gen) x)))
  (fn-of ("ltx-of" (self ltxa) (a ltxa) (b ltxb))
    ("principal-of" (ltxa self) (ltxa a) (ltxb b)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa)
  (uniq-gen ltxa y)
  (uniq-orig nb)
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (recv (cat na a b (exp (gen) x)))
      (send
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul y x)))))) (recv nb))
    ((send (cat self (exp (gen) ltxa)))))
  (label 21)
  (unrealized (0 0) (0 3))
  (preskeleton)
  (origs (nb (0 2)))
  (comment "Not a skeleton"))

(defskeleton dhcr-um3
  (vars (na nb data) (a b name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (precedes ((1 0) (0 0)))
  (absent (y (exp (gen) ltxa)) (y (exp (gen) ltxb)) (y (exp (gen) x)))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa)
  (uniq-gen ltxa y)
  (uniq-orig nb)
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (recv (cat na a b (exp (gen) x)))
      (send
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul y x)))))) (recv nb))
    ((send (cat a (exp (gen) ltxa)))))
  (label 22)
  (parent 21)
  (unrealized (0 3))
  (origs (nb (0 2)))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton dhcr-um3
  (vars (na nb data) (a b name) (y x ltxa ltxb rndx))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (defstrand init 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (x x) (y y))
  (precedes ((0 2) (2 2)) ((1 0) (0 0)) ((1 0) (2 0)) ((2 1) (0 1))
    ((2 3) (0 3)))
  (absent (x (exp (gen) ltxa)) (x (exp (gen) ltxb)) (y (exp (gen) ltxa))
    (y (exp (gen) ltxb)) (y (exp (gen) x)))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq ((exp (gen) y) (gen)) (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa)
  (uniq-gen y x ltxa)
  (uniq-orig na nb)
  (operation nonce-test (added-strand init 4) nb (0 3)
    (enc na nb a b
      (hash (exp (gen) (mul y ltxa)) (exp (gen) (mul x ltxb))
        (exp (gen) (mul y x)))))
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (recv (cat na a b (exp (gen) x)))
      (send
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul y ltxa)) (exp (gen) (mul x ltxb))
              (exp (gen) (mul y x)))))) (recv nb))
    ((send (cat a (exp (gen) ltxa))))
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul y ltxa)) (exp (gen) (mul x ltxb))
              (exp (gen) (mul y x)))))) (send nb)))
  (label 23)
  (parent 22)
  (unrealized)
  (shape)
  (maps
    ((0 1)
      ((ltxa ltxa) (ltxb ltxb) (a a) (b b) (y y) (x x) (na na)
        (nb nb))))
  (origs (na (2 1)) (nb (0 2))))

(defskeleton dhcr-um3
  (vars (na nb data) (a b name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (deflistener
    (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
      (exp (gen) (mul y x))))
  (precedes ((0 2) (2 0)) ((1 0) (0 0)) ((2 1) (0 3)))
  (absent (y (exp (gen) ltxa)) (y (exp (gen) ltxb)) (y (exp (gen) x)))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa)
  (uniq-gen ltxa y)
  (uniq-orig nb)
  (operation nonce-test
    (added-listener
      (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
        (exp (gen) (mul y x)))) nb (0 3)
    (enc na nb a b
      (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
        (exp (gen) (mul y x)))))
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (recv (cat na a b (exp (gen) x)))
      (send
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul y x)))))) (recv nb))
    ((send (cat a (exp (gen) ltxa))))
    ((recv
       (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
         (exp (gen) (mul y x))))
      (send
        (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
          (exp (gen) (mul y x))))))
  (label 24)
  (parent 22)
  (unrealized (2 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhcr-um3
  (vars (na nb data) (a b name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (deflistener
    (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
      (exp (gen) (mul y x))))
  (deflistener
    (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
      (exp (gen) (mul y x))))
  (precedes ((0 2) (3 0)) ((1 0) (0 0)) ((2 1) (0 3)) ((3 1) (2 0)))
  (absent (y (exp (gen) ltxa)) (y (exp (gen) ltxb)) (y (exp (gen) x)))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa)
  (uniq-gen ltxa y)
  (uniq-orig nb)
  (operation encryption-test
    (added-listener
      (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
        (exp (gen) (mul y x))))
    (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
      (exp (gen) (mul y x))) (2 0))
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (recv (cat na a b (exp (gen) x)))
      (send
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul y x)))))) (recv nb))
    ((send (cat a (exp (gen) ltxa))))
    ((recv
       (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
         (exp (gen) (mul y x))))
      (send
        (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
          (exp (gen) (mul y x)))))
    ((recv
       (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
         (exp (gen) (mul y x))))
      (send
        (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
          (exp (gen) (mul y x))))))
  (label 25)
  (parent 24)
  (unrealized (3 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhcr-um3
  (vars (na nb data) (a b name) (ltxa ltxb y rndx) (x expt))
  (defstrand resp 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (defstrand ltx-gen 1 (self a) (l ltxa))
  (deflistener
    (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
      (exp (gen) (mul y x))))
  (deflistener
    (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
      (exp (gen) (mul y x))))
  (deflistener (cat (exp (gen) ltxa) y))
  (precedes ((0 2) (4 0)) ((1 0) (0 0)) ((2 1) (0 3)) ((3 1) (2 0))
    ((4 1) (3 0)))
  (absent (y (exp (gen) ltxa)) (y (exp (gen) ltxb)) (y (exp (gen) x)))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq (a b) ((exp (gen) x) (gen)))
  (non-orig ltxa)
  (uniq-gen ltxa y)
  (uniq-orig nb)
  (operation nonce-test (added-listener (cat (exp (gen) ltxa) y))
    (exp (gen) (mul ltxa y)) (3 0))
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (recv (cat na a b (exp (gen) x)))
      (send
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul y x)))))) (recv nb))
    ((send (cat a (exp (gen) ltxa))))
    ((recv
       (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
         (exp (gen) (mul y x))))
      (send
        (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
          (exp (gen) (mul y x)))))
    ((recv
       (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
         (exp (gen) (mul y x))))
      (send
        (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
          (exp (gen) (mul y x)))))
    ((recv (cat (exp (gen) ltxa) y)) (send (cat (exp (gen) ltxa) y))))
  (label 26)
  (parent 25)
  (unrealized (4 0))
  (dead)
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol dhcr-um3 diffie-hellman
  (defrole init
    (vars (ltxa ltxb x rndx) (y expt) (a b name) (na nb data))
    (trace (recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul x y)))))) (send nb))
    (uniq-orig na)
    (uniq-gen x)
    (fn-of ("ltx-of" (a ltxa) (b ltxb))
      ("principal-of" (ltxa a) (ltxb b)))
    (neq ((exp (gen) y) (gen)))
    (absent (x (exp (gen) ltxa)) (x (exp (gen) ltxb))))
  (defrole resp
    (vars (ltxa ltxb y rndx) (x expt) (a b name) (na nb data))
    (trace (recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (recv (cat na a b (exp (gen) x)))
      (send
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul y x)))))) (recv nb))
    (uniq-orig nb)
    (uniq-gen y)
    (fn-of ("ltx-of" (a ltxa) (b ltxb))
      ("principal-of" (ltxa a) (ltxb b)))
    (neq ((exp (gen) x) (gen)))
    (absent (y (exp (gen) ltxa)) (y (exp (gen) ltxb))
      (y (exp (gen) x))))
  (defrole ltx-gen
    (vars (self name) (l rndx))
    (trace (send (cat self (exp (gen) l))) (recv "end-of-protocol")
      (send l))
    (uniq-gen l)
    (fn-of ("ltx-of" (self l)) ("principal-of" (l self)))))

(defskeleton dhcr-um3
  (vars (na nb data) (a b self self-0 name) (ltxa ltxb x rndx) (y expt))
  (defstrand init 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (x x) (y y))
  (deflistener
    (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
      (exp (gen) (mul x y))))
  (defstrand ltx-gen 3 (self self) (l ltxa))
  (defstrand ltx-gen 3 (self self-0) (l ltxb))
  (precedes ((0 3) (2 1)) ((0 3) (3 1)))
  (absent (x (exp (gen) ltxa)) (x (exp (gen) ltxb)))
  (fn-of ("ltx-of" (self-0 ltxb) (self ltxa) (a ltxa) (b ltxb))
    ("principal-of" (ltxb self-0) (ltxa self) (ltxa a) (ltxb b)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul x y)))))) (send nb))
    ((recv
       (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
         (exp (gen) (mul x y))))
      (send
        (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
          (exp (gen) (mul x y)))))
    ((send (cat self (exp (gen) ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat self-0 (exp (gen) ltxb))) (recv "end-of-protocol")
      (send ltxb)))
  (label 27)
  (unrealized (0 0) (0 2) (1 0))
  (preskeleton)
  (origs (na (0 1)))
  (comment "Not a skeleton"))

(defskeleton dhcr-um3
  (vars (na nb data) (a b name) (ltxa ltxb x rndx) (y expt))
  (defstrand init 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (x x) (y y))
  (deflistener
    (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
      (exp (gen) (mul x y))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (precedes ((0 1) (1 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((3 0) (0 0)))
  (absent (x (exp (gen) ltxa)) (x (exp (gen) ltxb)))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul x y)))))) (send nb))
    ((recv
       (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
         (exp (gen) (mul x y))))
      (send
        (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
          (exp (gen) (mul x y)))))
    ((send (cat a (exp (gen) ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (exp (gen) ltxb))) (recv "end-of-protocol")
      (send ltxb)))
  (label 28)
  (parent 27)
  (unrealized (0 2) (1 0))
  (origs (na (0 1)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhcr-um3
  (vars (na nb data) (a b name) (ltxa ltxb x rndx) (y expt))
  (defstrand init 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (x x) (y y))
  (deflistener
    (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
      (exp (gen) (mul x y))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
      (exp (gen) (mul x y))))
  (precedes ((0 1) (4 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((3 0) (0 0)) ((4 1) (1 0)))
  (absent (x (exp (gen) ltxa)) (x (exp (gen) ltxb)))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
        (exp (gen) (mul x y))))
    (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
      (exp (gen) (mul x y))) (1 0))
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul x y)))))) (send nb))
    ((recv
       (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
         (exp (gen) (mul x y))))
      (send
        (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
          (exp (gen) (mul x y)))))
    ((send (cat a (exp (gen) ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (exp (gen) ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
         (exp (gen) (mul x y))))
      (send
        (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
          (exp (gen) (mul x y))))))
  (label 29)
  (parent 28)
  (unrealized (0 2) (4 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton dhcr-um3
  (vars (na nb data) (a b name) (ltxa ltxb x rndx) (y expt))
  (defstrand init 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (x x) (y y))
  (deflistener
    (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
      (exp (gen) (mul x y))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
      (exp (gen) (mul x y))))
  (deflistener (cat (exp (gen) ltxb) x))
  (precedes ((0 1) (5 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((3 0) (0 0)) ((4 1) (1 0)) ((5 1) (4 0)))
  (absent (x (exp (gen) ltxa)) (x (exp (gen) ltxb)))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (operation nonce-test (added-listener (cat (exp (gen) ltxb) x))
    (exp (gen) (mul ltxb x)) (4 0))
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul x y)))))) (send nb))
    ((recv
       (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
         (exp (gen) (mul x y))))
      (send
        (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
          (exp (gen) (mul x y)))))
    ((send (cat a (exp (gen) ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (exp (gen) ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
         (exp (gen) (mul x y))))
      (send
        (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
          (exp (gen) (mul x y)))))
    ((recv (cat (exp (gen) ltxb) x)) (send (cat (exp (gen) ltxb) x))))
  (label 30)
  (parent 29)
  (unrealized (0 2) (5 0))
  (dead)
  (comment "empty cohort"))

(defskeleton dhcr-um3
  (vars (na nb data) (a b name) (ltxa ltxb x rndx) (y expt))
  (defstrand init 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (x x) (y y))
  (deflistener
    (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
      (exp (gen) (mul x y))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
      (exp (gen) (mul x y))))
  (deflistener (cat (exp (gen) x) ltxb))
  (precedes ((0 1) (5 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((3 0) (0 0)) ((4 1) (1 0)) ((5 1) (4 0)))
  (absent (x (exp (gen) ltxa)) (x (exp (gen) ltxb)))
  (fn-of ("ltx-of" (b ltxb) (a ltxa))
    ("principal-of" (ltxb b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa ltxb x)
  (uniq-orig na)
  (operation nonce-test (added-listener (cat (exp (gen) x) ltxb))
    (exp (gen) (mul ltxb x)) (4 0))
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
              (exp (gen) (mul x y)))))) (send nb))
    ((recv
       (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
         (exp (gen) (mul x y))))
      (send
        (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
          (exp (gen) (mul x y)))))
    ((send (cat a (exp (gen) ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (exp (gen) ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
         (exp (gen) (mul x y))))
      (send
        (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul ltxb x))
          (exp (gen) (mul x y)))))
    ((recv (cat (exp (gen) x) ltxb)) (send (cat (exp (gen) x) ltxb))))
  (label 31)
  (parent 29)
  (unrealized (0 2) (5 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhcr-um3
  (vars (na nb data) (a b name) (ltxa x rndx) (y expt) (l rndx))
  (defstrand init 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb l)
    (x x) (y y))
  (deflistener
    (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
      (exp (gen) (mul x y))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l l))
  (deflistener
    (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
      (exp (gen) (mul x y))))
  (deflistener (cat (exp (gen) x) l))
  (precedes ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0)) ((3 0) (0 0))
    ((3 2) (5 0)) ((4 1) (1 0)) ((5 1) (4 0)))
  (absent (x (exp (gen) ltxa)) (x (exp (gen) l)))
  (fn-of ("ltx-of" (b l) (a ltxa)) ("principal-of" (l b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa x l)
  (uniq-orig na)
  (operation nonce-test (displaced 6 3 ltx-gen 3) l (5 0))
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) l)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
              (exp (gen) (mul x y)))))) (send nb))
    ((recv
       (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
         (exp (gen) (mul x y))))
      (send
        (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
          (exp (gen) (mul x y)))))
    ((send (cat a (exp (gen) ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (exp (gen) l))) (recv "end-of-protocol") (send l))
    ((recv
       (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
         (exp (gen) (mul x y))))
      (send
        (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
          (exp (gen) (mul x y)))))
    ((recv (cat (exp (gen) x) l)) (send (cat (exp (gen) x) l))))
  (label 32)
  (parent 31)
  (unrealized (0 2))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton dhcr-um3
  (vars (na nb data) (a b name) (y x ltxa ltxb rndx))
  (defstrand init 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (x x) (y y))
  (deflistener
    (hash (exp (gen) (mul y ltxa)) (exp (gen) (mul x ltxb))
      (exp (gen) (mul y x))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat (exp (gen) (mul y ltxa)) (exp (gen) (mul x ltxb))
      (exp (gen) (mul y x))))
  (deflistener (cat (exp (gen) x) ltxb))
  (defstrand resp 3 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (precedes ((0 1) (6 1)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (6 0)) ((3 0) (0 0)) ((3 0) (6 0)) ((3 2) (5 0))
    ((4 1) (1 0)) ((5 1) (4 0)) ((6 2) (0 2)))
  (absent (y (exp (gen) ltxa)) (y (exp (gen) ltxb)) (y (exp (gen) x))
    (x (exp (gen) ltxa)) (x (exp (gen) ltxb)))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq ((exp (gen) x) (gen)) (a b) ((exp (gen) y) (gen)))
  (uniq-gen y x ltxa ltxb)
  (uniq-orig na nb)
  (operation encryption-test (added-strand resp 3)
    (enc na nb a b
      (hash (exp (gen) (mul y ltxa)) (exp (gen) (mul x ltxb))
        (exp (gen) (mul y x)))) (0 2))
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul y ltxa)) (exp (gen) (mul x ltxb))
              (exp (gen) (mul y x)))))) (send nb))
    ((recv
       (hash (exp (gen) (mul y ltxa)) (exp (gen) (mul x ltxb))
         (exp (gen) (mul y x))))
      (send
        (hash (exp (gen) (mul y ltxa)) (exp (gen) (mul x ltxb))
          (exp (gen) (mul y x)))))
    ((send (cat a (exp (gen) ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (exp (gen) ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat (exp (gen) (mul y ltxa)) (exp (gen) (mul x ltxb))
         (exp (gen) (mul y x))))
      (send
        (cat (exp (gen) (mul y ltxa)) (exp (gen) (mul x ltxb))
          (exp (gen) (mul y x)))))
    ((recv (cat (exp (gen) x) ltxb)) (send (cat (exp (gen) x) ltxb)))
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (recv (cat na a b (exp (gen) x)))
      (send
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul y ltxa)) (exp (gen) (mul x ltxb))
              (exp (gen) (mul y x))))))))
  (label 33)
  (parent 32)
  (unrealized (4 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton dhcr-um3
  (vars (na nb data) (a b name) (ltxa x rndx) (y expt) (l rndx))
  (defstrand init 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb l)
    (x x) (y y))
  (deflistener
    (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
      (exp (gen) (mul x y))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l l))
  (deflistener
    (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
      (exp (gen) (mul x y))))
  (deflistener (cat (exp (gen) x) l))
  (deflistener
    (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
      (exp (gen) (mul x y))))
  (precedes ((0 1) (6 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((3 0) (0 0)) ((3 2) (5 0)) ((4 1) (1 0)) ((5 1) (4 0))
    ((6 1) (0 2)))
  (absent (x (exp (gen) ltxa)) (x (exp (gen) l)))
  (fn-of ("ltx-of" (b l) (a ltxa)) ("principal-of" (l b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa x l)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
        (exp (gen) (mul x y))))
    (enc na nb a b
      (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
        (exp (gen) (mul x y)))) (0 2))
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) l)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
              (exp (gen) (mul x y)))))) (send nb))
    ((recv
       (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
         (exp (gen) (mul x y))))
      (send
        (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
          (exp (gen) (mul x y)))))
    ((send (cat a (exp (gen) ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (exp (gen) l))) (recv "end-of-protocol") (send l))
    ((recv
       (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
         (exp (gen) (mul x y))))
      (send
        (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
          (exp (gen) (mul x y)))))
    ((recv (cat (exp (gen) x) l)) (send (cat (exp (gen) x) l)))
    ((recv
       (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
         (exp (gen) (mul x y))))
      (send
        (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
          (exp (gen) (mul x y))))))
  (label 34)
  (parent 32)
  (unrealized (6 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhcr-um3
  (vars (na nb data) (a b name) (y x ltxa ltxb rndx))
  (defstrand init 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (x x) (y y))
  (deflistener
    (hash (exp (gen) (mul y ltxa)) (exp (gen) (mul x ltxb))
      (exp (gen) (mul y x))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat (exp (gen) (mul y ltxa)) (exp (gen) (mul x ltxb))
      (exp (gen) (mul y x))))
  (deflistener (cat (exp (gen) x) ltxb))
  (defstrand resp 3 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (deflistener (cat (exp (gen) y) ltxa))
  (precedes ((0 1) (6 1)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (6 0)) ((3 0) (0 0)) ((3 0) (6 0)) ((3 2) (5 0))
    ((4 1) (1 0)) ((5 1) (4 0)) ((6 2) (0 2)) ((6 2) (7 0))
    ((7 1) (4 0)))
  (absent (y (exp (gen) ltxa)) (y (exp (gen) ltxb)) (y (exp (gen) x))
    (x (exp (gen) ltxa)) (x (exp (gen) ltxb)))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq ((exp (gen) x) (gen)) (a b) ((exp (gen) y) (gen)))
  (uniq-gen y x ltxa ltxb)
  (uniq-orig na nb)
  (operation nonce-test (added-listener (cat (exp (gen) y) ltxa))
    (exp (gen) (mul y ltxa)) (4 0))
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul y ltxa)) (exp (gen) (mul x ltxb))
              (exp (gen) (mul y x)))))) (send nb))
    ((recv
       (hash (exp (gen) (mul y ltxa)) (exp (gen) (mul x ltxb))
         (exp (gen) (mul y x))))
      (send
        (hash (exp (gen) (mul y ltxa)) (exp (gen) (mul x ltxb))
          (exp (gen) (mul y x)))))
    ((send (cat a (exp (gen) ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (exp (gen) ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat (exp (gen) (mul y ltxa)) (exp (gen) (mul x ltxb))
         (exp (gen) (mul y x))))
      (send
        (cat (exp (gen) (mul y ltxa)) (exp (gen) (mul x ltxb))
          (exp (gen) (mul y x)))))
    ((recv (cat (exp (gen) x) ltxb)) (send (cat (exp (gen) x) ltxb)))
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (recv (cat na a b (exp (gen) x)))
      (send
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul y ltxa)) (exp (gen) (mul x ltxb))
              (exp (gen) (mul y x)))))))
    ((recv (cat (exp (gen) y) ltxa)) (send (cat (exp (gen) y) ltxa))))
  (label 35)
  (parent 33)
  (unrealized (4 0) (7 0))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton dhcr-um3
  (vars (na nb data) (a b name) (y x ltxa ltxb rndx))
  (defstrand init 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (x x) (y y))
  (deflistener
    (hash (exp (gen) (mul y ltxa)) (exp (gen) (mul x ltxb))
      (exp (gen) (mul y x))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat (exp (gen) (mul y ltxa)) (exp (gen) (mul x ltxb))
      (exp (gen) (mul y x))))
  (deflistener (cat (exp (gen) x) ltxb))
  (defstrand resp 3 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb ltxb)
    (y y) (x x))
  (deflistener (cat (exp (gen) ltxa) y))
  (precedes ((0 1) (6 1)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (6 0)) ((3 0) (0 0)) ((3 0) (6 0)) ((3 2) (5 0))
    ((4 1) (1 0)) ((5 1) (4 0)) ((6 2) (0 2)) ((6 2) (7 0))
    ((7 1) (4 0)))
  (absent (y (exp (gen) ltxa)) (y (exp (gen) ltxb)) (y (exp (gen) x))
    (x (exp (gen) ltxa)) (x (exp (gen) ltxb)))
  (fn-of ("ltx-of" (a ltxa) (b ltxb))
    ("principal-of" (ltxa a) (ltxb b)))
  (neq ((exp (gen) x) (gen)) (a b) ((exp (gen) y) (gen)))
  (uniq-gen y x ltxa ltxb)
  (uniq-orig na nb)
  (operation nonce-test (added-listener (cat (exp (gen) ltxa) y))
    (exp (gen) (mul y ltxa)) (4 0))
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul y ltxa)) (exp (gen) (mul x ltxb))
              (exp (gen) (mul y x)))))) (send nb))
    ((recv
       (hash (exp (gen) (mul y ltxa)) (exp (gen) (mul x ltxb))
         (exp (gen) (mul y x))))
      (send
        (hash (exp (gen) (mul y ltxa)) (exp (gen) (mul x ltxb))
          (exp (gen) (mul y x)))))
    ((send (cat a (exp (gen) ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (exp (gen) ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat (exp (gen) (mul y ltxa)) (exp (gen) (mul x ltxb))
         (exp (gen) (mul y x))))
      (send
        (cat (exp (gen) (mul y ltxa)) (exp (gen) (mul x ltxb))
          (exp (gen) (mul y x)))))
    ((recv (cat (exp (gen) x) ltxb)) (send (cat (exp (gen) x) ltxb)))
    ((recv (cat (exp (gen) ltxa) (exp (gen) ltxb)))
      (recv (cat na a b (exp (gen) x)))
      (send
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul y ltxa)) (exp (gen) (mul x ltxb))
              (exp (gen) (mul y x)))))))
    ((recv (cat (exp (gen) ltxa) y)) (send (cat (exp (gen) ltxa) y))))
  (label 36)
  (parent 33)
  (unrealized (7 0))
  (dead)
  (comment "empty cohort"))

(defskeleton dhcr-um3
  (vars (na nb data) (a b name) (ltxa x rndx) (y expt) (l rndx))
  (defstrand init 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb l)
    (x x) (y y))
  (deflistener
    (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
      (exp (gen) (mul x y))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l l))
  (deflistener
    (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
      (exp (gen) (mul x y))))
  (deflistener (cat (exp (gen) x) l))
  (deflistener
    (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
      (exp (gen) (mul x y))))
  (deflistener
    (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
      (exp (gen) (mul x y))))
  (precedes ((0 1) (7 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((3 0) (0 0)) ((3 2) (5 0)) ((4 1) (1 0)) ((5 1) (4 0))
    ((6 1) (0 2)) ((7 1) (6 0)))
  (absent (x (exp (gen) ltxa)) (x (exp (gen) l)))
  (fn-of ("ltx-of" (b l) (a ltxa)) ("principal-of" (l b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa x l)
  (uniq-orig na)
  (operation encryption-test
    (added-listener
      (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
        (exp (gen) (mul x y))))
    (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
      (exp (gen) (mul x y))) (6 0))
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) l)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
              (exp (gen) (mul x y)))))) (send nb))
    ((recv
       (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
         (exp (gen) (mul x y))))
      (send
        (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
          (exp (gen) (mul x y)))))
    ((send (cat a (exp (gen) ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (exp (gen) l))) (recv "end-of-protocol") (send l))
    ((recv
       (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
         (exp (gen) (mul x y))))
      (send
        (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
          (exp (gen) (mul x y)))))
    ((recv (cat (exp (gen) x) l)) (send (cat (exp (gen) x) l)))
    ((recv
       (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
         (exp (gen) (mul x y))))
      (send
        (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
          (exp (gen) (mul x y)))))
    ((recv
       (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
         (exp (gen) (mul x y))))
      (send
        (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
          (exp (gen) (mul x y))))))
  (label 37)
  (parent 34)
  (unrealized (7 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton dhcr-um3
  (vars (na nb data) (a b name) (y x ltxb l rndx))
  (defstrand init 4 (na na) (nb nb) (a a) (b b) (ltxa l) (ltxb ltxb)
    (x x) (y y))
  (deflistener
    (hash (exp (gen) (mul y l)) (exp (gen) (mul x ltxb))
      (exp (gen) (mul y x))))
  (defstrand ltx-gen 3 (self a) (l l))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat (exp (gen) (mul y l)) (exp (gen) (mul x ltxb))
      (exp (gen) (mul y x))))
  (deflistener (cat (exp (gen) x) ltxb))
  (defstrand resp 3 (na na) (nb nb) (a a) (b b) (ltxa l) (ltxb ltxb)
    (y y) (x x))
  (deflistener (cat (exp (gen) y) l))
  (precedes ((0 1) (6 1)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (6 0)) ((2 2) (7 0)) ((3 0) (0 0)) ((3 0) (6 0))
    ((3 2) (5 0)) ((4 1) (1 0)) ((5 1) (4 0)) ((6 2) (0 2))
    ((7 1) (4 0)))
  (absent (y (exp (gen) l)) (y (exp (gen) ltxb)) (y (exp (gen) x))
    (x (exp (gen) l)) (x (exp (gen) ltxb)))
  (fn-of ("ltx-of" (a l) (b ltxb)) ("principal-of" (l a) (ltxb b)))
  (neq ((exp (gen) x) (gen)) (a b) ((exp (gen) y) (gen)))
  (uniq-gen y x ltxb l)
  (uniq-orig na nb)
  (operation nonce-test (displaced 8 2 ltx-gen 3) l (7 0))
  (traces
    ((recv (cat (exp (gen) l) (exp (gen) ltxb)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul y l)) (exp (gen) (mul x ltxb))
              (exp (gen) (mul y x)))))) (send nb))
    ((recv
       (hash (exp (gen) (mul y l)) (exp (gen) (mul x ltxb))
         (exp (gen) (mul y x))))
      (send
        (hash (exp (gen) (mul y l)) (exp (gen) (mul x ltxb))
          (exp (gen) (mul y x)))))
    ((send (cat a (exp (gen) l))) (recv "end-of-protocol") (send l))
    ((send (cat b (exp (gen) ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat (exp (gen) (mul y l)) (exp (gen) (mul x ltxb))
         (exp (gen) (mul y x))))
      (send
        (cat (exp (gen) (mul y l)) (exp (gen) (mul x ltxb))
          (exp (gen) (mul y x)))))
    ((recv (cat (exp (gen) x) ltxb)) (send (cat (exp (gen) x) ltxb)))
    ((recv (cat (exp (gen) l) (exp (gen) ltxb)))
      (recv (cat na a b (exp (gen) x)))
      (send
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul y l)) (exp (gen) (mul x ltxb))
              (exp (gen) (mul y x)))))))
    ((recv (cat (exp (gen) y) l)) (send (cat (exp (gen) y) l))))
  (label 38)
  (parent 35)
  (unrealized (4 0))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton dhcr-um3
  (vars (na nb data) (a b name) (ltxa x rndx) (y expt) (l rndx))
  (defstrand init 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb l)
    (x x) (y y))
  (deflistener
    (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
      (exp (gen) (mul x y))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l l))
  (deflistener
    (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
      (exp (gen) (mul x y))))
  (deflistener (cat (exp (gen) x) l))
  (deflistener
    (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
      (exp (gen) (mul x y))))
  (deflistener
    (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
      (exp (gen) (mul x y))))
  (deflistener (cat (exp (gen) x) l))
  (precedes ((0 1) (8 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((3 0) (0 0)) ((3 2) (5 0)) ((4 1) (1 0)) ((5 1) (4 0))
    ((6 1) (0 2)) ((7 1) (6 0)) ((8 1) (7 0)))
  (absent (x (exp (gen) ltxa)) (x (exp (gen) l)))
  (fn-of ("ltx-of" (b l) (a ltxa)) ("principal-of" (l b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa x l)
  (uniq-orig na)
  (operation nonce-test (added-listener (cat (exp (gen) x) l))
    (exp (gen) (mul x l)) (7 0))
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) l)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
              (exp (gen) (mul x y)))))) (send nb))
    ((recv
       (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
         (exp (gen) (mul x y))))
      (send
        (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
          (exp (gen) (mul x y)))))
    ((send (cat a (exp (gen) ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (exp (gen) l))) (recv "end-of-protocol") (send l))
    ((recv
       (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
         (exp (gen) (mul x y))))
      (send
        (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
          (exp (gen) (mul x y)))))
    ((recv (cat (exp (gen) x) l)) (send (cat (exp (gen) x) l)))
    ((recv
       (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
         (exp (gen) (mul x y))))
      (send
        (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
          (exp (gen) (mul x y)))))
    ((recv
       (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
         (exp (gen) (mul x y))))
      (send
        (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
          (exp (gen) (mul x y)))))
    ((recv (cat (exp (gen) x) l)) (send (cat (exp (gen) x) l))))
  (label 39)
  (parent 37)
  (unrealized (8 0))
  (dead)
  (comment "empty cohort"))

(defskeleton dhcr-um3
  (vars (na nb data) (a b name) (ltxa x rndx) (y expt) (l rndx))
  (defstrand init 4 (na na) (nb nb) (a a) (b b) (ltxa ltxa) (ltxb l)
    (x x) (y y))
  (deflistener
    (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
      (exp (gen) (mul x y))))
  (defstrand ltx-gen 3 (self a) (l ltxa))
  (defstrand ltx-gen 3 (self b) (l l))
  (deflistener
    (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
      (exp (gen) (mul x y))))
  (deflistener (cat (exp (gen) x) l))
  (deflistener
    (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
      (exp (gen) (mul x y))))
  (deflistener
    (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
      (exp (gen) (mul x y))))
  (deflistener (cat (exp (gen) l) x))
  (precedes ((0 1) (8 0)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((3 0) (0 0)) ((3 2) (5 0)) ((4 1) (1 0)) ((5 1) (4 0))
    ((6 1) (0 2)) ((7 1) (6 0)) ((8 1) (7 0)))
  (absent (x (exp (gen) ltxa)) (x (exp (gen) l)))
  (fn-of ("ltx-of" (b l) (a ltxa)) ("principal-of" (l b) (ltxa a)))
  (neq (a b) ((exp (gen) y) (gen)))
  (uniq-gen ltxa x l)
  (uniq-orig na)
  (operation nonce-test (added-listener (cat (exp (gen) l) x))
    (exp (gen) (mul x l)) (7 0))
  (traces
    ((recv (cat (exp (gen) ltxa) (exp (gen) l)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
              (exp (gen) (mul x y)))))) (send nb))
    ((recv
       (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
         (exp (gen) (mul x y))))
      (send
        (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
          (exp (gen) (mul x y)))))
    ((send (cat a (exp (gen) ltxa))) (recv "end-of-protocol")
      (send ltxa))
    ((send (cat b (exp (gen) l))) (recv "end-of-protocol") (send l))
    ((recv
       (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
         (exp (gen) (mul x y))))
      (send
        (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
          (exp (gen) (mul x y)))))
    ((recv (cat (exp (gen) x) l)) (send (cat (exp (gen) x) l)))
    ((recv
       (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
         (exp (gen) (mul x y))))
      (send
        (hash (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
          (exp (gen) (mul x y)))))
    ((recv
       (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
         (exp (gen) (mul x y))))
      (send
        (cat (exp (gen) (mul ltxa y)) (exp (gen) (mul x l))
          (exp (gen) (mul x y)))))
    ((recv (cat (exp (gen) l) x)) (send (cat (exp (gen) l) x))))
  (label 40)
  (parent 37)
  (unrealized (8 0))
  (dead)
  (comment "empty cohort"))

(defskeleton dhcr-um3
  (vars (na nb data) (a b name) (y x ltxb l rndx))
  (defstrand init 4 (na na) (nb nb) (a a) (b b) (ltxa l) (ltxb ltxb)
    (x x) (y y))
  (deflistener
    (hash (exp (gen) (mul y l)) (exp (gen) (mul x ltxb))
      (exp (gen) (mul y x))))
  (defstrand ltx-gen 3 (self a) (l l))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat (exp (gen) (mul y l)) (exp (gen) (mul x ltxb))
      (exp (gen) (mul y x))))
  (deflistener (cat (exp (gen) x) ltxb))
  (defstrand resp 3 (na na) (nb nb) (a a) (b b) (ltxa l) (ltxb ltxb)
    (y y) (x x))
  (deflistener (cat (exp (gen) y) l))
  (deflistener (cat (exp (gen) y) x))
  (precedes ((0 1) (6 1)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (6 0)) ((2 2) (7 0)) ((3 0) (0 0)) ((3 0) (6 0))
    ((3 2) (5 0)) ((4 1) (1 0)) ((5 1) (4 0)) ((6 2) (0 2))
    ((6 2) (8 0)) ((7 1) (4 0)) ((8 1) (4 0)))
  (absent (y (exp (gen) l)) (y (exp (gen) ltxb)) (y (exp (gen) x))
    (x (exp (gen) l)) (x (exp (gen) ltxb)))
  (fn-of ("ltx-of" (a l) (b ltxb)) ("principal-of" (l a) (ltxb b)))
  (neq ((exp (gen) x) (gen)) (a b) ((exp (gen) y) (gen)))
  (uniq-gen y x ltxb l)
  (uniq-orig na nb)
  (operation nonce-test (added-listener (cat (exp (gen) y) x))
    (exp (gen) (mul y x)) (4 0))
  (traces
    ((recv (cat (exp (gen) l) (exp (gen) ltxb)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul y l)) (exp (gen) (mul x ltxb))
              (exp (gen) (mul y x)))))) (send nb))
    ((recv
       (hash (exp (gen) (mul y l)) (exp (gen) (mul x ltxb))
         (exp (gen) (mul y x))))
      (send
        (hash (exp (gen) (mul y l)) (exp (gen) (mul x ltxb))
          (exp (gen) (mul y x)))))
    ((send (cat a (exp (gen) l))) (recv "end-of-protocol") (send l))
    ((send (cat b (exp (gen) ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat (exp (gen) (mul y l)) (exp (gen) (mul x ltxb))
         (exp (gen) (mul y x))))
      (send
        (cat (exp (gen) (mul y l)) (exp (gen) (mul x ltxb))
          (exp (gen) (mul y x)))))
    ((recv (cat (exp (gen) x) ltxb)) (send (cat (exp (gen) x) ltxb)))
    ((recv (cat (exp (gen) l) (exp (gen) ltxb)))
      (recv (cat na a b (exp (gen) x)))
      (send
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul y l)) (exp (gen) (mul x ltxb))
              (exp (gen) (mul y x)))))))
    ((recv (cat (exp (gen) y) l)) (send (cat (exp (gen) y) l)))
    ((recv (cat (exp (gen) y) x)) (send (cat (exp (gen) y) x))))
  (label 41)
  (parent 38)
  (unrealized (8 0))
  (dead)
  (comment "empty cohort"))

(defskeleton dhcr-um3
  (vars (na nb data) (a b name) (y x ltxb l rndx))
  (defstrand init 4 (na na) (nb nb) (a a) (b b) (ltxa l) (ltxb ltxb)
    (x x) (y y))
  (deflistener
    (hash (exp (gen) (mul y l)) (exp (gen) (mul x ltxb))
      (exp (gen) (mul y x))))
  (defstrand ltx-gen 3 (self a) (l l))
  (defstrand ltx-gen 3 (self b) (l ltxb))
  (deflistener
    (cat (exp (gen) (mul y l)) (exp (gen) (mul x ltxb))
      (exp (gen) (mul y x))))
  (deflistener (cat (exp (gen) x) ltxb))
  (defstrand resp 3 (na na) (nb nb) (a a) (b b) (ltxa l) (ltxb ltxb)
    (y y) (x x))
  (deflistener (cat (exp (gen) y) l))
  (deflistener (cat (exp (gen) x) y))
  (precedes ((0 1) (6 1)) ((0 3) (2 1)) ((0 3) (3 1)) ((2 0) (0 0))
    ((2 0) (6 0)) ((2 2) (7 0)) ((3 0) (0 0)) ((3 0) (6 0))
    ((3 2) (5 0)) ((4 1) (1 0)) ((5 1) (4 0)) ((6 2) (0 2))
    ((6 2) (8 0)) ((7 1) (4 0)) ((8 1) (4 0)))
  (absent (y (exp (gen) l)) (y (exp (gen) ltxb)) (y (exp (gen) x))
    (x (exp (gen) l)) (x (exp (gen) ltxb)))
  (fn-of ("ltx-of" (a l) (b ltxb)) ("principal-of" (l a) (ltxb b)))
  (neq ((exp (gen) x) (gen)) (a b) ((exp (gen) y) (gen)))
  (uniq-gen y x ltxb l)
  (uniq-orig na nb)
  (operation nonce-test (added-listener (cat (exp (gen) x) y))
    (exp (gen) (mul y x)) (4 0))
  (traces
    ((recv (cat (exp (gen) l) (exp (gen) ltxb)))
      (send (cat na a b (exp (gen) x)))
      (recv
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul y l)) (exp (gen) (mul x ltxb))
              (exp (gen) (mul y x)))))) (send nb))
    ((recv
       (hash (exp (gen) (mul y l)) (exp (gen) (mul x ltxb))
         (exp (gen) (mul y x))))
      (send
        (hash (exp (gen) (mul y l)) (exp (gen) (mul x ltxb))
          (exp (gen) (mul y x)))))
    ((send (cat a (exp (gen) l))) (recv "end-of-protocol") (send l))
    ((send (cat b (exp (gen) ltxb))) (recv "end-of-protocol")
      (send ltxb))
    ((recv
       (cat (exp (gen) (mul y l)) (exp (gen) (mul x ltxb))
         (exp (gen) (mul y x))))
      (send
        (cat (exp (gen) (mul y l)) (exp (gen) (mul x ltxb))
          (exp (gen) (mul y x)))))
    ((recv (cat (exp (gen) x) ltxb)) (send (cat (exp (gen) x) ltxb)))
    ((recv (cat (exp (gen) l) (exp (gen) ltxb)))
      (recv (cat na a b (exp (gen) x)))
      (send
        (cat (exp (gen) y)
          (enc na nb a b
            (hash (exp (gen) (mul y l)) (exp (gen) (mul x ltxb))
              (exp (gen) (mul y x)))))))
    ((recv (cat (exp (gen) y) l)) (send (cat (exp (gen) y) l)))
    ((recv (cat (exp (gen) x) y)) (send (cat (exp (gen) x) y))))
  (label 42)
  (parent 38)
  (unrealized (8 0))
  (dead)
  (comment "empty cohort"))

(comment "Nothing left to do")
