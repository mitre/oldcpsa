(herald "Protocol Transformations With Rules")

(comment "CPSA 3.6.8")
(comment "All input read from tst/prottrans.scm")

(defprotocol ns basic
  (defrole init
    (vars (a b akey) (na nb text))
    (trace (send (enc na a b)) (recv (enc na nb a)) (send (enc nb b))))
  (defrole resp
    (vars (b a akey) (nb na text))
    (trace (recv (enc na a b)) (send (enc na nb a)) (recv (enc nb b))))
  (comment "Needham-Schroeder without rules"))

(defskeleton ns
  (vars (na nb text) (a b akey))
  (defstrand init 3 (na na) (nb nb) (a a) (b b))
  (non-orig (invk a) (invk b))
  (uniq-orig na)
  (comment "Initiator point-of-view")
  (traces ((send (enc na a b)) (recv (enc na nb a)) (send (enc nb b))))
  (label 0)
  (unrealized (0 1))
  (origs (na (0 0)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton ns
  (vars (na nb nb-0 text) (a b akey))
  (defstrand init 3 (na na) (nb nb) (a a) (b b))
  (defstrand resp 2 (nb nb-0) (na na) (b b) (a a))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (non-orig (invk a) (invk b))
  (uniq-orig na)
  (operation nonce-test (added-strand resp 2) na (0 1) (enc na a b))
  (traces ((send (enc na a b)) (recv (enc na nb a)) (send (enc nb b)))
    ((recv (enc na a b)) (send (enc na nb-0 a))))
  (label 1)
  (parent 0)
  (unrealized (0 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton ns
  (vars (na nb text) (a b akey))
  (defstrand init 3 (na na) (nb nb) (a a) (b b))
  (defstrand resp 2 (nb nb) (na na) (b b) (a a))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (non-orig (invk a) (invk b))
  (uniq-orig na)
  (operation nonce-test (contracted (nb-0 nb)) na (0 1) (enc na nb a)
    (enc na a b))
  (traces ((send (enc na a b)) (recv (enc na nb a)) (send (enc nb b)))
    ((recv (enc na a b)) (send (enc na nb a))))
  (label 2)
  (parent 1)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (b b) (na na) (nb nb))))
  (origs (na (0 0))))

(comment "Nothing left to do")

(defprotocol ns basic
  (defrole init
    (vars (a b akey) (na nb text))
    (trace (send (enc na a b)) (recv (enc na nb a)) (send (enc nb b))))
  (defrole resp
    (vars (b a akey) (nb na text))
    (trace (recv (enc na a b)) (send (enc na nb a)) (recv (enc nb b))))
  (comment "Needham-Schroeder without rules"))

(defskeleton ns
  (vars (nb na text) (a b akey))
  (defstrand resp 3 (nb nb) (na na) (b b) (a a))
  (non-orig (invk a))
  (uniq-orig nb)
  (comment "Responder point-of-view")
  (traces ((recv (enc na a b)) (send (enc na nb a)) (recv (enc nb b))))
  (label 3)
  (unrealized (0 2))
  (origs (nb (0 1)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton ns
  (vars (nb na text) (a b b-0 akey))
  (defstrand resp 3 (nb nb) (na na) (b b) (a a))
  (defstrand init 3 (na na) (nb nb) (a a) (b b-0))
  (precedes ((0 1) (1 1)) ((1 2) (0 2)))
  (non-orig (invk a))
  (uniq-orig nb)
  (operation nonce-test (added-strand init 3) nb (0 2) (enc na nb a))
  (traces ((recv (enc na a b)) (send (enc na nb a)) (recv (enc nb b)))
    ((send (enc na a b-0)) (recv (enc na nb a)) (send (enc nb b-0))))
  (label 4)
  (parent 3)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (nb nb) (b b) (na na))))
  (origs (nb (0 1))))

(comment "Nothing left to do")

(defprotocol unilateral basic
  (defrole init
    (vars (n text) (k akey))
    (trace (send (enc n k)) (recv n)))
  (defrole resp
    (vars (n text) (k akey))
    (trace (recv (enc n k)) (send n))))

(defskeleton unilateral
  (vars (n text) (k akey))
  (defstrand init 2 (n n) (k k))
  (non-orig (invk k))
  (uniq-orig n)
  (comment "The shape analysis sentence for this problem:"
    (defgoal unilateral
      (forall ((n text) (k akey) (z strd))
        (implies
          (and (p "init" z 2) (p "init" "n" z n) (p "init" "k" z k)
            (non (invk k)) (uniq-at n z 0))
          (exists ((z-0 strd))
            (and (p "resp" z-0 2) (p "resp" "n" z-0 n)
              (p "resp" "k" z-0 k) (prec z 0 z-0 0)
              (prec z-0 1 z 1)))))))
  (traces ((send (enc n k)) (recv n)))
  (label 5)
  (unrealized (0 1))
  (origs (n (0 0)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton unilateral
  (vars (n text) (k akey))
  (defstrand init 2 (n n) (k k))
  (defstrand resp 2 (n n) (k k))
  (precedes ((0 0) (1 0)) ((1 1) (0 1)))
  (non-orig (invk k))
  (uniq-orig n)
  (operation nonce-test (added-strand resp 2) n (0 1) (enc n k))
  (traces ((send (enc n k)) (recv n)) ((recv (enc n k)) (send n)))
  (label 6)
  (parent 5)
  (unrealized)
  (shape)
  (maps ((0) ((k k) (n n))))
  (origs (n (0 0))))

(comment "Nothing left to do")

(defprotocol ns-with-rule basic
  (defrole init
    (vars (a b akey) (na nb text))
    (trace (send (enc na a b)) (recv (enc na nb a)) (send (enc nb b))))
  (defrole resp
    (vars (b a akey) (nb na text))
    (trace (recv (enc na a b)) (send (enc na nb a)) (recv (enc nb b))))
  (defrule unilateral
    (forall ((r strd) (a akey) (nb text))
      (implies
        (and (p "resp" r 3) (p "resp" "a" r a) (p "resp" "nb" r nb)
          (non (invk a)) (uniq nb))
        (exists ((i strd))
          (and (p "init" i 3) (p "init" "a" i a) (p "init" "nb" i nb)
            (prec r 1 i 1) (prec i 2 r 2)))))
    (comment "Unilateral sas under the predicate mapping:")
    (comment (p "init" 2) "->" (p "resp" 3) "and")
    (comment (p "init" "n") "->" (p "resp" "nb") "and")
    (comment (p "init" "k") "->" (p "resp" "a") "and")
    (comment (p "resp" 2) "->" (p "init" 3) "and")
    (comment (p "resp" "n") "->" (p "init" "nb"))
    (comment (p "resp" "k") "->" (p "init" "a")))
  (comment "Needham-Schroeder with rule"))

(defskeleton ns-with-rule
  (vars (nb na text) (a b akey))
  (defstrand resp 3 (nb nb) (na na) (b b) (a a))
  (non-orig (invk a))
  (uniq-orig nb)
  (comment "Responder point-of-view")
  (traces ((recv (enc na a b)) (send (enc na nb a)) (recv (enc nb b))))
  (label 7)
  (unrealized (0 2))
  (origs (nb (0 1)))
  (comment "Not closed under rules"))

(defskeleton ns-with-rule
  (vars (na na-0 nb text) (b a b-0 akey))
  (defstrand resp 3 (nb nb) (na na) (b b) (a a))
  (defstrand init 3 (na na-0) (nb nb) (a a) (b b-0))
  (precedes ((0 1) (1 1)) ((1 2) (0 2)))
  (non-orig (invk a))
  (uniq-orig nb)
  (rule unilateral)
  (traces ((recv (enc na a b)) (send (enc na nb a)) (recv (enc nb b)))
    ((send (enc na-0 a b-0)) (recv (enc na-0 nb a))
      (send (enc nb b-0))))
  (label 8)
  (parent 7)
  (unrealized (1 1))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton ns-with-rule
  (vars (na nb text) (b a b-0 akey))
  (defstrand resp 3 (nb nb) (na na) (b b) (a a))
  (defstrand init 3 (na na) (nb nb) (a a) (b b-0))
  (precedes ((0 1) (1 1)) ((1 2) (0 2)))
  (non-orig (invk a))
  (uniq-orig nb)
  (operation nonce-test (contracted (na-0 na)) nb (1 1) (enc na nb a))
  (traces ((recv (enc na a b)) (send (enc na nb a)) (recv (enc nb b)))
    ((send (enc na a b-0)) (recv (enc na nb a)) (send (enc nb b-0))))
  (label 9)
  (parent 8)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (nb nb) (b b) (na na))))
  (origs (nb (0 1))))

(defskeleton ns-with-rule
  (vars (na na-0 nb text) (b a b-0 b-1 akey))
  (defstrand resp 3 (nb nb) (na na) (b b) (a a))
  (defstrand init 3 (na na-0) (nb nb) (a a) (b b-0))
  (defstrand init 3 (na na) (nb nb) (a a) (b b-1))
  (precedes ((0 1) (2 1)) ((1 2) (0 2)) ((2 2) (1 1)))
  (non-orig (invk a))
  (uniq-orig nb)
  (operation nonce-test (added-strand init 3) nb (1 1) (enc na nb a))
  (traces ((recv (enc na a b)) (send (enc na nb a)) (recv (enc nb b)))
    ((send (enc na-0 a b-0)) (recv (enc na-0 nb a)) (send (enc nb b-0)))
    ((send (enc na a b-1)) (recv (enc na nb a)) (send (enc nb b-1))))
  (label 10)
  (parent 8)
  (seen 9)
  (unrealized)
  (comment "1 in cohort - 0 not yet seen"))

(comment "Nothing left to do")
