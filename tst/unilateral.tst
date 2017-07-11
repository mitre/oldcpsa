(herald unilateral)

(comment "CPSA 3.2.2")
(comment "All input read from unilateral.scm")

(defprotocol unilateral basic
  (defrole init
    (vars (n text) (k akey))
    (trace (send (enc n k)) (recv n))
    (uniq-orig n))
  (defrole resp
    (vars (n text) (k akey))
    (trace (recv (enc n k)) (send n))))

(defskeleton unilateral
  (vars (n text) (k akey))
  (defstrand init 2 (n n) (k k))
  (non-orig (invk k))
  (uniq-orig n)
  (traces ((send (enc n k)) (recv n)))
  (label 0)
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
  (label 1)
  (parent 0)
  (unrealized)
  (shape)
  (maps ((0) ((k k) (n n))))
  (origs (n (0 0))))

(comment "Nothing left to do")

(defprotocol unilateral basic
  (defrole init
    (vars (n text) (k akey))
    (trace (send (enc n k)) (recv n))
    (uniq-orig n))
  (defrole resp
    (vars (n text) (k akey))
    (trace (recv (enc n k)) (send n))))

(defskeleton unilateral
  (vars (n text) (k akey))
  (defstrand resp 2 (n n) (k k))
  (non-orig (invk k))
  (pen-non-orig n)
  (traces ((recv (enc n k)) (send n)))
  (label 2)
  (unrealized (0 0))
  (origs)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton unilateral
  (vars (n text) (k k-0 akey))
  (defstrand resp 2 (n n) (k k))
  (defstrand init 1 (n n) (k k-0))
  (precedes ((1 0) (0 0)))
  (non-orig (invk k))
  (pen-non-orig n)
  (uniq-orig n)
  (operation nonce-test (added-strand init 1) n (0 0))
  (traces ((recv (enc n k)) (send n)) ((send (enc n k-0))))
  (label 3)
  (parent 2)
  (unrealized)
  (shape)
  (maps ((0) ((n n) (k k))))
  (origs (n (1 0))))

(comment "Nothing left to do")
