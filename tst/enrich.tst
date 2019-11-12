(herald enrich)

(comment "CPSA 3.6.5")
(comment "All input read from enrich.scm")

(defprotocol enrich basic
  (defrole extend
    (vars (value state mesg))
    (trace (recv value) (tran state (hash value state))))
  (defrole see (vars (state mesg)) (trace (obsv state))))

(defskeleton enrich
  (vars (state mesg))
  (defstrand extend 2 (value "1") (state (hash "0" state)))
  (defstrand see 1 (state (hash "0" state)))
  (traces
    ((recv "1") (tran (hash "0" state) (hash "1" (hash "0" state))))
    ((obsv (hash "0" state))))
  (label 0)
  (unrealized (0 1) (1 0))
  (origs)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton enrich
  (vars (state mesg))
  (defstrand extend 2 (value "1") (state (hash "0" state)))
  (defstrand see 1 (state (hash "0" state)))
  (defstrand extend 2 (value "0") (state state))
  (precedes ((2 1) (1 0)))
  (leadsto ((2 1) (1 0)))
  (operation state-passing-test (added-strand extend 2) (hash "0" state)
    (1 0))
  (traces
    ((recv "1") (tran (hash "0" state) (hash "1" (hash "0" state))))
    ((obsv (hash "0" state)))
    ((recv "0") (tran state (hash "0" state))))
  (label 1)
  (parent 0)
  (unrealized (0 1))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton enrich
  (vars (state mesg))
  (defstrand extend 2 (value "1") (state (hash "0" state)))
  (defstrand see 1 (state (hash "0" state)))
  (defstrand extend 2 (value "0") (state state))
  (precedes ((1 0) (0 1)) ((2 1) (0 1)) ((2 1) (1 0)))
  (leadsto ((2 1) (0 1)) ((2 1) (1 0)))
  (operation state-passing-test (displaced 3 2 extend 2)
    (hash "0" state) (0 1))
  (traces
    ((recv "1") (tran (hash "0" state) (hash "1" (hash "0" state))))
    ((obsv (hash "0" state)))
    ((recv "0") (tran state (hash "0" state))))
  (label 2)
  (parent 1)
  (unrealized)
  (shape)
  (maps ((0 1) ((state state))))
  (origs))

(defskeleton enrich
  (vars (state mesg))
  (defstrand extend 2 (value "1") (state (hash "0" state)))
  (defstrand see 1 (state (hash "0" state)))
  (defstrand extend 2 (value "0") (state state))
  (defstrand extend 2 (value "0") (state state))
  (precedes ((1 0) (0 1)) ((2 1) (1 0)) ((3 1) (0 1)))
  (leadsto ((2 1) (1 0)) ((3 1) (0 1)))
  (operation state-passing-test (added-strand extend 2) (hash "0" state)
    (0 1))
  (traces
    ((recv "1") (tran (hash "0" state) (hash "1" (hash "0" state))))
    ((obsv (hash "0" state))) ((recv "0") (tran state (hash "0" state)))
    ((recv "0") (tran state (hash "0" state))))
  (label 3)
  (parent 1)
  (unrealized)
  (shape)
  (maps ((0 1) ((state state))))
  (origs))

(comment "Nothing left to do")
