(herald enrich)

(defprotocol enrich basic
  (defrole extend
    (vars (value state mesg))
    (trace
     (recv value)
     (tran state (hash value state))))
  (defrole see
    (vars (state mesg))
    (trace
     (obsv state))))

(defskeleton enrich
  (vars (state mesg))
  (defstrand extend 2 (value "1") (state (hash "0" state)))
  (defstrand see 1 (state (hash "0" state))))
