(herald comment)

(defprotocol comment basic
  (defrole comment (vars (x text))
    (trace (recv x))
    (comment "role"))
  (comment "protocol"))

(defskeleton comment (vars)
  (defstrand comment 1)
  (comment "skeleton"))
