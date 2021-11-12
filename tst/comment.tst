(herald comment)

(comment "CPSA 3.6.8")
(comment "All input read from tst/comment.scm")

(defprotocol comment basic
  (defrole comment (vars (x text)) (trace (recv x)) (comment "role"))
  (comment "protocol"))

(defskeleton comment
  (vars (x text))
  (defstrand comment 1 (x x))
  (comment "skeleton")
  (traces ((recv x)))
  (label 0)
  (unrealized)
  (shape)
  (maps ((0) ((x x))))
  (origs))

(comment "Nothing left to do")
