(herald yolo)

(comment "CPSA 3.6.10")
(comment "All input read from tst/yolo.scm")

(defprotocol yolo basic
  (defrole init
    (vars (a name) (n text))
    (trace (send n) (recv (enc "yolo" n a (privk a)))))
  (defrole resp
    (vars (a name) (n text))
    (trace (recv n) (send (enc "yolo" n a (privk a))))
    (uniq-gen (privk a))))

(defskeleton yolo
  (vars (n text) (a name))
  (defstrand init 2 (n n) (a a))
  (non-orig (privk a))
  (traces ((send n) (recv (enc "yolo" n a (privk a)))))
  (label 0)
  (unrealized (0 1))
  (origs)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton yolo
  (vars (n text) (a name))
  (defstrand init 2 (n n) (a a))
  (defstrand resp 2 (n n) (a a))
  (precedes ((1 1) (0 1)))
  (non-orig (privk a))
  (uniq-gen (privk a))
  (operation encryption-test (added-strand resp 2)
    (enc "yolo" n a (privk a)) (0 1))
  (traces ((send n) (recv (enc "yolo" n a (privk a))))
    ((recv n) (send (enc "yolo" n a (privk a)))))
  (label 1)
  (parent 0)
  (unrealized)
  (shape)
  (maps ((0) ((a a) (n n))))
  (origs))

(comment "Nothing left to do")

(defprotocol yolo basic
  (defrole init
    (vars (a name) (n text))
    (trace (send n) (recv (enc "yolo" n a (privk a)))))
  (defrole resp
    (vars (a name) (n text))
    (trace (recv n) (send (enc "yolo" n a (privk a))))
    (uniq-gen (privk a))))

(defskeleton yolo
  (vars (n n-0 text) (a name))
  (defstrand resp 2 (n n) (a a))
  (defstrand resp 2 (n n-0) (a a))
  (uniq-gen (privk a) (privk a))
  (traces ((recv n) (send (enc "yolo" n a (privk a))))
    ((recv n-0) (send (enc "yolo" n-0 a (privk a)))))
  (label 2)
  (unrealized)
  (origs)
  (comment "Input cannot be made into a skeleton--nothing to do"))

(defprotocol yolo basic
  (defrole init
    (vars (a name) (n text))
    (trace (send n) (recv (enc "yolo" n a (privk a)))))
  (defrole resp
    (vars (a name) (n text))
    (trace (recv n) (send (enc "yolo" n a (privk a))))
    (uniq-gen (privk a))))

(defskeleton yolo
  (vars (n n-0 n-1 text) (a name))
  (defstrand init 2 (n n) (a a))
  (defstrand resp 2 (n n-0) (a a))
  (defstrand resp 2 (n n-1) (a a))
  (uniq-gen (privk a) (privk a))
  (traces ((send n) (recv (enc "yolo" n a (privk a))))
    ((recv n-0) (send (enc "yolo" n-0 a (privk a))))
    ((recv n-1) (send (enc "yolo" n-1 a (privk a)))))
  (label 3)
  (unrealized (0 1))
  (origs)
  (comment "Input cannot be made into a skeleton--nothing to do"))
