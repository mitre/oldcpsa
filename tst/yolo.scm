(herald yolo)

;;; This protocol illustates the odd effect of (uniq-gen (privk a)).

(defprotocol yolo basic
  (defrole init
    (vars (a name) (n text))
    (trace
     (send n)
     (recv (enc "yolo" n a (privk a)))))

  (defrole resp
    (vars (a name) (n text))
    (trace
     (recv n)
     (send (enc "yolo" n a (privk a))))
    (uniq-gen (privk a))))

;;; First query:  the protocol does authenticate the responder to the
;;; initiator.

(defskeleton yolo
  (vars (a name)(n text))
  (defstrand init 2 (n n) (a a))
  (non-orig (privk a)))

;;; Second query:  Two responder strands cannot be completed to any
;;; execution.  Oddly, the CPSA output has (unrealized) which may
;;; confuse the reader.  The only clue that this query has turned out
;;; to be dead is a comment:
;;;
;;; (comment "Input cannot be made into a skeleton--nothing to do")

(defskeleton yolo
  (vars (a name)(n text))
  (defstrand resp 2 (a a))
  (defstrand resp 2 (a a)))

;;; Third query:  Two responder strands cannot be completed to any
;;; execution, in this case with an initiator strand present too.
;;; CPSA's output now reports (unrealized (0 1)), together with the
;;; same comment that the input cannot be made into a skeleton.  

(defskeleton yolo
  (vars (a name)(n text))
  (defstrand init 2 (n n) (a a))
  (defstrand resp 2 (a a))
  (defstrand resp 2 (a a)))
