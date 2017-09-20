(herald "Kerberos PKINIT")

;;; This file includes three versions of the Kerberos PKINIT
;;; protocol. The first version is the flawed version that Cervesato
;;; et al. analyzed in "Breaking and Fixing Public-Key Kerberos." The
;;; other two versions are the two fixes that are explored in the same
;;; paper.

;;; We also express the goal that the authors identify as a property
;;; not met by the original flawed version, but which both fixes do
;;; achieve. Running CPSA with this input demonstrates that the flawed
;;; version does not satisfy the goal, but both fixed versions do.

;;; How to use this file:
;;;
;;; CPSA looks at the goal and creates an initial point of view that
;;; corresponds to the information contained in the antecedent of the
;;; implication of the goal. It then outputs a characterization
;;; (consisting of shapes) of all executions containing at least the
;;; structure contained in the point of view. It then evaluates the
;;; goal against each of those shapes. The guarantee CPSA provides is
;;; that a goal is achieved iff each of the shapes satisfies the goal.

;;; We can look at the output in pkinit.xhtml to determine what CPSA
;;; discovered. Notice that the shape associated with the first
;;; analysis (of the flawed version) contains an annotation that says
;;; (satisfies (no (c c) (as as) (k k) (z (0 1)))). This means that
;;; the goal is not satisfied. The pairs of terms represent a variable
;;; assignment for the logical variables of the goal into elements of
;;; the shape. This is the variable assignment of the antecendant
;;; which has no extension for which the conclusion is also
;;; satisfied. The shapes for the other analyses contain the
;;; annotation (satisfies yes) indicating that the goal is satisfied
;;; in the shape (and hence in any realized skeleton).

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       ;;
;; Protocol Descriptions ;;
;;                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is the flawed version of the Kerberos public key initial
;; round.
(defprotocol pkinit-flawed basic
  (defrole client
    (vars (c t as name) (n2 n1 text) (tc tk tgt data) (k ak skey))
    (trace (send (cat (enc tc n2 (privk c)) c t n1))
      (recv
        (cat (enc (enc k n2 (privk as)) (pubk c)) c tgt
          (enc ak n1 tk t k))))
    (uniq-orig n1 n2))
  (defrole auth
    (vars (c t as name) (n2 n1 text) (tc tk tgt data) (k ak skey))
    (trace (recv (cat (enc tc n2 (privk c)) c t n1))
      (send
        (cat (enc (enc k n2 (privk as)) (pubk c)) c tgt
          (enc ak n1 tk t k))))
    (uniq-orig k ak)))

;; This is the fix the authors proposed to the standards group. It
;; adds the value c in a new field inside the server's signature.
(defprotocol pkinit-fix1 basic
  (defrole client
    (vars (c t as name) (n2 n1 text) (tc tk tgt data) (k ak skey))
    (trace (send (cat (enc tc n2 (privk c)) c t n1))
      (recv
        (cat (enc (enc k n2 c (privk as)) (pubk c)) c tgt
          (enc ak n1 tk t k))))
    (uniq-orig n1 n2))
  (defrole auth
    (vars (c t as name) (n2 n1 text) (tc tk tgt data) (k ak skey))
    (trace (recv (cat (enc tc n2 (privk c)) c t n1))
      (send
        (cat (enc (enc k n2 c (privk as)) (pubk c)) c tgt
          (enc ak n1 tk t k))))
    (uniq-orig k ak)))

;; This is the fix that the standards group proposed and adopted. It
;; replaces n2 in the server's with a hash of the entire first
;; message.
(defprotocol pkinit-fix2 basic
  (defrole client
    (vars (c t as name) (n2 n1 text) (tc tk tgt data) (k ak skey))
    (trace (send (cat (enc tc n2 (privk c)) c t n1))
      (recv
        (cat (enc (enc k (hash (cat (enc tc n2 (privk c)) c t n1))
                       (privk as)) (pubk c)) c tgt (enc ak n1 tk t k))))
    (uniq-orig n1 n2))
  (defrole auth
    (vars (c t as name) (n2 n1 text) (tc tk tgt data) (k ak skey))
    (trace (recv (cat (enc tc n2 (privk c)) c t n1))
      (send
        (cat (enc (enc k (hash (cat (enc tc n2 (privk c)) c t n1))
                       (privk as)) (pubk c)) c tgt (enc ak n1 tk t k))))
    (uniq-orig k ak)))

;;;;;;;;;;;;;;;;;;;;;
;;                 ;;
;; Goal Statements ;;
;;                 ;;
;;;;;;;;;;;;;;;;;;;;;

;; This is the goal that the authors discovered was not met by the
;; original, flawed version, but which was met by both fixes.
(defgoal pkinit-flawed
  (forall ((c as name) (k skey) (z node))
          (implies
           (and (p "client" 1 z)
                (p "client" "c" z c)
                (p "client" "as" z as)
                (p "client" "k" z k)
                (non (privk as))
                (non (privk c)))
           (exists ((z-0 node))
                   (and (p "auth" 1 z-0)
                        (p "auth" "as" z-0 as)
                        (p "auth" "k" z-0 k)
                        (p "auth" "c" z-0 c))))))

;; Since we reference the protocol in the expression of the goal, we
;; must repeat it once for every protocol against which we would like
;; to evaluate the goal.
(defgoal pkinit-fix1
  (forall ((c as name) (k skey) (z node))
          (implies
           (and (p "client" 1 z)
                (p "client" "c" z c)
                (p "client" "as" z as)
                (p "client" "k" z k)
                (non (privk as))
                (non (privk c)))
           (exists ((z-0 node))
                   (and (p "auth" 1 z-0)
                        (p "auth" "as" z-0 as)
                        (p "auth" "k" z-0 k)
                        (p "auth" "c" z-0 c))))))

;; Repeated once more.
(defgoal pkinit-fix2
  (forall ((c as name) (k skey) (z node))
          (implies
           (and (p "client" 1 z)
                (p "client" "c" z c)
                (p "client" "as" z as)
                (p "client" "k" z k)
                (non (privk as))
                (non (privk c)))
           (exists ((z-0 node))
                   (and (p "auth" 1 z-0)
                        (p "auth" "as" z-0 as)
                        (p "auth" "k" z-0 k)
                        (p "auth" "c" z-0 c))))))

;;;;;;;;

(defgoal pkinit-flawed
  (forall ((c c-0 as name) (k skey) (z z-0 node))
          (implies
           (and (p "client" 1 z)
                (p "client" "c" z c)
                (p "client" "as" z as)
                (p "client" "k" z k)
                (p "auth" 1 z-0)
                (p "auth" "k" z-0 k)
                (p "auth" "c" z-0 c-0)
                (non (privk as))
                (non (privk c)))
           ;(exists ((z-1 node))
           ;        (p "client" 1 z-1))
           (= c c-0)
           )))

(defgoal pkinit-fix1
  (forall ((c c-0 as name) (k skey) (z z-0 node))
          (implies
           (and (p "client" 1 z)
                (p "client" "c" z c)
                (p "client" "as" z as)
                (p "client" "k" z k)
                (p "auth" 1 z-0)
                (p "auth" "k" z-0 k)
                (p "auth" "c" z-0 c-0)
                (non (privk as))
                (non (privk c)))
           (exists ((z-1 node))
                   (p "client" 1 z-1))
           ;(= c c-0)
           )))

(defgoal pkinit-fix2
  (forall ((c c-0 as name) (k skey) (z z-0 node))
          (implies
           (and (p "client" 1 z)
                (p "client" "c" z c)
                (p "client" "as" z as)
                (p "client" "k" z k)
                (p "auth" 1 z-0)
                (p "auth" "k" z-0 k)
                (p "auth" "c" z-0 c-0)
                (non (privk as))
                (non (privk c)))
           (exists ((z-1 node))
                   (p "client" 1 z-1)))))
