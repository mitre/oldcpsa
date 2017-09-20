;;; Skeleton transformers

;;; Provides
;;;   cpsa-cleanse     -- strips a skeleton of its comment fields
;;;   cpsa-strand-swap -- swaps two strands in a skeleton

(defun cpsa-cleanse (start end)
  "Strips the selected skeleton of comment fields and pretty prints the result."
  (interactive "r")
  (let ((skel (skel start end)))
    (goto-char end)
    (insert "\n\n" (pp-to-string (skel-to-list skel)))
    (message "cleansed")))

;; Construct an internal representation of a skeleton.

(defun skel (start end)
  (let* ((str (buffer-substring start end))
	 (form (car (read-from-string str))))
    (if (not (skelp form))
	(error "form not a skeleton")
      (let* ((two (get-skel-strands form))
	     (strands (car two))
	     (rest (cdr two)))
	(vector (cadr form)		; Protocol
		(cdar (cddr form))	; Vars
		strands			; Strand list
		(lookup 'precedes rest)	; Node orderings
		(lookup 'non-orig rest)	; Non-origination assumptions
		(lookup 'uniq-orig rest)))))) ; Unique origination

;;; Partial check that an S-expression represents a skeleton
(defun skelp (form)
  (and (consp form)
       (eq 'defskeleton (car form))
       (consp (cdr form))
       (symbolp (cadr form))
       (consp (cddr form))
       (eq 'vars (caar (cddr form)))
       (alistp (cdr (cddr form)))))

;;; Is form a list of pairs, where each pair starts with a symbol?
(defun alistp (form)
  (or (null form)
      (and (consp form)
	   (consp (car form))
	   (symbolp (caar form))
	   (alistp (cdr form)))))

;;; Returns the strands and the rest of the form
(defun get-skel-strands (form)
  (let ((strands '())
	(form (cdr (cddr form))))
    (while (and form (or (eq 'defstrand (caar form))
			 (eq 'deflistener (caar form))))
      (setq strands (cons (car form) strands))
      (setq form (cdr form)))
    (cons (reverse strands) form)))

;;; Removes the key on successful lookup
(defun lookup (key alist)
  (let ((result (assq key alist)))
    (if (consp result)
	(cdr result)
      result)))

;; Accessors to the internal representation of a skeleton.

(defun skel-prot (skel)
  (elt skel 0))

(defun skel-vars (skel)
  (elt skel 1))

(defun skel-strands (skel)
  (elt skel 2))

(defun skel-precedes (skel)
  (elt skel 3))

(defun skel-non-orig (skel)
  (elt skel 4))

(defun skel-uniq-orig (skel)
  (elt skel 5))

;; Convert from the internal representation to an S-expression

(defun skel-to-list (skel)
  (let ((l '()))
    (setq l (maybe-add 'uniq-orig (skel-uniq-orig skel) l))
    (setq l (maybe-add 'non-orig (skel-non-orig skel) l))
    (setq l (maybe-add 'precedes (skel-precedes skel) l))
    (setq l (strands-to-list (skel-strands skel) l))
    `(defskeleton ,(skel-prot skel)
       (vars ,@(skel-vars skel))
       ,@l)))

(defun maybe-add (key pair l)
  (if (consp pair)
      (cons (cons key pair) l)
    l))

(defun strands-to-list (strands l)
  (if (consp strands)
      (cons (car strands) (strands-to-list (cdr strands) l))
    l))

;;; Swap two strands

(defun cpsa-strand-swap (start end)
  "Swaps two strands in the selected skeleton and pretty prints the result."
  (interactive "r")
  (let* ((skel (skel start end))
	 (n (length (skel-strands skel)))
	 (s0 (enter-strand n))
	 (s1 (enter-strand n)))
    (aset skel 2 (permute-list s0 s1 (skel-strands skel)))
    (aset skel 3 (permute-precedes s0 s1 (skel-precedes skel)))
    (goto-char end)
    (insert "\n\n" (pp-to-string (skel-to-list skel)))
    (message "strands swapped")))

(defun enter-strand (n)
  (let ((s (read-minibuffer "Enter a strand: ")))
    (if (or (not (numberp s)) (< s 0) (<= n s))
	(error "Expecting a strand number")
      s)))

(defun nats (n)
  (let ((l '()))
    (while (> n 0)
      (setq n (- n 1))
      (setq l (cons n l)))
    l))

(defun permute-list (a b l)
  (let ((swap (lambda (x)
		(cond ((equal x a) b)
		      ((equal x b) a)
		      (t x)))))
    (mapcar (lambda (e)
	      (elt l (funcall swap e)))
	    (nats (length l)))))

(defun permute-precedes (a b l)
  (let ((swap (lambda (x)
		(cond ((equal x a) b)
		      ((equal x b) a)
		      (t x)))))
    (mapcar (lambda (p)
	      (let* ((n0 (car p))
		     (n1 (cadr p))
		     (s0 (funcall swap (car n0)))
		     (s1 (funcall swap (car n1))))
		(list
		 (list s0 (cadr n0))
		 (list s1 (cadr n1)))))
	  l)))
