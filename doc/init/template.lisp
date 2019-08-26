;; This is a minimal template for a CPSA input file.

;; Replace <TITLE> with the desired title and <PROTONAME>
;; with the desired name of your project.

;; The defrole template below may be copied and used as
;; a starting point for the roles of your protocol.
;; Change the <ROLENAME> field in each copy as desired.
;; Roles must have distinct names.

;; The basic cryptoalgebra is selected by default. If
;; your project requires the diffie-hellman algebra,
;; delete "basic" on the defprotocol line, uncomment
;; "diffie-hellman" on this same line and uncomment
;; the "(algebra diffie-hellman)" statement in the
;; herald.

;; Refer to the CPSA manual for more information
;; about syntax and additional features.

(herald "<TITLE>"
	;; (algebra diffie-hellman)
	)

(defprotocol <PROTONAME> basic ;; diffie-hellman

  (defrole <ROLENAME>
    (vars )
    (trace

     )
    )

  )

(defskeleton <PROTONAME>
  (vars )
  (defstrand )
  )
