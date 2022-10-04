;;; Mark Ryan's envelope protocol, implemented using TPMs.
;;; The protocol includes roles to emulate interactions with a simplified
;;; TPM (trusted platform module) that only has a single Platform
;;; Configuration Register (PCR).

;;; The true protocol is encoded in the "alice" role.  Alice is a
;;; teenager who wants to go out for an evening and maintain privacy
;;; about her itinerary, but her parents insist on being able to know in
;;; an emergency.

;;; Alice uses a TPM-created key to encrypt the itinerary (represented
;;; by 'v' for value).  Alice's parents can alter the state of the TPM
;;; in either of two ways, but not in both: they can extend with
;;; "obtain", which will enable decryption of 'v'.  Or, they can
;;; extend with "refuse", which serves to represent permanently declining
;;; the option to open the envelope.

(herald "Envelope Protocol" (bound 20))

;;; Encoding of a PCR extend operation
(defmacro (extend val old)
  (hash val old))

;; This is the refusal token
(defmacro (refuse n pcr v k aik)
  (enc "quote" (extend "refuse" (extend n pcr)) (enc v k) aik))

(defprotocol envelope basic

  ;; Power on sets the TPM to the boot state
  (defrole tpm-power-on
    (vars (state mesg))
    (trace
     (init "0")))

  ;; TPM Quote has a fake event to deal with the fact that a variable
  ;; of sort mesg must be acquired.
  (defrole tpm-quote
    (vars (nonce pcr mesg) (aik akey))
    (trace
     (recv (cat "quote" nonce))
     (obsv pcr)
     (send (enc "quote" pcr nonce aik)))
    (non-orig aik))

  ;; The extend command occurs only within an encrypted session.  We
  ;; assume some session key already exists
  (defrole tpm-extend-enc
    (vars (value state mesg) (esk skey) (tne tno data)
	  (tpmkey akey))
    (trace
     (recv (cat "establish transport"
		tpmkey (enc esk tpmkey)))
     (send (cat "establish transport" tne))
     (recv (cat "execute transport"
		(cat "extend" (enc value esk))
		tno "false"
		(hash esk (hash "execute transport"
				(hash "extend"
				      (enc value esk)))
				tne tno "false")))
     (tran state (extend value state)))
    (uniq-orig tne)
    ;; (non-orig (invk tpmkey))  ; JDG:  This assumption irrelevant
    )

  ;; This role creates a key whose use is restricted to a requested
  ;; pcr value (since we only model one pcr).  It doesn't create or
  ;; change any TPM state.
  (defrole tpm-create-key
    (vars (k aik akey) (pcr mesg) (esk skey))
    (trace
     (recv (enc "create key" pcr esk)) ;; encryption prevents weird shapes
     (send (enc "created" k pcr aik)));; no tpm state is set
    (uniq-orig k)
    (non-orig (invk k) aik esk))

  ;; This role receives an encryption and a previously made key
  ;; structure that restricts the decryption key to be used with a
  ;; certain pcr value.  It retrieves the current value and checks
  ;; that it matches before decrypting.
  (defrole tpm-decrypt
    (vars (m pcr mesg) (k aik akey))
    (trace
     (recv (cat "decrypt" (enc m k)))
     (recv (enc "created" k pcr aik))
     (obsv pcr)
     (send m))
    (non-orig aik))

  ;; Alice extends a pcr with a fresh nonce in an encrypted session.
  ;; She has the TPM create a new key whose use is bound to the hash
  ;; of pcr value she just created with the string "obtain".  She then
  ;; encrypts her fresh secret with this newly created key.  This role
  ;; has a fake reception event to deal with he fact that pcr must be
  ;; acquired.
  (defrole alice
    (vars (v n tne tno data) (esk1 esk skey) (k aik tpmkey akey)
	  (pcr mesg))
    (trace
     (send (cat "establish transport"
		tpmkey (enc esk tpmkey)))
     (recv (cat "establish transport" tne))
     (send (cat "execute transport"
		(cat "extend" (enc n esk))
		tno "false"
		(hash esk (hash "execute transport"
				(hash "extend"
				      (enc n esk)))
				tne tno "false")))
     (recv pcr)				; Fake event
     (send (enc "create key" (extend "obtain" (extend n pcr)) esk1))
     (recv (enc "created" k (extend "obtain" (extend n pcr)) aik))
     (send (enc v k)))
    (uniq-orig n v tno esk)
    (neq (tno n))
    (non-orig aik esk1 (invk tpmkey)	; JDG:  This assumption important
	      )))

;;; Initial skeleton
(defskeleton envelope
  (vars (v n data) (esk skey) (k aik tpmkey akey) (pcr mesg))
  (deflistener esk)
  (defstrand alice 7 (esk esk) (n n) (pcr pcr) (v v) (k k) (aik aik)))

(defskeleton envelope
  (vars (v n data) (k aik akey) (pcr mesg))
  (deflistener (refuse n pcr v k aik))
  (deflistener v)
  (defstrand alice 7 (n n) (pcr pcr) (v v) (k k) (aik aik)))

(defskeleton envelope
  (vars (v data))
  (deflistener v)
  (defstrand alice 7 (v v)))

(defskeleton envelope
  (vars (v n data) (k aik akey) (pcr mesg))
  (deflistener (refuse n pcr v k aik))
  ;;   (deflistener v)
  (defstrand alice 7 (n n) (pcr pcr) (v v) (k k) (aik aik)))
