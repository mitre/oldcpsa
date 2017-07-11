; Flawed and Fixed Kerberos

; In this example, the initiator is trying to send a private message
; to the responder.  Each are presumed to share a long-term private key
; with the key server, which will choose a fresh session key on request
; and encrypt it for both parties.

; A flaw exists in the protocol intentionally: the initiator will need
; confirmation that the session key is shared between the initiator and
; the partner they specified.

; This version of the protocol is not modeled properly and misses the flaw.
(defprotocol kerb-flawed basic
  (defrole init
    (vars (a b s name) (m n text) (k skey))
    (trace
       ;; Make the request
       (send (cat a b n)) 
       ;; Receive the encrypted key and ticket
       (recv (cat (enc k n (ltk a s)) (enc k a b (ltk b s)))) 
       (send (cat (enc m k) (enc k a b (ltk b s)))))
    (uniq-orig n))
  (defrole resp
    (vars (a b s name) (m text) (k skey))
    (trace
       (recv (cat (enc m k) (enc k a b (ltk b s))))))
  (defrole keyserv
    (vars (a b s name) (m n text) (k skey))
    (trace
       ;; Receive the request
       (recv (cat a b n))
       ;; Send the encrypted key and ticket
       (send (cat (enc k n (ltk a s)) (enc k a b (ltk b s)))))
    (uniq-orig k))
)

; This skeleton should be dead, even though m can leak
(defskeleton kerb-flawed
  (vars (a b s name) (m text))
  (defstrand init 3 (a a) (b b) (s s) (m m))
  (deflistener m)
  (non-orig (ltk a s) (ltk b s))
  (uniq-orig m))

; This version of the protocol is not modeled properly, with a generic variable (ticket)
(defprotocol kerb-flawed2 basic
  (defrole init
    (vars (a b s name) (ticket mesg) (m n text) (k skey))
    (trace
       ;; Make the request
       (send (cat a b n)) 
       ;; Receive the encrypted key and ticket
       (recv (cat (enc k n (ltk a s)) ticket))
       (send (cat (enc m k) ticket)))
    (uniq-orig n))
  (defrole resp
    (vars (a b s name) (m text) (k skey))
    (trace
       (recv (cat (enc m k) (enc k a b (ltk b s))))))
  (defrole keyserv
    (vars (a b s name) (m n text) (k skey))
    (trace
       ;; Receive the request
       (recv (cat a b n))
       ;; Send the encrypted key and ticket
       (send (cat (enc k n (ltk a s)) (enc k a b (ltk b s)))))
    (uniq-orig k))
)

; This skeleton should have a shape, demonstrating that m may be leaked.  
(defskeleton kerb-flawed2
  (vars (a b s name) (m text))
  (defstrand init 3 (a a) (b b) (s s) (m m))
  (deflistener m)
  (non-orig (ltk a s) (ltk b s))
  (uniq-orig m))
