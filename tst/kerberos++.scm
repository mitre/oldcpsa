; CPSA - Full Local Realm Kerberos

; In this example, cli (client) is trying to connect to the serv (service). 
; 
; 1) It first sends a request to the as (authentication server) for an initial tgt (ticket-granting ticket).

; 2) The AS responds by sending the TGT and a package containing a new session key encrypted under the long-term
;	 key of the client.

; 3) The client then sends the TGT along with an authenticator (encrypted message containing its name and a time-stamp
;	 encrypted under the session key received from the AS) to the TGS (Ticket-Granting Server).

; 4) Having received the TGT and authenticator, the TGS then decrypts the TGT to get the session key. It then 
;	 decrypts the authenticator to compare the contents of the TGT and the authenticator. If this is verified, it
;	 generates an encrypted session key package (same as above in step 2, which includes the name of service, time-stamp,
;	 lifetime stamp, and the session key all encrypted under the session key it used in previous step) and a ST 
;	 (Service Ticket) to send to the client.

; 5) The client then receives this package and decrypts the session key package to get the new session key to use 
;	 with the service. It then sends an Application Request (AP_REQ), which contains the service ticket it received
;	 in the previous step and an authenticator that the client generates, to the service. This authenticator is encrypted with
;	 the session key it extracted and contains its name and a time-stamp. This request is used as its credentials
;	 to access the service.


(defprotocol kerberos basic
  (defrole client
    (vars (cli tgs as serv name) (time t-prime life life-prime tgt st ack nonce text) (tgsk servk skey))
    (trace
	  ; Send the Initial Request (AS_REQ), which involves the client name, service name, and a lifetime stamp
	  (send (cat cli serv nonce life))
	  ; Get back response from the AS (AS_REP), which gives the TGT and an encrypted message with a new session key, 
	  ; client name, time-stamp, and lifetime stamp.
      (recv (cat (enc time nonce life tgsk tgs (ltk cli as)) tgt))
      (send (cat (enc cli time tgsk) tgt))
      (recv (cat (enc serv t-prime life-prime servk tgsk) st))
	  (send (cat (enc cli t-prime servk) st))
	  (recv (enc ack servk))) (uniq-orig nonce))
	  
  (defrole tgs
    (vars (cli tgs as serv name) (time t-prime life life-prime tgt st text) (tgsk servk skey))
    (trace 
	  (recv (cat (enc cli time tgsk) (enc time life tgsk cli (ltk tgs as))))
      (send (cat (enc serv t-prime life-prime servk tgsk) (enc cli serv t-prime life-prime servk (ltk serv as))))) (uniq-orig servk))
	  
  (defrole as
    (vars (cli tgs as serv name) (time life nonce text) (tgsk skey))
    (trace 
	  (recv (cat cli serv nonce life))
      (send
        (cat (enc time life nonce tgsk tgs (ltk cli as)) (enc time life tgsk cli (ltk tgs as))))) (uniq-orig tgsk))
		
  (defrole serv
	(vars (cli as serv name) (t-prime life-prime ack text) (servk skey))
	(trace
	  (recv (cat (enc cli t-prime servk) (enc cli serv t-prime life-prime servk (ltk serv as))))
	  (send (enc ack servk)))))

; we'll observe two shapes resulting from these preskeletons
; because the keyserver can reverse a and b with no change
; in the protocol
(defskeleton kerberos
  (vars (cli name) (tgs name) (as name) (serv name))
  (defstrand client 6 (cli cli) (tgs tgs) (as as) (serv serv))
  (non-orig (ltk cli as) (ltk tgs as) (ltk serv as)))

(defskeleton kerberos
  (vars (cli name) (tgs name) (as name) (serv name))
  (defstrand tgs 2 (cli cli) (tgs tgs) (as as) (serv serv))
  (non-orig (ltk cli as) (ltk tgs as) (ltk serv as)))

(defskeleton kerberos
  (vars (cli name) (tgs name) (as name) (serv name))
  (defstrand as 2 (cli cli) (tgs tgs) (as as) (serv serv))
  (non-orig (ltk cli as) (ltk tgs as) (ltk serv as)))
  
(defskeleton kerberos
  (vars (cli name) (serv name) (as name))
  (defstrand serv 2 (cli cli) (serv serv) (as as))
  (non-orig (ltk cli as) (ltk serv as)))
