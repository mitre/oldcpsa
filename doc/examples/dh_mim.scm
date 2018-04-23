; Unauthenticated Diffie-Hellman key exchange

; In this example, the initiator and responder roles participate in a
; Diffie-Hellman exchange, and then a nonce is transmitted by the
; initiator and received by the responder.  

; A man-in-the-middle attack is possible, because there is no 
; attempt made to authenticate either of the parties.  Therefore, the
; adversary may emulate the responder to the initator and vice versa.

(herald "Diffie-Hellman protocol, man-in-the-middle attack" (algebra diffie-hellman)
)

(defprotocol dh_mim diffie-hellman
  (defrole init
    (vars (x rndx) (h base) (n text))
    (trace
     (send (exp (gen) x))
     (recv h)
     (send (enc n (exp h x))))
    (uniq-gen x))
  (defrole resp
    (vars (y rndx) (h base) (n text))
    (trace
     (recv h)
     (send (exp (gen) y))
     (recv (enc n (exp h y))))
    (uniq-gen y))
  (comment "Diffie-hellman key exchange followed by an encryption"))


(defskeleton dh_mim
  (vars (n text) (hx hy base) (x y rndx))
  (defstrand init 3 (n n) (h hy) (x x))
  (defstrand resp 3 (n n) (h hx) (y y))
  (precedes ((0 2) (1 2)))
  (uniq-orig n)
  (pen-non-orig x y)
  (comment "Agreement on the encrypted text only"))

