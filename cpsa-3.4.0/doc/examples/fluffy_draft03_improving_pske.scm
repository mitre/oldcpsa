(herald "Fluffy: Simplified Key Exchange for Constrained Environments draft-hardjono-ace-fluffy-03"
 (comment "This version does not include timestamps or optional fields"))

;; The protocol aims to provide mutual authentication in addition to key establishment.
;; This model does not include timestamps or optional fields.
(defprotocol fluffy basic
 (defrole client 
  (vars (a b s name) (n1 n2 text) (k skey) (m mesg))
  (trace 
   (send (cat "req" a s b (enc (hash (cat "req" a s b)) a n1 (ltk s a))))
   (recv (cat "resp" a s b m (enc s b n1 k (ltk s a))))
   (send (cat "est" a s b m (enc (hash (cat "est" a s b m)) a n2 k)))
   (recv (enc "ack" a b n2 k))))
 (defrole skdc
  (vars (a b s name) (n1 text) (k skey))
  (trace
   (recv (cat "req" a s b (enc (hash (cat "req" a s b)) a n1 (ltk s a))))
   (send (cat "resp" a s b (enc a k (ltk s b)) (enc s b n1 k (ltk s a)))))
  (uniq-gen k))
 (defrole sp
  (vars (a b s name) (n2 text) (k skey))
  (trace
   (recv (cat "est" a s b (enc a k (ltk s b)) (enc (hash (cat "est" a s b (enc a k (ltk s b)))) a n2 k)))
   (send (enc "ack" a b n2 k)))))

;; Client's point-of-view
(defskeleton fluffy
 (vars (a b s name) (n1 n2 text) (k skey) (m mesg))
 (defstrand client 4 (a a) (b b) (s s) (n1 n1) (n2 n2) (k k) (m m))
 (non-orig (ltk s b) (ltk s a))
 (uniq-orig n1 n2)
 (comment "Client's point-of-view"))

;; Simple key distribution center principal's (SKDC) point-of-view
(defskeleton fluffy
 (vars (a b s name) (n1 text) (k skey))
 (defstrand skdc 2 (a a) (b b) (s s) (n1 n1) (k k))
 (non-orig (ltk s b) (ltk s a))
 (comment "Simple Key Distribution Center's point-of-view"))

;; Service principal or SP's point-of-view
(defskeleton fluffy
 (vars (a b s name) (n2 text) (k skey))
 (defstrand sp 2 (a a) (b b) (s s) (n2 n2) (k k))
 (non-orig (ltk s b) (ltk s a))
 (comment "Service Principal's point-of-view"))

