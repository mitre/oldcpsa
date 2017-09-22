(herald "GSKE, part of Fluffy: Simplified Key Exchange for Constrained Environments draft-hardjono-ace-fluffy-03"
 (comment "Based on the Internet-Draft: https://www.ietf.org/archive/id/draft-hardjono-ace-fluffy-03.txt"))

;; This model does not include timestamps or optional fields.
(defprotocol fluffy basic
 (defrole sp 
  (vars (b s name) (nb g text) (gk skey))
  (trace 
   (send (cat "req" s g (enc b nb (ltk s b))))
   (recv (cat "resp" b (enc s g nb gk (ltk s b))))))
 (defrole keyserv
  (vars (a b s name) (nb na g text) (gk skey))
  (trace
   (recv (cat "req" s g (enc b nb (ltk s b))))
   (send (cat "resp" b (enc s g nb gk (ltk s b))))
   (recv (cat "fetch" s g (enc a na (ltk s a))))
   (send (cat "deliver" a (enc s g na gk (ltk s a)))))
  (uniq-gen gk))
 (defrole client
  (vars (a b s name) (na g text) (gk skey))
  (trace
   (send (cat "fetch" s g (enc a na (ltk s a))))
   (recv (cat "deliver" a (enc s g na gk (ltk s a)))))))

;; Service principal's (sp) point-of-view
(defskeleton fluffy
 (vars (b s name) (nb text) (gk skey))
 (defstrand sp 2 (b b) (s s) (nb nb) (gk gk))
 (non-orig (ltk s b))
 (uniq-orig nb)
 (comment "Service Principal's point-of-view"))

;; Simple key distribution center's (skdc) point-of-view
(defskeleton fluffy
 (vars (a b s name) (nb na text) (gk skey))
 (defstrand keyserv 4 (a a) (b b) (s s) (nb nb) (na na) (gk gk))
 (non-orig (ltk s b) (ltk s a))
 (uniq-gen gk)
 (comment "SKDC's point-of-view"))

;; Client's point-of-view
(defskeleton fluffy
 (vars (a s name) (na text) (gk skey))
 (defstrand client 2 (a a) (s s) (na na) (gk gk))
 (non-orig (ltk s a))
 (uniq-orig na)
 (comment "Clients's point-of-view"))



