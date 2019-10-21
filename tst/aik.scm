(herald "Anonymous identity protocol from TCG")

(defprotocol aikprot basic
  (defrole ca
    (vars (mf name) (ek akey))
    (trace (send (enc "ekc" mf ek (privk mf))))
    (non-orig (invk ek)))
  (defrole tpm
    (vars (i x mf pc name) (ek k akey) (srk skey))
    (trace (recv (cat x i (enc "ekc" mf ek (privk mf))))
      (send (cat i k x (enc "ekc" mf ek (privk mf))))
      (recv (enc (enc "aic" i k x (privk pc)) ek))
      (send (cat (enc "aic" i k x (privk pc)) (enc k (invk k) srk))))
    (non-orig srk (invk ek))
    (uniq-orig k (invk k)))
  (defrole pca
    (vars (i x mf pc name) (ek k akey))
    (trace (recv (cat i k x (enc "ekc" mf ek (privk mf))))
      (send (enc (enc "aic" i k x (privk pc)) ek)))
    (non-orig (privk mf))))

(defskeleton aikprot
  (vars (i x mf pc name) (ek k akey) (srk skey))
  (defstrand tpm 4 (mf mf) (pc pc))
  (non-orig (privk pc)))

(defskeleton aikprot
  (vars (i x mf pc name) (ek k akey) (srk skey))
  (deflistener (enc "aic" i k x (privk pc)))
  (non-orig (privk pc)))
