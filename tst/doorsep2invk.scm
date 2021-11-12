(herald doorsep2invk (comment "Door Simple Example Protocol"))

(defprotocol doorsep basic
  (defrole init
    (vars (self peer akey) (skey skey) (data text))
    (trace (send (enc (enc skey (invk self)) peer))
      (recv (enc data skey)) (send data))
    (uniq-orig skey))
  (defrole resp
    (vars (self peer akey) (skey skey) (data text))
    (trace (recv (enc (enc skey (invk peer)) self))
      (send (enc data skey)) (recv data))
    (uniq-orig data))
  (comment "Doorsep's protocol using unnamed asymmetric keys"))

(defgoal doorsep
  (forall
    ((text+0 text) (skey+0 skey) (akey+1 akey+0 akey+3 akey)
     (z z-0 strd))
    (implies
      (and (p "resp" z 3) (p "init" z-0 1) (p "resp" "data" z text+0)
        (p "resp" "skey" z skey+0) (p "resp" "self" z akey+1)
        (p "resp" "peer" z (invk akey+3)) (p "init" "skey" z-0 skey+0)
        (p "init" "self" z-0 (invk akey+3)) (p "init" "peer" z-0 akey+0)
        (prec z-0 0 z 0) (non akey+3) (non (invk akey+0))
        (uniq-at skey+0 z-0 0) (uniq-at text+0 z 1))
      (and))))
