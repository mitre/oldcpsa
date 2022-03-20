(herald thinning (algebra diffie-hellman))

(comment "CPSA 3.6.10")
(comment "All input read from tst/thinning.scm")

(defprotocol provision diffie-hellman
  (defrole supplicant
    (vars (self pal admin name) (meas text) (count data))
    (trace
      (send (enc "endpoint" self pal meas (pubk self) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk self pal))
            (privk admin)) (bltk self pal) (pubk self))))
    (uniq-orig meas))
  (defrole admission
    (vars (self pal admin counter name) (meas-self text)
      (token count data))
    (trace
      (recv
        (enc "endpoint" self pal meas-self (pubk self) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count meas-self (hash (bltk self pal))
            (privk admin)) (bltk self pal) (pubk self))))
    (non-orig (privk counter))
    (uniq-orig token))
  (defrole counter
    (vars (t n data) (k akey))
    (trace (recv (enc "incr" t k)) (send (enc "count" n t (invk k))))
    (uniq-orig n))
  (defrule one-counter
    (forall ((z w strd) (counter1 counter2 name))
      (implies
        (and (p "admission" z 2) (p "admission" "counter" z counter1)
          (p "admission" w 2) (p "admission" "counter" w counter2))
        (= counter1 counter2)))))

(defskeleton provision
  (vars (meas meas-0 text) (count count-0 data) (left right admin name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (non-orig (privk left) (privk right) (privk admin))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0)
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right)))))
  (label 0)
  (unrealized (0 1) (1 1))
  (origs (meas-0 (1 0)) (meas (0 0)))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton provision
  (vars (meas text) (count data) (left admin name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal left) (admin admin))
  (non-orig (privk left) (privk admin))
  (pen-non-orig (bltk left left))
  (uniq-orig meas)
  (operation collapsed 1 0)
  (traces
    ((send (enc "endpoint" left left meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left left))
            (privk admin)) (bltk left left) (pubk left)))))
  (label 1)
  (parent 0)
  (unrealized (0 1))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton provision
  (vars (meas meas-0 text) (count count-0 token data)
    (left right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self left) (pal right) (admin admin)
    (counter counter))
  (precedes ((1 0) (2 0)) ((2 3) (1 1)))
  (non-orig (privk left) (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0 token)
  (operation encryption-test (added-strand admission 4)
    (enc "admitted" count-0 meas-0 (hash (bltk left right))
      (privk admin)) (1 1))
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" left right meas-0 (pubk left) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left)))))
  (label 2)
  (parent 0)
  (seen 6)
  (unrealized (0 1) (1 1) (2 0) (2 2))
  (comment "3 in cohort - 2 not yet seen"))

(defskeleton provision
  (vars (meas meas-0 text) (count count-0 token data)
    (left right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self right) (pal left) (admin admin)
    (counter counter))
  (precedes ((1 0) (2 0)) ((2 3) (1 1)))
  (non-orig (privk left) (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0 token)
  (operation encryption-test (added-strand admission 4)
    (enc "admitted" count-0 meas-0 (hash (bltk left right))
      (privk admin)) (1 1))
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right)))))
  (label 3)
  (parent 0)
  (unrealized (0 1) (2 2))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton provision
  (vars (meas text) (count token data) (left admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas) (token token) (count count)
    (self left) (pal left) (admin admin) (counter counter))
  (precedes ((0 0) (1 0)) ((1 3) (0 1)))
  (non-orig (privk left) (privk admin) (privk counter))
  (pen-non-orig (bltk left left))
  (uniq-orig meas token)
  (operation encryption-test (added-strand admission 4)
    (enc "admitted" count meas (hash (bltk left left)) (privk admin))
    (0 1))
  (traces
    ((send (enc "endpoint" left left meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left left))
            (privk admin)) (bltk left left) (pubk left))))
    ((recv (enc "endpoint" left left meas (pubk left) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left left))
            (privk admin)) (bltk left left) (pubk left)))))
  (label 4)
  (parent 1)
  (unrealized (1 2))
  (origs (token (1 1)) (meas (0 0)))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton provision
  (vars (meas meas-0 text) (count count-0 token data)
    (right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self right)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal right) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self right) (pal right) (admin admin)
    (counter counter))
  (precedes ((1 0) (2 0)) ((2 3) (1 1)))
  (non-orig (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk right right))
  (uniq-orig meas meas-0 token)
  (operation nonce-test (contracted (left right)) meas-0 (2 0)
    (enc "endpoint" right right meas-0 (pubk right) (pubk admin)))
  (traces
    ((send (enc "endpoint" right right meas (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk right right))
            (privk admin)) (bltk right right) (pubk right))))
    ((send
       (enc "endpoint" right right meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk right right))
            (privk admin)) (bltk right right) (pubk right))))
    ((recv
       (enc "endpoint" right right meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk right right))
            (privk admin)) (bltk right right) (pubk right)))))
  (label 5)
  (parent 2)
  (unrealized (0 1) (2 2))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton provision
  (vars (meas meas-0 text) (count count-0 token token-0 count-1 data)
    (left right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self left) (pal right) (admin admin)
    (counter counter))
  (defstrand admission 4 (meas-self meas-0) (token token-0)
    (count count-1) (self right) (pal left) (admin admin)
    (counter counter))
  (precedes ((1 0) (3 0)) ((2 3) (1 1)) ((3 3) (2 0)))
  (non-orig (privk left) (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0 token token-0)
  (rule one-counter)
  (operation nonce-test (added-strand admission 4) meas-0 (2 0)
    (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" left right meas-0 (pubk left) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((recv (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token-0 (pubk counter)))
      (recv (enc "count" count-1 token-0 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-1 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right)))))
  (label 6)
  (parent 2)
  (unrealized (0 1) (1 1) (2 0) (2 2) (3 2))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton provision
  (vars (meas meas-0 text) (count count-0 token data)
    (left right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self right) (pal left) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token) (n count-0) (k (pubk counter)))
  (precedes ((1 0) (2 0)) ((2 1) (3 0)) ((2 3) (1 1)) ((3 1) (2 2)))
  (non-orig (privk left) (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0 count-0 token)
  (operation encryption-test (added-strand counter 2)
    (enc "count" count-0 token (privk counter)) (2 2))
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "incr" token (pubk counter)))
      (send (enc "count" count-0 token (privk counter)))))
  (label 7)
  (parent 3)
  (seen 11 12)
  (unrealized (0 1))
  (comment "4 in cohort - 2 not yet seen"))

(defskeleton provision
  (vars (meas text) (count token data) (left admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas) (token token) (count count)
    (self left) (pal left) (admin admin) (counter counter))
  (defstrand counter 2 (t token) (n count) (k (pubk counter)))
  (precedes ((0 0) (1 0)) ((1 1) (2 0)) ((1 3) (0 1)) ((2 1) (1 2)))
  (non-orig (privk left) (privk admin) (privk counter))
  (pen-non-orig (bltk left left))
  (uniq-orig meas count token)
  (operation encryption-test (added-strand counter 2)
    (enc "count" count token (privk counter)) (1 2))
  (traces
    ((send (enc "endpoint" left left meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left left))
            (privk admin)) (bltk left left) (pubk left))))
    ((recv (enc "endpoint" left left meas (pubk left) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left left))
            (privk admin)) (bltk left left) (pubk left))))
    ((recv (enc "incr" token (pubk counter)))
      (send (enc "count" count token (privk counter)))))
  (label 8)
  (parent 4)
  (unrealized)
  (shape)
  (maps
    ((0 0)
      ((left left) (right left) (admin admin) (meas meas) (count count)
        (meas-0 meas) (count-0 count))))
  (origs (count (2 1)) (token (1 1)) (meas (0 0))))

(defskeleton provision
  (vars (meas meas-0 text) (count count-0 token data)
    (right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self right)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal right) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self right) (pal right) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token) (n count-0) (k (pubk counter)))
  (precedes ((1 0) (2 0)) ((2 1) (3 0)) ((2 3) (1 1)) ((3 1) (2 2)))
  (non-orig (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk right right))
  (uniq-orig meas meas-0 count-0 token)
  (operation encryption-test (added-strand counter 2)
    (enc "count" count-0 token (privk counter)) (2 2))
  (traces
    ((send (enc "endpoint" right right meas (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk right right))
            (privk admin)) (bltk right right) (pubk right))))
    ((send
       (enc "endpoint" right right meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk right right))
            (privk admin)) (bltk right right) (pubk right))))
    ((recv
       (enc "endpoint" right right meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk right right))
            (privk admin)) (bltk right right) (pubk right))))
    ((recv (enc "incr" token (pubk counter)))
      (send (enc "count" count-0 token (privk counter)))))
  (label 9)
  (parent 5)
  (seen 13)
  (unrealized (0 1))
  (comment "2 in cohort - 1 not yet seen"))

(defskeleton provision
  (vars (meas meas-0 text) (count count-0 token token-0 count-1 data)
    (left right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self left) (pal right) (admin admin)
    (counter counter))
  (defstrand admission 4 (meas-self meas-0) (token token-0)
    (count count-1) (self right) (pal left) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token-0) (n count-1) (k (pubk counter)))
  (precedes ((1 0) (3 0)) ((2 3) (1 1)) ((3 1) (4 0)) ((3 3) (2 0))
    ((4 1) (3 2)))
  (non-orig (privk left) (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0 token token-0 count-1)
  (operation encryption-test (added-strand counter 2)
    (enc "count" count-1 token-0 (privk counter)) (3 2))
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" left right meas-0 (pubk left) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((recv (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token-0 (pubk counter)))
      (recv (enc "count" count-1 token-0 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-1 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "incr" token-0 (pubk counter)))
      (send (enc "count" count-1 token-0 (privk counter)))))
  (label 10)
  (parent 6)
  (seen 15)
  (unrealized (0 1) (1 1) (2 0) (2 2))
  (comment "5 in cohort - 2 not yet seen"))

(defskeleton provision
  (vars (meas meas-0 text) (count count-0 token token-0 data)
    (left right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self right) (pal left) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token) (n count-0) (k (pubk counter)))
  (defstrand admission 4 (meas-self meas) (token token-0) (count count)
    (self right) (pal left) (admin admin) (counter counter))
  (precedes ((0 0) (4 0)) ((1 0) (2 0)) ((2 1) (3 0)) ((2 3) (1 1))
    ((3 1) (2 2)) ((4 3) (0 1)))
  (non-orig (privk left) (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0 count-0 token token-0)
  (rule one-counter)
  (operation encryption-test (added-strand admission 4)
    (enc "admitted" count meas (hash (bltk left right)) (privk admin))
    (0 1))
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "incr" token (pubk counter)))
      (send (enc "count" count-0 token (privk counter))))
    ((recv (enc "endpoint" right left meas (pubk right) (pubk admin)))
      (send (enc "incr" token-0 (pubk counter)))
      (recv (enc "count" count token-0 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right)))))
  (label 11)
  (parent 7)
  (seen 13 16)
  (unrealized (0 1) (4 0) (4 2))
  (comment "5 in cohort - 1 not yet seen"))

(defskeleton provision
  (vars (meas meas-0 text) (count count-0 token token-0 data)
    (left right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self right) (pal left) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token) (n count-0) (k (pubk counter)))
  (defstrand admission 4 (meas-self meas) (token token-0) (count count)
    (self left) (pal right) (admin admin) (counter counter))
  (precedes ((0 0) (4 0)) ((1 0) (2 0)) ((2 1) (3 0)) ((2 3) (1 1))
    ((3 1) (2 2)) ((4 3) (0 1)))
  (non-orig (privk left) (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0 count-0 token token-0)
  (rule one-counter)
  (operation encryption-test (added-strand admission 4)
    (enc "admitted" count meas (hash (bltk left right)) (privk admin))
    (0 1))
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "incr" token (pubk counter)))
      (send (enc "count" count-0 token (privk counter))))
    ((recv (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (send (enc "incr" token-0 (pubk counter)))
      (recv (enc "count" count token-0 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left)))))
  (label 12)
  (parent 7)
  (unrealized (4 2))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton provision
  (vars (meas meas-0 text) (count count-0 token token-0 data)
    (right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self right)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal right) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self right) (pal right) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token) (n count-0) (k (pubk counter)))
  (defstrand admission 4 (meas-self meas) (token token-0) (count count)
    (self right) (pal right) (admin admin) (counter counter))
  (precedes ((0 0) (4 0)) ((1 0) (2 0)) ((2 1) (3 0)) ((2 3) (1 1))
    ((3 1) (2 2)) ((4 3) (0 1)))
  (non-orig (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk right right))
  (uniq-orig meas meas-0 count-0 token token-0)
  (rule one-counter)
  (operation encryption-test (added-strand admission 4)
    (enc "admitted" count meas (hash (bltk right right)) (privk admin))
    (0 1))
  (traces
    ((send (enc "endpoint" right right meas (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk right right))
            (privk admin)) (bltk right right) (pubk right))))
    ((send
       (enc "endpoint" right right meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk right right))
            (privk admin)) (bltk right right) (pubk right))))
    ((recv
       (enc "endpoint" right right meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk right right))
            (privk admin)) (bltk right right) (pubk right))))
    ((recv (enc "incr" token (pubk counter)))
      (send (enc "count" count-0 token (privk counter))))
    ((recv (enc "endpoint" right right meas (pubk right) (pubk admin)))
      (send (enc "incr" token-0 (pubk counter)))
      (recv (enc "count" count token-0 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count meas (hash (bltk right right))
            (privk admin)) (bltk right right) (pubk right)))))
  (label 13)
  (parent 9)
  (unrealized (4 2))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton provision
  (vars (meas meas-0 text) (count count-0 token token-0 count-1 data)
    (left admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal left) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self left)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self left) (pal left) (admin admin)
    (counter counter))
  (defstrand admission 4 (meas-self meas-0) (token token-0)
    (count count-1) (self left) (pal left) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token-0) (n count-1) (k (pubk counter)))
  (precedes ((1 0) (3 0)) ((2 3) (1 1)) ((3 1) (4 0)) ((3 3) (2 0))
    ((4 1) (3 2)))
  (non-orig (privk left) (privk admin) (privk counter))
  (pen-non-orig (bltk left left))
  (uniq-orig meas meas-0 token token-0 count-1)
  (operation nonce-test (displaced 5 3 admission 4) meas-0 (2 0)
    (enc "admit"
      (enc "admitted" count-1 meas-0 (hash (bltk left right))
        (privk admin)) (bltk left right) (pubk right))
    (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
  (traces
    ((send (enc "endpoint" left left meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left left))
            (privk admin)) (bltk left left) (pubk left))))
    ((send (enc "endpoint" left left meas-0 (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left left))
            (privk admin)) (bltk left left) (pubk left))))
    ((recv (enc "endpoint" left left meas-0 (pubk left) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left left))
            (privk admin)) (bltk left left) (pubk left))))
    ((recv (enc "endpoint" left left meas-0 (pubk left) (pubk admin)))
      (send (enc "incr" token-0 (pubk counter)))
      (recv (enc "count" count-1 token-0 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-1 meas-0 (hash (bltk left left))
            (privk admin)) (bltk left left) (pubk left))))
    ((recv (enc "incr" token-0 (pubk counter)))
      (send (enc "count" count-1 token-0 (privk counter)))))
  (label 14)
  (parent 10)
  (unrealized (0 1) (2 2))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton provision
  (vars (meas meas-0 text)
    (count count-0 token token-0 count-1 token-1 count-2 data)
    (left right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self left) (pal right) (admin admin)
    (counter counter))
  (defstrand admission 4 (meas-self meas-0) (token token-0)
    (count count-1) (self right) (pal left) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token-0) (n count-1) (k (pubk counter)))
  (defstrand admission 4 (meas-self meas-0) (token token-1)
    (count count-2) (self right) (pal left) (admin admin)
    (counter counter))
  (precedes ((1 0) (3 0)) ((1 0) (5 0)) ((2 3) (1 1)) ((3 1) (4 0))
    ((3 3) (2 0)) ((4 1) (3 2)) ((5 3) (2 0)))
  (non-orig (privk left) (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0 token token-0 count-1 token-1)
  (rule one-counter)
  (operation nonce-test (added-strand admission 4) meas-0 (2 0)
    (enc "admit"
      (enc "admitted" count-1 meas-0 (hash (bltk left right))
        (privk admin)) (bltk left right) (pubk right))
    (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" left right meas-0 (pubk left) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((recv (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token-0 (pubk counter)))
      (recv (enc "count" count-1 token-0 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-1 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "incr" token-0 (pubk counter)))
      (send (enc "count" count-1 token-0 (privk counter))))
    ((recv (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token-1 (pubk counter)))
      (recv (enc "count" count-2 token-1 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-2 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right)))))
  (label 15)
  (parent 10)
  (unrealized (0 1) (1 1) (2 0) (2 2) (5 2))
  (dead)
  (comment "empty cohort"))

(defskeleton provision
  (vars (meas meas-0 text)
    (count count-0 token token-0 token-1 count-1 data)
    (left right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self right) (pal left) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token) (n count-0) (k (pubk counter)))
  (defstrand admission 4 (meas-self meas) (token token-0) (count count)
    (self right) (pal left) (admin admin) (counter counter))
  (defstrand admission 4 (meas-self meas) (token token-1)
    (count count-1) (self left) (pal right) (admin admin)
    (counter counter))
  (precedes ((0 0) (5 0)) ((1 0) (2 0)) ((2 1) (3 0)) ((2 3) (1 1))
    ((3 1) (2 2)) ((4 3) (0 1)) ((5 3) (4 0)))
  (non-orig (privk left) (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0 count-0 token token-0 token-1)
  (rule one-counter)
  (operation nonce-test (added-strand admission 4) meas (4 0)
    (enc "endpoint" left right meas (pubk left) (pubk admin)))
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "incr" token (pubk counter)))
      (send (enc "count" count-0 token (privk counter))))
    ((recv (enc "endpoint" right left meas (pubk right) (pubk admin)))
      (send (enc "incr" token-0 (pubk counter)))
      (recv (enc "count" count token-0 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (send (enc "incr" token-1 (pubk counter)))
      (recv (enc "count" count-1 token-1 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-1 meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left)))))
  (label 16)
  (parent 11)
  (unrealized (0 1) (4 0) (4 2) (5 2))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton provision
  (vars (meas meas-0 text) (count count-0 token token-0 data)
    (left right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self right) (pal left) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token) (n count-0) (k (pubk counter)))
  (defstrand admission 4 (meas-self meas) (token token-0) (count count)
    (self left) (pal right) (admin admin) (counter counter))
  (defstrand counter 2 (t token-0) (n count) (k (pubk counter)))
  (precedes ((0 0) (4 0)) ((1 0) (2 0)) ((2 1) (3 0)) ((2 3) (1 1))
    ((3 1) (2 2)) ((4 1) (5 0)) ((4 3) (0 1)) ((5 1) (4 2)))
  (non-orig (privk left) (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0 count count-0 token token-0)
  (operation encryption-test (added-strand counter 2)
    (enc "count" count token-0 (privk counter)) (4 2))
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "incr" token (pubk counter)))
      (send (enc "count" count-0 token (privk counter))))
    ((recv (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (send (enc "incr" token-0 (pubk counter)))
      (recv (enc "count" count token-0 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((recv (enc "incr" token-0 (pubk counter)))
      (send (enc "count" count token-0 (privk counter)))))
  (label 17)
  (parent 12)
  (unrealized)
  (shape)
  (maps
    ((0 1)
      ((left left) (right right) (admin admin) (meas meas) (count count)
        (meas-0 meas-0) (count-0 count-0))))
  (origs (count (5 1)) (token-0 (4 1)) (count-0 (3 1)) (token (2 1))
    (meas-0 (1 0)) (meas (0 0))))

(defskeleton provision
  (vars (meas meas-0 text) (count count-0 token token-0 data)
    (right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self right)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal right) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self right) (pal right) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token) (n count-0) (k (pubk counter)))
  (defstrand admission 4 (meas-self meas) (token token-0) (count count)
    (self right) (pal right) (admin admin) (counter counter))
  (defstrand counter 2 (t token-0) (n count) (k (pubk counter)))
  (precedes ((0 0) (4 0)) ((1 0) (2 0)) ((2 1) (3 0)) ((2 3) (1 1))
    ((3 1) (2 2)) ((4 1) (5 0)) ((4 3) (0 1)) ((5 1) (4 2)))
  (non-orig (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk right right))
  (uniq-orig meas meas-0 count count-0 token token-0)
  (operation encryption-test (added-strand counter 2)
    (enc "count" count token-0 (privk counter)) (4 2))
  (traces
    ((send (enc "endpoint" right right meas (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk right right))
            (privk admin)) (bltk right right) (pubk right))))
    ((send
       (enc "endpoint" right right meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk right right))
            (privk admin)) (bltk right right) (pubk right))))
    ((recv
       (enc "endpoint" right right meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk right right))
            (privk admin)) (bltk right right) (pubk right))))
    ((recv (enc "incr" token (pubk counter)))
      (send (enc "count" count-0 token (privk counter))))
    ((recv (enc "endpoint" right right meas (pubk right) (pubk admin)))
      (send (enc "incr" token-0 (pubk counter)))
      (recv (enc "count" count token-0 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count meas (hash (bltk right right))
            (privk admin)) (bltk right right) (pubk right))))
    ((recv (enc "incr" token-0 (pubk counter)))
      (send (enc "count" count token-0 (privk counter)))))
  (label 18)
  (parent 13)
  (seen 17)
  (unrealized)
  (comment "1 in cohort - 0 not yet seen"))

(defskeleton provision
  (vars (meas meas-0 text) (count count-0 token token-0 count-1 data)
    (left admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal left) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self left)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self left) (pal left) (admin admin)
    (counter counter))
  (defstrand admission 4 (meas-self meas-0) (token token-0)
    (count count-1) (self left) (pal left) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token-0) (n count-1) (k (pubk counter)))
  (defstrand counter 2 (t token) (n count-0) (k (pubk counter)))
  (precedes ((1 0) (3 0)) ((2 1) (5 0)) ((2 3) (1 1)) ((3 1) (4 0))
    ((3 3) (2 0)) ((4 1) (3 2)) ((5 1) (2 2)))
  (non-orig (privk left) (privk admin) (privk counter))
  (pen-non-orig (bltk left left))
  (uniq-orig meas meas-0 count-0 token token-0 count-1)
  (operation encryption-test (added-strand counter 2)
    (enc "count" count-0 token (privk counter)) (2 2))
  (traces
    ((send (enc "endpoint" left left meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left left))
            (privk admin)) (bltk left left) (pubk left))))
    ((send (enc "endpoint" left left meas-0 (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left left))
            (privk admin)) (bltk left left) (pubk left))))
    ((recv (enc "endpoint" left left meas-0 (pubk left) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left left))
            (privk admin)) (bltk left left) (pubk left))))
    ((recv (enc "endpoint" left left meas-0 (pubk left) (pubk admin)))
      (send (enc "incr" token-0 (pubk counter)))
      (recv (enc "count" count-1 token-0 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-1 meas-0 (hash (bltk left left))
            (privk admin)) (bltk left left) (pubk left))))
    ((recv (enc "incr" token-0 (pubk counter)))
      (send (enc "count" count-1 token-0 (privk counter))))
    ((recv (enc "incr" token (pubk counter)))
      (send (enc "count" count-0 token (privk counter)))))
  (label 19)
  (parent 14)
  (seen 21)
  (unrealized (0 1))
  (comment "4 in cohort - 1 not yet seen"))

(defskeleton provision
  (vars (meas meas-0 text)
    (count count-0 token token-0 token-1 count-1 data)
    (left right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self right) (pal left) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token) (n count-0) (k (pubk counter)))
  (defstrand admission 4 (meas-self meas) (token token-0) (count count)
    (self right) (pal left) (admin admin) (counter counter))
  (defstrand admission 4 (meas-self meas) (token token-1)
    (count count-1) (self left) (pal right) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token-1) (n count-1) (k (pubk counter)))
  (precedes ((0 0) (5 0)) ((1 0) (2 0)) ((2 1) (3 0)) ((2 3) (1 1))
    ((3 1) (2 2)) ((4 3) (0 1)) ((5 1) (6 0)) ((5 3) (4 0))
    ((6 1) (5 2)))
  (non-orig (privk left) (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0 count-0 token token-0 token-1 count-1)
  (operation encryption-test (added-strand counter 2)
    (enc "count" count-1 token-1 (privk counter)) (5 2))
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "incr" token (pubk counter)))
      (send (enc "count" count-0 token (privk counter))))
    ((recv (enc "endpoint" right left meas (pubk right) (pubk admin)))
      (send (enc "incr" token-0 (pubk counter)))
      (recv (enc "count" count token-0 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (send (enc "incr" token-1 (pubk counter)))
      (recv (enc "count" count-1 token-1 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-1 meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((recv (enc "incr" token-1 (pubk counter)))
      (send (enc "count" count-1 token-1 (privk counter)))))
  (label 20)
  (parent 16)
  (seen 23)
  (unrealized (0 1) (4 0) (4 2))
  (comment "7 in cohort - 2 not yet seen"))

(defskeleton provision
  (vars (meas meas-0 text)
    (count count-0 token token-0 count-1 token-1 data)
    (left admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal left) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self left)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self left) (pal left) (admin admin)
    (counter counter))
  (defstrand admission 4 (meas-self meas-0) (token token-0)
    (count count-1) (self left) (pal left) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token-0) (n count-1) (k (pubk counter)))
  (defstrand counter 2 (t token) (n count-0) (k (pubk counter)))
  (defstrand admission 4 (meas-self meas) (token token-1) (count count)
    (self left) (pal left) (admin admin) (counter counter))
  (precedes ((0 0) (6 0)) ((1 0) (3 0)) ((2 1) (5 0)) ((2 3) (1 1))
    ((3 1) (4 0)) ((3 3) (2 0)) ((4 1) (3 2)) ((5 1) (2 2))
    ((6 3) (0 1)))
  (non-orig (privk left) (privk admin) (privk counter))
  (pen-non-orig (bltk left left))
  (uniq-orig meas meas-0 count-0 token token-0 count-1 token-1)
  (rule one-counter)
  (operation encryption-test (added-strand admission 4)
    (enc "admitted" count meas (hash (bltk left left)) (privk admin))
    (0 1))
  (traces
    ((send (enc "endpoint" left left meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left left))
            (privk admin)) (bltk left left) (pubk left))))
    ((send (enc "endpoint" left left meas-0 (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left left))
            (privk admin)) (bltk left left) (pubk left))))
    ((recv (enc "endpoint" left left meas-0 (pubk left) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left left))
            (privk admin)) (bltk left left) (pubk left))))
    ((recv (enc "endpoint" left left meas-0 (pubk left) (pubk admin)))
      (send (enc "incr" token-0 (pubk counter)))
      (recv (enc "count" count-1 token-0 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-1 meas-0 (hash (bltk left left))
            (privk admin)) (bltk left left) (pubk left))))
    ((recv (enc "incr" token-0 (pubk counter)))
      (send (enc "count" count-1 token-0 (privk counter))))
    ((recv (enc "incr" token (pubk counter)))
      (send (enc "count" count-0 token (privk counter))))
    ((recv (enc "endpoint" left left meas (pubk left) (pubk admin)))
      (send (enc "incr" token-1 (pubk counter)))
      (recv (enc "count" count token-1 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left left))
            (privk admin)) (bltk left left) (pubk left)))))
  (label 21)
  (parent 19)
  (unrealized (6 2))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton provision
  (vars (meas meas-0 text)
    (count count-0 token token-0 token-1 count-1 data)
    (right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self right)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal right) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self right) (pal right) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token) (n count-0) (k (pubk counter)))
  (defstrand admission 4 (meas-self meas) (token token-0) (count count)
    (self right) (pal right) (admin admin) (counter counter))
  (defstrand admission 4 (meas-self meas) (token token-1)
    (count count-1) (self right) (pal right) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token-1) (n count-1) (k (pubk counter)))
  (precedes ((0 0) (5 0)) ((1 0) (2 0)) ((2 1) (3 0)) ((2 3) (1 1))
    ((3 1) (2 2)) ((4 3) (0 1)) ((5 1) (6 0)) ((5 3) (4 0))
    ((6 1) (5 2)))
  (non-orig (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk right right))
  (uniq-orig meas meas-0 count-0 token token-0 token-1 count-1)
  (operation nonce-test (displaced 7 5 admission 4) meas (4 0)
    (enc "admit"
      (enc "admitted" count-1 meas (hash (bltk left right))
        (privk admin)) (bltk left right) (pubk left))
    (enc "endpoint" left right meas (pubk left) (pubk admin)))
  (traces
    ((send (enc "endpoint" right right meas (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk right right))
            (privk admin)) (bltk right right) (pubk right))))
    ((send
       (enc "endpoint" right right meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk right right))
            (privk admin)) (bltk right right) (pubk right))))
    ((recv
       (enc "endpoint" right right meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk right right))
            (privk admin)) (bltk right right) (pubk right))))
    ((recv (enc "incr" token (pubk counter)))
      (send (enc "count" count-0 token (privk counter))))
    ((recv (enc "endpoint" right right meas (pubk right) (pubk admin)))
      (send (enc "incr" token-0 (pubk counter)))
      (recv (enc "count" count token-0 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count meas (hash (bltk right right))
            (privk admin)) (bltk right right) (pubk right))))
    ((recv (enc "endpoint" right right meas (pubk right) (pubk admin)))
      (send (enc "incr" token-1 (pubk counter)))
      (recv (enc "count" count-1 token-1 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-1 meas (hash (bltk right right))
            (privk admin)) (bltk right right) (pubk right))))
    ((recv (enc "incr" token-1 (pubk counter)))
      (send (enc "count" count-1 token-1 (privk counter)))))
  (label 22)
  (parent 20)
  (seen 24)
  (unrealized (4 2))
  (comment "1 in cohort - 0 not yet seen"))

(defskeleton provision
  (vars (meas meas-0 text)
    (count count-0 token token-0 token-1 count-1 token-2 count-2 data)
    (left right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self right) (pal left) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token) (n count-0) (k (pubk counter)))
  (defstrand admission 4 (meas-self meas) (token token-0) (count count)
    (self right) (pal left) (admin admin) (counter counter))
  (defstrand admission 4 (meas-self meas) (token token-1)
    (count count-1) (self left) (pal right) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token-1) (n count-1) (k (pubk counter)))
  (defstrand admission 4 (meas-self meas) (token token-2)
    (count count-2) (self left) (pal right) (admin admin)
    (counter counter))
  (precedes ((0 0) (5 0)) ((0 0) (7 0)) ((1 0) (2 0)) ((2 1) (3 0))
    ((2 3) (1 1)) ((3 1) (2 2)) ((4 3) (0 1)) ((5 1) (6 0))
    ((5 3) (4 0)) ((6 1) (5 2)) ((7 3) (4 0)))
  (non-orig (privk left) (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0 count-0 token token-0 token-1 count-1 token-2)
  (rule one-counter)
  (operation nonce-test (added-strand admission 4) meas (4 0)
    (enc "admit"
      (enc "admitted" count-1 meas (hash (bltk left right))
        (privk admin)) (bltk left right) (pubk left))
    (enc "endpoint" left right meas (pubk left) (pubk admin)))
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "incr" token (pubk counter)))
      (send (enc "count" count-0 token (privk counter))))
    ((recv (enc "endpoint" right left meas (pubk right) (pubk admin)))
      (send (enc "incr" token-0 (pubk counter)))
      (recv (enc "count" count token-0 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (send (enc "incr" token-1 (pubk counter)))
      (recv (enc "count" count-1 token-1 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-1 meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((recv (enc "incr" token-1 (pubk counter)))
      (send (enc "count" count-1 token-1 (privk counter))))
    ((recv (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (send (enc "incr" token-2 (pubk counter)))
      (recv (enc "count" count-2 token-2 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-2 meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left)))))
  (label 23)
  (parent 20)
  (unrealized (0 1) (4 0) (4 2) (7 2))
  (dead)
  (comment "empty cohort"))

(defskeleton provision
  (vars (meas meas-0 text)
    (count count-0 token token-0 count-1 token-1 data)
    (left admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal left) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self left)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self left) (pal left) (admin admin)
    (counter counter))
  (defstrand admission 4 (meas-self meas-0) (token token-0)
    (count count-1) (self left) (pal left) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token-0) (n count-1) (k (pubk counter)))
  (defstrand counter 2 (t token) (n count-0) (k (pubk counter)))
  (defstrand admission 4 (meas-self meas) (token token-1) (count count)
    (self left) (pal left) (admin admin) (counter counter))
  (defstrand counter 2 (t token-1) (n count) (k (pubk counter)))
  (precedes ((0 0) (6 0)) ((1 0) (3 0)) ((2 1) (5 0)) ((2 3) (1 1))
    ((3 1) (4 0)) ((3 3) (2 0)) ((4 1) (3 2)) ((5 1) (2 2))
    ((6 1) (7 0)) ((6 3) (0 1)) ((7 1) (6 2)))
  (non-orig (privk left) (privk admin) (privk counter))
  (pen-non-orig (bltk left left))
  (uniq-orig meas meas-0 count count-0 token token-0 count-1 token-1)
  (operation encryption-test (added-strand counter 2)
    (enc "count" count token-1 (privk counter)) (6 2))
  (traces
    ((send (enc "endpoint" left left meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left left))
            (privk admin)) (bltk left left) (pubk left))))
    ((send (enc "endpoint" left left meas-0 (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left left))
            (privk admin)) (bltk left left) (pubk left))))
    ((recv (enc "endpoint" left left meas-0 (pubk left) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left left))
            (privk admin)) (bltk left left) (pubk left))))
    ((recv (enc "endpoint" left left meas-0 (pubk left) (pubk admin)))
      (send (enc "incr" token-0 (pubk counter)))
      (recv (enc "count" count-1 token-0 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-1 meas-0 (hash (bltk left left))
            (privk admin)) (bltk left left) (pubk left))))
    ((recv (enc "incr" token-0 (pubk counter)))
      (send (enc "count" count-1 token-0 (privk counter))))
    ((recv (enc "incr" token (pubk counter)))
      (send (enc "count" count-0 token (privk counter))))
    ((recv (enc "endpoint" left left meas (pubk left) (pubk admin)))
      (send (enc "incr" token-1 (pubk counter)))
      (recv (enc "count" count token-1 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left left))
            (privk admin)) (bltk left left) (pubk left))))
    ((recv (enc "incr" token-1 (pubk counter)))
      (send (enc "count" count token-1 (privk counter)))))
  (label 24)
  (parent 21)
  (unrealized)
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton provision
  (vars (meas meas-0 text)
    (count count-0 token token-0 count-1 token-1 data)
    (left admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal left) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self left)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self left) (pal left) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token-0) (n count-1) (k (pubk counter)))
  (defstrand counter 2 (t token) (n count-0) (k (pubk counter)))
  (defstrand admission 4 (meas-self meas) (token token-1) (count count)
    (self left) (pal left) (admin admin) (counter counter))
  (defstrand counter 2 (t token-1) (n count) (k (pubk counter)))
  (precedes ((0 0) (5 0)) ((1 0) (3 0)) ((2 1) (4 0)) ((2 3) (1 1))
    ((3 1) (2 0)) ((4 1) (2 2)) ((5 1) (6 0)) ((5 3) (0 1))
    ((6 1) (5 2)))
  (non-orig (privk left) (privk admin) (privk counter))
  (pen-non-orig (bltk left left))
  (uniq-orig meas meas-0 count count-0 token count-1 token-1)
  (operation generalization deleted (3 0))
  (traces
    ((send (enc "endpoint" left left meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left left))
            (privk admin)) (bltk left left) (pubk left))))
    ((send (enc "endpoint" left left meas-0 (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left left))
            (privk admin)) (bltk left left) (pubk left))))
    ((recv (enc "endpoint" left left meas-0 (pubk left) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left left))
            (privk admin)) (bltk left left) (pubk left))))
    ((recv (enc "incr" token-0 (pubk counter)))
      (send (enc "count" count-1 token-0 (privk counter))))
    ((recv (enc "incr" token (pubk counter)))
      (send (enc "count" count-0 token (privk counter))))
    ((recv (enc "endpoint" left left meas (pubk left) (pubk admin)))
      (send (enc "incr" token-1 (pubk counter)))
      (recv (enc "count" count token-1 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left left))
            (privk admin)) (bltk left left) (pubk left))))
    ((recv (enc "incr" token-1 (pubk counter)))
      (send (enc "count" count token-1 (privk counter)))))
  (label 25)
  (parent 24)
  (seen 18)
  (unrealized)
  (comment "1 in cohort - 0 not yet seen"))

(comment "Nothing left to do")

(defprotocol provision diffie-hellman
  (defrole supplicant
    (vars (self pal admin name) (meas text) (count data))
    (trace
      (send (enc "endpoint" self pal meas (pubk self) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk self pal))
            (privk admin)) (bltk self pal) (pubk self))))
    (uniq-orig meas))
  (defrole admission
    (vars (self pal admin counter name) (meas-self text)
      (token count data))
    (trace
      (recv
        (enc "endpoint" self pal meas-self (pubk self) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count meas-self (hash (bltk self pal))
            (privk admin)) (bltk self pal) (pubk self))))
    (non-orig (privk counter))
    (uniq-orig token))
  (defrole counter
    (vars (t n data) (k akey))
    (trace (recv (enc "incr" t k)) (send (enc "count" n t (invk k))))
    (uniq-orig n))
  (defrule one-counter
    (forall ((z w strd) (counter1 counter2 name))
      (implies
        (and (p "admission" z 2) (p "admission" "counter" z counter1)
          (p "admission" w 2) (p "admission" "counter" w counter2))
        (= counter1 counter2)))))

(defskeleton provision
  (vars (meas meas-0 text) (count count-0 data) (left right admin name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (neq (left right))
  (non-orig (privk left) (privk right) (privk admin))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0)
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right)))))
  (label 26)
  (unrealized (0 1) (1 1))
  (origs (meas-0 (1 0)) (meas (0 0)))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton provision
  (vars (meas meas-0 text) (count count-0 token data)
    (left right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self left) (pal right) (admin admin)
    (counter counter))
  (precedes ((1 0) (2 0)) ((2 3) (1 1)))
  (neq (left right))
  (non-orig (privk left) (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0 token)
  (operation encryption-test (added-strand admission 4)
    (enc "admitted" count-0 meas-0 (hash (bltk left right))
      (privk admin)) (1 1))
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" left right meas-0 (pubk left) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left)))))
  (label 27)
  (parent 26)
  (seen 29)
  (unrealized (0 1) (1 1) (2 0) (2 2))
  (comment "2 in cohort - 1 not yet seen"))

(defskeleton provision
  (vars (meas meas-0 text) (count count-0 token data)
    (left right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self right) (pal left) (admin admin)
    (counter counter))
  (precedes ((1 0) (2 0)) ((2 3) (1 1)))
  (neq (left right))
  (non-orig (privk left) (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0 token)
  (operation encryption-test (added-strand admission 4)
    (enc "admitted" count-0 meas-0 (hash (bltk left right))
      (privk admin)) (1 1))
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right)))))
  (label 28)
  (parent 26)
  (unrealized (0 1) (2 2))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton provision
  (vars (meas meas-0 text) (count count-0 token token-0 count-1 data)
    (left right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self left) (pal right) (admin admin)
    (counter counter))
  (defstrand admission 4 (meas-self meas-0) (token token-0)
    (count count-1) (self right) (pal left) (admin admin)
    (counter counter))
  (precedes ((1 0) (3 0)) ((2 3) (1 1)) ((3 3) (2 0)))
  (neq (left right))
  (non-orig (privk left) (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0 token token-0)
  (rule one-counter)
  (operation nonce-test (added-strand admission 4) meas-0 (2 0)
    (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" left right meas-0 (pubk left) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((recv (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token-0 (pubk counter)))
      (recv (enc "count" count-1 token-0 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-1 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right)))))
  (label 29)
  (parent 27)
  (unrealized (0 1) (1 1) (2 0) (2 2) (3 2))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton provision
  (vars (meas meas-0 text) (count count-0 token data)
    (left right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self right) (pal left) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token) (n count-0) (k (pubk counter)))
  (precedes ((1 0) (2 0)) ((2 1) (3 0)) ((2 3) (1 1)) ((3 1) (2 2)))
  (neq (left right))
  (non-orig (privk left) (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0 count-0 token)
  (operation encryption-test (added-strand counter 2)
    (enc "count" count-0 token (privk counter)) (2 2))
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "incr" token (pubk counter)))
      (send (enc "count" count-0 token (privk counter)))))
  (label 30)
  (parent 28)
  (seen 32 33)
  (unrealized (0 1))
  (comment "4 in cohort - 2 not yet seen"))

(defskeleton provision
  (vars (meas meas-0 text) (count count-0 token token-0 count-1 data)
    (left right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self left) (pal right) (admin admin)
    (counter counter))
  (defstrand admission 4 (meas-self meas-0) (token token-0)
    (count count-1) (self right) (pal left) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token-0) (n count-1) (k (pubk counter)))
  (precedes ((1 0) (3 0)) ((2 3) (1 1)) ((3 1) (4 0)) ((3 3) (2 0))
    ((4 1) (3 2)))
  (neq (left right))
  (non-orig (privk left) (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0 token token-0 count-1)
  (operation encryption-test (added-strand counter 2)
    (enc "count" count-1 token-0 (privk counter)) (3 2))
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" left right meas-0 (pubk left) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((recv (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token-0 (pubk counter)))
      (recv (enc "count" count-1 token-0 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-1 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "incr" token-0 (pubk counter)))
      (send (enc "count" count-1 token-0 (privk counter)))))
  (label 31)
  (parent 29)
  (seen 34)
  (unrealized (0 1) (1 1) (2 0) (2 2))
  (comment "4 in cohort - 1 not yet seen"))

(defskeleton provision
  (vars (meas meas-0 text) (count count-0 token token-0 data)
    (left right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self right) (pal left) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token) (n count-0) (k (pubk counter)))
  (defstrand admission 4 (meas-self meas) (token token-0) (count count)
    (self right) (pal left) (admin admin) (counter counter))
  (precedes ((0 0) (4 0)) ((1 0) (2 0)) ((2 1) (3 0)) ((2 3) (1 1))
    ((3 1) (2 2)) ((4 3) (0 1)))
  (neq (left right))
  (non-orig (privk left) (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0 count-0 token token-0)
  (rule one-counter)
  (operation encryption-test (added-strand admission 4)
    (enc "admitted" count meas (hash (bltk left right)) (privk admin))
    (0 1))
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "incr" token (pubk counter)))
      (send (enc "count" count-0 token (privk counter))))
    ((recv (enc "endpoint" right left meas (pubk right) (pubk admin)))
      (send (enc "incr" token-0 (pubk counter)))
      (recv (enc "count" count token-0 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right)))))
  (label 32)
  (parent 30)
  (seen 35)
  (unrealized (0 1) (4 0) (4 2))
  (comment "4 in cohort - 1 not yet seen"))

(defskeleton provision
  (vars (meas meas-0 text) (count count-0 token token-0 data)
    (left right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self right) (pal left) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token) (n count-0) (k (pubk counter)))
  (defstrand admission 4 (meas-self meas) (token token-0) (count count)
    (self left) (pal right) (admin admin) (counter counter))
  (precedes ((0 0) (4 0)) ((1 0) (2 0)) ((2 1) (3 0)) ((2 3) (1 1))
    ((3 1) (2 2)) ((4 3) (0 1)))
  (neq (left right))
  (non-orig (privk left) (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0 count-0 token token-0)
  (rule one-counter)
  (operation encryption-test (added-strand admission 4)
    (enc "admitted" count meas (hash (bltk left right)) (privk admin))
    (0 1))
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "incr" token (pubk counter)))
      (send (enc "count" count-0 token (privk counter))))
    ((recv (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (send (enc "incr" token-0 (pubk counter)))
      (recv (enc "count" count token-0 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left)))))
  (label 33)
  (parent 30)
  (unrealized (4 2))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton provision
  (vars (meas meas-0 text)
    (count count-0 token token-0 count-1 token-1 count-2 data)
    (left right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self left) (pal right) (admin admin)
    (counter counter))
  (defstrand admission 4 (meas-self meas-0) (token token-0)
    (count count-1) (self right) (pal left) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token-0) (n count-1) (k (pubk counter)))
  (defstrand admission 4 (meas-self meas-0) (token token-1)
    (count count-2) (self right) (pal left) (admin admin)
    (counter counter))
  (precedes ((1 0) (3 0)) ((1 0) (5 0)) ((2 3) (1 1)) ((3 1) (4 0))
    ((3 3) (2 0)) ((4 1) (3 2)) ((5 3) (2 0)))
  (neq (left right))
  (non-orig (privk left) (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0 token token-0 count-1 token-1)
  (rule one-counter)
  (operation nonce-test (added-strand admission 4) meas-0 (2 0)
    (enc "admit"
      (enc "admitted" count-1 meas-0 (hash (bltk left right))
        (privk admin)) (bltk left right) (pubk right))
    (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" left right meas-0 (pubk left) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((recv (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token-0 (pubk counter)))
      (recv (enc "count" count-1 token-0 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-1 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "incr" token-0 (pubk counter)))
      (send (enc "count" count-1 token-0 (privk counter))))
    ((recv (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token-1 (pubk counter)))
      (recv (enc "count" count-2 token-1 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-2 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right)))))
  (label 34)
  (parent 31)
  (unrealized (0 1) (1 1) (2 0) (2 2) (5 2))
  (dead)
  (comment "empty cohort"))

(defskeleton provision
  (vars (meas meas-0 text)
    (count count-0 token token-0 token-1 count-1 data)
    (left right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self right) (pal left) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token) (n count-0) (k (pubk counter)))
  (defstrand admission 4 (meas-self meas) (token token-0) (count count)
    (self right) (pal left) (admin admin) (counter counter))
  (defstrand admission 4 (meas-self meas) (token token-1)
    (count count-1) (self left) (pal right) (admin admin)
    (counter counter))
  (precedes ((0 0) (5 0)) ((1 0) (2 0)) ((2 1) (3 0)) ((2 3) (1 1))
    ((3 1) (2 2)) ((4 3) (0 1)) ((5 3) (4 0)))
  (neq (left right))
  (non-orig (privk left) (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0 count-0 token token-0 token-1)
  (rule one-counter)
  (operation nonce-test (added-strand admission 4) meas (4 0)
    (enc "endpoint" left right meas (pubk left) (pubk admin)))
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "incr" token (pubk counter)))
      (send (enc "count" count-0 token (privk counter))))
    ((recv (enc "endpoint" right left meas (pubk right) (pubk admin)))
      (send (enc "incr" token-0 (pubk counter)))
      (recv (enc "count" count token-0 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (send (enc "incr" token-1 (pubk counter)))
      (recv (enc "count" count-1 token-1 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-1 meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left)))))
  (label 35)
  (parent 32)
  (unrealized (0 1) (4 0) (4 2) (5 2))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton provision
  (vars (meas meas-0 text) (count count-0 token token-0 data)
    (left right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self right) (pal left) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token) (n count-0) (k (pubk counter)))
  (defstrand admission 4 (meas-self meas) (token token-0) (count count)
    (self left) (pal right) (admin admin) (counter counter))
  (defstrand counter 2 (t token-0) (n count) (k (pubk counter)))
  (precedes ((0 0) (4 0)) ((1 0) (2 0)) ((2 1) (3 0)) ((2 3) (1 1))
    ((3 1) (2 2)) ((4 1) (5 0)) ((4 3) (0 1)) ((5 1) (4 2)))
  (neq (left right))
  (non-orig (privk left) (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0 count count-0 token token-0)
  (operation encryption-test (added-strand counter 2)
    (enc "count" count token-0 (privk counter)) (4 2))
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "incr" token (pubk counter)))
      (send (enc "count" count-0 token (privk counter))))
    ((recv (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (send (enc "incr" token-0 (pubk counter)))
      (recv (enc "count" count token-0 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((recv (enc "incr" token-0 (pubk counter)))
      (send (enc "count" count token-0 (privk counter)))))
  (label 36)
  (parent 33)
  (unrealized)
  (shape)
  (maps
    ((0 1)
      ((left left) (right right) (admin admin) (meas meas) (count count)
        (meas-0 meas-0) (count-0 count-0))))
  (origs (count (5 1)) (token-0 (4 1)) (count-0 (3 1)) (token (2 1))
    (meas-0 (1 0)) (meas (0 0))))

(defskeleton provision
  (vars (meas meas-0 text)
    (count count-0 token token-0 token-1 count-1 data)
    (left right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self right) (pal left) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token) (n count-0) (k (pubk counter)))
  (defstrand admission 4 (meas-self meas) (token token-0) (count count)
    (self right) (pal left) (admin admin) (counter counter))
  (defstrand admission 4 (meas-self meas) (token token-1)
    (count count-1) (self left) (pal right) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token-1) (n count-1) (k (pubk counter)))
  (precedes ((0 0) (5 0)) ((1 0) (2 0)) ((2 1) (3 0)) ((2 3) (1 1))
    ((3 1) (2 2)) ((4 3) (0 1)) ((5 1) (6 0)) ((5 3) (4 0))
    ((6 1) (5 2)))
  (neq (left right))
  (non-orig (privk left) (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0 count-0 token token-0 token-1 count-1)
  (operation encryption-test (added-strand counter 2)
    (enc "count" count-1 token-1 (privk counter)) (5 2))
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "incr" token (pubk counter)))
      (send (enc "count" count-0 token (privk counter))))
    ((recv (enc "endpoint" right left meas (pubk right) (pubk admin)))
      (send (enc "incr" token-0 (pubk counter)))
      (recv (enc "count" count token-0 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (send (enc "incr" token-1 (pubk counter)))
      (recv (enc "count" count-1 token-1 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-1 meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((recv (enc "incr" token-1 (pubk counter)))
      (send (enc "count" count-1 token-1 (privk counter)))))
  (label 37)
  (parent 35)
  (seen 38)
  (unrealized (0 1) (4 0) (4 2))
  (comment "6 in cohort - 1 not yet seen"))

(defskeleton provision
  (vars (meas meas-0 text)
    (count count-0 token token-0 token-1 count-1 token-2 count-2 data)
    (left right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self right) (pal left) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token) (n count-0) (k (pubk counter)))
  (defstrand admission 4 (meas-self meas) (token token-0) (count count)
    (self right) (pal left) (admin admin) (counter counter))
  (defstrand admission 4 (meas-self meas) (token token-1)
    (count count-1) (self left) (pal right) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token-1) (n count-1) (k (pubk counter)))
  (defstrand admission 4 (meas-self meas) (token token-2)
    (count count-2) (self left) (pal right) (admin admin)
    (counter counter))
  (precedes ((0 0) (5 0)) ((0 0) (7 0)) ((1 0) (2 0)) ((2 1) (3 0))
    ((2 3) (1 1)) ((3 1) (2 2)) ((4 3) (0 1)) ((5 1) (6 0))
    ((5 3) (4 0)) ((6 1) (5 2)) ((7 3) (4 0)))
  (neq (left right))
  (non-orig (privk left) (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0 count-0 token token-0 token-1 count-1 token-2)
  (rule one-counter)
  (operation nonce-test (added-strand admission 4) meas (4 0)
    (enc "admit"
      (enc "admitted" count-1 meas (hash (bltk left right))
        (privk admin)) (bltk left right) (pubk left))
    (enc "endpoint" left right meas (pubk left) (pubk admin)))
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "incr" token (pubk counter)))
      (send (enc "count" count-0 token (privk counter))))
    ((recv (enc "endpoint" right left meas (pubk right) (pubk admin)))
      (send (enc "incr" token-0 (pubk counter)))
      (recv (enc "count" count token-0 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (send (enc "incr" token-1 (pubk counter)))
      (recv (enc "count" count-1 token-1 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-1 meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((recv (enc "incr" token-1 (pubk counter)))
      (send (enc "count" count-1 token-1 (privk counter))))
    ((recv (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (send (enc "incr" token-2 (pubk counter)))
      (recv (enc "count" count-2 token-2 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-2 meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left)))))
  (label 38)
  (parent 37)
  (unrealized (0 1) (4 0) (4 2) (7 2))
  (dead)
  (comment "empty cohort"))

(comment "Nothing left to do")

(defprotocol provision diffie-hellman
  (defrole supplicant
    (vars (self pal admin name) (meas text) (count data))
    (trace
      (send (enc "endpoint" self pal meas (pubk self) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk self pal))
            (privk admin)) (bltk self pal) (pubk self))))
    (uniq-orig meas))
  (defrole admission
    (vars (self pal admin counter name) (meas-self text)
      (token count data))
    (trace
      (recv
        (enc "endpoint" self pal meas-self (pubk self) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count meas-self (hash (bltk self pal))
            (privk admin)) (bltk self pal) (pubk self))))
    (non-orig (privk counter))
    (uniq-orig token))
  (defrole counter
    (vars (t n data) (k akey))
    (trace (recv (enc "incr" t k)) (send (enc "count" n t (invk k))))
    (uniq-orig n))
  (defrule one-counter
    (forall ((z w strd) (counter1 counter2 name))
      (implies
        (and (p "admission" z 2) (p "admission" "counter" z counter1)
          (p "admission" w 2) (p "admission" "counter" w counter2))
        (= counter1 counter2)))))

(defskeleton provision
  (vars (meas meas-0 text) (count count-0 data) (left right admin name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (neq (left right) (right left))
  (non-orig (privk left) (privk right) (privk admin))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0)
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right)))))
  (label 39)
  (unrealized (0 1) (1 1))
  (origs (meas-0 (1 0)) (meas (0 0)))
  (comment "2 in cohort - 2 not yet seen"))

(defskeleton provision
  (vars (meas meas-0 text) (count count-0 token data)
    (left right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self left) (pal right) (admin admin)
    (counter counter))
  (precedes ((1 0) (2 0)) ((2 3) (1 1)))
  (neq (left right) (right left))
  (non-orig (privk left) (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0 token)
  (operation encryption-test (added-strand admission 4)
    (enc "admitted" count-0 meas-0 (hash (bltk left right))
      (privk admin)) (1 1))
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" left right meas-0 (pubk left) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left)))))
  (label 40)
  (parent 39)
  (seen 42)
  (unrealized (0 1) (1 1) (2 0) (2 2))
  (comment "2 in cohort - 1 not yet seen"))

(defskeleton provision
  (vars (meas meas-0 text) (count count-0 token data)
    (left right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self right) (pal left) (admin admin)
    (counter counter))
  (precedes ((1 0) (2 0)) ((2 3) (1 1)))
  (neq (left right) (right left))
  (non-orig (privk left) (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0 token)
  (operation encryption-test (added-strand admission 4)
    (enc "admitted" count-0 meas-0 (hash (bltk left right))
      (privk admin)) (1 1))
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right)))))
  (label 41)
  (parent 39)
  (unrealized (0 1) (2 2))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton provision
  (vars (meas meas-0 text) (count count-0 token token-0 count-1 data)
    (left right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self left) (pal right) (admin admin)
    (counter counter))
  (defstrand admission 4 (meas-self meas-0) (token token-0)
    (count count-1) (self right) (pal left) (admin admin)
    (counter counter))
  (precedes ((1 0) (3 0)) ((2 3) (1 1)) ((3 3) (2 0)))
  (neq (left right) (right left))
  (non-orig (privk left) (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0 token token-0)
  (rule one-counter)
  (operation nonce-test (added-strand admission 4) meas-0 (2 0)
    (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" left right meas-0 (pubk left) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((recv (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token-0 (pubk counter)))
      (recv (enc "count" count-1 token-0 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-1 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right)))))
  (label 42)
  (parent 40)
  (unrealized (0 1) (1 1) (2 0) (2 2) (3 2))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton provision
  (vars (meas meas-0 text) (count count-0 token data)
    (left right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self right) (pal left) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token) (n count-0) (k (pubk counter)))
  (precedes ((1 0) (2 0)) ((2 1) (3 0)) ((2 3) (1 1)) ((3 1) (2 2)))
  (neq (left right) (right left))
  (non-orig (privk left) (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0 count-0 token)
  (operation encryption-test (added-strand counter 2)
    (enc "count" count-0 token (privk counter)) (2 2))
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "incr" token (pubk counter)))
      (send (enc "count" count-0 token (privk counter)))))
  (label 43)
  (parent 41)
  (seen 45 46)
  (unrealized (0 1))
  (comment "4 in cohort - 2 not yet seen"))

(defskeleton provision
  (vars (meas meas-0 text) (count count-0 token token-0 count-1 data)
    (left right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self left) (pal right) (admin admin)
    (counter counter))
  (defstrand admission 4 (meas-self meas-0) (token token-0)
    (count count-1) (self right) (pal left) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token-0) (n count-1) (k (pubk counter)))
  (precedes ((1 0) (3 0)) ((2 3) (1 1)) ((3 1) (4 0)) ((3 3) (2 0))
    ((4 1) (3 2)))
  (neq (left right) (right left))
  (non-orig (privk left) (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0 token token-0 count-1)
  (operation encryption-test (added-strand counter 2)
    (enc "count" count-1 token-0 (privk counter)) (3 2))
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" left right meas-0 (pubk left) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((recv (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token-0 (pubk counter)))
      (recv (enc "count" count-1 token-0 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-1 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "incr" token-0 (pubk counter)))
      (send (enc "count" count-1 token-0 (privk counter)))))
  (label 44)
  (parent 42)
  (seen 47)
  (unrealized (0 1) (1 1) (2 0) (2 2))
  (comment "4 in cohort - 1 not yet seen"))

(defskeleton provision
  (vars (meas meas-0 text) (count count-0 token token-0 data)
    (left right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self right) (pal left) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token) (n count-0) (k (pubk counter)))
  (defstrand admission 4 (meas-self meas) (token token-0) (count count)
    (self right) (pal left) (admin admin) (counter counter))
  (precedes ((0 0) (4 0)) ((1 0) (2 0)) ((2 1) (3 0)) ((2 3) (1 1))
    ((3 1) (2 2)) ((4 3) (0 1)))
  (neq (left right) (right left))
  (non-orig (privk left) (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0 count-0 token token-0)
  (rule one-counter)
  (operation encryption-test (added-strand admission 4)
    (enc "admitted" count meas (hash (bltk left right)) (privk admin))
    (0 1))
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "incr" token (pubk counter)))
      (send (enc "count" count-0 token (privk counter))))
    ((recv (enc "endpoint" right left meas (pubk right) (pubk admin)))
      (send (enc "incr" token-0 (pubk counter)))
      (recv (enc "count" count token-0 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right)))))
  (label 45)
  (parent 43)
  (seen 48)
  (unrealized (0 1) (4 0) (4 2))
  (comment "4 in cohort - 1 not yet seen"))

(defskeleton provision
  (vars (meas meas-0 text) (count count-0 token token-0 data)
    (left right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self right) (pal left) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token) (n count-0) (k (pubk counter)))
  (defstrand admission 4 (meas-self meas) (token token-0) (count count)
    (self left) (pal right) (admin admin) (counter counter))
  (precedes ((0 0) (4 0)) ((1 0) (2 0)) ((2 1) (3 0)) ((2 3) (1 1))
    ((3 1) (2 2)) ((4 3) (0 1)))
  (neq (left right) (right left))
  (non-orig (privk left) (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0 count-0 token token-0)
  (rule one-counter)
  (operation encryption-test (added-strand admission 4)
    (enc "admitted" count meas (hash (bltk left right)) (privk admin))
    (0 1))
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "incr" token (pubk counter)))
      (send (enc "count" count-0 token (privk counter))))
    ((recv (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (send (enc "incr" token-0 (pubk counter)))
      (recv (enc "count" count token-0 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left)))))
  (label 46)
  (parent 43)
  (unrealized (4 2))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton provision
  (vars (meas meas-0 text)
    (count count-0 token token-0 count-1 token-1 count-2 data)
    (left right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self left) (pal right) (admin admin)
    (counter counter))
  (defstrand admission 4 (meas-self meas-0) (token token-0)
    (count count-1) (self right) (pal left) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token-0) (n count-1) (k (pubk counter)))
  (defstrand admission 4 (meas-self meas-0) (token token-1)
    (count count-2) (self right) (pal left) (admin admin)
    (counter counter))
  (precedes ((1 0) (3 0)) ((1 0) (5 0)) ((2 3) (1 1)) ((3 1) (4 0))
    ((3 3) (2 0)) ((4 1) (3 2)) ((5 3) (2 0)))
  (neq (left right) (right left))
  (non-orig (privk left) (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0 token token-0 count-1 token-1)
  (rule one-counter)
  (operation nonce-test (added-strand admission 4) meas-0 (2 0)
    (enc "admit"
      (enc "admitted" count-1 meas-0 (hash (bltk left right))
        (privk admin)) (bltk left right) (pubk right))
    (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" left right meas-0 (pubk left) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((recv (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token-0 (pubk counter)))
      (recv (enc "count" count-1 token-0 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-1 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "incr" token-0 (pubk counter)))
      (send (enc "count" count-1 token-0 (privk counter))))
    ((recv (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token-1 (pubk counter)))
      (recv (enc "count" count-2 token-1 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-2 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right)))))
  (label 47)
  (parent 44)
  (unrealized (0 1) (1 1) (2 0) (2 2) (5 2))
  (dead)
  (comment "empty cohort"))

(defskeleton provision
  (vars (meas meas-0 text)
    (count count-0 token token-0 token-1 count-1 data)
    (left right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self right) (pal left) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token) (n count-0) (k (pubk counter)))
  (defstrand admission 4 (meas-self meas) (token token-0) (count count)
    (self right) (pal left) (admin admin) (counter counter))
  (defstrand admission 4 (meas-self meas) (token token-1)
    (count count-1) (self left) (pal right) (admin admin)
    (counter counter))
  (precedes ((0 0) (5 0)) ((1 0) (2 0)) ((2 1) (3 0)) ((2 3) (1 1))
    ((3 1) (2 2)) ((4 3) (0 1)) ((5 3) (4 0)))
  (neq (left right) (right left))
  (non-orig (privk left) (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0 count-0 token token-0 token-1)
  (rule one-counter)
  (operation nonce-test (added-strand admission 4) meas (4 0)
    (enc "endpoint" left right meas (pubk left) (pubk admin)))
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "incr" token (pubk counter)))
      (send (enc "count" count-0 token (privk counter))))
    ((recv (enc "endpoint" right left meas (pubk right) (pubk admin)))
      (send (enc "incr" token-0 (pubk counter)))
      (recv (enc "count" count token-0 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (send (enc "incr" token-1 (pubk counter)))
      (recv (enc "count" count-1 token-1 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-1 meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left)))))
  (label 48)
  (parent 45)
  (unrealized (0 1) (4 0) (4 2) (5 2))
  (comment "1 in cohort - 1 not yet seen"))

(defskeleton provision
  (vars (meas meas-0 text) (count count-0 token token-0 data)
    (left right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self right) (pal left) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token) (n count-0) (k (pubk counter)))
  (defstrand admission 4 (meas-self meas) (token token-0) (count count)
    (self left) (pal right) (admin admin) (counter counter))
  (defstrand counter 2 (t token-0) (n count) (k (pubk counter)))
  (precedes ((0 0) (4 0)) ((1 0) (2 0)) ((2 1) (3 0)) ((2 3) (1 1))
    ((3 1) (2 2)) ((4 1) (5 0)) ((4 3) (0 1)) ((5 1) (4 2)))
  (neq (left right) (right left))
  (non-orig (privk left) (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0 count count-0 token token-0)
  (operation encryption-test (added-strand counter 2)
    (enc "count" count token-0 (privk counter)) (4 2))
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "incr" token (pubk counter)))
      (send (enc "count" count-0 token (privk counter))))
    ((recv (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (send (enc "incr" token-0 (pubk counter)))
      (recv (enc "count" count token-0 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((recv (enc "incr" token-0 (pubk counter)))
      (send (enc "count" count token-0 (privk counter)))))
  (label 49)
  (parent 46)
  (unrealized)
  (shape)
  (maps
    ((0 1)
      ((left left) (right right) (admin admin) (meas meas) (count count)
        (meas-0 meas-0) (count-0 count-0))))
  (origs (count (5 1)) (token-0 (4 1)) (count-0 (3 1)) (token (2 1))
    (meas-0 (1 0)) (meas (0 0))))

(defskeleton provision
  (vars (meas meas-0 text)
    (count count-0 token token-0 token-1 count-1 data)
    (left right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self right) (pal left) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token) (n count-0) (k (pubk counter)))
  (defstrand admission 4 (meas-self meas) (token token-0) (count count)
    (self right) (pal left) (admin admin) (counter counter))
  (defstrand admission 4 (meas-self meas) (token token-1)
    (count count-1) (self left) (pal right) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token-1) (n count-1) (k (pubk counter)))
  (precedes ((0 0) (5 0)) ((1 0) (2 0)) ((2 1) (3 0)) ((2 3) (1 1))
    ((3 1) (2 2)) ((4 3) (0 1)) ((5 1) (6 0)) ((5 3) (4 0))
    ((6 1) (5 2)))
  (neq (left right) (right left))
  (non-orig (privk left) (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0 count-0 token token-0 token-1 count-1)
  (operation encryption-test (added-strand counter 2)
    (enc "count" count-1 token-1 (privk counter)) (5 2))
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "incr" token (pubk counter)))
      (send (enc "count" count-0 token (privk counter))))
    ((recv (enc "endpoint" right left meas (pubk right) (pubk admin)))
      (send (enc "incr" token-0 (pubk counter)))
      (recv (enc "count" count token-0 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (send (enc "incr" token-1 (pubk counter)))
      (recv (enc "count" count-1 token-1 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-1 meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((recv (enc "incr" token-1 (pubk counter)))
      (send (enc "count" count-1 token-1 (privk counter)))))
  (label 50)
  (parent 48)
  (seen 51)
  (unrealized (0 1) (4 0) (4 2))
  (comment "6 in cohort - 1 not yet seen"))

(defskeleton provision
  (vars (meas meas-0 text)
    (count count-0 token token-0 token-1 count-1 token-2 count-2 data)
    (left right admin counter name))
  (defstrand supplicant 2 (meas meas) (count count) (self left)
    (pal right) (admin admin))
  (defstrand supplicant 2 (meas meas-0) (count count-0) (self right)
    (pal left) (admin admin))
  (defstrand admission 4 (meas-self meas-0) (token token)
    (count count-0) (self right) (pal left) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token) (n count-0) (k (pubk counter)))
  (defstrand admission 4 (meas-self meas) (token token-0) (count count)
    (self right) (pal left) (admin admin) (counter counter))
  (defstrand admission 4 (meas-self meas) (token token-1)
    (count count-1) (self left) (pal right) (admin admin)
    (counter counter))
  (defstrand counter 2 (t token-1) (n count-1) (k (pubk counter)))
  (defstrand admission 4 (meas-self meas) (token token-2)
    (count count-2) (self left) (pal right) (admin admin)
    (counter counter))
  (precedes ((0 0) (5 0)) ((0 0) (7 0)) ((1 0) (2 0)) ((2 1) (3 0))
    ((2 3) (1 1)) ((3 1) (2 2)) ((4 3) (0 1)) ((5 1) (6 0))
    ((5 3) (4 0)) ((6 1) (5 2)) ((7 3) (4 0)))
  (neq (left right) (right left))
  (non-orig (privk left) (privk right) (privk admin) (privk counter))
  (pen-non-orig (bltk left right))
  (uniq-orig meas meas-0 count-0 token token-0 token-1 count-1 token-2)
  (rule one-counter)
  (operation nonce-test (added-strand admission 4) meas (4 0)
    (enc "admit"
      (enc "admitted" count-1 meas (hash (bltk left right))
        (privk admin)) (bltk left right) (pubk left))
    (enc "endpoint" left right meas (pubk left) (pubk admin)))
  (traces
    ((send (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((send (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (recv
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" right left meas-0 (pubk right) (pubk admin)))
      (send (enc "incr" token (pubk counter)))
      (recv (enc "count" count-0 token (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-0 meas-0 (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "incr" token (pubk counter)))
      (send (enc "count" count-0 token (privk counter))))
    ((recv (enc "endpoint" right left meas (pubk right) (pubk admin)))
      (send (enc "incr" token-0 (pubk counter)))
      (recv (enc "count" count token-0 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk right))))
    ((recv (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (send (enc "incr" token-1 (pubk counter)))
      (recv (enc "count" count-1 token-1 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-1 meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left))))
    ((recv (enc "incr" token-1 (pubk counter)))
      (send (enc "count" count-1 token-1 (privk counter))))
    ((recv (enc "endpoint" left right meas (pubk left) (pubk admin)))
      (send (enc "incr" token-2 (pubk counter)))
      (recv (enc "count" count-2 token-2 (privk counter)))
      (send
        (enc "admit"
          (enc "admitted" count-2 meas (hash (bltk left right))
            (privk admin)) (bltk left right) (pubk left)))))
  (label 51)
  (parent 50)
  (unrealized (0 1) (4 0) (4 2) (7 2))
  (dead)
  (comment "empty cohort"))

(comment "Nothing left to do")
