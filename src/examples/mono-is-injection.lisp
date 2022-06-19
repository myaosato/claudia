(defpackage :claudia/examples/mono-is-injection
  (:use :cl
        :claudia/theorem)
  (:export :app
           :==
           :->
           :inj->mono))
(in-package :claudia/examples/mono-is-injection)

(def-func app *)
(def-predicate == 2)
(def-const ->) ;; (app (app -> x) _) => x


(def-theorem inj->mono
    (→ (∀ x (∀ y (→ (== (app f x) (app f y))  (== x y))))
       (∀ g (∀ h (→ (∀ z (== (app f (app g z)) (app f (app h z))))
                    (∀ z (== (app g z) (app h z)))))))
    (:vars (x y f z g h))
  (TO-R 0)
  (FORALL-R 0)
  (FORALL-R 0)
  (TO-R 0)
  (FORALL-R 0)
  (FORALL-L 0 z)
  (FORALL-L 0 (APP G z) 1)
  (FORALL-L 0 (APP H z) 1)
  (TO-L 0 1)
  (ID 0)
  (ID 0))

(def-theorem mono->inj
    (→ (∀ g (∀ h (→ (∀ z (== (app f (app g z)) (app f (app h z))))
                    (∀ z (== (app g z) (app h z))))))
       (∀ x (∀ y (→ (== (app f x) (app f y)) (== x y)))))
    (:vars (x y f g h z))
  ;; WIP) just memo, it is not proof 
  (TO-R 0 0)
  (FORALL-R 0 0)
  (FORALL-R 0 0)
  (FORALL-L 0 (APP -> X) 0)
  (FORALL-L 0 (APP -> Y) 0)
  (TO-R 0 0)
  (TO-L 0 1)
  (FORALL-R 0 0)
  (WR 0 1)
  (FORALL-L 1 Z 1)
  (WL 1 0))
