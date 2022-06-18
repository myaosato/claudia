(defpackage :claudia/examples/mono-is-injection
  (:use :cl
        :claudia/theorem)
  (:export :app
           :==
           :inj->mono))
(in-package :claudia/examples/mono-is-injection)

(def-func app *)
(def-predicate == 2)

(def-theorem inj->mono
    (→ (∀ x (∀ y (→ (== (app f x) (app f y))  (== x y))))
       (∀ g (∀ h (→ (∀ x (== (app f (app g x)) (app f (app h x))))
                    (∀ x (== (app g x) (app h x)))))))
    (:vars (x y f g h))
  (TO-R 0)
  (FORALL-R 0)
  (FORALL-R 0)
  (TO-R 0)
  (FORALL-R 0)
  (FORALL-L 0 X)
  (FORALL-L 0 (APP G X) 1)
  (FORALL-L 0 (APP H X) 1)
  (TO-L 0 1)
  (ID 0)
  (ID 0))

(def-theorem mono->inj
    (→ (∀ g (∀ h (→ (∀ x (== (app f (app g x)) (app f (app h x))))
                    (∀ x (== (app g x) (app h x))))))
       (∀ x (∀ y (→ (== (app f x) (app f y))  (== x y)))))
    (:vars (x y f g h))
  )
