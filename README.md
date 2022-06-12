# Claudia

proof assistant ?

```console
CL-USER> (claudia/examples/cpl:law-of-exclded-middle)
---------------- [GOAL]
[0]:   ⊢  (A ∨ ¬A)
---------------- [CR]
[0]:   ⊢  (A ∨ ¬A), (A ∨ ¬A)
---------------- [OR-R1]
[0]:   ⊢  A, (A ∨ ¬A)
---------------- [PR]
[0]:   ⊢  (A ∨ ¬A), A
---------------- [OR-R2]
[0]:   ⊢  ¬A, A
---------------- [NOT-R]
[0]: A  ⊢  A
---------------- [ID]
Complete !!
NIL
```
