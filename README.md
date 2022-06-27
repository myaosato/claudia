# Claudia

proof assistant ?

## example (on repl style)

```console
CL-USER> (use-package :claudia/repl)
T
CL-USER> (start-proof (→ (→ (→ a b) a) a) :props (a b))
PROP: A: #<Prop: A>
PROP: B: #<Prop: B>
---------------- [GOAL]
[0]:   ⊢  (((A → B) → A) → A)
#<GOAL: [0]:   ⊢  (((A → B) → A) → A) >
CL-USER> (to-r 0)
---------------- [TO-R]
[0]: ((A → B) → A)  ⊢  A
#<GOAL: [0]: ((A → B) → A)  ⊢  A >
CL-USER> (to-l 0)
---------------- [TO-L]
[0]:   ⊢  (A → B), A
[1]: A  ⊢  A
#<GOAL: [0]:   ⊢  (A → B), A
[1]: A  ⊢  A >
CL-USER> (to-r 0)
---------------- [TO-R]
[0]: A  ⊢  B, A
[1]: A  ⊢  A
#<GOAL: [0]: A  ⊢  B, A
[1]: A  ⊢  A >
CL-USER> (id 0)
---------------- [ID]
[0]: A  ⊢  A
#<GOAL: [0]: A  ⊢  A >
CL-USER> (id 0)
---------------- [ID]
Complete !!
#<GOAL: Complete !! >
CL-USER> (proof-hist)
---------------- [GOAL]
[0]:   ⊢  (((A → B) → A) → A)
---------------- [TO-R]
[0]: ((A → B) → A)  ⊢  A
---------------- [TO-L]
[0]:   ⊢  (A → B), A
[1]: A  ⊢  A
---------------- [TO-R]
[0]: A  ⊢  B, A
[1]: A  ⊢  A
---------------- [ID]
[0]: A  ⊢  A
---------------- [ID]
Complete !!
NIL
CL-USER> (export-proof)
(DEF-THEOREM #:RANDOM-NAME-638
    (→ (→ (→ A B) A) A)
    (:PROPS (B A) :VARS NIL)
  (TO-R 0 0)
  (TO-L 0 0)
  (TO-R 0 0)
  (ID 0)
  (ID 0))
T
CL-USER> (undo)
---------------- [GOAL]
[0]:   ⊢  (((A → B) → A) → A)
---------------- [TO-R]
[0]: ((A → B) → A)  ⊢  A
---------------- [TO-L]
[0]:   ⊢  (A → B), A
[1]: A  ⊢  A
---------------- [TO-R]
[0]: A  ⊢  B, A
[1]: A  ⊢  A
---------------- [ID]
[0]: A  ⊢  A
POP: [
---------------- [ID]
#<GOAL: Complete !! >
]
NIL
CL-USER> (proof-hist)
---------------- [GOAL]
[0]:   ⊢  (((A → B) → A) → A)
---------------- [TO-R]
[0]: ((A → B) → A)  ⊢  A
---------------- [TO-L]
[0]:   ⊢  (A → B), A
[1]: A  ⊢  A
---------------- [TO-R]
[0]: A  ⊢  B, A
[1]: A  ⊢  A
---------------- [ID]
[0]: A  ⊢  A
NIL
CL-USER> 
```

## example (writing code style)

please check [examples](/src/examples)
