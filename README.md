# Claudia

proof assistant ?

## examples

### example (on repl style)

```console
CL-USER> (use-package :claudia/api/repl)
T
CL-USER> (start-proof (→ (→ (→ a b) a) a) :props (a b))
PROP: A: #<Prop: A>
PROP: B: #<Prop: B>
---------------- [GOAL]
[0]:   ⊢  (((A → B) → A) → A)
#<GOAL: [0]:   ⊢  (((A → B) → A) → A) >
CL-USER> (to-r)
---------------- [TO-R]
[0]: ((A → B) → A)  ⊢  A
#<GOAL: [0]: ((A → B) → A)  ⊢  A >
CL-USER> (to-l)
---------------- [TO-L]
[0]:   ⊢  (A → B), A
[1]: A  ⊢  A
#<GOAL: [0]:   ⊢  (A → B), A
[1]: A  ⊢  A >
CL-USER> (to-r)
---------------- [TO-R]
[0]: A  ⊢  B, A
[1]: A  ⊢  A
#<GOAL: [0]: A  ⊢  B, A
[1]: A  ⊢  A >
CL-USER> (id)
---------------- [ID]
[0]: A  ⊢  A
#<GOAL: [0]: A  ⊢  A >
CL-USER> (id)
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

### example (writing code style)

please check [examples](/src/examples)
