# Claudia

proof assistant ?

```console
CL-USER> (claudia/examples/cpl:law-of-exclded-middle)
↓↓↓ GOAL ↓↓↓ 
H0: 
C0: (A ∨ ¬A)
↓↓↓ CR ↓↓↓ 
H0: 
C0: (A ∨ ¬A), (A ∨ ¬A)
↓↓↓ OR-R1 ↓↓↓ 
H0: 
C0: A, (A ∨ ¬A)
↓↓↓ PR ↓↓↓ 
H0: 
C0: (A ∨ ¬A), A
↓↓↓ OR-R2 ↓↓↓ 
H0: 
C0: ¬A, A
↓↓↓ NOT-R ↓↓↓ 
H0: A
C0: A
↓↓↓ ID ↓↓↓ 
Complete !!
NIL
```
