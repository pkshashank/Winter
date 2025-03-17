This is the implementation of extended boolean semantics in Haskell.
The parsing is done via GF.
The semantic analysis is done via Haskell.


Some input-output pairs are:

I: 2 is prime

O: "(prime 2)"


I: 2 and 3 are prime

O:"((and (prime 2)) (prime 3))"


I : 7 and 8 are coprime

O: "(∃x3:Se.((coprime x3) & (((7 ∈ x3) & (8 ∈ x3)) & (∀x4:Se.(((7 ∈ x4) & (8 ∈ x4)) -> (x3 ⊆ x4))))))"
