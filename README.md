This is the implementation of extended boolean semantics in Haskell.
The parsing is done via GF.
The semantic analysis is done via Haskell.

Some input-output pairs are:
I: 2 is prime
O: "(prime 2)"

I: 2 and 3 are prime
O:"((and (prime 2)) (prime 3))"

I : 2 or 3 is prime
O: "((or (prime 2)) (prime 3))"

I: 2 and 3 or 2 are prime
O: "((and (prime 2)) ((or (prime 3)) (prime 2)))"

