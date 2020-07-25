# rm-coder-decoder
A script for coding/decoding register machines as natural numbers. 

Example usage after loading in ghci.
(It might be necessary to add type annotations when decoding)
``` haskell
p = Program [Halt]
encodedP = encode p :: Nat
decode encodedP :: Progam
```
