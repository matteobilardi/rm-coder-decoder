{-# LANGUAGE FlexibleInstances #-}
module RMEncoderDecoder where

type Nat = Integer

data SingleBracketPair = Nat :<>: Nat -- <x, y>
data DoubleBracketPair = Nat :<<>>: Nat -- <<x, y>>

newtype Label = L Nat 
    deriving (Eq, Show)

newtype Register = R Nat 
    deriving (Eq, Show)

data Body = Halt 
          | Plus Register Label 
          | Minus Register (Label, Label)
    deriving (Eq, Show)

newtype Program = Program [Body] 
    deriving (Eq, Show)

class Codable c where
    encode :: c -> Nat
    decode :: Nat -> c

instance Codable Body where 
    encode Halt                            = 0
    encode (Plus (R reg) (L dst))          = encode ((2 * reg) :<<>>: dst) 
    encode (Minus (R reg) (L dst, L dst')) = encode ((2 * reg + 1) :<<>>: encode (dst :<>: dst'))

    decode 0 = Halt
    decode n
        | even x     = Plus (R (x `div` 2)) (L y)
        | otherwise  = Minus (R ((x - 1) `div` 2)) (L l, L l')
        where 
            x :<<>>: y = decode n
            l :<>: l'  = decode y

instance Codable [Nat] where 
    encode = foldr (\x y -> encode (x :<<>>: y)) 0

    decode 0 = []
    decode n = x : decode l
        where x :<<>>: l = decode n
            
instance Codable Program where 
    encode (Program bs) = encode encodedBs
        where
            encodedBs :: [Nat] 
            encodedBs = map encode bs

    decode n = Program (map decode decodedList)
        where 
            decodedList :: [Nat] 
            decodedList = decode n

highestPowerOf2DivisorLog :: Nat -> Nat
highestPowerOf2DivisorLog 0 = error "0 is invalid input"
highestPowerOf2DivisorLog n
    | even n    = 1 + highestPowerOf2DivisorLog (n `div` 2)
    | otherwise = 0

instance Codable DoubleBracketPair where
    encode (x :<<>>: y) = 2 ^ x * (2 * y + 1)

    decode n = x :<<>>: y
        where 
            x = highestPowerOf2DivisorLog n
            y = (n `div` (2 ^ x) - 1) `div` 2

instance Codable SingleBracketPair where
    encode (x :<>: y) = 2 ^ x * (2 * y + 1) - 1

    decode n = x :<>: y
        where x :<<>>: y = decode (n + 1)
