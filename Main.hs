module Main where

import Data.Maybe(fromJust)

data Fraction = Fraction Int Int

instance Show Fraction where
    show (Fraction a b) = show a ++ "/" ++ show b 

add :: Fraction -> Fraction -> Fraction
add (Fraction a b) (Fraction c d) = Fraction ((a*d) + (c*b)) e
    where e = lcm b d

sub :: Fraction -> Fraction -> Fraction
sub (Fraction a b) (Fraction c d) = Fraction ((a*d) - (c*b)) e
    where e = lcm b d

mul :: Fraction -> Fraction -> Fraction
mul (Fraction a b) (Fraction c d) = Fraction (a*c) (b*d)

div :: Fraction -> Fraction -> Fraction
div a (Fraction c d) = mul a (Fraction d c)

simplify :: Fraction -> (Fraction, Maybe Int)
simplify f@(Fraction 0 b) = (f, Just 0)
simplify f@(Fraction a b)
    | a == b    = (f, Just 1)
    | otherwise = (f, Nothing)

data Transition = Transition { state1 :: String,
                               probability :: Fraction,
                               state2 :: String }
instance Show Transition where
    show (Transition s1 p s2) = "[" ++ s1 ++ "] -( " ++ show p ++ " )-> [" ++ s2 ++ "]"

newtype MarkovChain = MarkovChain [Transition]

instance Show MarkovChain where
    show (MarkovChain ts) = show ts

main :: IO ()
main = undefined


