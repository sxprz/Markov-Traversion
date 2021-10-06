module Markov (main, Markov.add, Markov.sub, Markov.mul, Markov.div, Fraction) where

import Data.Maybe(fromJust)

data Fraction = Fraction Int Int | Int Int | Undefined

instance Show Fraction where
    show (Fraction a b) = show a ++ "/" ++ show b 
    show (Int i)        = show i
    show Undefined      = "Undefined"

add :: Fraction -> Fraction -> Fraction
add (Fraction a b) (Fraction c d) = Fraction ((a*d) + (c*b)) e
    where e = lcm b d
add (Int i) (Fraction a b)        = Fraction (a+(i*b)) b
add (Fraction a b) (Int i)        = Fraction (a+(i*b)) b
add (Int i) (Int j)               = Int (i+j)
add _ _                           = Undefined

sub :: Fraction -> Fraction -> Fraction
sub (Fraction a b) (Fraction c d) = Fraction ((a*d) - (c*b)) e
    where e = lcm b d
sub (Int i) (Fraction a b)        = Fraction ((i*b)-a) b
sub (Fraction a b) (Int i)        = Fraction (a-(i*b)) b
sub (Int i) (Int j)               = Int (i-j)
sub _ _                           = Undefined

mul :: Fraction -> Fraction -> Fraction
mul (Fraction 1 b) (Fraction c 1) = Fraction c b
mul (Fraction a 1) (Fraction 1 d) = Fraction a d
mul (Fraction a b) (Fraction c d)
    | gcd_ad > 1 = simplify $ mul (Fraction (Prelude.div a gcd_ad) b) (Fraction c (Prelude.div d gcd_ad))
    | gcd_bc > 1 = simplify $ mul (Fraction a (Prelude.div b gcd_bc)) (Fraction (Prelude.div c gcd_bc) d)
    | otherwise = Fraction (a*c) (b*d)
    where gcd_ad = gcd a d
          gcd_bc = gcd b c
mul (Int i) (Fraction a b)        = Fraction (a*i) b
mul (Fraction a b) (Int i)        = Fraction (a*i) b
mul (Int i) (Int j)               = Int (i*j)
mul _ _                           = Undefined

div :: Fraction -> Fraction -> Fraction
div f@(Fraction a b) (Fraction c d) = mul f (Fraction d c)
div (Int i) f@(Fraction _ _)        = Markov.div (Fraction i 1) f
div f@(Fraction _ _) (Int i)        = mul f (Fraction 1 i)
div (Int i) (Int j)                 = Fraction i j
div _ _                             = Undefined

simplify :: Fraction -> Fraction
simplify i@(Int a)        = i
simplify f@(Fraction 0 b) = Int 0
simplify f@(Fraction a b)
    | b == 0       = Undefined
    | a == b       = Int 1
    | b == 1       = Int a
    | mod b a == 0 = Fraction 1 (Prelude.div b a)
    | mod a b == 0 = Int (Prelude.div a b)
    | otherwise    = f
simplify _         = Undefined

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


