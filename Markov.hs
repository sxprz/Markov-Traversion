module Markov (main, Markov.add, Markov.sub, Markov.mul, Markov.div, Fraction) where

import Data.Maybe(fromJust)

-- Fraction data type either as a fraction with numerator and denominator or as an int or completly undefined
-- Undefined happens in case the denominator is zero in the simplification
data Fraction = Fraction Int Int | Int Int | Undefined

instance Show Fraction where
    show (Fraction a b) = show a ++ "/" ++ show b 
    show (Int i)        = show i
    show Undefined      = "Undefined"

-- Basic arithmetic for fractions
-- Addition of fractions
add :: Fraction -> Fraction -> Fraction
add (Fraction a b) (Fraction c d) = Fraction ((a*d) + (c*b)) e
    where e = lcm b d
add (Int i) (Fraction a b)        = Fraction (a+(i*b)) b
add (Fraction a b) (Int i)        = Fraction (a+(i*b)) b
add (Int i) (Int j)               = Int (i+j)
add _ _                           = Undefined

-- Subtraction of fractions
sub :: Fraction -> Fraction -> Fraction
sub (Fraction a b) (Fraction c d) = Fraction ((a*d) - (c*b)) e
    where e = lcm b d
sub (Int i) (Fraction a b)        = Fraction ((i*b)-a) b
sub (Fraction a b) (Int i)        = Fraction (a-(i*b)) b
sub (Int i) (Int j)               = Int (i-j)
sub _ _                           = Undefined

-- Multiplication of + some simplification steps for fractions
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

-- Division of fractions
div :: Fraction -> Fraction -> Fraction
div f@(Fraction a b) (Fraction c d) = mul f (Fraction d c)
div (Int i) f@(Fraction _ _)        = Markov.div (Fraction i 1) f
div f@(Fraction _ _) (Int i)        = mul f (Fraction 1 i)
div (Int i) (Int j)                 = Fraction i j
div _ _                             = Undefined

-- Function to simplify fractions that would be otherwise integers, etc.
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

-- Transition data type decoded in a start state transitioning to another state with a certain probability
data Transition = Transition { state1 :: String,
                               probability :: Fraction,
                               state2 :: String }
instance Show Transition where
    show (Transition s1 p s2) = "[" ++ s1 ++ "] -( " ++ show p ++ " )-> [" ++ s2 ++ "]"

-- Functions for getting values out of Transition data type
fstState :: Transition -> String
fstState (Transition a _ _) = a

sndState :: Transition -> String
sndState (Transition _ _ b) = b

probTransition :: Transition -> Fraction
probTransition (Transition _ p _) = p

-- MarkovChain data type consisting of a list of transitions and a list of all states
data MarkovChain = MarkovChain [Transition] [String]

instance Show MarkovChain where
    show (MarkovChain ts ss) = show ss ++ " : " ++ show ts

-- Probability of arriving at a certain state
dstProbability :: MarkovChain -> String -> String -> Fraction
dstProbability = undefined

-- Probability of returning to the same state
retProbability :: MarkovChain -> String -> Fraction
retProbability = undefined

-- Transition time in number of states from state a to b
transitionTime :: MarkovChain -> String -> String -> Int
transitionTime = undefined

-- Return time in number of states from state a to a
returnTime :: MarkovChain -> String -> Int
returnTime = undefined

main :: IO ()
main = undefined


