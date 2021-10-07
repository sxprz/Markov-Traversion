module Markov (main, Markov.add, Markov.sub, Markov.mul, Markov.div, Transition, create, addTransition) where

import Data.Maybe(fromJust)
import Data.List(nub)

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
add (Fraction a b) (Fraction c d) = Fraction (a*d + c*b) e
    where e = lcm b d
add (Int i) (Fraction a b)        = Fraction (a+i*b) b
add (Fraction a b) (Int i)        = Fraction (a+i*b) b
add (Int i) (Int j)               = Int (i+j)
add _ _                           = Undefined

-- Subtraction of fractions
sub :: Fraction -> Fraction -> Fraction
sub (Fraction a b) (Fraction c d) = Fraction (a*d - c*b) e
    where e = lcm b d
sub (Int i) (Fraction a b)        = Fraction (i*b-a) b
sub (Fraction a b) (Int i)        = Fraction (a-i*b) b
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

-- Compares the state names in a transition
cmpStates :: String -> String -> Transition -> Bool
cmpStates a b (Transition c _ d) = a == c && b == d

cmpStatesAndRetProb :: String -> String -> Transition -> Fraction
cmpStatesAndRetProb a b (Transition c p d) = if a == c && b == d then p else Int 0

checkAbsorption :: MarkovChain -> String -> Bool
checkAbsorption m a = let ts = findTransitions m a in length ts == 1 && cmpStates a a(head ts)

-- MarkovChain data type consisting of a list of transitions and a list of all states
data MarkovChain = MarkovChain [Transition] [String]

instance Show MarkovChain where
    show (MarkovChain ts ss) = show ss ++ " : " ++ show ts

-- MarkovChain data type is not reachable from outside,
-- so this helps instancing one
create :: [Transition] -> MarkovChain
create []  = MarkovChain [] []
create [t] = MarkovChain [t] $ nub [fstState t, sndState t]
create ts  = MarkovChain ts $ nub([(fstState t, sndState t) | t <- ts] >>= (\(x,y) -> [x,y]))

create' :: [(String, String, Int, Int)] -> MarkovChain
create' list = create [Transition a (simplify (Fraction c d)) b | (a,b,c,d) <- list]

addTransition :: MarkovChain -> Transition -> MarkovChain
addTransition (MarkovChain ts ss) t@(Transition a p b) = MarkovChain (t:ts) $ nub (ss ++ [a,b])

-- Finder functions for transitions in Markov chain
-- Finds the first transition that passes the requirements that a and b must be the states of the transition
-- Returns a Maybe type
findTransition :: MarkovChain -> String -> String -> Maybe Transition
findTransition (MarkovChain ts _) a b = if null found then Nothing else Just (head found)
    where found = [t | t <- ts, cmpStates a b t]

findTransitionAndRetProb :: MarkovChain -> String -> String -> Fraction
findTransitionAndRetProb (MarkovChain ts _) a b = if null found then Int 0 else (probTransition . head) found
    where found = [t | t <- ts, cmpStates a b t]

findTransitions :: MarkovChain -> String -> [Transition]
findTransitions (MarkovChain ts _) a = [t | t@(Transition a' _ _) <- ts, a' == a]

data Step = Step String String deriving Eq

instance Show Step where
    show (Step s s') = "f_" ++ s ++ "," ++ s'

data EquationSys = Step :=: [Step]

instance Show EquationSys where
    show (s :=: ss) = show s ++ " = " ++ prettyPrintEquation ss

prettyPrintEquation :: [Step] -> String
prettyPrintEquation []     = []
prettyPrintEquation [s]    = show s
prettyPrintEquation (s:ss) = (show s++"+") ++ prettyPrintEquation ss 

buildEquationSys :: MarkovChain -> String -> String -> EquationSys
buildEquationSys m a b = Step a b :=: (if absorpCond then [Step "absorbed" b] else eliminateAllAbsorptions m (f m a b 0))
    where absorpCond = checkAbsorption m a

f :: MarkovChain -> String -> String -> Int -> [Step]
f m@(MarkovChain ts ss) a b 1 = [Step k b | k <- ss, k /= b]
f m@(MarkovChain ts ss) a b depth = concat [f m k b (depth+1) | k <- ss, k /= b && not (checkAbsorption m k)]

eliminateAllAbsorptions :: MarkovChain -> [Step] -> [Step]
eliminateAllAbsorptions m steps = [step | step@(Step a b) <- steps, not $ checkAbsorption m a]

-- Probability of arriving at a certain state
dstProbability :: MarkovChain -> String -> String -> Fraction
dstProbability m a b = undefined

-- Probability of returning to the same state
retProbability :: MarkovChain -> String -> Fraction
retProbability m a = undefined

{-f :: MarkovChain -> String -> String -> [Fraction] -> Fraction
f m@(MarkovChain ts ss) a b
    | a == b              = Int 1
    | checkAbsorption m a = Int 0
    | otherwise           = add (findTransitionAndRetProb m a a)
                            $ foldl add (Int 0) [mul (findTransitionAndRetProb m b k) (f m k b) | k <- ss, k /= b]-}

-- Transition time in number of states from state a to b
transitionTime :: MarkovChain -> String -> String -> Fraction
transitionTime m a b = undefined

-- Return time in number of states from state a to a
returnTime :: MarkovChain -> String -> Fraction
returnTime m a = undefined

main :: IO ()
main =  do
        let m = create' [("0","1",1,2),("1","0",1,2),("0","2",1,2),("2","0",1,2),("1","3",1,2),("2","3",1,2),("3","3",1,1)]
        print (buildEquationSys m "1" "0")
