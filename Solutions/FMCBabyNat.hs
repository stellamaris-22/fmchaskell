module FMCBabyNat where

-- Do not alter this import!
import Prelude ( Show(..) , Eq(..) , undefined )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

-- define a new data type called Nat by listing all forms
data Nat = O | S Nat
  deriving (Eq, Show)

-- some sugar
zero, one, two, three, four, five, six, seven, eight :: Nat
zero  = O
one   = S zero
two   = S one
three = S two
four  = S three
five  = S four
six   = S five
seven = S six
eight = S seven

-- addition
(+) :: Nat -> Nat -> Nat
n + O   = n
n + S m = S (n + m)

 -- syntactic associativity: L
 -- syntactic precedence: 6
infixl 6 +

-- Output: O means False, S O means True
isZero :: Nat -> Nat
isZero O     = S O
isZero (S _) = O

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O     = O
pred (S n) = n

-- Output: O means False, S O means True
even :: Nat -> Nat
even O         = S O
even (S O)     = O
even (S (S n)) = even (n)  

odd :: Nat -> Nat
odd O         = O
odd (S O)     = S O
odd (S (S n)) = odd (n)  

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus O _ = O
monus n O = n
monus (S n) (S m) = monus n m


(-*) :: Nat -> Nat -> Nat
(-*) = monus
infixl 6 -*

-- multiplication
(*) :: Nat -> Nat -> Nat
_ * O     = O
n * (S m) = n * m + n


infixl 7 *

-- exponentiation
(^) :: Nat -> Nat -> Nat
_ ^ O     = S O
n ^ (S m) = n ^ m * n


-- decide: infix? ? ^
infixr 8 ^


-- quotient
(/) :: Nat -> Nat -> Nat 
O / _ = O 
n / m = S ((n -* m) / m) -* (n < m) -- if n<m, (/) is called one time more than it should
infixl 7 /

-- remainder
(%) :: Nat -> Nat -> Nat
O % _ = O
n % m = n -* (n / m) * m
infixl 7 %
 
-- divides
-- just for a change, we start by defining the "symbolic" operator
-- and then define `devides` as a synonym to it
-- again, outputs: O means False, S O means True
(|||) :: Nat -> Nat -> Nat
O ||| _     = O
_ ||| (S O) = S O
n ||| m     = (m % n) < S O --if  remainder is zero, outputs S O; else, outputs O

infixl 5 |||

-- x `absDiff` y = |x - y|
-- (Careful here: this - is the actual minus operator we know from the integers!)
absDiff :: Nat -> Nat -> Nat
absDiff n m = (m -* n) + (n -* m) 

(|-|) :: Nat -> Nat -> Nat
(|-|) = absDiff

factorial :: Nat -> Nat
factorial O     = S O
factorial (S n) = factorial n * S n

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O     = O
sg (S n) = S O
-- never negative

lo :: Nat -> Nat -> Nat
lo O _     = O -- for base-case's sake
lo _ O     = undefined
lo _ (S O) = undefined
lo (S O) _ = O
lo n m     = S (lo (n/m) m) -* (n < m) --if n<m, lo is called one time more than it should

-- extra
-- Output: O means False, S O means True
-- less than
(<)::Nat->Nat->Nat
_ < O = O
O < _ = S O
S n < S m = n < m

infix 5 <