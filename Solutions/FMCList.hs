{-# LANGUAGE GADTs #-}

module FMCList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C

{- import qualified ... as ... ?

To use a function from a qualified import
you need to prefix its name with its alias and a dot:
P.head   C.toUpper   etc.

I import these for you to test the original functions on ghci:

ghci> :t C.toUpper
C.toUpper :: Char -> Char

You MUST NOT use ANY of these in your code

-}


{- Our lists vs Haskell lists

Our definition:

data List a where
  Nil  :: List a
  Cons :: a -> List a -> List a

Here we use Haskell's built-in lists and associated syntactic sugar.
It is as if it was defined like this:

    data [a] = [] | (x : xs)

or like this:

    data [a] where
      []  :: [a]
      (:) :: a -> [a] -> [a]

write [a]       for our List a
write []        for our List
write []        for our Nil
write (x : xs)  for our Cons x xs
write [u,v]     for our u `Cons` (v `Cons` Nil)

-}

head :: [a] -> a
head [x]      = x
head (x : _) = x

tail :: [a] -> [a]
tail []       = []
tail (_ : xs) = xs

null :: [a] -> Bool
null [] = True
null _  = False

length :: Integral i => [a] -> i
length []       = 0
length (_ : xs) = 1 + length xs

-- Sum of all?
sum :: Num a => [a] -> a
sum []       = 0
sum (x : xs) = x + sum xs

-- Product of all?
product :: Num a => [a] -> a
product []       = 1
product (x : xs) = x * product xs

-- List with all items but in reverse order
reverse :: [a] -> [a]
reverse []       = []
reverse (x : xs) = reverse xs ++ (x : [])

(++) :: [a] -> [a] -> [a]
[] ++ l       = l;
l ++ []       = l;
(x : xs) ++ l = (x : (xs ++ l))

-- right-associative for performance!
-- (what?!)
infixr 5 ++
-- makes sense bcs on this format of 
-- construction the list is on the right
-- and new elements come from the left

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc x [] = [x]
snoc x y  = y ++ [x]
-- is it supposed to put the element on 
-- the end of the list...?

-- swapped arguments?
(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?!)
infixl 5 +++
-- makes sense cause it inserts elements 
-- from the right

minimum :: Ord a => [a] -> a
minimum [x]      = x 
minimum (x : xs) = if x <= (minimum xs)
                   then x
                   else minimum xs

maximum :: Ord a => [a] -> a
maximum [x]      = x 
maximum (x : xs) = if x > (maximum xs)
                   then x
                   else maximum xs

-- signatures from line 220
-- take a -> [a] -> ???
-- drop a -> [a] -> ???

-- ???
-- takeWhile
-- dropWhile

-- ???
-- tails
-- init
-- inits

-- half-sure...but sure of signature
subsequences :: [a] -> [a] -> [a]
subsequences = undefined

--im assuming im meant to say if 
--any of the items in the list is
--same as first argument
any :: Eq a => a->[a]->Bool
any _ [] = False
any x (y:ys) = if x == y
               then True
               else any x ys

--im assuming im meant to say if 
--all of the items in the list are
--same as first argument
all :: Eq a => a->[a]->Bool
all _ [] = True
all x (y:ys) = if x /= y
               then False
               else all x ys

--these feel weirdly like number abuse
--or used only for Bool lists
--which means prolly both my guesses 
--are wrong :p
-- and
-- or

--in my mind thats the same as ++
-- concat

-- ??
-- elem using the funciton 'any' above

-- elem': same as elem but elementary definition
-- (without using other functions except (==))

-- ...what?
-- (!!)

-- alter list? hash-table kinda map?
-- filter
-- map

--to know if there's a "cycle"...? repetitions, replicas?
-- cycle
-- repeat
-- replicate

--im *assuming* ill get two lists and see if 
--one is in the beginning/middle/end
--of the other
-- isPrefixOf
-- isInfixOf
-- isSuffixOf

--just...what...
-- zip
-- zipWith

--merge kind of thing? intercalate two lists?
-- intercalate
-- nub

--no idea what take/drop are but hey 
--got their signatures
-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

--is this a break or a function?
-- break

--lists...of lists...somehow...?
-- lines
-- words
-- unlines
-- unwords

--auxiliary
line::[[a]]->[a]
line [] = []
line ([]:xss) = []
line ((x:xs):xss) = (x:line xss)

rmv_line::[[a]]->[[a]]
rmv_line [] = []
rmv_line ([]:xss) = [[]]
rmv_line ((_:xs):xss) = (xs:(rmv_line xss))

--def matrix transposition prolly can do
transpose::[[a]]->[[a]]
transpose [] = []
transpose ([]:_) = []
transpose xss = ([] <: line xss) ++ transpose (rmv_line xss)
--God only knows how this worked

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome [] = True
palindrome s  = s == reverse s 
-- my best for now but im pretty sure ill
-- be able to use recursion by extracting last
-- and comparing to first and doing && 
-- with palindrome rest-of-the-list

-- also are we supposed to ignore punctuation...?
{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

--extra from class
get:: Integral b => b->[a]->a
get _ []     = undefined
get x (y:ys) = if x == 0
               then y
               else get (x-1) ys