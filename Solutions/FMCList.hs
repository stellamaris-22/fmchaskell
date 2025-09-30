{-# LANGUAGE GADTs #-}

module FMCList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    , fst, snd
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

-- Sum of all
sum :: Num a => [a] -> a
sum []       = 0
sum (x : xs) = x + sum xs

-- Product of all
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

-- swapped arguments
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


-- First n elements of the list
take :: Integral b => b -> [a] -> [a]
take n xs = fst (take_drop n xs)

--rest of the list after take
drop :: Integral b => b -> [a] -> [a]
drop n xs = snd (take_drop n xs)

-- same but with a predicate 

-- First elements of the list that attend a predicate
take_while :: (a -> Bool) -> [a] -> [a]
take_while p xs = fst (take_drop_while p xs)

--rest of the list after take_while 
drop_while :: (a -> Bool) -> [a] -> [a]
drop_while p xs = snd (take_drop_while p xs)

-- remove one by one 
tails :: [a]  -> [[a]]
tails []     = [[]]
tails (x:xs) = [x:xs] ++ tails xs

-- Same as tail but for head. Maybe worth a future final or smth
init :: [a] -> [a]
init []     = []
init [_]    = []
init (x:xs) = (x:init xs)

-- Same as tails but for init
inits :: [a] -> [[a]]
inits [] = [[]]
inits xs = (inits (init xs))++(xs:[])


-- Powerset of list
subsequences :: [a] -> [[a]]
subsequences [] = [[]] 
subsequences [x] = [[],[x]]
subsequences [x,y] = [[],[x],[y],[x,y]]
subsequences [x,y,z] = [[],[x],[y],[z],[x,y],[y,z],[x,z],[x,y,z]]
--this really feels way too complicated rn...

--im assuming im meant to say if 
--any of the items in the list
--attends a predicate
any :: (a->Bool)->[a]->Bool
any _ [] = False
any p (x:xs) = if p x
               then True
               else any p xs

--im assuming im meant to say if 
--all of the items in the list are
--attends a predicate
all :: (a->Bool)->[a]->Bool
all _ [] = True
all p (x:xs) = if p x 
               then all p xs
               else False

-- For Bool lists only
and :: [Bool] -> Bool
and []       = True
and (x : xs) = x && (and xs)

or :: [Bool] -> Bool
or []       = False
or (x : xs) = x || (or xs)

--list of lists into one only
concat :: [[a]] -> [a]
concat [xs] = xs
concat (xs:xss) = xs ++ (concat xss)

-- True if specific element is in list, else otherwise
-- using the funciton 'any' above
elem :: Eq a => a -> [a] -> Bool
elem x xs = any (==x) xs

-- elem': same as elem but elementary definition
-- (without using other functions except (==))
elem' :: Eq a => a -> [a] -> Bool 
elem' _ [] = False
elem' x (y:ys) = if x == y
                 then True
                 else elem' x ys


-- at operator
(!!) :: Integral b => [a] -> b -> a
[] !! _ = error"Out of bounds!!"
(x:xs) !! n = if n == 0
              then x
              else xs !! (n-1)


-- filter based off of a predicate
filter :: [a] -> (a -> Bool) -> [a]
filter [] _ = []
filter (x:xs) p = if p x
                  then x:(filter xs p)
                  else filter xs p



-- map a list with a function's output
map :: [a] -> (a -> b) -> [b]
map [] _ = []
map (x:xs) f = (f x):(map xs f)

--nice infinite functions...
--endlessly cycling through list
cycle :: [a] -> [a]
cycle [] = []
cycle xs = xs ++ cycle xs

--endlessly repeating an element
repeat :: a -> [a]
repeat x = x:(repeat x)

--finite version of repeat
replicate :: Integral a => a -> b -> [b]
replicate n y = take n (repeat y)
--perfect occasion to put haskells sloth at test

--im *assuming* ill get two lists and see if 
--one is in the beginning/middle/end
--of the other
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf (x:xs) (y:ys) = if x == y
                           then isPrefixOf xs ys
                           else False
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

--simple insertion sort. heard 
--quicksort can be done in two lines...
sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = insert x (sort xs)






--extras i needed

--like head/tail dichotomy but with init
final :: [a] -> a
final [x] = x
final (x:xs) = final (xs)


--used in matrix transposition, gives first line in a matrix
line::[[a]]->[a]
line [] = []
line ([]:xss) = []
line ((x:xs):xss) = (x:line xss)

--used in matrix transposition, removes first line in a matrix
rmv_line::[[a]]->[[a]]
rmv_line [] = []
rmv_line ([]:xss) = [[]]
rmv_line ((_:xs):xss) = (xs:(rmv_line xss))

  
--Doing all the work only once
take_drop:: Integral b => b -> [a] -> ([a], [a])
take_drop _ []     = ([],[])
take_drop 0 xs     = ([],xs)
take_drop n (x:xs) = let next = take_drop (n-1) xs
                     in (x:fst next,snd next)
 
--Doing all the work only once
take_drop_while:: (a -> Bool) -> [a] -> ([a], [a])
take_drop_while _ []     = ([],[])
take_drop_while p (x:xs) = if p x
                           then let next = take_drop_while p xs
                                in (x:fst next,snd next)
                           else ([],x:xs)


--insert on sorted list
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) = if x < y
                  then x:(y:ys)
                  else y:(insert x ys) 
