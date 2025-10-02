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
head l = fst (body l)

tail :: [a] -> [a]
tail l = snd (body l)

null :: [a] -> Bool
null [] = True
null _  = False

length :: [a] -> Int
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
reverse (x : xs) = reverse xs <: x

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
take :: Int -> [a] -> [a]
take n xs = fst (take_drop n xs)

--rest of the list after take
drop :: Int -> [a] -> [a]
drop n xs = snd (take_drop n xs)

-- same but with a predicate 

-- First elements of the list that attend a predicate
take_while :: (a -> Bool) -> [a] -> [a]
take_while p xs = fst (break (not . p) xs)

--rest of the list after take_while 
drop_while :: (a -> Bool) -> [a] -> [a]
drop_while p xs = snd (break (not . p) xs)

-- remove one by one 
tails :: [a]  -> [[a]]
tails []     = [[]]
tails (x:xs) = [x:xs] ++ tails xs

-- Same as tail but for head. Maybe worth a future final or smth
init :: [a] -> [a]
init l = fst (middle l)

-- Same as tails but for init
inits :: [a] -> [[a]]
inits [] = [[]]
inits xs = (inits (init xs))++(xs:[])



-- likely a better way. couldnt think of it.
-- Powerset of list
subsequences :: Eq a => [a] -> [[a]]
subsequences []  = [[]] 
subsequences [x] = [[],[x]]
subsequences (x:xs) = nub ((subsequences xs) ++ (combine x (subsequences xs))) 

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
concat [] = []
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
(!!) :: [a] -> Int -> a
[] !! _ = error"Out of bounds!!"
(x:xs) !! n = if n == 0
              then x
              else xs !! (n-1)


-- filter based off of a predicate
filter :: (a -> Bool) -> [a] -> [a]
filter _ []     = []
filter p (x:xs) = if p x
                  then x:(filter p xs)
                  else filter p xs



-- map a list with a function's output
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = (f x):(map f xs)

--nice infinite functions...
--endlessly cycling through list
cycle :: [a] -> [a]
cycle [] = []
cycle xs = xs ++ cycle xs

--endlessly repeating an element
repeat :: a -> [a]
repeat x = x:(repeat x)

--finite version of repeat
replicate :: Int -> b -> [b]
replicate n y = take n (repeat y)
--perfect occasion to put haskells sloth at test

--gets two lists and see if 
--one is in the beginning/middle/end
--of the other
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _          = True
isPrefixOf _  []         = False
isPrefixOf (x:xs) (y:ys) = if x == y
                           then isPrefixOf xs ys
                           else False

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf [] _     = True
isInfixOf l []     = False
isInfixOf l (y:ys) = if isPrefixOf l (y:ys)
                     then True
                     else isInfixOf l ys

isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf [] _ = True
isSuffixOf _ [] = False
isSuffixOf xs ys = isPrefixOf (reverse xs) (reverse ys)

--turns two lists into a list of pairs
zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x,y):zip xs ys 

-- uses a two-param function to turn the lists into one
zipWith :: (a->b->c)->[a]->[b]->[c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x:xs) (y:ys) = (f x y):(zipWith f xs ys)

-- turns list into one big list with prev lists intercalated by first arg
intercalate :: [a] -> [[a]] -> [a]
intercalate _ [] = []
intercalate [] xss = concat xss
intercalate ys (xs:xss) = if null xss
                          then xs
                          else xs ++ ys ++ (intercalate ys xss)

--removes repeated elements, maintains first
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x:(nub (remove x xs))



splitAt :: Int -> [a] -> ([a],[a])
splitAt n xs = take_drop n xs
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)
-- got no idea... 


--split when predicate is no longer satisfied
break :: (a -> Bool) -> [a] -> ([a],[a])
break _ []     = ([],[])
break p (x:xs) = if p x
                 then ([],x:xs)
                 else let next = break p xs
                      in (x:fst next,snd next)


--string manip

--breaks strings into list of strings on each \n
lines :: String -> [String]
lines "" = []
lines cs = let (f,ss) = break (== '\n') cs 
           in f : case ss of
                    []      -> []
                    (c:cs)  -> lines cs

--breaks strings into list of strings on each whitespace
words :: String -> [String]
words "" = []
words cs = let (f,ss) = break (C.isSpace) cs 
           in f : case ss of
                    []      -> []
                    (c:cs)  -> words cs

--adds '\n' between strings of list until its a big string
unlines :: [String] -> String
unlines [] = ""
unlines l  = intercalate "\n" l 

--adds ' ' between strings of list until its a big string
unwords :: [String] -> String
unwords [] = ""
unwords l  = intercalate " " l 


--matrix transposition
transpose::[[a]]->[[a]]
transpose [] = []
transpose ([]:xss) = transpose xss
transpose xss = [line xss] ++ transpose (rmv_line xss)
--God only knows how this worked

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome []  = True
palindrome [c] = True
palindrome ls  = let s = treat_str ls
                 in s == reverse s

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

--extra from class

--simple insertion sort. heard 
--quicksort can be done in two lines...
sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = insert x (sort xs)

--extras i needed


--like head/tail dichotomy but with init
final :: [a] -> a
final l = snd (middle l)

--used in matrix transposition, gives first line in a matrix
line::[[a]]->[a]
line [] = []
line ([]:xss) = []
line ((x:xs):xss) = (x:line xss)

--used in matrix transposition, removes first line in a matrix
rmv_line::[[a]]->[[a]]
rmv_line [] = []
rmv_line ([]:xss) = rmv_line xss
rmv_line ((_:xs):xss) = (xs:(rmv_line xss))

  
--Doing all the work only once
take_drop:: Int -> [a] -> ([a], [a])
take_drop _ []     = ([],[])
take_drop 0 xs     = ([],xs)
take_drop n (x:xs) = let next = take_drop (n-1) xs
                     in (x:fst next,snd next)

--insert on sorted list
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) = if x < y
                  then x:(y:ys)
                  else y:(insert x ys) 

--auxiliary for subsequence combinations
combine :: a -> [[a]] -> [[a]]
combine n [] = [[n]]
combine n [[]] = [[n]]
combine n (xs:xss) = (n:xs):(combine n xss)

--aux to remove all instances of a an item
remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove n (x:xs) = if n == x
                  then remove n xs
                  else x:(remove n xs)

--removes whitespaces and punctuation from string
--foor palindrome checking
treat_str :: String -> String
treat_str "" = ""
treat_str (c:cs) = if C.isAlpha c
                   then (C.toLower c):(treat_str cs) 
                   else treat_str cs

--head & tail at once
body :: [a] -> (a,[a])
body []     = undefined
body (x:xs) = (x,xs)

--init & final at once
middle :: [a] -> ([a],a)
middle [] = undefined
middle [x] = ([],x)
middle (x:xs) = let (i,f) = middle xs
                in (x:i, f)