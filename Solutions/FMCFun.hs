{-# LANGUAGE GADTs #-}

module ExList where

import Prelude hiding
    ( (.) , ($)
    , flip , curry , uncurry
    , iterate
    )

-- use your mind to infer the types, don't cheat!

-- curry takes a "traditional" binary function
-- and returns its currified version
curry :: ((a, b)->c) -> (a->b->c)
curry f x y = f (x,y)

-- uncurry takes a currified function
-- and returns its "traditional" binary version
uncurry :: (a->b->c) -> ((a, b)->c)
uncurry f (x,y) = f x y

-- flip takes a (currified) binary function
-- and returns one that behaves the same but takes its arguments in the opposite order
flip :: (a->b->c) -> (b->a->c)
flip f b' a' = f a' b'

-- (.) takes two composable functions and returns their composition
(.) :: (a -> b) -> (b -> c) -> (a -> c)
(f . g) x = g (f x)

-- (.>) is composition but in diagramatic notation (should be ; but Haskell forbids)
(.>) = flip (.)

-- ($) takes a function and a suitable argument and applies the function to the argument
-- think: why would we ever want that?
($) :: (a -> b) -> a -> b
f $ x = f x 
--no idea why...

-- iterate: figure it out by its type
iterate :: (a -> a) -> a -> [a]
iterate f x = (f x):iterate f (f x)
-- maybe input/output list...?

-- orbit
orbit = flip iterate
