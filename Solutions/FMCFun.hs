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
curry f = undefined

-- uncurry takes a currified function
-- and returns its "traditional" binary version
uncurry :: (a->b->c) -> ((a, b)->c)
uncurry f = undefined

-- flip takes a (currified) binary function
-- and returns one that behaves the same but takes its arguments in the opposite order

-- (.) takes two composable functions and returns their composition

-- (.>) is composition but in diagramatic notation (should be ; but Haskell forbids)
(.>) = flip (.)

-- ($) takes a function and a suitable argument and applies the function to the argument
-- think: why would we ever want that?

-- iterate: figure it out by its type
-- if this guy         v        was a Num id have an idea...
iterate :: (a -> a) -> a -> [a]
iterate = undefined
-- maybe input/output list...?

-- orbit
orbit = flip iterate

