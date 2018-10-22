-- CS 440 Fall 2018
-- for homework 3

-- Instructions: Fill in the missing parts below.  Rename this file to
-- Maybe_State.hs so that loading the test file can find this file.
-- Also fill in the spot below with the names of your team.  Also delete
-- these instructions before handing in your file.  Note this file won't
-- compile as is.

-- NAMES: 

----------------------------------------------------------------------
module Maybe_State (Mstate(..), apply, fmap, pure, (<*>), (>>=), empty, (<|>)) where

-- Modify State monad to make state function return a Maybe (value, state) instead
-- of a plain (value, state) pair.

import Control.Applicative
import Data.Char

-- A maybe state value is a wrapped function that takes a
-- state and returns Nothing or Just (value, state) pair
--
newtype Mstate state val = Mstate (state -> Maybe(val, state)) 

apply (Mstate f) = f  -- unwrap a maybe state value to reveal function

-- Class types for Mstate

instance Functor (Mstate state) where
    -- fmap :: (a -> b) -> Mstate state a -> Mstate state b
    fmap a_b state_a = Mstate(\state1 ->  Just(a_b (fmap (fst) (apply(state_a) "") :: Maybe a)), state1)
        -- Feel free to rename the parameters (a_b, state_a, state1) if you like.

        -- *** Fill In ***

        
{-
instance Applicative (Mstate state) where
    -- pure :: a -> Mstate state a
    pure v = Mstate (\state -> Just (v, state))

    -- (<*>) :: Mstate state (a -> b) -> Mstate state a -> Mstate state b
    state_a_b <*> state_a = Mstate(\state1 ->

        -- *** Fill In ***        

        )

instance Monad (Mstate state) where
    -- (>>=) :: Mstate state a -> (a -> Mstate state b) -> Mstate state b
   state_a >>= f = Mstate (\state ->

        -- *** Fill In ***

        )

-- Choose between two states: If p and q are maybe states, then
-- p <|> q is a maybe state that chooses the first of p or q that
-- returns Just a value.  (If p returns Nothing, it returns what q
-- returns; if running p returns Just a value, we just return that
-- without running q.)
--
instance Alternative (Mstate state) where
    -- empty :: Mstate state a
    empty = Mstate (\state -> Nothing)

    -- (<|>) :: Mstate s a -> Mstate s a -> Mstate s a
    p <|> q = Mstate (\state -> case apply p state of

        -- *** Fill In ***

    )-}