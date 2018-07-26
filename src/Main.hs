
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE TypeInType #-}
-- {-# LANGUAGE BangPatterns #-}

module Main where

import Packed.Stack
import Packed.Cursors.Internal.Std (Unrestricted(..))

import Data.Typeable
import Data.Int (Int32)
import Foreign.Storable (Storable)
----------------------------------------
           
main :: IO ()
main = do
  test1 (3.3::Double)
  test1 (12::Int32)
  test1 (44::Int)


       
test1 :: forall a . (Storable a, Show a, Typeable a) => a -> IO ()
test1 inp = do
  putStrLn ("Running stack example: test1")
  let (Unrestricted num) = withStack 1024 f1 
  putStrLn ("  Pushed and popped: " ++ show (num::a) ++ " of type "++ show(typeOf(undefined::a)))
 where 
  f1 :: Stack a ->. Unrestricted a
  f1 s1 = f2 (pop (push (inp) s1))

  f2 :: (# Stack a, Maybe (Unrestricted a) #) ->. Unrestricted a
  f2 (# s2, elt #) = f3 (deleteStack s2) elt

  f3 :: () ->. Maybe (Unrestricted a) ->. Unrestricted a
  f3 () (Just uelt) = uelt
  f3 () Nothing = error "impossible"


