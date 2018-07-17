-- |

module Main where

-- import qualified Mutable as C

----------------------------------------

data Stream a

----------------------------------------

data Stack a 


pop  :: Storable a => Stack a ->. (Stack , Maybe a)
-- push :: Storable a => a ->. Stack a ->. Stack a



f :: Int ->. Int
f x = x    

main = putStrLn (f "hello world")
