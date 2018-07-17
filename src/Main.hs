-- |

module Main where

-- import qualified Cursors.Mutable as C
import qualified Linear.Common
-- import qualified Linear.Std
-- import qualified Linear.Unsafe
import Foreign.Storable

----------------------------------------

data Stream a

----------------------------------------

data Stack a 


pop  :: Storable a => Stack a ->. (Stack a, Maybe a)
pop = undefined

-- push :: Storable a => a ->. Stack a ->. Stack a


f :: String ->. String
f x = x    

main = putStrLn (f "hello world")
