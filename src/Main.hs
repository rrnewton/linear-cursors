{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |

module Main where

import Cursors.Mutable as C
import Cursors.Internal.Unsafe (unsafeCastLinear, unsafeCastLinear2)

import Foreign.Storable
import Data.Int (Int32)    
import GHC.Prim (Int#)

-- UNSAFE:    
import qualified Data.ByteString.Unsafe as U
import GHC.ForeignPtr (ForeignPtr(..))
-- import GHC.Ptr    
-- import Foreign.C.Types (CChar)
import Control.Exception (evaluate)

import Control.DeepSeq
import Data.ByteString (ByteString)
-- import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as U
-- import Data.Word
import GHC.Int
import Foreign.Storable
import Foreign.Marshal.Alloc (mallocBytes,free)
import GHC.Ptr    
import Foreign.C.Types (CChar)
import Prelude hiding (($))
-- import GHC.Types(RuntimeRep, Type)
import GHC.Prim (Addr#, (<=#), (+#))
import Data.ByteString.Internal (ByteString(..))
import GHC.ForeignPtr (ForeignPtr(..))
import System.IO.Unsafe (unsafePerformIO, unsafeDupablePerformIO)


----------------------------------------

-- | A series of serialized objects of the same type.
-- type Stream a = Has# '[Int32]
    

----------------------------------------

-- | A stack data structure, packed into a single flat buffer.
-- 
-- Internal notes: We represent objects as [<data>,<size>]*, with the
-- size coming after the data.  We leave the pointer at the size of
-- the last-written object.  This tells us how much to "rewind".
-- 
type Stack a = (# Int#, Addr#, Int# #) -- size, base, offset
-- type Stack a = Has# '[Int32]
-- data Stack a = Stack (Has# '[Int32])
-- type Stack a = Has '[Int32]

    
{-# INLINABLE pop #-}
pop :: Storable a => Stack a ->. (# Stack a, Maybe a #)
pop = undefined
  where
    s2 = undefined

{-# INLINABLE push #-}
-- | Push an object onto a stack, serializing it and mutating the stack.
push :: forall a . Storable a => a ->. Stack a ->. Stack a
push = unsafeCastLinear2 f
  where       
   f :: a -> Stack a -> Stack a
   f val (# sz,base,offset #) =
     let !(I# szval) = sizeOf (val::a) in
     case offset +# szval <=# sz of
       0# -> (error ("push: Stack of size "++show(I# sz)++" ran out of space"))
       _  -> unsafeDupablePerformIO (poke (Ptr base `plusPtr` I# offset) val) `seq`
             (# sz, base, offset +# szval #)

-- | Allocate a fresh stack with a new buffer.    
withStack :: Int -> (Stack a ->. Unrestricted b) ->. Unrestricted b
withStack sz@(I# sz_) = unsafeCastLinear f
 where
   f :: (Stack a ->. Unrestricted b) -> Unrestricted b
   f fn = unsafePerformIO (do {
            -- ByteArray.alloc regionSize $ \ bs -> fn (Needs bs)
            -- DANGER: don't float out:
            Ptr p <- mallocBytes sz;
            -- res <- evaluate (fn (# 0#, p #));
            res <- evaluate (fn (# sz_, p, 0# #));
            -- bs  <- U.unsafePackMallocCStringLen ( Ptr p, sz );
            -- res <- evaluate (fn (Has bs));
            -- free (Ptr p);
            return res;
          })

-- | Deletion frees the storage associated with a stack.
delete :: Stack a ->. ()
delete = unsafeCastLinear (\_ -> ())

----------------------------------------

-- | Drop bytes towards the "right" end 
dropEnd :: C.Has '[(Int32,a)] -> C.Has '[(Int32,a)]
dropEnd (C.Has (PS (ForeignPtr addr _) (I# offset) _)) =
    undefined
           
main = do
  putStrLn ("Running stack example.")
  let (Unrestricted num) = withStack 1024 f1 
  putStrLn ("Got number: " ++ show (num::Int))
 where
   f1 s1 = f2 (pop (push (33::Int) s1))
   f2 (# s2, elt #) = f3 (delete s2) elt
   f3 () (Just elt) = Unrestricted elt
