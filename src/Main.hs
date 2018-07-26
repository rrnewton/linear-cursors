{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

-- |

module Main where

import Control.Exception (evaluate)
import Cursors.Internal.Unsafe (unsafeCastLinear, unsafeCastLinear2)
import Cursors.Mutable as C
import Data.Int
import Data.Typeable
import Foreign.Marshal.Alloc (mallocBytes) -- free
import Foreign.Storable
import GHC.Int
import GHC.Prim (Addr#, (<=#), (+#), (-#),
                 plusAddr#, addr2Int#, int2Addr# )
import GHC.Prim (Int#)
import GHC.Ptr    
import Prelude hiding (($))
import System.IO.Unsafe (unsafePerformIO, unsafeDupablePerformIO)

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

dbgPrint :: String -> IO ()
-- dbgPrint = putStrLn
dbgPrint _ = return ()
    
{-# INLINABLE pop #-}
pop :: forall a . (Show a, Storable a) =>
       Stack a ->. (# Stack a, Maybe (Unrestricted a) #)
pop = unsafeCastLinear f
  where
    !(I# szsz_) = sizeOf (sizeOf ())
    f :: Stack a -> (# Stack a, Maybe (Unrestricted a) #)
    f (# sz,base,offset #) =
      let a1 = plusAddr# base offset
          !(I# valsz#) = unsafeDupablePerformIO (do
                            dbgPrint (" [dbg] Reading length at "++show(Ptr a1))
                            peek (Ptr a1))
      in
        case valsz# of
          -1# -> (# (# sz, base, offset #), Nothing #)
          _   -> 
            let a2 = (Ptr (int2Addr# (addr2Int# a1 -# valsz#)))
                val = unsafeDupablePerformIO
                      (do dbgPrint (" [dbg] reading val of length "++show(I# valsz#)
                                    ++" from addr "++show a2)
                          dbgPrint (" [dbg] decrementing offset by: "++show(I# valsz#)
                                    ++"+"++show(I# szsz_)++" to "++
                                     show (I# ((offset -# valsz#) -# szsz_)))
                          peek a2)
            in
              unsafePerformIO (dbgPrint (" [dbg] val read "++show val)) `seq`
              (# (# sz, base, (offset -# valsz#) -# szsz_ #),
                 Just (Unrestricted val) #)



{-# INLINABLE push #-}
-- | Push an object onto a stack, serializing it and mutating the stack.
push :: forall a . Storable a => a ->. Stack a ->. Stack a
push = unsafeCastLinear2 f
  where
   !(I# szsz_) = sizeOf (sizeOf ())

   f :: a -> Stack a -> Stack a
   f val (# sz,base,offset #) =
     let !szval@(I# szval_) = sizeOf (val::a) in     
     case offset +# szval_ +# szsz_ <=# sz of
       0# -> (error ("push: Stack of size "++show(I# sz)++" ran out of space"))
       _  -> unsafeDupablePerformIO
             (do -- Jump past the already-written size of the LAST field:
                 let a1 = (plusAddr# (plusAddr# base offset) szsz_)
                     a2 = (plusAddr# a1 szval_)
                 poke (Ptr a1) val
                 poke (Ptr a2) szval
                 dbgPrint (" [dbg] Poking at "++show (Ptr a1)++" and "++ show (Ptr a2)
                          ++ " sizes "++show (I# szval_, I# szsz_))
                 return ())
              `seq` (# sz, base, offset +# szsz_ +# szval_  #)

-- | Allocate a fresh stack with a new buffer.    
withStack :: Int -> (Stack a ->. Unrestricted b) ->. Unrestricted b
withStack sz@(I# sz_) = unsafeCastLinear f
 where
   f :: (Stack a ->. Unrestricted b) -> Unrestricted b
   f fn = unsafePerformIO (do {
            -- Alternatives: ByteArray.alloc, U.unsafePackMallocCStringLen ...
            Ptr p <- mallocBytes sz; -- <- DANGER: don't float out.
            poke (Ptr p) (-1 :: Int); -- Signal "end of stack".
            dbgPrint (" [dbg] Poked -1 at "++show (Ptr p));
            res <- evaluate (fn (# sz_, p, 0# #));
            return res;
          })

-- | Deletion frees the storage associated with a stack.
delete :: Stack a ->. ()
delete = unsafeCastLinear (\_ -> ())

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
  f2 (# s2, elt #) = f3 (delete s2) elt

  f3 :: () ->. Maybe (Unrestricted a) ->. Unrestricted a
  f3 () (Just uelt) = uelt
  f3 () Nothing = error "impossible"


