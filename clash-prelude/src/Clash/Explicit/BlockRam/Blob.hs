{-|
Copyright  :  (C) 2021     , QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

= Efficient bundling of initial RAM content with the compiled code

Leveraging Template Haskell, the initial content for the blockRAM components in
this module is stored alongside the compiled Haskell code. It covers use cases
where passing the initial content as a 'Clash.Sized.Vector.Vec' turns out to be
inefficient.

The data is stored efficiently, with very little overhead (worst-case 7%, often
0%).

-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- {-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
-- {-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
-- {-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

-- See: https://github.com/clash-lang/clash-compiler/commit/721fcfa9198925661cd836668705f817bddaae3c
-- as to why we need this.
{-# OPTIONS_GHC -fno-cpr-anal #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Explicit.BlockRam.Blob
  ( blockRamBlob
  , MemBlob
  , createMemBlob
  , unpackMemBlob
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Unsafe as B
import GHC.Exts (Addr#)
import GHC.Stack (HasCallStack) -- , withFrozenCallStack)
import GHC.TypeLits (KnownNat)
import Language.Haskell.TH
  (Dec, integerL, litE, litT, mkName, normalB, numTyLit, Q, sigD, stringPrimL,
   valD, varP)
import System.IO.Unsafe (unsafePerformIO)

import Clash.Explicit.BlockRam.Internal (MemBlob(..), packBVs, unpackNats)
import Clash.Explicit.Signal (Enable, KnownDomain)
import Clash.Promoted.Nat (natToInteger, natToNum, SNat(..))
import Clash.Signal.Internal (Clock, Signal(..))
import Clash.Sized.Internal.BitVector (Bit(..), BitVector(..))

-- | Create a blockRAM with space for @n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is /undefined/, reading it will throw an
-- 'Clash.XException.XException'
--
blockRamBlob
  :: forall dom n m addr
   . KnownDomain dom
  => KnownNat n
  => KnownNat m
  => Enum addr
  => HasCallStack
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Enable dom
  -- ^ Global enable
  -> MemBlob n m
  -- ^ Initial content of the BRAM, also determines the size, @n@, of the BRAM.
   --
   -- __NB__: __MUST__ be a constant.
  -> Signal dom addr
  -- ^ Read address @r@
  -> Signal dom (Maybe (addr, BitVector m))
  -- ^ (write address @w@, value to write)
  -> Signal dom (BitVector m)
  -- ^ Value of the @blockRAM@ at address @r@ from the previous clock cycle
blockRamBlob = \clk gen content rd wrM -> undefined
{-# INLINE blockRamBlob #-}

-- | Create a 'MemBlob' from a list of values
--
-- Since this uses Template Haskell, the list of values given as the final
-- argument cannot refer to anything defined in the same module.
--
-- === __Example__
--
-- @
-- 'createMemBlob' "content" 'Nothing' [ 15 :: 'BitVector' 8 .. 17 ]
--
-- ram clk en = 'blockRamBlob' clk en content
-- @
--
-- The @Maybe@ datatype has don't care bits, where the actual value does not
-- matter. But the bits need a defined value in the memory. Either 0 or 1 can be
-- used, and both are valid representations of the data.
--
-- >>> import qualified Prelude as P
-- >>> let es = P.map pack [ Nothing, Just (7 :: Unsigned 8), Just 8 ]
-- >>> :{
-- createMemBlob "content0" (Just 0) es
-- createMemBlob "content1" (Just 1) es
-- x = 1
-- :}
-- >>> let pr = mapM_ (putStrLn . show)
-- >>> pr es
-- 0b0_...._....
-- 0b1_0000_0111
-- 0b1_0000_1000
-- >>> pr $ unpackMemBlob content0
-- 0b0_0000_0000
-- 0b1_0000_0111
-- 0b1_0000_1000
-- >>> pr $ unpackMemBlob content1
-- 0b0_1111_1111
-- 0b1_0000_0111
-- 0b1_0000_1000
--
-- Note how we hinted to @clashi@ that our multi-line command was a list of
-- declarations by including a dummy declaration @x = 1@. Without this trick,
-- @clashi@ would expect an expression and the Template Haskell would not work.
createMemBlob
  :: forall m f
   . ( HasCallStack
     , Foldable f
     , KnownNat m
     )
  => String
  -- ^ Name of the binding to generate
  -> Maybe Bit
  -- ^ Value to map don't care bits to. 'Nothing' means throwing an error on
  -- don't care bits.
  -> f (BitVector m)
  -- The content of the MemBlob
  -> Q [Dec]
createMemBlob name care es = sequence
  [ sigD name0 [t| MemBlob $(n) $(m) |]
  , valD (varP name0) (normalB [| MemBlob SNat SNat $(runsLen) $(runs)
                                  $(endsLen) $(ends) |]) []
  ]
 where
  name0 = mkName name
  n = litT . numTyLit $ toInteger len
  m = litT . numTyLit $ natToInteger @m
  runsLen = litE . integerL . toInteger $ L.length runsB
  runs = litE . stringPrimL $ L.unpack runsB
  endsLen = litE . integerL . toInteger $ L.length endsB
  ends = litE . stringPrimL $ L.unpack endsB
  (len, runsB, endsB) = packBVs care es

-- | Convert a 'MemBlob' back to a list
--
-- __NB__: Not synthesizable
unpackMemBlob
  :: forall n m
   . MemBlob n m
  -> [BitVector m]
unpackMemBlob (MemBlob SNat SNat runsLen runs endsLen ends)
  = map (BV 0) $
      unpackNats (natToNum @n) (natToNum @m) runsB endsB
 where
  runsB = unsafePerformIO $ B.unsafePackAddressLen runsLen runs
  endsB = unsafePerformIO $ B.unsafePackAddressLen endsLen ends
