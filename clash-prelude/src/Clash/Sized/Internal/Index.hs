{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2016-2019, Myrtle Software Ltd,
                  2021     , QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE Unsafe #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

{-# OPTIONS_HADDOCK show-extensions not-home #-}

module Clash.Sized.Internal.Index
  ( -- * Datatypes
    Index (..)
    -- * Construction
  , fromSNat
  -- * Accessors
  -- ** Length information
  , size#
    -- * Type classes
    -- ** BitPack
  , pack#
  , unpack#
    -- ** Eq
  , eq#
  , neq#
    -- ** Ord
  , lt#
  , ge#
  , gt#
  , le#
    -- ** Enum (not synthesizable)
  , enumFrom#
  , enumFromThen#
  , enumFromTo#
  , enumFromThenTo#
    -- ** Bounded
  , maxBound#
    -- ** Num
  , (+#)
  , (-#)
  , (*#)
  , negate#
  , fromInteger#
    -- ** ExtendingNum
  , plus#
  , minus#
  , times#
    -- ** Integral
  , quot#
  , rem#
  , toInteger#
    -- ** Resize
  , resize#
  )
where

import Prelude hiding             (even, odd)

import Control.DeepSeq            (NFData (..))
import Data.Bits                  (Bits (..), FiniteBits (..))
import Data.Data                  (Data)
import Data.Default.Class         (Default (..))
import Text.Read                  (Read (..), ReadPrec)
import Text.Printf                (PrintfArg (..), printf)
import Data.Ix                    (Ix(..))
import Language.Haskell.TH        (appT, conT, litT, numTyLit, sigE)
import Language.Haskell.TH.Syntax (Lift(..))
#if MIN_VERSION_template_haskell(2,16,0)
import Language.Haskell.TH.Compat
#endif
#if MIN_VERSION_template_haskell(2,17,0)
import Language.Haskell.TH        (Quote, Type)
#else
import Language.Haskell.TH        (TypeQ)
#endif
import GHC.Generics               (Generic)
import GHC.Natural                (Natural, naturalFromInteger)
import GHC.Natural                (naturalToInteger)
import GHC.Stack                  (HasCallStack)
import GHC.TypeLits               (KnownNat, Nat, type (+), type (-),
                                   type (*), type (<=), natVal)
import GHC.TypeLits.Extra         (CLog)
import Test.QuickCheck.Arbitrary  (Arbitrary (..), CoArbitrary (..),
                                   arbitraryBoundedIntegral,
                                   coarbitraryIntegral, shrinkIntegral)

import Clash.Class.BitPack.Internal (BitPack (..), packXWith)
import Clash.Class.Num            (ExtendingNum (..), SaturatingNum (..),
                                   SaturationMode (..))
import Clash.Class.Parity         (Parity (..))
import Clash.Class.Resize         (Resize (..))
import Clash.Class.BitPack.BitIndex (replaceBit)
import {-# SOURCE #-} Clash.Sized.Internal.BitVector (BitVector (BV), high, low, undefError)
import qualified Clash.Sized.Internal.BitVector as BV
import Clash.Promoted.Nat         (SNat(..), snatToNum, natToInteger, leToPlusKN)
import Clash.XException
  (ShowX (..), NFDataX (..), errorX, showsPrecXWith, rwhnfX)

{- $setup
>>> import Clash.Sized.Internal.Index
-}

type role Index nominal

-- | Arbitrary-bounded unsigned integer represented by @ceil(log_2(n))@ bits.
--
-- Given an upper bound @n@, an 'Index' @n@ number has a range of: [0 .. @n@-1]
--
-- >>> maxBound :: Index 8
-- 7
-- >>> minBound :: Index 8
-- 0
-- >>> read (show (maxBound :: Index 8)) :: Index 8
-- 7
-- >>> 1 + 2 :: Index 8
-- 3
-- >>> 2 + 6 :: Index 8
-- *** Exception: X: Clash.Sized.Index: result 8 is out of bounds: [0..7]
-- ...
-- >>> 1 - 3 :: Index 8
-- *** Exception: X: Clash.Sized.Index: result -2 is out of bounds: [0..7]
-- ...
-- >>> 2 * 3 :: Index 8
-- 6
-- >>> 2 * 4 :: Index 8
-- *** Exception: X: Clash.Sized.Index: result 8 is out of bounds: [0..7]
-- ...
--
-- Index has the <https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#roles type role>
--
-- >>> :i Index
-- type role Index nominal
-- ...
--
-- as it is not safe to coerce between different range Index. To change the
-- size, use the functions in the 'Clash.Class.Resize.Resize' class.
#if MIN_VERSION_base(4,15,0)
data Index (n :: Nat) =
    -- | The constructor, 'I', and the field, 'unsafeToInteger', are not
    -- synthesizable.
    I { unsafeToInteger :: !Integer }
#else
newtype Index (n :: Nat) =
    -- | The constructor, 'I', and the field, 'unsafeToInteger', are not
    -- synthesizable.
    I { unsafeToInteger :: Integer }
#endif
  deriving (Data, Generic)

{-# NOINLINE size# #-}
size# :: (KnownNat n, 1 <= n) => Index n -> Int
size# = BV.size# . pack#

instance NFData (Index n) where
  rnf (I i) = rnf i `seq` ()
  {-# NOINLINE rnf #-}
  -- NOINLINE is needed so that Clash doesn't trip on the "Index ~# Integer"
  -- coercion

instance (KnownNat n, 1 <= n) => BitPack (Index n) where
  type BitSize (Index n) = CLog 2 n
  pack   = packXWith pack#
  unpack = unpack#

-- | Safely convert an `SNat` value to an `Index`
fromSNat :: (KnownNat m, n + 1 <= m) => SNat n -> Index m
fromSNat = snatToNum

{-# NOINLINE pack# #-}
pack# :: Index n -> BitVector (CLog 2 n)
pack# (I i) = BV 0 (naturalFromInteger i)

{-# NOINLINE unpack# #-}
unpack# :: (KnownNat n, 1 <= n) => BitVector (CLog 2 n) -> Index n
unpack# (BV 0 i) = fromInteger_INLINE (naturalToInteger i)
unpack# bv = undefError "Index.unpack" [bv]

instance Eq (Index n) where
  (==) = eq#
  (/=) = neq#

{-# NOINLINE eq# #-}
eq# :: (Index n) -> (Index n) -> Bool
(I n) `eq#` (I m) = n == m

{-# NOINLINE neq# #-}
neq# :: (Index n) -> (Index n) -> Bool
(I n) `neq#` (I m) = n /= m

instance Ord (Index n) where
  (<)  = lt#
  (>=) = ge#
  (>)  = gt#
  (<=) = le#

lt#,ge#,gt#,le# :: Index n -> Index n -> Bool
{-# NOINLINE lt# #-}
lt# (I n) (I m) = n < m
{-# NOINLINE ge# #-}
ge# (I n) (I m) = n >= m
{-# NOINLINE gt# #-}
gt# (I n) (I m) = n > m
{-# NOINLINE le# #-}
le# (I n) (I m) = n <= m

-- | The functions: 'enumFrom', 'enumFromThen', 'enumFromTo', and
-- 'enumFromThenTo', are not synthesizable.
instance KnownNat n => Enum (Index n) where
  succ           = (+# fromInteger# 1)
  pred           = (-# fromInteger# 1)
  toEnum         = fromInteger# . toInteger
  fromEnum       = fromEnum . toInteger#
  enumFrom       = enumFrom#
  enumFromThen   = enumFromThen#
  enumFromTo     = enumFromTo#
  enumFromThenTo = enumFromThenTo#

enumFrom# :: forall n. KnownNat n => Index n -> [Index n]
enumFrom# x = [x .. maxBound]
{-# NOINLINE enumFrom# #-}

enumFromThen# :: forall n. KnownNat n => Index n -> Index n -> [Index n]
enumFromThen# x y = if x <= y then [x, y .. maxBound] else [x, y .. minBound]
{-# NOINLINE enumFromThen# #-}

enumFromTo# :: Index n -> Index n -> [Index n]
enumFromTo# x y = map I [unsafeToInteger x .. unsafeToInteger y]
{-# NOINLINE enumFromTo# #-}

enumFromThenTo# :: Index n -> Index n -> Index n -> [Index n]
enumFromThenTo# x1 x2 y = map I [unsafeToInteger x1, unsafeToInteger x2 .. unsafeToInteger y]
{-# NOINLINE enumFromThenTo# #-}

instance KnownNat n => Bounded (Index n) where
  minBound = fromInteger# 0
  maxBound = maxBound#

maxBound# :: forall n. KnownNat n => Index n
maxBound# =
  case natToInteger @n of
    0 -> errorX "maxBound of 'Index 0' is undefined"
    n -> fromInteger_INLINE (n - 1)
{-# NOINLINE maxBound# #-}

-- | Operators report an error on overflow and underflow
instance KnownNat n => Num (Index n) where
  (+)         = (+#)
  (-)         = (-#)
  (*)         = (*#)
  negate      = negate#
  abs         = id
  signum i    = if i == 0 then 0 else 1
  fromInteger = fromInteger#

(+#),(-#),(*#) :: KnownNat n => Index n -> Index n -> Index n
{-# NOINLINE (+#) #-}
(+#) (I a) (I b) = fromInteger_INLINE $ a + b

{-# NOINLINE (-#) #-}
(-#) (I a) (I b) = fromInteger_INLINE $ a - b

{-# NOINLINE (*#) #-}
(*#) (I a) (I b) = fromInteger_INLINE $ a * b

negate# :: KnownNat n => Index n -> Index n
negate# 0 = 0
negate# i = maxBound -# i +# 1

fromInteger# :: KnownNat n => Integer -> Index n
{-# NOINLINE fromInteger# #-}
fromInteger# = fromInteger_INLINE
{-# INLINE fromInteger_INLINE #-}
fromInteger_INLINE :: forall n . (HasCallStack, KnownNat n) => Integer -> Index n
fromInteger_INLINE i = bound `seq` if i > (-1) && i < bound then I i else err
  where
    bound = natToInteger @n
    err   = errorX ("Clash.Sized.Index: result " ++ show i ++
                   " is out of bounds: [0.." ++ show (bound - 1) ++ "]")

instance ExtendingNum (Index m) (Index n) where
  type AResult (Index m) (Index n) = Index (m + n - 1)
  add  = plus#
  sub = minus#
  type MResult (Index m) (Index n) = Index (((m - 1) * (n - 1)) + 1)
  mul = times#

plus#, minus# :: Index m -> Index n -> Index (m + n - 1)
{-# NOINLINE plus# #-}
plus# (I a) (I b) = I (a + b)

{-# NOINLINE minus# #-}
minus# (I a) (I b) =
  let z   = a - b
      err = error ("Clash.Sized.Index.minus: result " ++ show z ++
                   " is smaller than 0")
      res = if z < 0 then err else I z
  in  res

{-# NOINLINE times# #-}
times# :: Index m -> Index n -> Index (((m - 1) * (n - 1)) + 1)
times# (I a) (I b) = I (a * b)

instance (KnownNat n, 1 <= n) => SaturatingNum (Index n) where
  satAdd SatWrap !a !b =
    case natToInteger @n of
      1 -> fromInteger# 0
      _ -> leToPlusKN @1 @n $
        case plus# a b of
          z | let m = fromInteger# (natToInteger @n)
            , z >= m -> resize# (z - m)
          z -> resize# z
  satAdd SatZero a b =
    leToPlusKN @1 @n $
      case plus# a b of
        z | let m = fromInteger# (natToInteger @(n - 1))
          , z > m -> fromInteger# 0
        z -> resize# z
  satAdd SatError a b =
    leToPlusKN @1 @n $
      case plus# a b of
        z | let m = fromInteger# (natToInteger @(n - 1))
          , z > m -> errorX "Index.satAdd: overflow"
        z -> resize# z
  satAdd _ a b =
    leToPlusKN @1 @n $
      case plus# a b of
        z | let m = fromInteger# (natToInteger @(n - 1))
          , z > m -> maxBound#
        z -> resize# z

  satSub SatWrap a b =
    if lt# a b
       then maxBound -# (b -# a) +# 1
       else a -# b
  satSub SatError a b =
    if lt# a b
       then errorX "Index.satSub: underflow"
       else a -# b
  satSub _ a b =
    if lt# a b
       then fromInteger# 0
       else a -# b

  satMul SatWrap !a !b =
    case natToInteger @n of
      1 -> fromInteger# 0
      2 -> case a of {0 -> 0; _ -> b}
      _ -> leToPlusKN @1 @n $
        case times# a b of
          z -> let m = fromInteger# (natToInteger @n)
               in resize# (z `mod` m)
  satMul SatZero a b =
    leToPlusKN @1 @n $
      case times# a b of
        z | let m = fromInteger# (natToInteger @(n - 1))
          , z > m -> fromInteger# 0
        z -> resize# z
  satMul SatError a b =
    leToPlusKN @1 @n $
      case times# a b of
        z | let m = fromInteger# (natToInteger @(n - 1))
          , z > m -> errorX "Index.satMul: overflow"
        z -> resize# z
  satMul _ a b =
    leToPlusKN @1 @n $
      case times# a b of
        z | let m = fromInteger# (natToInteger @(n - 1))
          , z > m -> maxBound#
        z -> resize# z

  satSucc SatError !a =
    case natToInteger @n of
      1 -> errorX "Index.satSucc: overflow"
      _ -> satAdd SatError a $ fromInteger# 1
  satSucc satMode !a =
    case natToInteger @n of
      1 -> fromInteger# 0
      _ -> satAdd satMode a $ fromInteger# 1
  {-# INLINE satSucc #-}

  satPred SatError !a =
    case natToInteger @n of
      1 -> errorX "Index.satPred: underflow"
      _ -> satSub SatError a $ fromInteger# 1
  satPred satMode !a =
    case natToInteger @n of
      1 -> fromInteger# 0
      _ -> satSub satMode a $ fromInteger# 1
  {-# INLINE satPred #-}

instance KnownNat n => Real (Index n) where
  toRational = toRational . toInteger#

instance KnownNat n => Integral (Index n) where
  quot        = quot#
  rem         = rem#
  div         = quot#
  mod         = rem#
  quotRem n d = (n `quot#` d,n `rem#` d)
  divMod  n d = (n `quot#` d,n `rem#` d)
  toInteger   = toInteger#

quot#,rem# :: Index n -> Index n -> Index n
{-# NOINLINE quot# #-}
(I a) `quot#` (I b) = I (a `div` b)
{-# NOINLINE rem# #-}
(I a) `rem#` (I b) = I (a `rem` b)

{-# NOINLINE toInteger# #-}
toInteger# :: Index n -> Integer
toInteger# (I n) = n

instance KnownNat n => PrintfArg (Index n) where
  formatArg = formatArg . toInteger

instance (KnownNat n, 1 <= n) => Parity (Index n) where
  even = even . pack
  odd = odd . pack

instance (KnownNat n, 1 <= n) => Bits (Index n) where
  a .&. b           = unpack# $ BV.and# (pack# a) (pack# b)
  a .|. b           = unpack# $ BV.or# (pack# a) (pack# b)
  xor a b           = unpack# $ BV.xor# (pack# a) (pack# b)
  complement        = unpack# . BV.complement# . pack#
  zeroBits          = unpack# zeroBits
  bit i             = unpack# $ bit i
  setBit v i        = unpack# $ replaceBit i high (pack# v)
  clearBit v i      = unpack# $ replaceBit i low  (pack# v)
  complementBit v i = unpack# $ complementBit (pack# v) i
  testBit v i       = testBit (pack# v) i
  bitSizeMaybe v    = Just (size# v)
  bitSize           = size#
  isSigned _        = False
  shiftL v i        = unpack# $ shiftL (pack# v) i
  shiftR v i        = unpack# $ shiftR (pack# v) i
  rotateL v i       = unpack# $ rotateL (pack# v) i
  rotateR v i       = unpack# $ rotateR (pack# v) i
  popCount i        = popCount (pack# i)

instance (KnownNat n, 1 <= n) => FiniteBits (Index n) where
  finiteBitSize        = size#
  countLeadingZeros  i = countLeadingZeros  (pack# i)
  countTrailingZeros i = countTrailingZeros (pack# i)

instance Resize Index where
  resize     = resize#
  zeroExtend = extend
  truncateB  = resize#

resize# :: KnownNat m => Index n -> Index m
resize# (I i) = fromInteger_INLINE i
{-# NOINLINE resize# #-}

instance KnownNat n => Lift (Index n) where
  lift u@(I i) = sigE [| fromInteger# i |] (decIndex (natVal u))
  {-# NOINLINE lift #-}
#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped = liftTypedFromUntyped
#endif

#if MIN_VERSION_template_haskell(2,17,0)
decIndex :: Quote m => Integer -> m Type
#else
decIndex :: Integer -> TypeQ
#endif
decIndex n = appT (conT ''Index) (litT $ numTyLit n)

instance Show (Index n) where
  show (I i) = show i
  {-# NOINLINE show #-}

instance ShowX (Index n) where
  showsPrecX = showsPrecXWith showsPrec

instance NFDataX (Index n) where
  deepErrorX = errorX
  rnfX = rwhnfX

-- | None of the 'Read' class' methods are synthesizable.
instance KnownNat n => Read (Index n) where
  readPrec = fromIntegral <$> (readPrec :: ReadPrec Natural)

instance KnownNat n => Default (Index n) where
  def = fromInteger# 0

instance KnownNat n => Arbitrary (Index n) where
  arbitrary = arbitraryBoundedIntegral
  shrink    = shrinkIndex

shrinkIndex :: KnownNat n => Index n -> [Index n]
shrinkIndex x | natVal x < 3 = case toInteger x of
                                 1 -> [0]
                                 _ -> []
              -- 'shrinkIntegral' uses "`quot` 2", which for 'Index' types with
              -- an upper bound less than 2 results in an error.
              | otherwise    = shrinkIntegral x

instance KnownNat n => CoArbitrary (Index n) where
  coarbitrary = coarbitraryIntegral

instance (KnownNat n) => Ix (Index n) where
  range (a, b) = [a..b]
  index ab@(a, b) x
    | inRange ab x = fromIntegral $ x - a
    | otherwise = error $ printf "Index %d out of bounds (%d, %d)" x a b
  inRange (a, b) x = a <= x && x <= b
