{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Timeout
  ( TimedOut(..)
  , TimeoutUnit(..)
  , aTimeoutUnit
  , timeoutUnitNanos
  , Timeout(..)
  , aTimeout
  , (#)
  , (#>)
  , (#<)
  , instantly
  ) where

import Prelude hiding (print)
import Data.Typeable (Typeable)
import Data.Ix (Ix)
import Data.Word (Word64)
import Data.Proxy (Proxy(..))
import Data.Monoid (mconcat)
import Data.Textual (Printable(..), Textual(..))
import Data.Textual.Fractional (Sign(..), Decimal(..), Optional(..),
                                fractional')
import Text.Printer (Printer(char7, string7), (<>))
import Text.Parser.Combinators ((<?>), unexpected)
import qualified Text.Parser.Char as PC
import Control.Applicative
import Control.Monad (when)
import Control.Exception (Exception)

-- | Exception that is raised when an operation times out.
--   Not used by the package itself, it is here so that users don't need to
--   roll their own exception type every time.
data TimedOut = TimedOut deriving (Typeable, Eq, Show)

instance Exception TimedOut

-- | Timeout unit.
data TimeoutUnit = NanoSecond
                 | MicroSecond
                 | MilliSecond
                 | Second
                 | Minute
                 | Hour
                 | Day
                 | Week
                 deriving (Typeable, Show, Read, Eq, Ord, Bounded, Ix, Enum)

-- | 'TimeoutUnit' proxy value.
aTimeoutUnit ∷ Proxy TimeoutUnit
aTimeoutUnit = Proxy

instance Printable TimeoutUnit where
  print NanoSecond  = string7 "ns"
  print MicroSecond = string7 "us"
  print MilliSecond = string7 "ms"
  print Second      = char7 's'
  print Minute      = char7 'm'
  print Hour        = char7 'h'
  print Day         = char7 'd'
  print Week        = char7 'w'
  {-# INLINABLE print #-}

instance Textual TimeoutUnit where
  textual = (<?> "timeout unit") $ do
    c ← PC.oneOf "numshdw" 
    case c of
      'n' → PC.char 's' *> pure NanoSecond
      'u' → PC.char 's' *> pure MicroSecond
      'm' → maybe Minute (const MilliSecond) <$> optional (PC.char 's')
      's' → pure Second
      'h' → pure Hour
      'd' → pure Day
      _   → pure Week

-- | Amount of nanoseconds in a timeout unit.
timeoutUnitNanos ∷ TimeoutUnit → Word64
timeoutUnitNanos NanoSecond  = 1
timeoutUnitNanos MicroSecond = 1000
timeoutUnitNanos MilliSecond = 1000000
timeoutUnitNanos Second      = 1000000000
timeoutUnitNanos Minute      = 60 * 1000000000
timeoutUnitNanos Hour        = 60 * 60 * 1000000000
timeoutUnitNanos Day         = 24 * 60 * 60 * 1000000000
timeoutUnitNanos Week        = 7 * 24 * 60 * 60 * 1000000000
{-# INLINABLE timeoutUnitNanos #-}

-- | Timeout in nanoseconds.
newtype Timeout = Timeout Word64
  deriving (Typeable, Show, Read,  Eq, Ord, Bounded, Ix, Enum,
            Num, Real, Integral)

-- | 'Timeout' proxy value.
aTimeout ∷ Proxy Timeout
aTimeout = Proxy

infix 9 #
infix 8 #>, #<

-- | Convert the given number of timeout units to 'Timeout'.
(#) ∷ Word64 → TimeoutUnit → Timeout
n # u = Timeout $ n * timeoutUnitNanos u
{-# INLINE (#) #-}

-- | Extract number of units (rounding up).
(#>) ∷ Timeout → TimeoutUnit → Word64
(Timeout tt) #> u = if r == 0 then q else q + 1
  where (q, r) = tt `quotRem` timeoutUnitNanos u
{-# INLINE (#>) #-}

-- | Extract number of units (rounding down).
(#<) ∷ Timeout → TimeoutUnit → Word64
(Timeout tt) #< u = tt `quot` timeoutUnitNanos u
{-# INLINE (#<) #-}

instance Printable Timeout where
  print (Timeout tt) =
      if null ss then string7 "0ns" else mconcat (reverse ss)
    where
      ss = snd $ ($ enumFrom NanoSecond) $ (`foldr` (tt, [])) $ \u (t, ss') →
        let (q, r) = t `quotRem` timeoutUnitNanos u in
          (r, if q == 0 then ss' else (print q <> print u) : ss')

instance Textual Timeout where
  textual = (<?> "timeout") $ do
      a ← amount
      u ← textual
      let r = a * fromIntegral (timeoutUnitNanos u) ∷ Rational
      if u == minBound then result r else go u r
    where
      amount = fractional' (pure NonNegative) Decimal Required
                           (PC.char '.' *> pure ()) (pure Nothing)
      go u r = do
        ma ← optional amount
        case ma of
          Nothing → result r
          Just a → do
            u' ← textual
            when (u' >= u) $ unexpected "timeout units must get smaller"
            let r' = r + a * fromIntegral (timeoutUnitNanos u')
            if u' == minBound then result r' else go u' r'
      result r = let c = ceiling r ∷ Integer in
                   if (c ∷ Integer) > fromIntegral (maxBound ∷ Word64)
                   then return maxBound
                   else return $ Timeout $ fromIntegral c

-- | Zero timeout. The event in question should occur immediately.
instantly ∷ Timeout
instantly = Timeout 0
{-# INLINE instantly #-}

