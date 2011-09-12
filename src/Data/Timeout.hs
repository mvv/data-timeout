{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Timeout (
    TimedOut(..),
    TimeoutUnit(..),
    timeoutUnitNanos,
    Timeout(..),
    (#),
    (#>),
    (#<),
    instantly
  ) where

import Data.Typeable (Typeable)
import Data.Ix (Ix)
import Data.Word (Word64)
import Data.List (intercalate)
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
                 deriving (Typeable, Eq, Ord, Bounded, Ix, Enum)

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
{-# INLINE timeoutUnitNanos #-}

-- | Timeout in nanoseconds.
newtype Timeout = Timeout Word64
  deriving (Typeable, Eq, Ord, Bounded, Ix, Enum, Num, Real, Integral)

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

timeoutUnitAbbr ∷ TimeoutUnit → String
timeoutUnitAbbr NanoSecond  = "ns"
timeoutUnitAbbr MicroSecond = "us"
timeoutUnitAbbr MilliSecond = "ms"
timeoutUnitAbbr Second      = "s"
timeoutUnitAbbr Minute      = "m"
timeoutUnitAbbr Hour        = "h"
timeoutUnitAbbr Day         = "d"
timeoutUnitAbbr Week        = "w"

instance Show Timeout where
  show (Timeout tt) =
      if null ss then "instant" else intercalate " " (reverse ss)
    where
      ss = snd $ ($ enumFrom NanoSecond) $ (`foldr` (tt, [])) $ \u (t, ss') →
        let (q, r) = t `quotRem` timeoutUnitNanos u
            abbr   = timeoutUnitAbbr u in
          (r, if q == 0 then ss' else (show q ++ " " ++ abbr) : ss')

-- | Zero timeout. The event in question should occur immediately.
instantly ∷ Timeout
instantly = 0 # NanoSecond
{-# INLINE instantly #-}

