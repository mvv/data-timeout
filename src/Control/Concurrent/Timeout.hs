{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
-- Do no complain about the System.Timeout import.
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Control.Concurrent.Timeout (
    timeout,
    threadDelay
  ) where

import Data.Typeable
import Data.Timeout
import Data.Unique
import Control.Applicative
import Control.Monad.Base
import Control.Exception
import qualified Control.Concurrent as C
-- Imported for Haddock.
import qualified System.Timeout as C

data TimeoutException = TimeoutException Unique
  deriving (Typeable, Eq)

instance Show TimeoutException where
  show _ = "<<timeout>>"

instance Exception TimeoutException

-- | A version of 'C.timeout' that takes 'Timeout' instead of number of
--   microseconds.
timeout ∷ MonadBase IO μ ⇒ Timeout → IO α → μ (Maybe α)
timeout tt _ | tt == instantly = return Nothing
timeout tt io = liftBase $ do
  pid <- C.myThreadId
  ex  <- TimeoutException <$> newUnique
  handleJust (\e -> if e == ex then Just () else Nothing)
             (\_ -> return Nothing)
             (bracket (C.forkIO (threadDelay tt >> C.throwTo pid ex))
                      (C.killThread)
                      (\_ -> Just <$> io))

-- | A version of 'C.threadDelay' that takes 'Timeout' instead of number of
--   microseconds.
threadDelay ∷ MonadBase IO μ ⇒ Timeout → μ ()
threadDelay tt | tt == instantly = return ()
threadDelay tt = liftBase $ C.threadDelay (fromIntegral us') >> go us'
  where us = tt #> MicroSecond
        maxUs = fromIntegral (maxBound ∷ Int)
        us' = maxUs `min` us
        go passed = case us - passed of
          0    → return ()
          left → C.threadDelay (fromIntegral us'') >> go us''
            where us'' = maxUs `min` left

