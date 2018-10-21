{-# LANGUAGE CPP #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts #-}

module Control.Concurrent.STM.Timeout
  ( registerDelay
  ) where

import Data.Timeout
import Control.Monad (void)
import Control.Monad.Base
import Control.Monad.STM
import Control.Concurrent.STM.TVar hiding (registerDelay)
import GHC.Event (registerTimeout)
#if MIN_VERSION_base(4,7,0)
import GHC.Event (getSystemTimerManager)
#else
import GHC.Event (getSystemEventManager)
#endif

registerDelay ∷ MonadBase IO μ ⇒ Timeout → μ (TVar Bool)
registerDelay tt = liftBase $
  if tt == instantly
    then newTVarIO True
    else do
      ttv ← newTVarIO False
#if MIN_VERSION_base(4,7,0)
      tmm ← getSystemTimerManager
#else
      Just tmm ← getSystemEventManager
#endif
      let us = tt #> MicroSecond
          maxUs = fromIntegral (maxBound ∷ Int)
          us' = maxUs `min` us
          rearm passed =
            case us - passed of
              0 → atomically $ writeTVar ttv True
              left → do
                let us'' = maxUs `min` left
                void $ registerTimeout tmm (fromIntegral us'') $
                  rearm $ passed + us''
      void $ registerTimeout tmm (fromIntegral us') $ rearm us'
      return ttv
