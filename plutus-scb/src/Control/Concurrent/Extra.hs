module Control.Concurrent.Extra
    ( threadDelayTime
    ) where

import           Control.Concurrent     (threadDelay)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Time.Units        (TimeUnit, toMicroseconds)

-- | Exactly like `threadDelay`, but with an API that makes it easier to be clear about units.
-- For example, you can replace:
--
-- > threadDelay (10 * 1000 * 1000)
--
-- ...with:
--
-- > threadDelayTime (10 :: Seconds)
--
threadDelayTime :: (MonadIO m, TimeUnit a) => a -> m ()
threadDelayTime = liftIO . threadDelay . fromIntegral . toMicroseconds
