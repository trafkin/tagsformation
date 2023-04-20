{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Helpers where

import Amazonka
import Control.Monad.IO.Class
import Data.Generics.Labels ()
import qualified Data.Text.IO as Text


say :: MonadIO m => Text -> m ()
say = liftIO . Text.putStrLn
