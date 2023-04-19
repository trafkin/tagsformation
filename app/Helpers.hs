{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Helpers where

import Amazonka
import Control.Monad
import Control.Monad.IO.Class
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Generics.Labels ()
import qualified Data.Text.IO as Text
import System.IO


say :: MonadIO m => Text -> m ()
say = liftIO . Text.putStrLn
