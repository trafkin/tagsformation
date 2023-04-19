{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Amazonka
import Helpers
import Buckets
import CloudformationTags
import Amazonka.S3
import Amazonka.CloudFormation
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Generics.Labels ()
import qualified Data.Text.IO as Text
import System.IO


main :: IO ()
main =
    --listBucket Ohio "rivendel-sccache"
    listRoles Ohio
