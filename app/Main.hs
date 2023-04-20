{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Amazonka
import Helpers
import Buckets
import CloudformationTags


main :: IO ()
main =
    --listBucket Ohio "rivendel-sccache"
    listStacks Ohio
