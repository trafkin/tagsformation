{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Amazonka
import CloudformationTags


main :: IO ()
main =
    --listBucket Ohio "rivendel-sccache"
    listStacks Ohio
