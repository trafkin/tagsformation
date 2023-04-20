{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module CloudformationTags where

import Helpers
import Amazonka
import Amazonka.CloudFormation
import Control.Lens
import Control.Monad
import Data.Maybe (isJust)
import Data.Generics.Labels ()
import System.IO


--listRoles :: Region -> IO ()
--listRoles r = do
  --lgr <- newLogger Debug stdout
  --env <- newEnv discover <&> set #envLogger lgr . within r
  --say  "Listing Roles"


listStacks :: Region -> IO ()
listStacks r = do
  lgr <- newLogger Debug stdout
  env <- newEnv discover <&> set #envLogger lgr . within r

  runResourceT $ do
    rs <- view #stacks <$> send env newDescribeStacks
    case rs of
      Nothing -> say "No Stacks"
      Just stacks -> do
        filteredStacks <- filterM (\s -> return $ isJust ( s ^. #tags) ) stacks

        forM_ filteredStacks $ \s -> do
              say $ "Stack: " <> toText (s ^. #stackName)
              say $ "Status: " <> toText (s ^. #stackStatus)
              say $ "Created: " <> toText (s ^. #creationTime)
              case s ^. #tags of
                Just ts -> do
                  say "Tags:"
                  forM_ ts $ \t -> do
                        say $ "  " <> toText (t ^. #key) <> ": " <> toText (t ^. #value)
                Nothing -> say "No Tags"


