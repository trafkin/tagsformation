
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module IAMTags where

import           Amazonka
import           Amazonka.Iam
import           Conduit                 (ResourceT)
import           Control.Lens
import           Control.Monad
import           Data.Generics.Labels    ()
import           Data.Maybe              (isJust)
import           Helpers
import           System.IO


--listRoles :: Region -> IO ()
--listRoles r = do
  --lgr <- newLogger Debug stdout
  --env <- newEnv discover <&> set #envLogger lgr . within r
  --say  "Listing Roles"

describeRoles :: Region -> ResourceT IO ()
describeRoles r = do
-- TODO: Put this logic on some reader monad, or global setup
  
  lgr <- newLogger Debug stdout
  env <- newEnv discover <&> set #envLogger lgr . within r

  rs <- view #stacks <$> send env listRoles
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

listStacks :: Region -> IO ()
listStacks r = do
   describeTags r
   & runResourceT
