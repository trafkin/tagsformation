
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module IAMTags where

import           Amazonka
import           Amazonka.IAM
import           Conduit              (ResourceT)
import           Control.Lens
import           Control.Monad
import           Data.Generics.Labels ()
import           Data.Maybe           (isJust)
import           Helpers


--listRoles :: Region -> IO ()
--listRoles r = do
  --lgr <- newLogger Debug stdout
  --env <- newEnv discover <&> set #envLogger lgr . within r
  --say  "Listing Roles"

describeRoles :: Env -> ResourceT IO ()
describeRoles env = do
  roles <- view #roles <$> send env newListRoles
  say $ "Found " <> toText (length roles) <> " Roles."
  forM_ roles $ \s -> do
    say $ "Role: " <> toText (s ^. #roleName)


listRoles :: Env -> IO ()
listRoles env = do
   describeRoles env
    & runResourceT
