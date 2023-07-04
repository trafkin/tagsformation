{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module CloudformationTags where

import           Amazonka
import           Amazonka.CloudFormation
import           Conduit                 (ResourceT)
import           Control.Lens
import           Control.Monad
import           Data.Generics.Labels    ()
import           Helpers


--listRoles :: Region -> IO ()
--listRoles r = do
  --lgr <- newLogger Debug stdout
  --env <- newEnv discover <&> set #envLogger lgr . within r
  --say  "Listing Roles"


describeTags :: Env -> ResourceT IO ()
describeTags env = do

  rs <- view #stacks <$> send env newDescribeStacks
  case rs of
      Nothing -> say "No Stacks"
      Just stacks -> do

        forM_ stacks $ \s -> do
              say $ "Stack: " <> toText (s ^. #stackName)
              say $ "Status: " <> toText (s ^. #stackStatus)
              say $ "Created: " <> toText (s ^. #creationTime)
              say $ "RoleARN: " <> toText (s ^. #roleARN. _Just)
              say "Capabilities:"
              let capas =  (s ^.. #capabilities. _Just)  & concat 
              forM_ capas $ \s' -> say $ " " <> toText s'
                

listStacks :: [Env] -> IO ()
listStacks r = do
    let tasks = map describeTags r
    
    forM_ tasks $ \task -> do
        task & runResourceT

