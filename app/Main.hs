{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module Main (main) where

import           Amazonka
import           CloudformationTags
import           Control.Lens
import           Data.Generics.Labels ()
import           Options.Applicative
import           System.IO

regionOption :: Parser [Region]
regionOption = option auto
  ( long "region"
  <> short 'r'
  <> metavar "REGION"
  <> value [Ohio]
  <> showDefault
  <> help "AWS region"
  )


data Args = Args
    { all     :: Bool
    , profile :: String
    , regions :: [Region]
    }
    deriving (Show)


argParser :: Parser Args
argParser = Args
    <$> switch (long "all" <> help "All Regions")
    <*> strOption
        (long "profile"
        <> metavar "AWS_PROFILE"
        <> help "Profile to authenticate"
        )
    <*> regionOption

main :: IO ()
main = do
    --listBucket Ohio "rivendel-sccache"
  -- TODO: Put this logic on some reader monad, or global setup
  --
    options <- execParser opts
    print options
    
    --lgr <- newLogger Info stdout
    --env <- newEnv discover <&> (#envLogger .~ lgr ) . within Ohio


    --listStacks env
    --listRoles env
  where
    opts = info (argParser <**>  helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )
