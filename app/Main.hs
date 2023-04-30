{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import           Amazonka
import           CloudformationTags
import           Control.Lens
import           Data.Generics.Labels    ()
import           IAMTags
import           Options.Applicative
import           System.IO
import GHC.Generics
import Control.Monad (forM)

data Args = Args
    { _all     :: Bool
    , _aws_profile :: Maybe String
    , _aws_regions :: Maybe [Region]
    }
    deriving (Show, Generic)

(makeLenses ''Args)


parseRegion :: String -> Maybe Region
parseRegion "mumbai"           = Just Mumbai
parseRegion "sydney"           = Just Sydney
parseRegion "singapore"        = Just Singapore
parseRegion "osaka"            = Just Osaka
parseRegion "seoul"            = Just Seoul
parseRegion "tokyo"            = Just Tokyo
parseRegion "hong_kong"        = Just HongKong
parseRegion "ningxia"          = Just Ningxia
parseRegion "beijing"          = Just Beijing
parseRegion "cape_town"        = Just CapeTown
parseRegion "bahrain"          = Just Bahrain
parseRegion "stockholm"        = Just Stockholm
parseRegion "paris"            = Just Paris
parseRegion "milan"            = Just Milan
parseRegion "london"           = Just London
parseRegion "ireland"          = Just Ireland
parseRegion "frankfurt"        = Just Frankfurt
parseRegion "sao_paulo"        = Just SaoPaulo
parseRegion "montreal"         = Just Montreal
parseRegion "govcloud-east"    = Just GovCloudEast
parseRegion "govcloud-west"    = Just GovCloudWest
parseRegion "oregon"           = Just Oregon
parseRegion "north_california" = Just NorthCalifornia
parseRegion "ohio"             = Just Ohio
parseRegion "north_virginia"   = Just NorthVirginia
parseRegion _                  = Nothing

regionParser :: Parser (Maybe [Region])
regionParser = optional . many $ option (maybeReader parseRegion) $
  long "region"
  <> short 'r'
  <> metavar "REGION"
  <> help "The AWS region to use"


profileParser :: Parser (Maybe String)
profileParser = optional $ strOption $ long "profile" <> metavar "AWS_PROFILE" <> help "Profile to authenticate"


argParser :: Parser Args
argParser = Args
    <$> switch (long "all" <> help "All Regions")
    <*> profileParser
    <*> regionParser

appRunner :: Args -> IO ()
appRunner args = do

    let regions = args ^. aws_regions

    case regions of
      Just r -> do 
        lgr <- newLogger Debug stdout
        envs <- do
            forM r $ \region -> do
                newEnv discover <&> (#envLogger .~ lgr ) . within region

        listStacks envs

      Nothing -> do
        putStrLn "No region provided"

main :: IO ()
main = do
    --listBucket Ohio "rivendel-sccache"
    arguments <- execParser opts
    appRunner arguments

  where
    opts = info (argParser <**>  helper)
      ( fullDesc
     <> progDesc "Tool to list all stacks in a region"
     <> header "tagsformation - list tags of cloudformation tags" )
