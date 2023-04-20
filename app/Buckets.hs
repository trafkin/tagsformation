{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Buckets where

import Amazonka
import Amazonka.S3
import Helpers
import Control.Lens
import Control.Monad
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Generics.Labels ()
import System.IO


listAll ::
  -- | Region to operate in.
  Region ->
  IO ()
listAll r =

  newLogger Info stdout >>= \lgr ->
  newEnv discover <&> set #envLogger lgr . within r >>= \env  ->

  let val :: ToText a => Maybe a -> Text
      val = maybe "Nothing" toText
      lat v = maybe mempty (mappend " - " . toText) (v ^. #isLatest)
      key v = val (v ^. #key) <> ": " <> val (v ^. #versionId) <> lat v
  in

  runResourceT $ do
    say "Listing Buckets .."
    Just bs <- view #buckets <$> send env newListBuckets
    say $ "Found " <> toText (length bs) <> " Buckets."

    forM_ bs $ \(view #name -> b) -> do
      say $ "Listing Object Versions in: " <> toText b
      runConduit $
        paginate env (newListObjectVersions b)
          .| CL.concatMap (toListOf $ #versions . _Just . folded)
          .| CL.mapM_ (say . mappend " -> " . key)



listBucket :: Region -> BucketName -> IO ()
listBucket r b =
    newLogger Debug stdout >>= \lgr ->
        newEnv discover <&> set #envLogger lgr .within r >>= \env ->

        let
            key v = toText (v ^. #key)
        in
            runResourceT $ do
                runConduit $
                    paginate env (newListObjects b)
                      .| CL.concatMap (toListOf $ #contents . _Just . folded)
                      .| CL.mapM_ (say . mappend " -> " . key)



getFile ::
  -- | Region to operate in.
  Region ->
  BucketName ->
  -- | The source object key.
  ObjectKey ->
  -- | The destination file to save as.
  FilePath ->
  IO ()
getFile r b k f = do
  lgr <- newLogger Debug stdout
  env <- newEnv discover <&> set #envLogger lgr . within r

  runResourceT $ do
    rs <- send env (newGetObject b k)
    view #body rs `sinkBody` CB.sinkFile f
    say $
      "Successfully Download: "
        <> toText b
        <> " - "
        <> toText k
        <> " to "
        <> toText f

putChunkedFile ::
  -- | Region to operate in.
  Region ->
  -- | The bucket to store the file in.
  BucketName ->
  -- | The destination object key.
  ObjectKey ->
  -- | The chunk size to send env.
  ChunkSize ->
  -- | The source file to upload.
  FilePath ->
  IO ()
putChunkedFile r b k c f = do
  lgr <- newLogger Debug stdout
  env <- newEnv discover <&> set #envLogger lgr . within r

  runResourceT $ do
    bdy <- chunkedFile c f
    void . send env $ newPutObject b k bdy
    say $
      "Successfully Uploaded: "
        <> toText f
        <> " to "
        <> toText b
        <> " - "
        <> toText k



-- Here is Cloudformation stuff
--

