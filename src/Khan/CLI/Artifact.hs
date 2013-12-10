{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Khan.CLI.Artifact
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.CLI.Artifact (commands) where

import           Khan.Internal
import           Khan.Internal.Ansible
import qualified Khan.Model.Bucket     as Bucket
import qualified Khan.Model.Object     as Object
import           Khan.Prelude          hiding (sync)
import           Network.AWS.S3        (AWS)

data Object = Object
    { oBucket  :: !Text
    , oKey     :: !Text
    , oPath    :: !FilePath
    , oForce   :: !Bool
    , oAnsible :: !Bool
    } deriving (Show)

objectParser :: Parser Object
objectParser = Object
    <$> textOption "bucket" (short 'b')
        "Bucket."
    <*> textOption "key" (short 'k')
        "Key."
    <*> pathOption "file" (short 'f' <> action "file")
        "Local file."
    <*> switchOption "force" False
        "Overwrite if exists."
    <*> ansibleOption

instance Options Object

data Bucket = Bucket
    { bBucket  :: !Text
    , bPrefix  :: Maybe Text
    , bDir     :: !FilePath
    , bN       :: !Int
    , bForce   :: !Bool
    , bAnsible :: !Bool
    } deriving (Show)

bucketParser :: Parser Bucket
bucketParser = Bucket
    <$> textOption "bucket" (short 'b')
        "Bucket."
    <*> optional (textOption "prefix" (short 'p')
        "Prefix.")
    <*> pathOption "dir" (short 'd' <> action "directory")
        "Local file."
    <*> readOption "concurrency" "INT" (short 'n' <> value 4)
        "Number of simultaneous downloads."
    <*> switchOption "force" False
        "Overwrite if exists."
    <*> ansibleOption

instance Options Bucket

commands :: Mod CommandFields Command
commands = mconcat
    [ command "upload" (object Object.upload) objectParser
        "Upload an object to S3."
    , command "download" (object Object.download) objectParser
        "Download an object to disk."
    , command "sync" sync bucketParser
        "Synchronize a bucket to disk."
    ]

object :: (Text -> Text -> FilePath -> AWS Bool) -> Common -> Object -> AWS ()
object g c Object{..}
    | oAnsible  = capture c "object {}/{}" [oBucket, oKey] f
    | otherwise = void f
  where
    f = g oBucket oKey oPath

sync :: Common -> Bucket -> AWS ()
sync c Bucket{..}
    | bAnsible  = capture c "bucket {}/{}" [bBucket, fromMaybe "" bPrefix] f
    | otherwise = void f
  where
    f = Bucket.download bN bBucket bPrefix bDir
