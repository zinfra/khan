{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Khan.CLI.Ansible
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.CLI.Ansible
    ( commands

    -- * Convenience exports for the Image CLI
    , Ansible (..)
    , playbook
    ) where

import           Control.Monad              (mplus)
import qualified Data.Aeson.Encode.Pretty   as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.HashMap.Strict        as Map
import qualified Data.Set                   as Set
import qualified Data.Text                  as Text
import qualified Data.Text.Format           as Format
import qualified Data.Text.Lazy.IO          as LText
import           Data.Time.Clock.POSIX
import qualified Filesystem.Path.CurrentOS  as Path
import           Khan.Internal
import           Khan.Model.Ansible
import qualified Khan.Model.Instance        as Instance
import qualified Khan.Model.Key             as Key
import qualified Khan.Model.Tag             as Tag
import           Khan.Prelude
import           Network.AWS
import           Network.AWS.EC2            hiding (Failed, Image)
import           System.Directory
import qualified System.IO                  as IO
import qualified System.Posix.Files         as Posix
import           System.Process             (callCommand)

data Inventory = Inventory
    { iEnv    :: !Env
    , iSilent :: !Bool
    , iList   :: !Bool
    , iHost   :: Maybe Text
    }

inventoryParser :: EnvMap -> Parser Inventory
inventoryParser env = Inventory
    <$> envOption env
    <*> switchOption "silent" False
        "Don't output inventory results to stdout."
    <*> switchOption "list" True
        "List."
    <*> optional (textOption "host" mempty
        "Host.")

instance Options Inventory where
    validate Inventory{..} =
        check iEnv "--env must be specified."

data Ansible = Ansible
    { aEnv    :: !Env
    , aRKeys  :: !RKeysBucket
    , aKey    :: Maybe FilePath
    , aBin    :: Maybe Text
    , aRetain :: !Int
    , aForce  :: !Bool
    , aArgs   :: [String]
    }

ansibleParser :: EnvMap -> Parser Ansible
ansibleParser env = Ansible
    <$> envOption env
    <*> rKeysOption env
    <*> keyOption
    <*> optional (textOption "bin" (short 'b')
        "Ansible binary name to exec.")
    <*> readOption "retention" "SECONDS" (value 360)
        "Number of seconds to cache inventory results for."
    <*> switchOption "force" False
        "Force update of any previously cached results."
    <*> argsOption str (action "file")
        "Pass through arguments to ansible."

instance Options Ansible where
    validate Ansible{..} = do
        check aEnv "--env must be specified."
        check aArgs "Pass ansible options through using the -- delimiter.\n\
                    \Usage: khan ansible [KHAN OPTIONS] -- [ANSIBLE OPTIONS]."

instance Naming Ansible where
    names Ansible{..} = unversioned "base" aEnv

commands :: EnvMap -> Mod CommandFields Command
commands env = mconcat
    [ command "ansible" ansible (ansibleParser env)
        "Run 'ansible' supplying it with khan_* facts and inventory."
    , command "playbook" playbook (ansibleParser env)
        "Run 'ansible-playbook' supplying it with khan_* facts and inventory."
    , command "inventory" inventory (inventoryParser env)
        "Output ansible compatible inventory in JSON format."
    ]

inventory :: Common -> Inventory -> AWS ()
inventory Common{..} Inventory{..} = do
    j <- Aeson.encodePretty . JS <$> maybe list (const $ return Map.empty) iHost
    i <- inventoryPath cCache iEnv

    debug "Writing inventory to {}" [i]
    liftIO $ LBS.writeFile (Path.encodeString i) (j <> "\n")

    debug_ "Writing inventory to stdout"
    unless iSilent . liftIO $ LBS.putStrLn j
  where
    list = Instance.findAll [] [Tag.filter Tag.env [_env iEnv]] >>=
        foldlM hosts Map.empty

    hosts m RunningInstancesItemType{..} = case riitDnsName of
        Nothing   -> return m
        Just fqdn -> do
            t@Tags{..} <- Tag.lookup $ Tag.flatten riitTagSet

            let n@Names{..} = names t
                host        = Host fqdn tagDomain n cRegion
                update k    = Map.insertWith (<>) k (Set.singleton host)

            return $! foldl' (flip update) m
                [roleName, envName, Text.pack $ show cRegion, "khan", tagDomain]

playbook :: Common -> Ansible -> AWS ()
playbook c a@Ansible{..} = ansible c $ a
    { aBin  = aBin `mplus` Just "ansible-playbook"
    , aArgs = extraVars a (cRegion c) aArgs
    }

ansible :: Common -> Ansible -> AWS ()
ansible c@Common{..} a@Ansible{..} = do
    let bin = Text.unpack $ fromMaybe "ansible" aBin

    which bin

    k <- maybe (Key.path aRKeys a cLKeys) return aKey
    i <- inventoryPath cCache aEnv

    let script = Path.encodeString $ i <.> "sh"
        inv    = Path.encodeString i

    whenM ((|| aForce) <$> exceeds inv) $ do
        log "Limit of {}s exceeded for {}, refreshing..." [show aRetain, inv]
        inventory c $ Inventory aEnv True True Nothing

    debug "Writing inventory script to {}" [script]
    liftIO . LText.writeFile script $
        Format.format "#!/usr/bin/env bash\nset -e\nexec cat {}\n" [i]

    debug "Setting +rwx on {}" [script]
    liftIO $ Posix.setFileMode script Posix.ownerModes

    debug_ "Setting line-buffering on stdout and stderr"
    liftIO $ do
        IO.hSetBuffering IO.stdout IO.LineBuffering
        IO.hSetBuffering IO.stderr IO.LineBuffering

    let cmd = unwords ["ANSIBLE_FORCE_COLOR=1", unwords (bin : args k script)]

    log "{}" [cmd]
    liftEitherT . sync $ callCommand cmd
  where
    args k s = aArgs +$+
        [ ("-i", s)
        , ("--private-key", Path.encodeString k)
        ]

    exceeds i = liftIO $ do
        p <- doesFileExist i
        if not p
            then return True
            else do
                s  <- Posix.getFileStatus i
                ts <- getPOSIXTime
                return $ ts - Posix.modificationTimeHiRes s > fromIntegral aRetain
