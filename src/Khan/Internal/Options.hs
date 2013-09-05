{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}

-- Module      : Khan.Internal.Options
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Internal.Options
    (
    -- * Application Options
      Khan     (..)

    -- * Program Helpers
    , Discover (..)
    , Validate (..)
    , Subcommand
    , defineOptions
    , check
    , runProgram
    , subCommand
    ) where

import Control.Error
import Control.Monad
import Data.Text.Encoding
import Khan.Internal.Log
import Khan.Internal.OptionTypes
import Khan.Internal.Types
import Network.AWS
import Options                hiding (boolOption)
import System.Environment
import System.Exit

defineOptions "Khan" $ do
    boolOption "kDebug" "debug" False
        "Log debug output"

    maybeTextOption "kRole" "iam-role" ""
        "IAM role - if specified takes precendence over access/secret keys."

    maybeTextOption "kAccess" "access-key" ""
        "AWS access key."

    maybeTextOption "kSecret" "secret-key" ""
        "AWS secret key."

    boolOption "kDiscovery" "discovery" False
        "Populate options from EC2 metadata. Requires --iam-role."

deriving instance Show Khan

instance Validate Khan where
    validate Khan{..} = do
        check (kDiscovery && noRole)
            "--iam-role must be specified in order to use --discovery."
        check (or [isNothing kAccess, isNothing kSecret, noRole])
            "--access-key and --secret-key must be specified unless --iam-role is used."
      where
        noRole = isNothing kRole

check :: (Monad m, Invalid a) => a -> String -> EitherT String m ()
check x = when (invalid x) . throwT

subCommand :: (Show a, Options a, Discover a, Validate a)
           => String
           -> (a -> AWSContext b)
           -> Subcommand Khan (Script b)
subCommand name action = Options.subcommand name runner
  where
    runner k@Khan{..} o _ = do
        setLogging kDebug
        logStep "Running Khan..." k
        validate k
        auth <- credentials $ creds kRole
        fmapLT show . runAWS auth kDebug $ do
            opts <- disco kDiscovery o
            fmapLT Error $ validate opts
            action opts

    disco True  = (logDebug "Performing discovery..." >>) . discover
    disco False = (logDebug "Skipping discovery..."   >>) . return

    creds = maybe (FromEnv "ACCESS_KEY_ID" "SECRET_ACCESS_KEY")
                  (FromRole . encodeUtf8)

runProgram :: Options o => [(String, [Subcommand o (Script a)])] -> IO a
runProgram cmds = do
    args <- getArgs
    case args of
        []       -> help
        (a:argv) -> maybe help (run argv) $ a `lookup` cmds
  where
    help = do
        putStrLn $ parsedHelp (parseOptions [] :: ParsedOptions Khan)
        putStrLn $ unlines ("Subcommands:" : map (("  " ++) . fst) cmds ++ [""])
        putStrLn "No subcommand specified"
        exitFailure

    run argv sub =
        let parsed = parseSubcommand sub argv
        in case parsedSubcommand parsed of
               Just cmd -> runScript cmd
               Nothing  -> case parsedError parsed of
                   Just ex -> do
                       putStrLn $ parsedHelp parsed
                       putStrLn ex
                       exitFailure
                   Nothing -> do
                       putStrLn $ parsedHelp parsed
                       exitSuccess
