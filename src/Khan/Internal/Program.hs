{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}

-- Module      : Khan.Internal.Program
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Internal.Program
    (
    -- * Application Options
      Khan     (..)
    , Command  (..)
    , SubCommand

    -- * Program Helpers
    , Discover (..)
    , Validate (..)
    , defineOptions
    , check
    , runProgram
    , subCommand
    ) where

import           Control.Applicative
import           Control.Error
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8     as BS
import           Data.List                 (find)
import qualified Data.Text as Text
import           Data.Text.Encoding
import           Khan.Internal.Log
import           Khan.Internal.Options
import           Khan.Internal.Types
import           Network.AWS               hiding (accessKey, secretKey)
import           Network.AWS.EC2.Metadata
import           Options                   hiding (boolOption, stringOption)
import           System.Environment
import           System.Exit

accessKey, secretKey :: String
accessKey = "ACCESS_KEY_ID"
secretKey = "SECRET_ACCESS_KEY"

defineOptions "Khan" $ do
    boolOption "kDebug" "debug" False
        "Log debug output"

    boolOption "kDiscovery" "discovery" False
        "Populate options from EC2 metadata and tags."

    maybeTextOption "kRole" "iam-role" Text.empty
        "IAM role - if specified takes precendence over access/secret keys."

    stringOption "kAccess" "access-key" ""
        "AWS access key."

    stringOption "kSecret" "secret-key" ""
        "AWS secret key."

    regionOption "kRegion" "region" Ireland
        "Region to operate in."

deriving instance Show Khan

initialise :: MonadIO m => Khan -> EitherT Error m Khan
initialise k@Khan{..}
    | isJust kRole = right k
    | validKeys k  = right k
    | otherwise    = lookupKeys
  where
    lookupKeys = tryIO' $ do
        acc <- env accessKey kAccess
        sec <- env secretKey kSecret
        return $! k { kAccess = acc, kSecret = sec }

    env k' v
        | null v    = fromMaybe "" <$> lookupEnv k'
        | otherwise = return v

instance Discover Khan where
    discover k@Khan{..} = do
        az  <- BS.unpack . BS.init <$> metadata AvailabilityZone
        reg <- fmapError $ tryRead ("Failed to read region from: " ++ az) az
        return $! k { kRegion = reg }

instance Validate Khan where
    validate Khan{..} = do
        check (kDiscovery && role)
            "--iam-role must be specified in order to use --discovery."
        check (null kAccess && role) $ msg "--access-key" accessKey
        check (null kSecret && role) $ msg "--secret-key" secretKey
      where
        role = isNothing kRole

        msg k e = k ++ " must be specified or "
            ++ e ++ " env must be set if --iam-role is not set."

validKeys :: Khan -> Bool
validKeys Khan{..} = (not . null) `all` [kAccess, kSecret]

check :: (Monad m, Invalid a) => a -> String -> EitherT Error m ()
check x = when (invalid x) . throwT . Error

type SubCommand = Subcommand Khan (EitherT Error IO ())

subCommand :: (Show a, Options a, Discover a, Validate a)
           => String
           -> (a -> AWSContext ())
           -> SubCommand
subCommand name action = Options.subcommand name run
  where
    run k o _ = do
        khan@Khan{..} <- initialise k
        setLogging kDebug
        validate khan
        auth <- credentials $ creds khan
        logInfo $ "Setting region to " ++ show kRegion ++ "..."
        runAWS auth kDebug . within kRegion $ do
            opts <- disco kDiscovery o
            validate opts
            action opts

    disco True  = (logDebug "Performing discovery..." >>) . discover
    disco False = (logDebug "Skipping discovery..."   >>) . return

    creds Khan{..}
        | Just r <- kRole = FromRole $! encodeUtf8 r
        | otherwise       = FromKeys (BS.pack kAccess) (BS.pack kSecret)

data Command = Command
    { cmdName :: String
    , cmdDesc :: String
    , cmdSubs :: [SubCommand]
    }

runProgram :: [Command] -> IO ()
runProgram cmds = do
    args <- getArgs
    case args of
        []     -> help "Additional subcommand required."
        (a:as) -> maybe (help $ "Unknown subcommand \"" ++ a ++ "\".")
                        (run as . cmdSubs) $ find ((== a) . cmdName) cmds
  where
    run argv sub =
        let parsed = parseSubcommand sub argv
        in case parsedSubcommand parsed of
            Just cmd -> runScript $ fmapLT awsError cmd <* logInfo "Exiting..."
            Nothing  -> case parsedError parsed of
                Just ex -> do
                    putStrLn $ parsedHelp parsed
                    putStrLn ex
                    exitFailure
                Nothing -> do
                    putStrLn $ parsedHelp parsed
                    exitSuccess
    help msg = do
        putStrLn $ parsedHelp (parseOptions [] :: ParsedOptions Khan)
        putStrLn $ unlines ("Subcommands:" : map desc cmds ++ [""])
        putStrLn msg
        exitFailure

    desc Command{..} =
        "  " ++ cmdName ++ replicate (28 - length cmdName) ' ' ++ cmdDesc

    awsError (Error s) = s
    awsError (Ex e)    = show e