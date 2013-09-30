{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}

-- Module      : Khan.Application
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Application (command) where

import           Control.Applicative
import           Control.Concurrent       (threadDelay)
import qualified Control.Concurrent.Async as A
import           Control.Error
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Monoid
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Text.IO             as Text
import           Data.Version
import           Khan.Internal
import           Network.AWS
import           Network.AWS.AutoScaling  hiding (Filter)
import           Network.AWS.EC2
import           Network.AWS.IAM
import           Network.AWS.Internal
import           Pipes
import qualified Pipes.Prelude            as Pipes
import           Text.Show.Pretty

defineOptions "App" $ do
    textOption "aName" "name" ""
        "Name of the application."

    textOption "aEnv" "env" defaultEnv
        "Environment of the application."

    stringOption "aPolicy" "policy" "./role-policy.json"
        "Role policy file."

    stringOption "aTrust" "trust" "./trust-relationship.json"
        "Trust relationship file."

deriving instance Show App

instance Discover App

instance Validate App where
    validate App{..} = do
        check aName "--name must be specified."
        check aEnv  "--env must be specified."
        checkPath aPolicy $ aPolicy ++ " specified by --policy must exist."
        checkPath aTrust  $ aTrust ++ " specified by --trust must exist."

defineOptions "Deploy" $ do
    textOption "dName" "name" ""
        "Name of the application."

    textOption "dEnv" "env" defaultEnv
        "Environment to deploy the application into."

    versionOption "dVersion" "version" defaultVersion
        "Version of the application."

    textsOption "dZones" "zone" []
        "Availability zone in which instances are provisioned."

    integerOption "dGrace" "grace" 20
        "Seconds until healthchecks are activated."

    integerOption "dMin" "min" 1
        "Minimum number of instances."

    integerOption "dMax" "max" 20
        "Maximum number of instances."

    integerOption "dCapacity" "desired" 2
        "Desired number of instances."

    integerOption "dCooldown" "cooldown" 60
        "Seconds between scaling activities."

    instanceTypeOption "dType" "instance" M1_Medium
        "Type of instance to provision."

deriving instance Show Deploy

instance Discover Deploy

instance Validate Deploy where
    validate Deploy{..} = do
        check dName     "--name must be specified."
        check dEnv      "--env must be specified."
        check dZones    "--zone must be specified at least once."
        check dGrace    "--grace must be greater than 0."
        check dMin      "--min must be greater than 0."
        check dMax      "--max must be greater than 0."
        check dCapacity "--desired must be greater than 0."
        check dCooldown "--cooldown must be greater than 0"
        check (defaultVersion == dVersion) "--version must be specified."

defineOptions "Route" $ do
    textOption "rName" "name" ""
        "Name of the application."

    textOption "rEnv" "env" defaultEnv
        "Environment to deploy the application into."

    versionOption "rVersion" "version" defaultVersion
        "Version of the application."

deriving instance Show Route

instance Discover Route

instance Validate Route where
    validate Route{..} = do
        check rName "--name must be specified."
        check rEnv  "--env must be specified."
        check (defaultVersion == rVersion) "--version must be specified."

command :: Command
command = Command "app" "Manage Applications."
    [ subCommand "create" create
    , subCommand "deploy" deploy
    , subCommand "info"   info
    , subCommand "route"  route
    ]
  where
    create App{..} = do
        (policy, trust) <- liftIO $ (,)
            <$> Text.readFile aPolicy
            <*> Text.readFile aTrust

        r <- sendAsync $ CreateRole trust Nothing role
        k <- sendAsync $ CreateKeyPair role

        wait r >>= checkError (("EntityAlreadyExists" ==) . etCode . erError)
        wait k >>= either exist write

        -- Write key and upload to special S3 bucket?

        send_ $ PutRolePolicy policy role role
        logInfo . Text.unpack $ "Updated policy for role " <> role
      where
        role = roleName aName aEnv
        path = Text.unpack role ++ ".pem"

        exist e = do
            checkError
                (("InvalidKeyPair.Duplicate" ==) . ecCode . head . eerErrors)
                (Left e)
            logInfo . Text.unpack $ "KeyPair " <> role <> " exists, not writing."

        write k = do
            liftIO . Text.writeFile path $ ckqKeyMaterial k
            logInfo $ "Wrote new KeyPair to " ++ path

    deploy Deploy{..} = do
        r <- sendAsync $ GetRole role
        k <- sendAsync $ DescribeKeyPairs [role] []
        a <- sendAsync $ DescribeImages [] [] ["self"] [Filter "name" [name]]
        s <- sendAsync $ CreateSecurityGroup role role Nothing

        waitAsync_ r <* logInfo ("Found Role " ++ Text.unpack role)
        waitAsync_ k <* logInfo ("Found KeyPair " ++ Text.unpack role)

        ami <- (listToMaybe . djImagesSet <$> waitAsync a) >>=
            (fmap diritImageId . noteError "Failed to find any matching AMIs")
        logInfo $ "Found AMI " ++ Text.unpack ami

        wait s >>= checkError (("InvalidGroup.Duplicate" ==) . ecCode . head . eerErrors)
        logInfo $ "Found Security Group " ++ Text.unpack role

        -- s   <- async $ do
        --     if 

        --     if isRight res
        --         then send_ $ AuthorizeSecurityGroupIngress Nothing (Just role)
        --             [ flip (IpPermissionType TCP 8080 8080) []
        --                 [ UserIdGroupPair Nothing Nothing (Just role)
        --                 ]
        --             ]
        --         else return res

        -- waitAsync_ s

        -- Create versioned LaunchConfiguration (Exists == OK)
        _ <- send $ CreateLaunchConfiguration
            (Members [])
            Nothing
            (Just role) -- Role
            ami
            Nothing
            dType
            Nothing
            Nothing -- (Just keypair)
            name -- Versioned Name
            Nothing
            (Members []) -- securityGroups
            Nothing
            Nothing

        logInfo $ "Created Launch Configuration " ++ Text.unpack name

        -- Create ASG (Exists == Error!)
        -- let a = CreateAutoScalingGroup
        --             name -- Versioned Name
        --             (Members dZones)
        --             (Just dCooldown)
        --             (Just dCapacity)
        --             (Just dGrace)
        --             (Just "EC2")
        --             launchConfigName
        --             (Members [])
        --             dMin
        --             dMax
        --             Nothing
        --             (Members tags)
        --             (Members [])
        --             Nothing

        -- logInfo $ show a
      where
        role = roleName dName dEnv
        name = versionName role dVersion

    info App{..} = return ()

    route Route{..} = return ()

roleName :: Text -> Text -> Text
roleName name env = Text.concat [name, "-", env]

versionName :: Text -> Version -> Text
versionName name ver = Text.concat [name, "_", safeVersion ver]
