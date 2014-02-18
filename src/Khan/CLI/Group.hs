{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Khan.CLI.Group
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.CLI.Group (commands) where

import qualified Data.Text                as Text
import           Khan.Internal
import           Khan.Internal.Ansible
import qualified Khan.Model.SecurityGroup as Security
import           Khan.Prelude
import           Network.AWS.EC2

data Group = Group
    { gRole    :: !Role
    , gEnv     :: !Env
    , gAnsible :: !Bool
    }

groupParser :: EnvMap -> Parser Group
groupParser env = Group
    <$> roleOption
    <*> envOption env
    <*> ansibleOption

instance Options Group where
    validate Group{..} =
        check gEnv "--env must be specified."

instance Naming Group where
    names Group{..} = unversioned gRole gEnv

data Update = Update
    { uRole    :: !Role
    , uEnv     :: !Env
    , uRules   :: [IpPermissionType]
    , uAnsible :: !Bool
    }

updateParser :: EnvMap -> Parser Update
updateParser env = Update
    <$> roleOption
    <*> envOption env
    <*> many (customOption "rule" "RULE" parseRule mempty
        "tcp|udp|icmp:from_port:to_port:[group|0.0.0.0,...]")
    <*> ansibleOption

instance Options Update

instance Naming Update where
    names Update{..} = unversioned uRole uEnv

commands :: EnvMap -> Mod CommandFields Command
commands env = group "group" "Security Groups." $ mconcat
    [ command "info" info (groupParser env)
        "Display information about a security group."
    , command "create" (modify Security.create) (groupParser env)
        "Create a security group."
    , command "delete" (modify Security.delete) (groupParser env)
        "Delete a security group."
    , command "update" update (updateParser env)
        "Update a security group with a new rule set."
    ]

info :: Common -> Group -> AWS ()
info _ (names -> Names{..}) =
    Security.find groupName >>= maybe (return ()) (log_ . format)
  where
    format SecurityGroupItemType{..} = Text.init . Text.unlines $
        [ "OwnerId             = "  <> sgitOwnerId
        , "GroupId             = "  <> sgitGroupId
        , "GroupName           = "  <> sgitGroupName
        , "GroupDescription    = "  <> sgitGroupDescription
        , "VpcId               = "  <> fromMaybe "''" sgitVpcId
        , "IpPermissionsEgress = [" <> showRules sgitIpPermissionsEgress <> "]"
        , "IpPermissions       = [" <> showRules sgitIpPermissions <> "]"
        ]

modify :: (Text -> AWS Bool) -> Common -> Group -> AWS ()
modify f c g@Group{..}
    | not gAnsible = void $ f groupName
    | otherwise    = capture c "security group {}" [groupName] $ f groupName
  where
    Names{..} = names g

update :: Common -> Update -> AWS ()
update c u@Update{..}
    | not uAnsible = void $ Security.update groupName uRules
    | otherwise    = capture c "security group {}" [groupName] $
        Security.update groupName uRules
  where
    Names{..} = names u
