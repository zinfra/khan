{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

-- Module      : Khan.Model.SecurityGroup
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Model.SecurityGroup
    ( find
    , create
    , update
    , delete
    ) where

import Data.List       ((\\), sort)
import Khan.Internal
import Khan.Prelude    hiding (find, min, max)
import Network.AWS.EC2 hiding (Instance)

find :: Naming a => a -> AWS (Maybe SecurityGroupItemType)
find (names -> Names{..}) = do
    log "Searching for Security Group {}" [groupName]
    mg <- fmap groupMay . sendCatch $ DescribeSecurityGroups [groupName] [] []
    when (isNothing mg) $ log "Unable to find Security Group {}" [groupName]
    return mg
  where
    groupMay (Right x) = headMay . toList $ dshrSecurityGroupInfo x
    groupMay (Left  _) = Nothing

create :: Naming a => a -> AWS SecurityGroupItemType
create (names -> n@Names{..}) = find n >>= maybe f return
  where
    f = do
        log "Security Group {} not found, creating..." [groupName]
        gid <- fmap csgrGroupId . send $
            CreateSecurityGroup groupName groupName Nothing
        log "Security Group {} created." [gid]
        find n >>= noteAWS "Unable to find created Security Group {}" [groupName]

-- FIXME: diff causes rules to be revoked before re-adding, due to shallow diff
-- which doesn't inspect the inner UserIdGroupPairs, this could potentially cause
-- a brief netsplit.
update :: Naming a => a -> [IpPermissionType] -> AWS Bool
update (names -> n@Names{..}) (sort -> rules) = create n >>= f
  where
    f grp = do
        log "Updating Security Group {}..." [groupName]

        let gid  = sgitGroupId grp
            fs   = map (UserIdGroupPair Nothing Nothing . uigGroupName)
            gs   = map (\p -> p { iptGroups = fs $ iptGroups p })
            ps   = sort . gs $ sgitIpPermissions grp
            auth = rules \\ ps
            rev  = ps \\ rules

        unless (null rev) $ do
            log "Revoking {} on {}..." [showRules rev, groupName]
            send_ $ RevokeSecurityGroupIngress (Just gid) Nothing rev

        unless (null auth) $ do
            log "Authorizing {} on {}..." [showRules auth, groupName]
            send_ $ AuthorizeSecurityGroupIngress (Just gid) Nothing auth

        log "Security Group {} updated." [groupName]
        return . not $ null auth && null rev

delete :: Naming a => a -> AWS Bool
delete (names -> n@Names{..}) = find n >>= maybe (return False) f
  where
    f = const $ do
        log "Deleting Security Group {}..." [groupName]
        send_ $ DeleteSecurityGroup (Just groupName) Nothing
        log_ "Security Group deleted."
        return True