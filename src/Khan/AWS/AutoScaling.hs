{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Khan.AWS.AutoScaling
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.AWS.AutoScaling where

import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Khan.Internal
import           Network.AWS
import           Network.AWS.AutoScaling hiding (Filter)
import           Network.AWS.Internal
import           Prelude                 hiding (min, max)

createConfig :: Naming a => a -> Text -> InstanceType -> AWS ()
createConfig (names -> Names{..}) ami typ = do
    c <- sendCatch $ CreateLaunchConfiguration
        (Members [])
        Nothing
        (Just profileName)              -- Instance Profile
        ami                             -- Image Id
        Nothing
        typ                             -- Instance Type
        Nothing
        (Just keyName)                  -- Key Pair Name
        appName                         -- Launch Configuration Name
        Nothing
        (Members [groupName, sshGroup envName]) -- Security Groups
        Nothing
        Nothing                         -- User Data
    verifyAS "AlreadyExists" c
    logInfo "Created Launch Configuration {}" [appName]

deleteConfig :: Naming a => a -> AWS ()
deleteConfig (names -> Names{..}) = do
    send_ $ DeleteLaunchConfiguration appName
    logInfo "Deleted Launch Configuration {}" [appName]

findGroup :: Naming a => a -> AWS (Maybe AutoScalingGroup)
findGroup (names -> Names{..}) = fmap
    (listToMaybe . members . dasgrAutoScalingGroups . dashrDescribeAutoScalingGroupsResult)
    (send $ DescribeAutoScalingGroups (Members [appName]) Nothing Nothing)

createGroup :: Naming a
            => a
            -> Text
            -> [AvailabilityZone]
            -> Integer
            -> Integer
            -> Integer
            -> Integer
            -> Integer
            -> AWS ()
createGroup (names -> n@Names{..}) dom zones cool desired grace min max = do
    reg <- currentRegion
    send_ $ CreateAutoScalingGroup
        appName                        -- Name
        (Members zones)                -- Zones
        (Just cool)                    -- Default Cooldown
        (Just desired)                 -- Desired Capacity
        (Just grace)                   -- Grace Period
        (Just "EC2")                   -- Health Check Type: EC2 | ELB
        appName                        -- Launch Configuration Name
        (Members [])
        max
        min
        Nothing
        (Members $ tags reg) -- Tags
        (Members [])
        Nothing
    logInfo "Created Auto Scaling Group {}" [appName]
    -- Create and update level2 'name' DNS SRV record
    -- Health checks, monitoring, statistics
  where
    tags r = map (uncurry tag) $
        (discoTag, Text.concat [appName, ".", reg]) : requiredTags n dom
      where
        tag k v = Tag k
           (Just True)
           (Just appName)
           (Just "auto-scaling-group")
           (Just v)

        reg = Text.pack $ show r

updateGroup :: Naming a
            => a
            -> Maybe Integer
            -> Maybe Integer
            -> Maybe Integer
            -> Maybe Integer
            -> Maybe Integer
            -> AWS ()
updateGroup (names -> n@Names{..}) cool desired grace min max = do
    AutoScalingGroup{..} <- findGroup n >>=
        noteErrorF "Auto Scaling Group %s doesn't exist." [appName]
    send_ $ UpdateAutoScalingGroup
        appName
        (Members asgAvailabilityZones)
        cool
        desired
        grace
        Nothing
        Nothing
        max
        min
        Nothing
        (Members asgTerminationPolicies)
        Nothing
    logInfo "Updated Auto Scaling Group {}" [appName]

deleteGroup :: Naming a => a -> AWS ()
deleteGroup (names -> Names{..}) = do
    send_ $ DeleteAutoScalingGroup appName (Just True)
    logInfo "Delete of Auto Scaling Group {} in progress" [appName]