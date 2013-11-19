{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

-- Module      : Khan.Model.Instance
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Model.Instance
    ( findAll
    , run
    , tag
    , wait
    ) where

import Control.Arrow      ((***))
import Control.Concurrent (threadDelay)
import Data.List          (partition)
import Khan.Internal
import Khan.Prelude       hiding (min, max)
import Network.AWS.EC2    hiding (Instance, wait)

findAll :: [InstanceId] -> [Filter] -> AWS [RunningInstancesItemType]
findAll ids = fmap (concatMap ritInstancesSet . dirReservationSet) .
    send . DescribeInstances ids

run :: Naming a
    => a
    -> ImageId
    -> InstanceType
    -> AvailabilityZone
    -> Integer
    -> Integer
    -> Bool
    -> AWS [RunningInstancesItemType]
run (names -> Names{..}) image typ az min max opt =
    fmap rirInstancesSet . send $ RunInstances
        image
        min
        max
        (Just keyName)
        []                            -- Group Ids
        [groupName, sshGroup envName] -- Group Names
        Nothing                       -- User Data
        (Just typ)
        (Just $ PlacementType (Just az) Nothing Nothing)
        Nothing
        Nothing
        []                            -- Block Devices
        (Just $ MonitoringInstanceType True)
        Nothing
        Nothing                       -- FIXME: Disable API Termination
        Nothing                       -- Shutdown Behaviour
        Nothing                       -- Private IP
        Nothing                       -- llient Token
        []                            -- NICs
        [IamInstanceProfileRequestType Nothing (Just profileName)]
        (Just opt)

tag :: Naming a => a -> Text -> [InstanceId] -> AWS ()
tag (names -> n) dom ids = do
    log_ "Tagging instances..."
    send_ . CreateTags (map unInstanceId ids)
          . map (uncurry ResourceTagSetItemType)
          $ defaultTags n dom

wait :: [InstanceId] -> AWS ()
wait []  = log_ "All instances running"
wait ids = do
    xs <- findAll ids []
    let (ps, rs) = join (***) (map riitInstanceId) $ pending xs
    unless (null rs) $
        log "Instances marked as running: {}" [rs]
    unless (null ps) $ do
        log "Instances still pending: {}" [ps]
        log_ "Waiting..."
        liftIO . threadDelay $ 1000000 * 30
    wait ps
  where
    pending = partition (("pending" ==) . istName . riitInstanceState)
