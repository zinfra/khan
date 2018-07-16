{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE LambdaCase          #-}

-- Module      : Khan.Model.EC2.VPC
-- Copyright   : (c) 2018 Wire Swiss GmbH <backend@wire.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Wire Swiss GmbH <backend@wire.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Model.EC2.VPC
    (
    -- * API
      resolve
    , find
    ) where

import Khan.Internal
import Khan.Prelude    hiding (find, min, max)
import Network.AWS.EC2 hiding (Instance)

-- | Turn a 'VpcRef' into a VPC ID. Might error out.
resolve
    :: VpcRef
    -> AWS Text
resolve (VpcId s) = pure s
resolve (VpcName s) = find s >>= \case
    Nothing -> throwAWS "Couldn't find VPC called {}" [s]
    Just v  -> pure (vitVpcId v)

find
    :: Text           -- ^ VPC name
    -> AWS (Maybe VpcItemType)
find name = do
    say "Searching for VPC {}" [name]
    vpcMay <$> sendCatch (DescribeVpcs [] [Filter "tag:Name" [name]])
  where
    vpcMay (Right x) = headMay . toList $ dvrVpcSet x
    vpcMay (Left  _) = Nothing
