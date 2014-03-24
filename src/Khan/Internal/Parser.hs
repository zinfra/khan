{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Khan.Internal.Parser
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Internal.Parser
    ( parseString
    , parseText
    ) where

import           Control.Monad
import           Data.Attoparsec.Text
import qualified Data.Text            as Text
import           Data.Tuple
import           Khan.Internal.Types
import           Khan.Prelude         hiding (find, min, max)
import           Network.AWS.EC2      hiding (Protocol(..), Instance)
import qualified Network.AWS.EC2      as EC2

parseString :: TextParser a => String -> Either String a
parseString = parseText . Text.pack

parseText :: TextParser a => Text -> Either String a
parseText = parseOnly parser

class TextParser a where
    parser :: Parser a

instance TextParser IpPermissionType where
    parser = do
        p <- parser
        f <- decimal <* char ':'
        t <- decimal <* char ':'
        g <- sepBy1 (eitherP parser parser) (char ',')
        return . uncurry (IpPermissionType p f t)
               . swap
               $ partitionEithers g

instance TextParser IpRange where
    parser = do
        a <- takeTill (== '.') <* char '.'
        b <- takeTill (== '.') <* char '.'
        c <- takeTill (== '.') <* char '.'
        d <- segment
        return . IpRange $ Text.intercalate "." [a, b, c, d]

instance TextParser UserIdGroupPair where
    parser = UserIdGroupPair Nothing Nothing <$> (Just <$> segment)

instance TextParser EC2.Protocol where
    parser = do
        p <- takeTill (== ':') <* char ':'
        case p of
            "tcp"  -> return EC2.TCP
            "udp"  -> return EC2.UDP
            "icmp" -> return EC2.ICMP
            s      -> fail (Text.unpack s)

instance TextParser Protocol where
    parser = do
        p <- takeTill (== ':') <* char ':'
        case p of
            "http"  -> return HTTP
            "https" -> return HTTPS
            "tcp"   -> return TCP
            "ssl"   -> return SSL
            s       -> fail (Text.unpack s)

instance TextParser Frontend where
    parser = FE <$> parser <*> decimal <* endOfInput

instance TextParser Backend where
    parser = BE <$> parser <*> decimal <*> takeText

segment :: Parser Text
segment = Text.pack <$> many1 (satisfy $ notInClass ":|,")
