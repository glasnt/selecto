--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Selecto.Chevalier where

import Chevalier.Types
import Control.Concurrent
import Control.Exception
import Data.Locator
import Data.Monoid ((<>))
import Data.ProtocolBuffers hiding (field)
import Data.Serialize
import Data.Text (append, pack, splitOn, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LazyBuilder
import Marquise.Client
import Snap.Core (urlEncode)
import System.Timeout (timeout)
import System.ZMQ4 hiding (source)

chevalier :: String -> MVar SourceQuery -> IO ()
chevalier chevalier_url query_mvar =
    withContext $ \c -> withSocket c Req $ \s -> do
        connect s chevalier_url
        loop s
  where
    loop s = do
        query <- takeMVar query_mvar

        result <- timeout chevalierTimeout . try $ do
            send s [SendMore] (encodeUtf8 $ sourceOrigin query)
            send s [] $ encodeChevalierRequest $ buildChevalierRequest query
            receive s >>= decodeChevalierResponse

        case result of
            Nothing -> chevalier chevalier_url query_mvar
            Just either_result -> do
                putMVar (sourceResponse query) either_result
                case either_result of
                    Left _ -> chevalier chevalier_url query_mvar
                    _      -> loop s


    chevalierTimeout = 60000000 -- 60s

    encodeChevalierRequest = runPut . encodeMessage

    decodeChevalierResponse bs = do
        let decoded = runGet decodeMessage bs
        parsed <- either (throwIO . BurstDecodeFailure) return decoded
        let chevalier_error = getField $ chevalierError parsed
        case chevalier_error of
            Just e -> throwIO $ ChevalierFailure e
            Nothing -> return $ buildSources parsed

    buildSources r = map urlSafeSource (getField $ sources r)

    urlSafeSource s =
        let a   = getField $ Chevalier.Types.address s
            ad  = pack . show . Address $ fromIntegral a
        in removeTailComma $ LazyBuilder.toLazyText $ LazyBuilder.fromText (urlEncodeText  ad <> "," )

           where 
              urlEncodeText = decodeUtf8 . urlEncode . encodeUtf8 -- fail
              removeTailComma txt
                 | LT.null txt = txt
                 | otherwise   = LT.init txt

    buildChevalierRequest (SourceQuery  q _ page page_size _ ) = SourceRequest
	{ requestTags    = putField $ q
	, startPage      = putField $ Just $ fromIntegral page
	, sourcesPerPage = putField $ Just $ fromIntegral page_size
	, addressKey     = putField Nothing
	}

    buildTags q =
        let k, v = splitOn wildcard q
        in  [SourceTag { field = putField k, value = putField (wrap v)} | k, v <- values ]
      where
        wildcard = "*"
        wrap v =  append wildcard $ append v $ wildcard
