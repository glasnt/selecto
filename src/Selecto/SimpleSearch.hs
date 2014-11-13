--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Selecto.SimpleSearch where

import Chevalier.Types (SourceQuery (..))
import Control.Applicative
import Control.Concurrent hiding (yield)
import Control.Monad.IO.Class
import Data.ByteString.Lazy.Builder (stringUtf8)
import Data.Maybe
import Selecto.Util
import Snap.Core
import System.Timeout (timeout)

simpleSearch :: MVar SourceQuery -> Snap ()
simpleSearch chevalier_mvar = do
    query     <- utf8Or400 =<< fromMaybe "*"  <$> getParam "q"

    -- Address can be used to get information based on an ID
    -- Follows the logic of query - if wildcard, return all things (no filtering on addres)
    address   <- utf8Or400 =<< fromMaybe "*" <$> getParam "address"
    page      <- toInt     <$> fromMaybe "0"  <$> getParam "page"
    page_size <- toInt     <$> fromMaybe "64" <$> getParam "page_size"

    origin <- getParam "origin" >>= (\o -> case o of
        Just bs -> utf8Or400 bs
        Nothing -> writeError 400 $ stringUtf8 "Must specify 'origin'")

    maybe_response <- liftIO $ do
        response_mvar <- newEmptyMVar
        putMVar chevalier_mvar $
            SourceQuery query address page page_size origin response_mvar
        timeout chevalierTimeout $ takeMVar response_mvar

    either_response <- maybe timeoutError return maybe_response
    either chevalierError writeJSON either_response
  where
    chevalierTimeout = 10000000 -- 10 seconds

    chevalierError e =
        writeError 500 $ stringUtf8 ("Exception talking to chevalier backend" ++ show e)

    timeoutError = do
        let msg = "Timed out talking to chevalier backend"
        writeError 500 $ stringUtf8 msg

