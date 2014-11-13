--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Selecto.Chevalier (chevalier)
import Selecto.NagiosSearch (nagiosSearch)
import Snap.Core
import Snap.Http.Server
import System.Environment (getEnv)

main :: IO ()
main = do
    chevalier_url <- getEnv "CHEVALIER_URL"
    chevalier_query_mvar <- newEmptyMVar
    let start_chevalier = chevalier chevalier_url chevalier_query_mvar

    chevalier_threads <- replicateM 16 $ async start_chevalier
    async $ watchThreads chevalier_threads start_chevalier

    quickHttpServe $
        ifTop (writeBS docString) <|>
        route [("suggest/nagios/:origin/host/:host/service/:service", nagiosSearch chevalier_query_mvar)]

  where
    -- Wait for any thread to explode, then restart it whilst logging the
    -- exception.
    watchThreads as restart_action = do
        (a, err) <- waitAnyCatch as

        case err of
            Right _ -> putStrLn "Thread exited normally (shouldn't happen)"
            Left e -> putStrLn $ "Thread exploded: " ++ show e

        restarted <- async restart_action
        threadDelay 1000000

        let remaining_threads = filter (/= a) as
        watchThreads (restarted:remaining_threads) restart_action

    docString = "<html>Machiavelli Suggestion Engine</html>"
